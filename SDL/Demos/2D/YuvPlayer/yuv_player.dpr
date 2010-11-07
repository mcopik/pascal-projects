program yuv_player;
{******************************************************************}
{                                                                  }
{       Object Pascal Example of a YUV Player                      }
{                                                                  }
{                                                                  }
{ The initial developer of the Pascal code is :                    }
{    David Pethes (imcold)                                         }
{                                                                  }
{ Portions created by David Pethes are                             }
{ Copyright (C) 30.5.2007 David Pethes.                            }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://www.sf.net/projects/jedi-sdl   }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{   Simple application for displaying raw YUV420 planar video      }
{ files on YUV overlay window. The advantage of using the overlay  }
{ is, that we don't need to do the YUV -> RGB colorspace           }
{ conversion by ourselves.                                         }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL in your path .                      }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   compilation:                                                   }
{  tested and working under freepascal (2.x) and delphi (7.0)      }
{                                                                  }
{  more information about YUV:                                     }
{  http://en.wikipedia.org/wiki/Yuv                                }
{                                                                  }
{  planar YUV 4:2:0 image size calculation:                        }
{   y plane size:                                                  }
{     y = height * width                                           }
{  u and v plane size: width and height are divided by two         }
{     (because of chroma subsampling), thus:                       }
{     u = (height/2) * (width/2)                                   }
{     v = (height/2) * (width/2)                                   }
{                                                                  }
{ usage:                                                           }
{   yuv_player <filename> [options]                                }
{   input must be raw YUV 4:2:0 file                               }
{ options:                                                         }
{   -w <int>  set width ( default 320 )                            }
{   -h <int>  set height ( default 240 )                           }
{   -f <int>  set frames per second for playback ( default 25 )    }
{   -u        swap u and v plane                                   }
{ keys:                                                            }
{   q, Esc    quit                                                 }
{   p         pause                                                }
{   r         reset window dimensions to input dimensions          }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   May   30 2007 - DP : Initial release.                          }
{                                                                  }
{                                                                  }
{******************************************************************}

{$APPTYPE CONSOLE}

uses
  sdl;

const
  //autodetect bits per pixel
  DISPLAY_BPP = 0;
  //if the colors look unnatural (orange -> violet), change to SDL_YV12_OVERLAY or use the -u switch
  //it's because the u/v planes in some input files are swapped and we can't know their order beforehand
  OVERLAY_CSP = SDL_IYUV_OVERLAY;
  //OVERLAY_CSP = SDL_YV12_OVERLAY;

  DEFAULT_FPS = 25;
  DEFAULT_WIDTH = 320;
  DEFAULT_HEIGHT = 240;

type
  //yuv image type
  yuv_image_t = record
    plane : array[ 0..2 ] of PUint8;
    size : longword;
  end;


var
  //image properties
  width : word;
  height : word;
  iyuv : yuv_image_t;
  swapuv : boolean;

  //input file properties
  in_file_name : string;
  in_file : file;
  in_frame_count : longword;
  fps : word = DEFAULT_FPS;
  frames_read : longword;
  frame_duration : longword;
  frame_start_time : longword;

  //sdl vars
  screen : PSDL_Surface;
  scr_width : word = DEFAULT_WIDTH;
  scr_height : word = DEFAULT_HEIGHT;
  overlay : PSDL_Overlay;
  drect : TSDL_Rect;
  event : TSDL_Event;

  //player state vars
  sdl_initialized : boolean;
  play : boolean;
  pause : boolean;
  pause_pressed : boolean;


{
  yp_error
  -yuv player error; called when unexpected events occur
}

procedure yp_error( s : string );
begin
  writeln( '[error] ', s );
  if sdl_initialized then
    SDL_Quit;
  halt;
end;


{
  frame_alloc, frame_free
  -compute and allocate memory for planar yuv 4:2:0 image
}

procedure frame_alloc;
begin
  //check sizes
  if ( width mod 2 > 0 ) or ( height mod 2 > 0 ) then
    yp_error( 'width or height is not multiple of 2' );

  //yuv420 plane sizes:
  //y = w*h; u = y/4, v = y/4
  iyuv.size := width * height + ( width * height div 2 );

  //we want to have a continuous memory allocated for our yuv image:
  //this way u and v planes will be located right after the y plane,
  //and we can read all three planes from the input file at once.
  //Swapping the u/v planes is just a matter of swapping the pointers.
  //Allocating separate memory for each planes, using 3xblockread
  //and swapping the u/v planes in display_frame would have the same effect
  //(but due to 3 blockreads it would be probably a little bit slower)
  getmem( iyuv.plane[ 0 ], iyuv.size );

  //set plane pointers
  iyuv.plane[ 1 ] := iyuv.plane[ 0 ];
  inc( iyuv.plane[ 1 ], width * height );
  iyuv.plane[ 2 ] := iyuv.plane[ 1 ];
  if swapuv then
    inc( iyuv.plane[ 1 ], ( width * height div 4 ) )
  else
    inc( iyuv.plane[ 2 ], ( width * height div 4 ) );
end;

procedure frame_free;
begin
  freemem( iyuv.plane[ 0 ] );
end;


{
  open_file, close_file, read_frame, seek_to_start
}

procedure open_file;
begin
  Assign( in_file, in_file_name );
  Reset( in_file, 1 );
  in_frame_count := FileSize( in_file ) div iyuv.size;
  if ( in_frame_count = 0 ) then
    yp_error( 'input contains no frames' );
end;

procedure close_file;
begin
  Close( in_file );
end;

procedure read_frame;
begin
  if not ( eof( in_file ) ) then
    blockread( in_file, iyuv.plane[ 0 ]^, iyuv.size )
  else
    yp_error( 'read past the eof' );
end;

procedure seek_to_start;
begin
  Seek( in_file, 0 );
end;


{
  init_sdl, init_display, init_overlay, quit_sdl
}

procedure init_sdl;
begin
  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_TIMER ) > 0 then
    yp_error( 'yp_init_sdl: SDL_Init failed' );
  sdl_initialized := true;
end;

procedure init_display;
var
  flags : UInt32;
begin
  flags := SDL_SWSURFACE or SDL_RESIZABLE;
  screen := SDL_SetVideoMode( scr_width, scr_height, DISPLAY_BPP, flags );
  if screen = nil then
    yp_error( 'yp_init_sdl: SDL_SetVideoMode failed' );
  SDL_WM_SetCaption( 'yuv player', nil );



  drect.x := 0;
  drect.y := 0;
  drect.w := scr_width;
  drect.h := scr_height;
end;

procedure init_overlay;
begin
  overlay := SDL_CreateYUVOverlay( width, height, OVERLAY_CSP, screen );
  if overlay = nil then
    yp_error( 'yp_init_sdl: SDL_CreateYUVOverlay failed' );
end;

procedure quit_sdl;
begin
  SDL_FreeYUVOverlay( overlay );
  SDL_FreeSurface( screen );
  SDL_Quit;
end;


{
  mem_copy
  -copy (lines*src_width) bytes from src to dest,
   skip (dest_width - src_width) bytes after each line
}

procedure mem_copy( src, dest : PUint8; src_width, dest_width : word; lines : word );
var
  c : word;
begin
  for c := 1 to lines do
  begin
    move( src^, dest^, src_width );
    inc( src, src_width );
    inc( dest, dest_width );
  end;
end;

{
  display_frame
  -image is displayed by copying to overlay and by calling SDL_DisplayYUVOverlay().
   Overlay must be locked during manipulation. Since YUV420 is a planar format,
   we copy the planes separately - overlay^.pixels points to 3 pointers to the actual overlay pixels
   (planes). Also, the pitch of the overlay planes ('width of the plane') can be
   bigger than user-defined width defined at SDL_CreateYUVOverlay();
   so we will be using the mem_copy() procedure, which copies the plane per lines.
}

procedure display_frame;
var
  pitch : PUInt16;
  plane : PPUInt8;
begin
  SDL_LockYUVOverlay( overlay );

  pitch := overlay^.pitches; //set to first pitch
  plane := PPUInt8( overlay^.pixels ); //set to first plane
  mem_copy( iyuv.plane[ 0 ], plane^, width, pitch^, height ); //copy y plane to overlay
  inc( pitch ); //next pitch
  inc( plane ); //next plane
  mem_copy( iyuv.plane[ 1 ], plane^, width div 2, pitch^, height div 2 ); //copy
  inc( pitch );
  inc( plane );
  mem_copy( iyuv.plane[ 2 ], plane^, width div 2, pitch^, height div 2 );

  SDL_UnlockYUVOverlay( overlay );
  SDL_DisplayYUVOverlay( overlay, @drect );
end;


{
  help
  -print info
}

procedure help;
begin
  writeln( 'usage:  yuv_player <filename> [options]' );
  writeln( '  input must be raw planar YUV 4:2:0 file' );
  writeln( 'options:' );
  writeln( '  -w <int>  set width for playback (', DEFAULT_WIDTH, ')' );
  writeln( '  -h <int>  set height for playback (', DEFAULT_HEIGHT, ')' );
  writeln( '  -f <int>  set frames per second for playback (', DEFAULT_FPS, ')' );
  writeln( '  -u        swap u and v plane' );
  writeln( '  -?        print help' );
  writeln( 'keys:' );
  writeln( '  q, Esc    quit' );
  writeln( '  p         pause' );
  writeln( '  r         reset window dimensions to input dimensions');
end;

{
  parse_options
  -get options from command line: read input name, width and height from right to left
   and the rest from left to right
}

procedure parse_options;
var
  c : integer;
  e : integer;
begin
  if ParamCount < 1 then
  begin
    help;
    halt;
  end;

  width  := scr_width;
  height := scr_height;
  in_file_name := ParamStr( 1 );

  if ParamCount > 1 then
  begin
    for c := 2 to ParamCount do
    begin
      if ParamStr( c ) = '-u' then
        swapuv := true;
      if ParamStr( c ) = '-f' then
      begin
        val( ParamStr( c + 1 ), fps, e );
        if e <> 0 then
          yp_error( 'wrong parameters' );
      end;
      if ParamStr( c ) = '-w' then
      begin
        val( ParamStr( c + 1 ), width, e );
        if e <> 0 then
          yp_error( 'wrong parameters' );
        scr_width := width;
      end;
      if ParamStr( c ) = '-h' then
      begin
        val( ParamStr( c + 1 ), height, e );
        if e <> 0 then
          yp_error( 'wrong parameters' );
        scr_height := height;
      end;
      if ParamStr( c ) = '-?' then
        help;
    end;
  end;
end;


(******************************************************************************
  main
******************************************************************************)
begin
  writeln( 'yuv player example' );

//player initialization
  parse_options;
  frame_alloc;
  open_file;
  init_sdl;
  init_display;
  init_overlay;
  writeln( 'playing: ', in_file_name, ', ', in_frame_count, ' frames, ',
    width, 'x', height, ' @ ', fps, ' fps' );

//playback setup
  frames_read := 0;
  frame_start_time := 0;
  frame_duration := 1000 div fps;
  play := true;
  pause := false;
  pause_pressed := false;

//event loop
  while play do
  begin
    SDL_PollEvent( @event );
    case event.type_ of
      //quit
      SDL_QUITEV :
        play := false;
      //key pressed
      SDL_KEYDOWN :
        case event.key.keysym.sym of
          SDLK_q,
            SDLK_ESCAPE :
            play := false;
          SDLK_p :
            if not ( pause_pressed ) then
            begin
              pause := not ( pause );
              if pause then
                write( 'paused', #13 )
              else
                write( '      ', #13 ); //clear the 'pause' text
              pause_pressed := true;
            end;
          //reset window dimensions to input dimensions
          SDLK_r :
            begin
              scr_width := width;
              scr_height := height;
              init_display;
            end;
        end;
      //key released
      SDL_KEYUP :
        case event.key.keysym.sym of
          SDLK_p :
            pause_pressed := false;
        end;
      //resize
      SDL_VIDEORESIZE :
        begin
          scr_width := event.resize.w;
          scr_height := event.resize.h;
          init_display;
        end;
    end;

    //if the player is not paused and it's time for a new frame,
    //read frame from file and display it
    if not ( pause ) and ( ( SDL_GetTicks - frame_start_time ) >= frame_duration ) then
    begin
      frame_start_time := SDL_GetTicks;
      read_frame;
      inc( frames_read );
      write( frames_read : 4, #13 );
        //if we are at the end of file then set file to beginning and play again
      if frames_read = in_frame_count then
      begin
        seek_to_start;
        frames_read := 0;
      end;
      display_frame;
    end;

    //save some cpu ticks for other applications too, we don't want/need a 100% cpu usage
    SDL_Delay( 1 );
  end;

//free resources
  quit_sdl;
  close_file;
  frame_free;

  writeln( #10'playback finished.' );
end.

