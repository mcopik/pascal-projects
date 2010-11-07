program GunSite;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                   GunSite demo                                               }
{                                                                              }
{ based on...                                                                  }
{ Focus on SDL example fosdl3_2.cpp by Ernest Pazera.                          }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2003 - 2100 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{ Stephan Schonberg <sschonbe@fgcu.edu>                                        }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Shows how to use the SDLUtils function SDL_2xBlit for zooming              }
{   And to perform an Alpha Mask.                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{  Press C to toggle Windows cursor and Graphics cursor                        }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{ March      18     2003 - DL : Initial creation                               }
{ March      27     2003 - SS : Added clipping for top and left edges          }
{                                                                              }
{                                                                              }
{******************************************************************************}


uses
  SysUtils,
  sdl,
  sdl_image,
  logger,
  sdli386utils;

const
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 32;

var
  // display surface
  FpDisplaySurface : PSDL_Surface = nil;
  FpBackGround : PSDL_Surface = nil;
  FpZoomMask : PSDL_Surface = nil;
  Fp2xImage : PSDL_Surface = nil;
  Fp1xImage : PSDL_Surface = nil;

  // event structure
  FEvent : TSDL_Event;

  // data for cursor
  FCursorData : array[0..31] of Uint32  =
  (
	$00C00100,
	$003C1E00,
	$00436100,
	$807C9F00,
	$60422103,
	$9041C104,
	$50400105,
	$2840010A,
	$24400112,
	$14400114,
	$0A400128,
	$0AC00128,
	$0A000028,
	$09000048,
	$FD0FF85F,
	$01080840,
	$FD0FF85F,
	$09000048,
	$0A000028,
	$0AC00128,
	$0A400128,
	$14400114,
	$24400112,
	$2840010A,
	$50400105,
	$9041C104,
	$60422103,
	$807C9F00,
	$00436100,
	$003C1E00,
	$00C00100,
	$00000000
  );

  // mask for cursor
  FCursorMask : array[0..31] of Uint32  =
  (
	$00C00100,
	$00FC1F00,
	$00FF7F00,
	$80FFFF00,
	$E0C3E103,
	$F0C1C107,
	$70C00107,
	$38C0010E,
	$3CC0011E,
	$1CC0011C,
	$0EC00138,
	$0EC00138,
	$0E000038,
	$0F000078,
	$FF0FF87F,
	$FF0FF87F,
	$FF0FF87F,
	$0F000078,
	$0E000038,
	$0EC00138,
	$0EC00138,
	$1CC0011C,
	$3CC0011E,
	$38C0010E,
	$70C00107,
	$F0C1C107,
	$E0C3E103,
	$80FFFF00,
	$00FF7F00,
	$00FC1F00,
	$00C00100,
	$00000000
  );

  // cursor pointer
  FCursor : PSDL_Cursor;
  FZoomRect : TSDL_Rect;
  FFinalSrcRect : TSDL_Rect;
  FOuterColorKey : Cardinal;
  FInnerColorKey : Cardinal;

function TerminateGracefully : Boolean;
begin
  SDL_Quit;
  result := true;
end;

// main function
const
  TITLE = 'GunSite Demo - showing zooming and masking effect : Press C to Toggle Cursors';
var
  FRectX, FRectY : integer;
  FDone : Boolean;
  FShowCursor : Boolean;
begin
	// initialize SDL
	if ( SDL_Init( SDL_INIT_VIDEO ) = -1 ) then
	begin
		// error initializing SDL

		// report the error
		Log.LogError( 'Could not initialize SDL!', 'Main' );

		// end the program
		halt( 1 );
	end
	else
	begin
		// SDL initialized

		// report success
    Log.LogStatus( 'SDL initialized properly!!', 'Main' );

		// set up to uninitialize SDL at halt
    AddTerminateProc( TerminateGracefully )
	end;

  // set the title bar in environments that support it
  SDL_WM_SetCaption( TITLE, nil );

	// create windowed environment
	FpDisplaySurface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, SDL_DOUBLEBUF );

	// error check
	if ( FpDisplaySurface = nil ) then
	begin
		// report error
    Log.LogError( 'Could not set up display surface!!', 'Main' );
		// halt the program
		halt( 1 );
	end;

  FOuterColorKey := SDL_MapRGB( FpDisplaySurface.format, 255, 0, 255 );
  FInnerColorKey := SDL_MapRGB( FpDisplaySurface.format, 0, 0, 255 );

  FpBackGround := IMG_Load( 'images/commandos2.jpg' );
  FpZoomMask := SDL_LoadBMP( 'images/zoommaskbig.bmp' );

	// create the cursor
	FCursor := SDL_CreateCursor( @FCursorData, @FCursorMask, 32, 32, 15, 15 );

  // set the cursor
  SDL_SetCursor( FCursor );

  // do not show cursor by default
  FShowCursor := false;
  SDL_ShowCursor( SDL_DISABLE  );

  // set ZoomRect initial settings
  FZoomRect.x := 0;
  FZoomRect.y := 0;
  FZoomRect.w := FpZoomMask.w;
  FZoomRect.h := FpZoomMask.h;

  // initialize mouse tracking vars
  FRectX := 0;
  FRectY := 0;

  Fp2xImage := SDL_CreateRGBSurface( SDL_SRCCOLORKEY, ( FpZoomMask.w * 2 ), ( FpZoomMask.h * 2 ), 32,
    FpDisplaySurface.format.RMask, FpDisplaySurface.format.GMask, FpDisplaySurface.format.BMask,
    FpDisplaySurface.format.AMask );
  Fp1xImage := SDL_CreateRGBSurface( SDL_SRCCOLORKEY, FpZoomMask.w, FpZoomMask.h, 32,
    FpDisplaySurface.format.RMask, FpDisplaySurface.format.GMask, FpDisplaySurface.format.BMask,
    FpDisplaySurface.format.AMask );

  // blit background to start off with
  SDL_BlitSurface( FpBackGround, nil, FpDisplaySurface, nil );

  FDone := false;
	// repeat until FDone is set to something else
	while not FDone do
	begin
		// wait for an event
		while SDL_PollEvent( @FEvent ) > 0 do
		begin
      // check for a quit event
      case FEvent.type_ of
        SDL_QUITEV : FDone := true;

        SDL_KEYDOWN :
        begin
          case FEvent.key.keysym.sym of
             SDLK_ESCAPE :
               FDone := true;

             SDLK_C: // toggle cursor
             begin
               FShowCursor := not FShowCursor;
               if FShowCursor then
                 SDL_ShowCursor( SDL_ENABLE  )
               else
                 SDL_ShowCursor( SDL_DISABLE  );
             end;
          end;
        end;

        SDL_MOUSEMOTION :
        begin

          FRectX := FEvent.motion.x - ( FpZoomMask.w shr 1 ); //  shr = div by 2
          FRectY := FEvent.motion.y - ( FpZoomMask.h shr 1 );

          // wipe old gunsite away
          SDL_BlitSurface( FpBackGround, @FZoomRect, FpDisplaySurface, @FZoomRect );

          // preserve the new position of the gunsite?
          // (Without this, the zoomed blit may 'wiggle' a pixel
          //  or two based on previous motion of the mouse)
          FZoomRect.x := FRectX;
          FZoomRect.y := FRectY;

          // copy the normal background image to the 1x surface
          // so we can use the surface with the 2x operation
          if FRectX < 1
          then FZoomRect.w := FZoomRect.w - FRectX;

          if FRectY < 1
          then FZoomRect.h := FZoomRect.h - FRecty;

          // copy the area that we're going to zoom later
          SDL_BlitSurface( FpBackGround, @FZoomRect, Fp1xImage, nil );

          // make a zoomed version
          SDL_2xBlit( Fp1xImage, Fp2xImage );

          // prepare to copy zoomed version back
          FZoomRect.x := ( FpZoomMask.w shr 1 );
          FZoomRect.y := ( FpZoomMask.h shr 1 );

          // copy just the zoomed rectangle back to the overlay
          SDL_BlitSurface( Fp2xImage, @FZoomRect, Fp1xImage, nil );

          FZoomRect.x := FRectX;
          FZoomRect.y := FRectY;
        end;
      end;
		end;

    if not FShowCursor then // if we're showing the GunSite...
    begin
      SDL_SetColorKey( FpZoomMask, SDL_SRCCOLORKEY or SDL_RLEACCEL, FInnerColorKey);
      SDL_BlitSurface( FpZoomMask, nil, Fp1xImage, nil );
      SDL_SetColorKey( Fp1xImage, SDL_SRCCOLORKEY or SDL_RLEACCEL, FOuterColorKey);
    end;

    // prepare the source location for the final blit
    FFinalSrcRect.h := Fp1xImage.h;
    FFinalSrcRect.w := Fp1xImage.w;
    FFinalSrcRect.x := 0;
    FFinalSrcRect.y := 0;

    // adjust blit area if we've reached top or left edges
    if FRectX < 1 then
      begin  // manual clip when gunsite reaches left edge
        FFinalSrcRect.w := Fp1xImage.w + FRectX;
        FFinalSrcRect.x := -FRectX;
       end;

    if FRectY < 1 then
      begin  // manual clip when gunsite reaches top edge
        FFinalSrcRect.h := Fp1xImage.h + FRectY;
        FFinalSrcRect.y := -FRectY;
       end;

    // adjust final blit location if we've reached left or top screen edges
    if FZoomRect.x < 0
    then FZoomRect.x := 0;

    if FZoomRect.y < 0
    then FZoomRect.y := 0;

    // perform the final blit of the gunsite with zoomed area to the display
    SDL_BlitSurface( Fp1xImage, @FFinalSrcRect, FpDisplaySurface, @FZoomRect );

    // let's show the back buffer
    SDL_Flip( FpDisplaySurface );
	end;

	// free the cursor
	SDL_FreeCursor( FCursor );

  SDL_FreeSurface( Fp1xImage );
  SDL_FreeSurface( Fp2xImage );
  SDL_FreeSurface( FpZoomMask );
  SDL_FreeSurface( FpBackGround );
  SDL_FreeSurface( FpDisplaySurface );

	// normal termination
  Log.LogStatus( 'Terminating normally!!', 'Main' );

end.