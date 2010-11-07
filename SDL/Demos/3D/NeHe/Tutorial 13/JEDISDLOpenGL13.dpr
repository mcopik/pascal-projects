program JEDISDLOpenGL13;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Ti Leggett <leggett@eecs.tulane.edu>,  are   }
{ Copyright (C) 2001 Ti Leggett.                                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson13.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL13.dpr                }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
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
{  Shows how to use OpenGL with the SDL libraries                  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Also Makes uses of Mike Lischke's Cross-Platform OpenGL header.}
{   You can pick it up from...                                     }
{   http://www.lischke-online.de/Graphics.html#OpenGL12            }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   Jan   25 2005 - DL : Initial translation.                      }
{                                                                  }
{******************************************************************}
uses
  SysUtils,
  sdl,
  gl,
  glext;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

  MAX_TEXTURES = 1;

var
  // This is our SDL surface
  surface : PSDL_Surface;

  base : GLuint; // Base Display List For The Font
  texture : array[ 0..MAX_TEXTURES - 1 ] of GLuInt; // Storage For 5 Textures

  //Status indicator
  Status : Boolean = false;

  adjust : integer = 3;
  lives : integer = 5;
  level : integer = 1;
  level2 : integer = 1;
  stage : integer = 1;

procedure TerminateApplication;
begin
  // Clean up our font list
  glDeleteLists( base, 256 );

  // Clean up our textures
  glDeleteTextures( MAX_TEXTURES, @texture[ 0 ] );


  SDL_FreeSurface( surface );

  // clean up the window
  SDL_QUIT;

  //UnLoadOpenGL;
  Halt( 0 );
end;

// Load Bitmaps And Convert To Textures

function LoadGLTextures : Boolean;
var
  // Create storage space for the texture
  TextureImage : array[ 0..MAX_TEXTURES - 1 ] of PSDL_Surface;
  loop : integer;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage[ 0 ] := SDL_LoadBMP( 'images/font.bmp' );

  if ( TextureImage[ 0 ] <> nil ) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures( MAX_TEXTURES, @texture[ 0 ] );

    for loop := 0 to MAX_TEXTURES - 1 do
    begin
      glBindTexture( GL_TEXTURE_2D, texture[ loop ] );

      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage[ loop ].w,
        TextureImage[ loop ].h, 0, GL_BGR,
        GL_UNSIGNED_BYTE,
        TextureImage[ loop ].pixels );

      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    end;
  end
  else
  begin
    //Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ), 'LoadGLTextures' );
    TerminateApplication;
  end;

  // Free up any memory we may have used
  for loop := 0 to MAX_TEXTURES - 1 do
  begin
    if TextureImage[ loop ] <> nil then
      SDL_FreeSurface( TextureImage[ loop ] );
  end;

  result := Status;
end;

// function to build our font list

procedure BuildFont;
var
  loop : GLuint; // Loop variable               
  cx : single; // Holds Our X Character Coord 
  cy : single; // Holds Our Y Character Coord 
begin
  // Creating 256 Display List 
  base := glGenLists( 256 );
  /// Select Our Font Texture 
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

  // Loop Through All 256 Lists 
  for loop := 0 to 255 do
  begin
    {* NOTE:
     *  BMPs are stored with the top-leftmost pixel being the
     * last byte and the bottom-rightmost pixel being the first
     * byte. So an image that is displayed as
     *    1 0
     *    0 0
     * is represented data-wise like
     *    0 0
     *    0 1
     * And because SDL_LoadBMP loads the raw data without
     * translating to how it is thought of when viewed we need
     * to start at the bottom-right corner of the data and work
     * backwards to get everything properly. So the below code has
     * been modified to reflect this. Examine how this is done and
     * how the original tutorial is done to grasp the differences.
     *
     * As a side note BMPs are also stored as BGR instead of RGB
     * and that is why we load the texture using GL_BGR. It's
     * bass-ackwards I know but whattaya gonna do?
     *}

    // X Position Of Current Character
    cx := 1 - ( loop mod 16 ) / 16.0;
    // Y Position Of Current Character 
    cy := 1 - ( loop div 16 ) / 16.0;

    // Start Building A List
    glNewList( base + ( 255 - loop ), GL_COMPILE );
    // Use A Quad For Each Character
    glBegin( GL_QUADS );
    // Texture Coord (Bottom Left)
    glTexCoord2f( cx - 0.0625, cy );
    // Vertex Coord (Bottom Left) 
    glVertex2i( 0, 16 );

    // Texture Coord (Bottom Right)
    glTexCoord2f( cx, cy );
    // Vertex Coord (Bottom Right)
    glVertex2i( 16, 16 );

    // Texture Coord (Top Right)
    glTexCoord2f( cx, cy - 0.0625 );
    // Vertex Coord (Top Right)
    glVertex2i( 16, 0 );

    // Texture Coord (Top Left)
    glTexCoord2f( cx - 0.0625, cy - 0.0625 );
    // Vertex Coord (Top Left)
    glVertex2i( 0, 0 );
    glEnd;

    // Move To The Left Of The Character
    glTranslated( 15, 0, 0 );
    glEndList;
  end;
end;

// function to reset our viewport after a window resize

function ResizeWindow( width : integer; height : integer ) : Boolean;
begin
  // Protect against a divide by zero
  if ( height = 0 ) then
    height := 1;
  // Setup our viewport.
  glViewport( 0, 0, width, height );
  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our ortho perspective/view
  glOrtho( 0.0, width, height, 0.0, -1.0, 1.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );
  // Reset The View
  glLoadIdentity;
  result := true;
end;

// function to handle key press events

procedure HandleKeyPress( keysym : PSDL_keysym );
begin
  case keysym.sym of
    SDLK_ESCAPE :
      // ESC key was pressed
      TerminateApplication;

    SDLK_SPACE :
      begin

      end;

    SDLK_a :
      begin
        
      end;

    SDLK_RIGHT :
      begin
        
      end;

    SDLK_LEFT :
      begin


      end;

    SDLK_UP :
      begin
        
      end;

    SDLK_DOWN :
      begin
        
      end;

    SDLK_RETURN :
      begin
        if ( keysym.Modifier and KMOD_ALT <> 0 ) then
        begin
          {* Alt+Enter key was pressed
           * this toggles fullscreen mode
           *}
          SDL_WM_ToggleFullScreen( surface );
        end;
      end;
  end;
end;
// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL : Boolean;
begin
  // Load in the texture
  if ( not LoadGLTextures ) then
  begin
    result := false;
    exit;
  end;

  // Build The Font
  BuildFont;

  // Enable smooth shading
  glShadeModel( GL_SMOOTH );

  // Set the background black
  glClearColor( 0.0, 0.0, 0.0, 0.5 );

  // Depth buffer setup
  glClearDepth( 1.0 );

  // Set Line Antialiasing
  glHint( GL_LINE_SMOOTH_HINT, GL_NICEST );

  // Enable Blending
  glEnable( GL_BLEND );

  // Type Of Blending To Use
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  result := true;
end;

// Function to draw the string on the screen
procedure glDrawText( x, y : GLint; Text : string; fontset : integer );
begin
   // Did User Choose An Invalid Character Set? 
  if ( fontset > 1 ) then
    fontset := 1;

  // Enable Texture Mapping 
  glEnable( GL_TEXTURE_2D );

  // Select our texture 
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

  // Disable depth testing 
  glDisable( GL_DEPTH_TEST );

  // Reset The Modelview Matrix 
  glLoadIdentity;

  // Position The Text (0,0 - Bottom Left) 
  glTranslated( x, y, 0 );

  // Choose The Font Set (0 or 1) 
  glListBase( base - 32 + ( 128 * fontset ) );

  // If Set 0 Is Being Used Enlarge Font 
  if ( fontset = 0 ) then
    // Enlarge Font Width And Height 
    glScalef( 1.5, 2.0, 1.0 );

  // Write The Text To The Screen 
  glCallLists( Length( Text ), GL_BYTE, PChar( Text ) );

  // Disable Texture Mapping 
  glDisable( GL_TEXTURE_2D );

  // Re-enable Depth Testing 
  glEnable( GL_DEPTH_TEST );
end;

// The main drawing function.

procedure DrawGLScene;
begin
  // Clear The Screen And The Depth Buffer
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  // Reset the view
  glLoadIdentity;

  // Set Color To Yellow
  glColor3f( 1.0, 1.0, 0.0 );

  // Write Bitmap Fonts On The Screen
  glDrawText( ( surface.w div 2 ) - 100, ( surface.h div 2 ) - 38, 'Bitmap Fonts', 1 );

  // Write using On The Screen
  glDrawText( ( surface.w div 2 ) - 80, ( surface.h div 2 ) - 18, 'using ', 1 );

  // Set Color To Purple
  glColor3f( 1.0, 0.5, 1.0 );

  // Write JEDI-SDL On The Screen
  glDrawText( ( surface.w div 2 ), ( surface.h div 2 ) - 22, 'JEDI-SDL', 0 );

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;

begin
  // Initialize SDL with Audio
  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_AUDIO ) < 0 ) then
  begin
    // Log.LogError( Format( 'Could not initialize SDL : %s', [ SDL_GetError ] ), 'Main' );
    TerminateApplication;
  end;

  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;
  if ( videoInfo = nil ) then
  begin
    // Log.LogError( Format( 'Video query failed : %s', [ SDL_GetError ] ),  'Main' );
    TerminateApplication;
  end;
  // the flags to pass to SDL_SetVideoMode
  videoFlags := SDL_OPENGL; // Enable OpenGL in SDL
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware
  // This checks to see if surfaces can be stored in memory
  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;
  // This checks if hardware blits can be done 
  if ( videoInfo.blit_hw <> 0 ) then
    videoFlags := videoFlags or SDL_HWACCEL;
  // Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it
  SDL_WM_SetCaption( 'Jeff Molofee''s OpenGL Code Tutorial 13 using JEDI-SDL', nil
    );
  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing
  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    // Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]),'Main' );
    TerminateApplication;
  end;

  // Enable key repeat 
  if SDL_EnableKeyRepeat( 100, SDL_DEFAULT_REPEAT_INTERVAL ) = -1 then
  begin
    // Log.LogWarning( Format( 'Setting keyboard repeat failed: %s', [ SDL_GetError ] ), 'Main' );
    TerminateApplication;
  end;

  // Loop, drawing and checking events
  InitGL;

  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

  Done := False;
  while ( not Done ) do
  begin
    // This could go in a separate function
    while ( SDL_PollEvent( @event ) = 1 ) do
    begin
      case event.type_ of
        SDL_QUITEV :
          begin
            Done := true;
          end;
        SDL_KEYDOWN :
          begin
            // handle key presses
            HandleKeyPress( @event.key.keysym );
          end;
        SDL_VIDEORESIZE :
          begin
            surface := SDL_SetVideoMode( event.resize.w, event.resize.h, SCREEN_BPP, videoflags );
            if ( surface = nil ) then
            begin
              // Log.LogError( Format( 'Could not get a surface after resize : %s', [ SDL_GetError ] ), 'Main' );
              TerminateApplication;
            end;
            InitGL;
            ResizeWindow( event.resize.w, event.resize.h );
          end;
      end;
    end;
    
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.


