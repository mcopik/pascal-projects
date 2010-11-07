program BasicShader;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ The original Pascal code is : BasicShader.dpr                    }
{ The initial developer of the Pascal code is :                    }
{ Dean Ellis : technomage@delphigamerlcom                          }
{                                                                  }
{ Portions created by Dean Ellis are                               }
{ Copyright (C) 2006 Dean Ellis.                                   }
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
{  Shows how to use OpenGL 2.0 shaders with the SDL libraries      }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
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
{******************************************************************}

uses
  SysUtils,
  Classes,
  gl,
  glu,
  logger,
  sdl,
  glext;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

var
  // This is our SDL surface
  surface : PSDL_Surface;
  ShaderProgram: GLuint;

// free the shader program
// the fragment and vertex shaders will be freed also
procedure FreeBasicShaderProgram;
begin
  if ShaderProgram <> 0 then
    glDeleteProgram(ShaderProgram);
end;

procedure TerminateApplication;
begin
  FreeBasicShaderProgram;
  SDL_QUIT;
  Halt(0);
end;

// Get the shader log infromation

function ShaderGetInfoLog(s: GLuint): String;
var
  blen, slen: Integer;
  infolog: array of Char;
begin

  glGetShaderiv(s, GL_INFO_LOG_LENGTH, @blen);

  if blen > 1 then
  begin
    SetLength(infolog, blen);
    glGetShaderInfoLog(s, blen, @slen, @infolog[0]);
    Result := String(infolog);
    Exit;
  end;

  Result := '';

end;


// create a shader from the string passed in
function CreateShaderFromString(const ASource: string; AShaderType: GLenum): gluint;
var src: PChar;
    len, compiled: glint;
    msg: string;
begin
   Result := glCreateShader(AShaderType);
  len := Length(ASource);
  src := PChar(ASource);
  glShaderSource(Result, 1, @src, @len);
  glCompileShader(Result);
  glGetShaderiv(Result, GL_COMPILE_STATUS, @compiled);
  msg := ShaderGetInfoLog(Result);
  if compiled <> GL_TRUE then
  begin
    Log.LogError( Format('Error compiling shader : %s',[msg]),
      'Main' );
    TerminateApplication;
  end;
end;

// Load a shader from a file

function CreateShaderFromFile(const ASource: string; AShaderType: Glenum): gluint;
var ShaderSource: TStringList;
begin
  ShaderSource := TStringList.Create;
  try
    ShaderSource.LoadFromFile(ASource);
    Result := CreateShaderFromString(ShaderSource.Text, AShaderType);
  finally
    ShaderSource.Free;
  end;
end;

// Loads a shader propgram from file by loading the vertex and fragment shaders
procedure LoadBasicShaderProgram;
var VertexShader, FragmentShader: Gluint;
begin
  // create the shader program
  ShaderProgram := glCreateProgram();
  // load the vertex and fragment shaders
  VertexShader := CreateShaderFromFile('simple.vert', GL_VERTEX_SHADER);
  FragmentShader := CreateShaderFromFile('simple.frag', GL_FRAGMENT_SHADER);

  // attach the shaders to the program

  glAttachShader(ShaderProgram, VertexShader);
  glAttachShader(ShaderProgram, FragmentShader);

  // link the program
  glLinkProgram(ShaderProgram);

  // activate the program
  glUseProgram(ShaderProgram);
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

  // Set our perspective 
  gluPerspective( 45.0, width / height, 0.1, 100.0 );

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

    SDLK_F1 :
      {* F1 key was pressed
       * this toggles fullscreen mode
       *}
      SDL_WM_ToggleFullScreen( surface );

    SDLK_RETURN :
    begin
      if (keysym.Modifier and KMOD_ALT <> 0) then
      begin
      end;
    end;
  end;
end;

// A general OpenGL initialization function.  Sets all of the initial parameters.

procedure InitGL;
// We call this right after our OpenGL window is created.
begin
  if not Load_GL_version_2_0 then
  begin
    Log.LogError( 'Opengl 2.0 API not Supported',
      'Main' );
    TerminateApplication;
  end;
  // Enable smooth shading 
  glShadeModel( GL_SMOOTH );

  // Set the background black 
  glClearColor( 0.0, 0.0, 0.0, 0.0 );

  // Depth buffer setup 
  glClearDepth( 1.0 );

  // Enables Depth Testing 
  glEnable( GL_DEPTH_TEST );

  // The Type Of Depth Test To Do 
  glDepthFunc( GL_LEQUAL );

  // Really Nice Perspective Calculations 
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

  // Load the basic shaders
  LoadBasicShaderProgram;
end;

// The main drawing function.

procedure DrawGLScene;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  // Clear The Screen And The Depth Buffer
  glLoadIdentity; // Reset The View

  glTranslatef( -1.5, 0.0, -6.0 ); // Move Left 1.5 Units And Into The Screen 6.0

  // draw a triangle
  glBegin( GL_POLYGON ); // start drawing a polygon
  glVertex3f( 0.0, 1.0, 0.0 ); // Top
  glVertex3f( 1.0, -1.0, 0.0 ); // Bottom Right
  glVertex3f( -1.0, -1.0, 0.0 ); // Bottom Left
  glEnd; // we're done with the polygon

  glTranslatef( 3.0, 0.0, 0.0 ); // Move Right 3 Units

  // draw a square (quadrilateral)
  glBegin( GL_QUADS ); // start drawing a polygon (4 sided)
  glVertex3f( -1.0, 1.0, 0.0 ); // Top Left
  glVertex3f( 1.0, 1.0, 0.0 ); // Top Right
  glVertex3f( 1.0, -1.0, 0.0 ); // Bottom Right
  glVertex3f( -1.0, -1.0, 0.0 ); // Bottom Left
  glEnd; // done with the polygon

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
begin
  // Initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Could not initialize SDL : %s', [SDL_GetError] ),
      'Main' );
    TerminateApplication;
  end;

  // Fetch the video info 
  videoInfo := SDL_GetVideoInfo;

  if ( videoInfo = nil ) then
  begin
    Log.LogError( Format( 'Video query failed : %s', [SDL_GetError] ),
      'Main' );
    TerminateApplication;
  end;

  // the flags to pass to SDL_SetVideoMode 
  videoFlags := SDL_OPENGL;                  // Enable OpenGL in SDL 
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering 
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware 

  // This checks to see if surfaces can be stored in memory 
  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;

  // This checks if hardware blits can be done * /
  if ( videoInfo.blit_hw <> 0 ) then
    videoFlags := videoFlags or SDL_HWACCEL;

  // Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it 
  SDL_WM_SetCaption( 'Basic Opengl 2.0 Shader Example using JEDI-SDL', nil
    );

  videoflags := videoFlags or SDL_RESIZABLE;    // Enable window resizing 

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [SDL_GetError]
      ),
      'Main' );
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
            Log.LogError( Format( 'Could not get a surface after resize : %s', [SDL_GetError] ),
            'Main' );
            TerminateApplication;
          end;
          ResizeWindow( event.resize.w, event.resize.h );
        end;
      end;
    end;
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.
