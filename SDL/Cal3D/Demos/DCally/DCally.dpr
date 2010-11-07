program DCally;

// JEDI-SDL port of Cal3D (C) 2004 Dominique Louis <Dominique@SavageSoftware.com.au>
// Delphi 6PE port of Cal3D (C) 2001 Paul TOTH <tothpaul@free.fr>
// Cal3D-0.6 (4. Aug. 2001) (C) 2001 Bruno 'Beosil' Heidelberger

{
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Programming Notes
-----------------
When compiling this demo, whether you are using Delphi/Kylix or FreePascal
  or some other Pascal compiler, make sure you define NOT_OO somewhere so that
  the Input classes use the correct method types.

}


uses
  SysUtils,
  menu,
  demo,
  model,
  sdl,
  sdlinput,
  logger;

var
  idlemax : integer;
  surface : PSDL_Surface;
  Done : Boolean;

procedure idleFunc; cdecl;
var
  t : integer;
begin
  t := SDL_GetTicks;
  TheDemo.OnIdle;
  t := SDL_GetTicks - t;
  if t >= idlemax then
  begin
    idlemax := t;
  end;
end;

var
  mousemax : integer;

procedure MouseButtonDownEvent( Button : Integer; Shift : TSDLMod; MousePos : TPoint );
var
  t : integer;
begin
  t := SDL_GetTicks;
  // redirect the message to the demo instance
  theDemo.onMouseButtonDown( button, MousePos.x, theDemo.Height - MousePos.y - 1 );

  t := SDL_GetTicks - t;
  if ( t > 0 ) and ( t > mousemax ) then
  begin
    mousemax := t;
  end;
end;

procedure MouseButtonUpEvent( Button : Integer; Shift : TSDLMod; MousePos : TPoint );
var
  t : integer;
begin
  t := SDL_GetTicks;
  theDemo.onMouseButtonUp( button, MousePos.x, theDemo.Height - MousePos.y - 1 );

  t := SDL_GetTicks - t;
  if ( t > 0 ) and ( t > mousemax ) then
  begin
    mousemax := t;
  end;
end;

var
  motionMax : integer;

procedure MouseMoveEvent( Shift : TSDLMod; CurrentPos : TPoint; RelativePos : TPoint );
var
  t : integer;
begin
  t := SDL_GetTicks;
  // redirect the message to the demo instance
  theDemo.onMouseMove( CurrentPos.x, theDemo.Height - CurrentPos.y - 1 );
  t := SDL_GetTicks - t;
  if ( t >= motionMax ) and ( t > 0 ) then
  begin
    motionmax := t;
  end;
end;

procedure ResizeWindow( width, height : integer );
begin
  // set the new width/height values
  theDemo.setDimension( width, height );
end;
var
  displaymax : integer;

procedure DrawGLScene;
var
  t : integer;
begin
  t := SDL_GetTicks;
  // render the scene
  theDemo.onRender;
  t := SDL_GetTicks - t;
  if t > displaymax then
  begin
    displaymax := t;
  end;
end;

procedure KeyBoardEvent( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 );
begin
  // redirect the message to the demo instance
  //theDemo.onKey( Key., x, theDemo.Height - y - 1 );
  if Key = SDLK_ESCAPE then
    Done := true;
end;

procedure TerminateApplication;
begin
  SDL_Quit;
  Halt( 0 );
end;

var
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
  DInputManager : TSDLInputManager;
begin
  // Initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Could not initialize SDL : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;
  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;
  if ( videoInfo = nil ) then
  begin
    Log.LogError( Format( 'Video query failed : %s', [ SDL_GetError ] ),
      'Main' );
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
  SDL_WM_SetCaption( 'DCal3D using JEDI-SDL', nil );
  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing
  surface := SDL_SetVideoMode( 640, 480, 32, videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ),
      'Main' );
    TerminateApplication;
  end;

  // create our demo instance
  TheDemo := TDemo.Create;

  // Make sure that the InputManager is started
  DInputManager := TSDLInputManager.Create( [ itKeyBoard, itMouse ] );
  DInputManager.KeyBoard.OnKeyDown := KeyBoardEvent;
  DInputManager.KeyBoard.OnKeyUp := KeyBoardEvent;
  DInputManager.Mouse.OnMouseDown := MouseButtonDownEvent;
  DInputManager.Mouse.OnMouseUp := MouseButtonUpEvent;
  DInputManager.Mouse.OnMouseMove := MouseMoveEvent;
  DInputManager.Mouse.HideCursor;

  // Loop, drawing and checking events
  // initialize our demo instance
  if ( theDemo.onInit = false ) then
  begin
    halt;
  end;

  ReSizeWindow( 640, 480 );
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
        SDL_VIDEORESIZE :
          begin
            surface := SDL_SetVideoMode( event.resize.w, event.resize.h,
              32, videoflags );
            if ( surface = nil ) then
            begin
              Log.LogError( Format( 'Could not get a surface after resize : %s',
                [ SDL_GetError ] ),
                'Main' );
              TerminateApplication;
            end;
            //InitialiseScene;
            ResizeWindow( event.resize.w, event.resize.h );
          end;
      end;
      DInputManager.UpdateInputs( event );
    end;
    idleFunc;

    // draw the scene
    DrawGLScene;
  end;
  DInputManager.Free;
  theDemo.onShutdown;
  theDemo.Free;
  TerminateApplication;
end.

