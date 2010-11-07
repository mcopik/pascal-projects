program SDLppFx;
{$APPTYPE CONSOLE}
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


}


uses
  SysUtils,
  sdl,
  sdl_image,
  gl,
  glu,
  logger,
  fxBurn,
  fxBurn2,
  fxExplosion,
  ppFXcore in '..\Pas\ppFXcore.pas',
  ppFXlib in '..\Pas\ppFXlib.pas',
  ppFXrender in '..\Pas\ppFXrender.pas';

var
  surface : PSDL_Surface;
  Done : Boolean;
  Engine: TFxSystem;
  last: UInt32 = 0;


procedure DrawGLScene;
var
  current : UInt32;
begin
  current := SDL_GetTicks;
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glLoadIdentity();
  glTranslatef(0.0,0.0,-2.5);
  glDepthMask(GL_FALSE);
  Engine.advance(current-last);
  Engine.render;
  glDepthMask(GL_TRUE);
  last := current;
//  glFlush;
  SDL_GL_SwapBuffers;
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
  SDL_WM_SetCaption( 'PixelprachtFX using JEDI-SDL', nil );
  surface := SDL_SetVideoMode( 640, 480, 0, videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ),
      'Main' );
    TerminateApplication;
  end;

  Done := False;

  glViewport( 0, 0, 640, 480 );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity( );
  gluPerspective( 60.0, 640 / 480, 0.01, 500 );
  glMatrixMode( GL_MODELVIEW );
  glClearColor(0.0, 0.0, 0.0, 0.5);    // Bildschirm löschen (schwarz)
  glClearDepth(1.0);		       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);	       // Aktiviert Depth Testing
  glDepthFunc(GL_LEQUAL);	       // Bestimmt den Typ des Depth Testing

  Engine := TFxExplosion.create;

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
            case event.key.keysym.sym of
              SDLK_ESCAPE : Done := true;
              SDLK_1, SDLK_KP1 :
                begin
                  if Engine <> nil then FreeAndNil(Engine);
                  Engine := TFxBurn.Create ;
                end;
              SDLK_2, SDLK_KP2 :
                begin
                  if Engine <> nil then FreeAndNil(Engine);
                  Engine := TFxBurn2.Create ;
                end;
              SDLK_3, SDLK_KP3 :
                begin
                  if Engine <> nil then FreeAndNil(Engine);
                  Engine := TFxExplosion.Create ;
                end;

            end;

          end;
      end;
    end;
    // draw the scene
    DrawGLScene;
    SDL_Delay(10);
  end;
  Engine.Free;
  TerminateApplication;
end.
