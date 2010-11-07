// =============================================================================
//
//  SDLNewtonBasicDemo.pas
//   Very basic demo that shows how to use Newton Game Dynamics.
//
// =============================================================================
//
//   2005/06 by Sascha Willems (www.delphigl.de)
//   Newton Game Dynamics © 2003-2006 by Julio Jerez (www.newtondynamics.com)
//
// =============================================================================
//
//   Contents of this file are subject to the Mozilla Public Licencse 1.1
//   which can be obtained at http://opensource.org/licenses/mozilla1.1.php
//
// =============================================================================

program SDLNewtonBasicDemo;

{$IFDEF FPC}
{$MODE DELPHI}
{$APPTYPE GUI}
{$UNITPATH ../common}
{$ENDIF}

uses
  sdl,
  newton,
  gl,
  glu,
  glext;

type
  TVector3f = record
    X, Y, Z : Single;
  end;
  TMatrix4f = array[ 0..3, 0..3 ] of Single;

 // TNewtonBox =================================================================
 // Since we want to spawn more than just one box we wrap the physics and
 // graphics object into a nice class
  TNewtonBox = class
    NewtonBody : PNewtonBody; // Pointer to the rigid body created by newton
    Matrix : TMatrix4f; // Used to retrieve the matrix from newton
    Size : TVector3f; // Stored for rendering the correct size
    constructor Create( pSize, pPosition : TVector3f; pMass : Single );
    procedure Render;
  end;

var
  Rotation     : Single;

 // Physics objects
  NewtonWorld  : PNewtonWorld;
  NewtonBox    : array of TNewtonBox;

 // Physics timing
  AccTimeSlice : Single;
  TimeLastFrame : Cardinal;

 // SDL
  Surface      : PSDL_Surface;

// =============================================================================
//  V3
// =============================================================================

function V3( pX, pY, pZ : Single ) : TVector3f;
begin
  Result.x := pX;
  Result.y := pY;
  Result.z := pZ;
end;

// *****************************************************************************
//  Newton callbacks
// *****************************************************************************
//  Newton heavily (depending on your design) relies on callbacks. It uses
//  them to calculate forces, so you can with one line change the callback
//  and have a body react in a total differen way.
// *****************************************************************************

// =============================================================================
//  ForceAndTorqueCallback
// =============================================================================
//  This is the callback that is called by Newton for every rigid body and in
//  which all forces (in this simple demo only gravity) are applied.
// =============================================================================

procedure ForceAndTorqueCallback( const body : PNewtonBody ); cdecl;
var
  Mass         : Single;
  Inertia      : TVector3f;
  Force        : TVector3f;
begin
  NewtonBodyGetMassMatrix( Body, @Mass, @Inertia.x, @Inertia.y, @Inertia.z );
  Force := V3( 0, -9.8 * Mass, 0 );
  NewtonBodyAddForce( Body, @Force.x );
end;

// *****************************************************************************
//  TNewtonBox
// *****************************************************************************

// =============================================================================
//  TNewtonBox.Create
// =============================================================================
//  Creates the rigid body of this box. This is the most interesting and also
//  most important function.
// =============================================================================

constructor TNewtonBox.Create( pSize, pPosition : TVector3f; pMass : Single );
var
  Inertia      : TVector3f;
  Collision    : PNewtonCollision;
begin
  Size := pSize;
// Create a box collision
  Collision := NewtonCreateBox( NewtonWorld, pSize.x, pSize.y, pSize.z, nil );
// Create the rigid body
  NewtonBody := NewtonCreateBody( NewtonWorld, Collision );
// Remove the collider, we don't need it anymore
  NewtonReleaseCollision( NewtonWorld, Collision );
// Now we calculate the moment of intertia for this box. Note that a correct
// moment of inertia is CRUCIAL for the CORRECT PHYSICAL BEHAVIOUR of a body,
// so we use an special equation for calculating it
  Inertia.x := pMass * ( pSize.y * pSize.y + pSize.z * pSize.z ) / 12;
  Inertia.y := pMass * ( pSize.x * pSize.x + pSize.z * pSize.z ) / 12;
  Inertia.z := pMass * ( pSize.x * pSize.x + pSize.y * pSize.y ) / 12;
// Set the bodies mass and moment of inertia
  NewtonBodySetMassMatrix( NewtonBody, pMass, Inertia.x, Inertia.y, Inertia.z );
// Now set the position of the body's matrix
  NewtonBodyGetMatrix( NewtonBody, @Matrix[ 0, 0 ] );
  Matrix[ 3, 0 ] := pPosition.x;
  Matrix[ 3, 1 ] := pPosition.y;
  Matrix[ 3, 2 ] := pPosition.z;
  NewtonBodySetMatrix( NewtonBody, @Matrix[ 0, 0 ] );
// Finally set the callback in which the forces on this body will be applied
  NewtonBodySetForceAndTorqueCallBack( NewtonBody, ForceAndTorqueCallBack );
end;

// =============================================================================
//  TNewtonBox.Render
// =============================================================================
//  This function retrives the current matrix of this box from newton and then
//  renders it.
// =============================================================================

procedure TNewtonBox.Render;
begin
// Get current matrix
  NewtonBodyGetMatrix( NewtonBody, @Matrix[ 0, 0 ] );
// Matrices from newton are luckily byte-compatible with matrices from newton,
// which means that we can directly pass them to the GL
  glPushMatrix;
  glMultMatrixf( @Matrix[ 0, 0 ] );
 // Scale to correct size
  glScalef( Size.x / 2, Size.y / 2, Size.z / 2 );
 // Render the box using quads
  glBegin( GL_QUADS );
  glNormal3f( 0, 0, 1 );
  glVertex3f( -1, -1, 1 );
  glVertex3f( 1, -1, 1 );
  glVertex3f( 1, 1, 1 );
  glVertex3f( -1, 1, 1 );
  glNormal3f( 0, 0, -1 );
  glVertex3f( -1, -1, -1 );
  glVertex3f( -1, 1, -1 );
  glVertex3f( 1, 1, -1 );
  glVertex3f( 1, -1, -1 );
  glNormal3f( 0, 1, 0 );
  glVertex3f( -1, 1, -1 );
  glVertex3f( -1, 1, 1 );
  glVertex3f( 1, 1, 1 );
  glVertex3f( 1, 1, -1 );
  glNormal3f( 0, -1, 0 );
  glVertex3f( -1, -1, -1 );
  glVertex3f( 1, -1, -1 );
  glVertex3f( 1, -1, 1 );
  glVertex3f( -1, -1, 1 );
  glNormal3f( 1, 0, 0 );
  glVertex3f( 1, -1, -1 );
  glVertex3f( 1, 1, -1 );
  glVertex3f( 1, 1, 1 );
  glVertex3f( 1, -1, 1 );
  glNormal3f( -1, 0, 0 );
  glVertex3f( -1, -1, -1 );
  glVertex3f( -1, -1, 1 );
  glVertex3f( -1, 1, 1 );
  glVertex3f( -1, 1, -1 );
  glEnd;
  glPopMatrix;
end;

// =============================================================================
//  InitNewton
// =============================================================================
//  Create our NewtonWorld and some bodies in it
// =============================================================================

procedure InitNewton;
var
  TmpV         : TVector3f;
  i            : Integer;
begin
  Randomize;
  NewtonWorld := NewtonCreate( nil, nil );
  NewtonSetPlatformArchitecture( NewtonWorld, 0 );
// Create a static floor first
  SetLength( NewtonBox, 1 );
  NewtonBox[ 0 ] := TNewtonBox.Create( V3( 10, 0.5, 10 ), V3( 0, -2, 0 ), 0 );
// Now create some randomly positioned boxes at different heights
  for i := 0 to 9 do
  begin
    SetLength( NewtonBox, Length( NewtonBox ) + 1 );
    NewtonBox[ High( NewtonBox ) ] := TNewtonBox.Create( V3( 1, 1, 1 ), V3( 0, 3 + i * 1.1, 0 ), 10 );
 // Give the body some random spin (aka "omega")
    TmpV := V3( Random( 5 ), Random( 5 ), Random( 5 ) );
    NewtonBodySetOmega( NewtonBox[ High( NewtonBox ) ].NewtonBody, @TmpV.x );
  end;
end;

// =============================================================================
//  InitGL
// =============================================================================

procedure InitGL;
begin
  glEnable( GL_DEPTH_TEST );
  glDepthFunc( GL_LEQUAL );
  glClearColor( 0, 0, 0.3, 0 );
end;

// =============================================================================
//  Render
// =============================================================================

procedure Render;
var
  i            : Integer;
begin
// Accumulative time slicing
  AccTimeSlice := AccTimeSlice + ( SDL_GetTicks - TimeLastFrame );
  TimeLastFrame := SDL_GetTicks;

  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );
  glEnable( GL_NORMALIZE );

  glTranslatef( 0, 0, -15 );
  glRotatef( 15, 1, 0, 0 );
  glRotatef( 45, 0, 1, 0 );

// Render boxes
  if Length( NewtonBox ) > 0 then
    for i := 0 to High( NewtonBox ) do
      NewtonBox[ i ].Render;

  SDL_GL_SwapBuffers;

  Rotation := Rotation + 0.1;
  if Rotation > 360 then
    Rotation := Rotation - 360;

// Correct timing is crucial for physics calculations if they should run the same
// speed, no matter what FPS. So we use a method called "accumulative timeslicing"
// which will give us the same results across all framerates
  while AccTimeSlice > 12 do
  begin
    NewtonUpdate( NewtonWorld, ( 12 / 1000 ) );
    AccTimeSlice := AccTimeSlice - 12;
  end;
end;

// =============================================================================
//  TerminateApplication
// =============================================================================

procedure TerminateApplication;
var
  i            : Integer;
begin
// Clear box array
  if Length( NewtonBox ) > 0 then
    for i := 0 to High( NewtonBox ) do
      NewtonBox[ i ].Free;
// Destroy the Newton world
  NewtonDestroy( NewtonWorld );
  SDL_QUIT;
  Halt( 0 );
end;

// =============================================================================
//  HandleKeyPress
// =============================================================================

procedure HandleKeyPress( keysym : PSDL_keysym );
begin
  case KeySym.Sym of
    SDLK_ESCAPE : TerminateApplication;
  end;
end;

// =============================================================================
//  ResizeWindow
// =============================================================================

function ResizeWindow( Width, Height : Integer ) : Boolean;
begin
  if Height = 0 then
    Height := 1;
  glViewport( 0, 0, Width, Height );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluPerspective( 60, Width / Height, 1, 128 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  Result := True;
end;

// =============================================================================
//  Main
// =============================================================================
var
  VideoFlags   : UInt32;
  VideoInfo    : PSDL_VideoInfo;
  Event        : TSDL_Event;
  Done         : Boolean;
begin
// Initialize SDL
  if SDL_Init( SDL_INIT_VIDEO ) < 0 then
    TerminateApplication;
// Get video information from SDL
  VideoInfo := SDL_GetVideoInfo;
// Set flags to use OpenGL, double buffer and hardware
  VideoFlags := SDL_OPENGL or SDL_DOUBLEBUF or SDL_HWPALETTE or SDL_RESIZABLE;
// Check if surface can be stored on hardware memory
  if VideoInfo.hw_available <> 0 then
    VideoFlags := VideoFlags or SDL_HWSURFACE
  else
    VideoFlags := VideoFlags or SDL_SWSURFACE;
// Checks hardware blits
  if VideoInfo.blit_hw <> 0 then
    VideoFlags := VideoFlags or SDL_HWACCEL;
// Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
// Window title
  SDL_WM_SetCaption( 'Newton - Simple Demo (SDL) - 2005/06 by Sascha Willems', nil );
// Create SDL surface
  Surface := SDL_SetVideoMode( 640, 480, 32, Videoflags );
  if not Assigned( Surface ) then
    TerminateApplication;

  InitGL;
  ReSizeWindow( 640, 480 );
  InitNewton;
  TimeLastFrame := SDL_GetTicks;
// Main loop
  Done := False;
  while not Done do
  begin
    while SDL_PollEvent( @Event ) = 1 do
    begin
      case event.type_ of
        SDL_QUITEV : Done := true;
        SDL_KEYDOWN : HandleKeyPress( @Event.Key.KeySym );
        SDL_VIDEORESIZE :
          begin
            Surface := SDL_SetVideoMode( event.resize.w, event.resize.h, 32, videoflags );
            if surface = nil then
              TerminateApplication;
            ReSizeWindow( event.resize.w, event.resize.h );
            InitGL;
          end;
      end;
    end;
    Render;
  end;
  TerminateApplication;
end.

