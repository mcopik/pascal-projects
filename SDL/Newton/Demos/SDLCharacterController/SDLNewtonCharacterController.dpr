// =============================================================================
//
//  SDLNewtonCharacterController.pas
//   Sample implementation of a character controller for FPS-like movement
//
//   Parts (and the whole base idea) of the character controller are based on
//   ideas and the implementation of walaber's character controller
//   (www.walaber.com), so thanks go to him
//
// =============================================================================
//
//   Copyright © 2005/06 by Sascha Willems (www.delphigl.de)
//   Newton Game Dynamics © 2003-2006 by Julio Jerez (www.newtondynamics.com)
//
// =============================================================================
//
//   Contents of this file are subject to the Mozilla Public Licencse 1.1
//   which can be obtained at http://opensource.org/licenses/mozilla1.1.php
//
// =============================================================================

program SDLNewtonCharacterController;

{$IFDEF FPC}
{$MODE DELPHI}
{$APPTYPE GUI}
{$UNITPATH ../common}
{$ENDIF}

uses
  gl3DS in '..\common\gl3DS.pas',
  gl3DSMath in '..\common\gl3DSMath.pas',
  Math,
  sdl,
  newton,
  gl,
  glu,
  glext,
  glMatrix in '..\common\glmatrix.pas',
  glMatrixHelper in '..\common\glMatrixHelper.pas';

type

 // TNewtonBox =================================================================
  TNewtonBox = class
    NewtonBody : PNewtonBody;
    Matrix : TMatrix4f;
    Size : TVector3f;
    constructor Create( pSize, pPosition : TVector3f; pMass : Single );
    procedure Render;
  end;

 // TCharacterController =======================================================
  TCharacterController = class
    Body : PNewtonBody;
    ExternalForce : TVector3f; // Is used to apply external forces, e.g. upon key press
    Matrix : TMatrix4f;
    Run : Boolean; // If true, velocity in the callback is scaled
    Rotation : TVector3f;
    Movement : TVector3f;
    Size : TVector3f;
    constructor Create( pSize : TVector3f );
    procedure SetCamera;
    procedure Jump;
    procedure ProcessInput;
  end;

var

 // Physics objects
  NewtonWorld  : PNewtonWorld;
  NewtonBox    : array of TNewtonBox;

 // Physics timing
  AccTimeSlice : Single;
  TimeLastFrame : Cardinal;
  Time         : Single;

 // SDL
  Surface      : PSDL_Surface;
  KeyState     : TKeyStateArr;

 // Level (3DS)
  CollTree3DS  : PNewtonCollision;
  Body3DS      : PNewtonBody;
  Scene3DS     : TAll3DSMesh;

 // Character controller
  Character    : TCharacterController;
  Distance     : Single;

  MousePos     : TPoint;

  Material     : record
    LevelID : Integer;
    PlayerID : Integer;
  end;

  ClientSize   : TPoint;

const
  PlayerSpeed  = 10;
  PlayerMass   = 25;
  PlayerJumpForce = 5000;

// *****************************************************************************
//  Character controller
// *****************************************************************************
//  Following is the callback that transforms the character and also it's class
// *****************************************************************************

// =============================================================================
//  CharacterApplyForceCallback
// =============================================================================
//  This is the most important part of the whole character controller. It's
//  the callback that applies the force to move the character into the
//  direction the player wants him to move
// =============================================================================

procedure CharacterApplyForceCallback( const Body : PNewtonBody ); cdecl;
var
  Mass         : Single;
  Ixx          : Single;
  Iyy          : Single;
  Izz          : Single;
  Force        : TVector3f;
  Velocity     : TVector3f;
  Length       : Single;
  GoalVelocity : TVector3f;
  Accel        : TVector3f;
  UserData     : Pointer;
begin
  Accel := V3( 0, 0, 0 );
  Time := SDL_GetTicks - Time;
// User data was set to the class of the character controll, so we do a typecast
  UserData := NewtonBodyGetUserData( Body );
  if UserData = nil then
    exit;
  with TCharacterController( UserData ) do
  begin
 // First we add the gravity
    NewtonBodyGetMassMatrix( Body, @Mass, @Ixx, @Iyy, @Izz );
    Force := V3( 0, -9.8 * Mass, 0 );
    NewtonBodyAddForce( Body, @Force[ 0 ] );

 // Get matrix from newton
    NewtonBodyGetMatrix( Body, @Matrix[ 0, 0 ] );

 // Now normalize movement vector
    Length := Sqrt( Sqr( Movement[ X ] ) + Sqr( Movement[ Z ] ) );
    if Length = 0 then
      Length := 1;
    Movement := V3( Movement[ X ] / Length, Movement[ Y ], Movement[ Z ] / Length );

 // Get the current velocity
    NewtonBodyGetVelocity( Body, @Velocity[ 0 ] );

 // Get velocity we want to apply on our player
    GoalVelocity := V3( Movement[ X ] * PlayerSpeed, 0, Movement[ Z ] * PlayerSpeed );

 // Scale it by 3 if the player wants to run
    if Run then
      GoalVelocity := V3( GoalVelocity[ x ] * 3, GoalVelocity[ y ] * 3, GoalVelocity[ z ] * 3 );

 // Calculate acceleration needed to get to our goal velocity
    if Time = 0 then
      Time := 1;
    Accel[ X ] := 0.3 * ( ( GoalVelocity[ X ] - Velocity[ X ] ) / ( Time / 10 ) ) * 100;
    Accel[ Z ] := 0.3 * ( ( GoalVelocity[ Z ] - Velocity[ Z ] ) / ( Time / 10 ) ) * 100;

 // Limit acceleration
    if Accel[ X ] > 200 then
      Accel[ X ] := 200;
    if Accel[ X ] < -200 then
      Accel[ X ] := -200;
    if Accel[ Z ] > 200 then
      Accel[ Z ] := 200;
    if Accel[ Z ] < -200 then
      Accel[ Z ] := -200;

 // Now finally add the force to the player's body
    NewtonBodyAddForce( Body, @Accel[ 0 ] );

 // If there is any external force (e.g. due to jumping) add it too
    NewtonBodyAddForce( Body, @ExternalForce[ 0 ] );
    if ( ExternalForce[ x ] <> 0 ) or ( ExternalForce[ y ] <> 0 ) or ( ExternalForce[ z ] <> 0 ) then
      ExternalForce := V3( 0, 0, 0 );
  end;

  Time := SDL_GetTicks;
end;

// =============================================================================
//  CharacterRayCastFilter
// =============================================================================
//  This is just used to pass the distance to any ray-casted objects, so
//  we can see if the player is standing on top of something. Used for jumping
// =============================================================================

function CharacterRayCastFilter( const body : PNewtonBody; const hitNormal : PFloat; collisionID : Int; userData : Pointer; intersectParam : Float ) : Float; cdecl;
begin
  Result := IntersectParam;
  if Body = Character.Body then
    exit;
  Distance := IntersectParam;
end;

// *****************************************************************************
//  TCharacterController
// *****************************************************************************

// =============================================================================
//  TCharacterController.Create
// =============================================================================

constructor TCharacterController.Create( pSize : TVector3f );
const
  UpDir        : array[ 0..2 ] of Single = ( 0, 1, 0 );
var
  Collider     : PNewtonCollision;
  StartMatrix  : TMatrix4f;
begin
  Size := pSize;
// Create an ellipsoid as base for the collider
  Collider := NewtonCreateSphere( NewtonWorld, Size[ x ], Size[ y ], Size[ z ], nil );
// Create rigid body
  Body := NewtonCreateBody( NewtonWorld, Collider );
// We don't need the collider anymore
  NewtonReleaseCollision( NewtonWorld, Collider );

// Disable auto freezing
  NewtonBodySetAutoFreeze( Body, 0 );
// Activate him
  NewtonWorldUnfreezeBody( NewtonWorld, Body );
// Set callback
  NewtonBodySetForceAndTorqueCallBack( Body, CharacterApplyForceCallback );
// Give it a realistic mass
  NewtonBodySetMassMatrix( Body, 12, 1 / 5 * PlayerMass * ( Sqr( Size[ y ] ) + Sqr( Size[ x ] ) ), 1 / 5 * PlayerMass * ( Sqr( Size[ z ] ) + Sqr( Size[ x ] ) ), 1 / 5 * PlayerMass * ( Sqr( Size[ z ] ) + Sqr( Size[ y ] ) ) );
// Set it's position
  Matrix_SetIdentity( StartMatrix );
  Matrix_SetTransform( StartMatrix, V3( 0, 20, 0 ) );
  NewtonBodySetMatrix( Body, @StartMatrix[ 0, 0 ] );
// The player should not fall over, so we attach an up-vector joint to it.
// This type of joint will make the player always stay up in the direction
// that the joint was set to (in this case up on y)
  NewtonConstraintCreateUpVector( NewtonWorld, @UpDir[ 0 ], Body );
// Finally set the user data to point to this class, so we can easily access
// it later on in any of the callbacks
  NewtonBodySetUserData( Body, self );
end;

// =============================================================================
//  TCharacterController.SetCamera
// =============================================================================

procedure TCharacterController.SetCamera;
begin
  NewtonBodyGetMatrix( Body, @Matrix[ 0, 0 ] );
  glRotatef( Rotation[ 0 ], 1, 0, 0 );
  glRotatef( Rotation[ 1 ], 0, -1, 0 );
  glRotatef( Rotation[ 2 ], 0, 0, 1 );
  glTranslatef( -Matrix[ 3, 0 ], -Matrix[ 3, 1 ], -Matrix[ 3, 2 ] );
end;

// =============================================================================
//  TCharacterController.Jump
// =============================================================================

procedure TCharacterController.Jump;
var
  P1, P2       : TVector3f;
begin
// Shoot a ray down from players position to see if the is touching ground
  P1 := V3( Matrix[ 3, 0 ], Matrix[ 3, 1 ], Matrix[ 3, 2 ] );
  P2 := V3( Matrix[ 3, 0 ], Matrix[ 3, 1 ] - Size[ y ] * 1.1, Matrix[ 3, 2 ] );
  Distance := 1.1;
  NewtonWorldRayCast( NewtonWorld, @P1[ x ], @P2[ x ], @CharacterRayCastFilter, nil, nil );
// When the ray hit something, the distance returned by the filter is a value
// smaller than one
  if Distance < 1 then
  begin
 // Store the jumping force, cause forces can only be applied within a callback
    ExternalForce := V3( 0, PlayerJumpForce, 0 );
  end;
end;

// =============================================================================
//  TCharacterController.ProcessInput
// =============================================================================

procedure TCharacterController.ProcessInput;
var
  dX, dZ       : Single;
  Mult         : Single;
begin
// Reset movement vector
  Movement := V3( 0, 0, 0 );
  Mult := 0;
// Forwards and backwards
  if ( KeyState[ SDLK_W ] = 1 ) or ( KeyState[ SDLK_S ] = 1 ) then
  begin
    dX := Sin( DegToRad( Rotation[ 1 ] ) );
    dZ := Cos( DegToRad( Rotation[ 1 ] ) );
    if KeyState[ SDLK_W ] = 1 then
      Mult := -1;
    if KeyState[ SDLK_S ] = 1 then
      Mult := 1;
    Movement[ 0 ] := Movement[ 0 ] + dX * Mult;
    Movement[ 2 ] := Movement[ 2 ] + dZ * Mult;
  end;
// Strafing
  if ( KeyState[ SDLK_A ] = 1 ) or ( KeyState[ SDLK_D ] = 1 ) then
  begin
    dX := Sin( DegToRad( Rotation[ 1 ] + 90 ) );
    dZ := Cos( DegToRad( Rotation[ 1 ] + 90 ) );
    if KeyState[ SDLK_A ] = 1 then
      Mult := -1;
    if KeyState[ SDLK_D ] = 1 then
      Mult := 1;
    Movement[ 0 ] := Movement[ 0 ] + dX * Mult;
    Movement[ 2 ] := Movement[ 2 ] + dZ * Mult;
  end;
  if KeyState[ SDLK_RSHIFT ] = 1 then
    Character.Run := True
  else
    Character.Run := False;
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
  NewtonBodyGetMassMatrix( Body, @Mass, @Inertia[ 0 ], @Inertia[ 1 ], @Inertia[ 2 ] );
  Force := V3( 0, -9.8 * Mass, 0 );
  NewtonBodyAddForce( Body, @Force[ 0 ] );
end;

// =============================================================================
//  Create3DSCollisionTree
// =============================================================================
//  Sends the data of a 3DS scene to newton to lets it build a static
//  tree collision
// =============================================================================

procedure Create3DSCollisionTree( const pScene3DS : TAll3DSMesh );
var
  TmpFace      : array[ 0..2 ] of T3DPoint;
  TmpM         : TMatrix4f;
  Min          : TVector3f;
  Max          : TVector3f;
  m            : Integer;
  f            : Word;
begin
  CollTree3DS := NewtonCreateTreeCollision( NewtonWorld, nil );
// Go through all meshes and add the faces of it to the tree collision
  NewtonTreeCollisionBeginBuild( CollTree3DS );
  if pScene3DS.NumMeshes > 0 then
    for m := 0 to pScene3DS.NumMeshes - 1 do
      if pScene3DS.Mesh[ m ].NumFaces > 0 then
        with pScene3DS.Mesh[ m ] do
        begin
          f := 0;
          repeat
            TmpFace[ 0 ] := Vertex[ Face[ f + 2 ] ];
            TmpFace[ 1 ] := Vertex[ Face[ f + 1 ] ];
            TmpFace[ 2 ] := Vertex[ Face[ f ] ];
            NewtonTreeCollisionAddFace( CollTree3DS, 3, @TmpFace[ 0 ], SizeOf( T3DPoint ), 1 );
            inc( f, 3 );
          until f > NumFaces - 1;
        end;
  NewtonTreeCollisionEndBuild( CollTree3DS, 0 );
  Body3DS := NewtonCreateBody( NewtonWorld, CollTree3DS );
// Set position to 0/0/0
  Matrix_SetIdentity( TmpM );
  NewtonBodySetMatrix( Body3DS, @TmpM[ 0, 0 ] );
// Get AABB and set limits of the newton world
  NewtonCollisionCalculateAABB( CollTree3DS, @TmpM[ 0, 0 ], @Min[ 0 ], @Max[ 0 ] );
  NewtonSetWorldSize( NewtonWorld, @Min[ 0 ], @Max[ 0 ] );
  NewtonReleaseCollision( NewtonWorld, CollTree3DS );
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
  Collision := NewtonCreateBox( NewtonWorld, pSize[ 0 ], pSize[ 1 ], pSize[ 2 ], nil );
// Create the rigid body
  NewtonBody := NewtonCreateBody( NewtonWorld, Collision );
// Remove the collider, we don't need it anymore
  NewtonReleaseCollision( NewtonWorld, Collision );
// Now we calculate the moment of intertia for this box. Note that a correct
// moment of inertia is CRUCIAL for the CORRECT PHYSICAL BEHAVIOUR of a body,
// so we use an special equation for calculating it
  Inertia[ 0 ] := pMass * ( pSize[ 1 ] * pSize[ 1 ] + pSize[ 2 ] * pSize[ 2 ] ) / 12;
  Inertia[ 1 ] := pMass * ( pSize[ 0 ] * pSize[ 0 ] + pSize[ 2 ] * pSize[ 2 ] ) / 12;
  Inertia[ 2 ] := pMass * ( pSize[ 0 ] * pSize[ 0 ] + pSize[ 1 ] * pSize[ 1 ] ) / 12;
// Set the bodies mass and moment of inertia
  NewtonBodySetMassMatrix( NewtonBody, pMass, Inertia[ 0 ], Inertia[ 1 ], Inertia[ 2 ] );
// Now set the position of the body's matrix
  NewtonBodyGetMatrix( NewtonBody, @Matrix[ 0, 0 ] );
  Matrix[ 3, 0 ] := pPosition[ 0 ];
  Matrix[ 3, 1 ] := pPosition[ 1 ];
  Matrix[ 3, 2 ] := pPosition[ 2 ];
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
  glColor3f( 0.5, 0.5, 0.5 );
  glMultMatrixf( @Matrix[ 0, 0 ] );
 // Scale to correct size
  glScalef( Size[ 0 ] / 2, Size[ 1 ] / 2, Size[ 2 ] / 2 );
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
  i, j, k      : Integer;
begin
  Randomize;
  NewtonWorld := NewtonCreate( nil, nil );
  NewtonSetPlatformArchitecture( NewtonWorld, 0 );
// Load level
  Scene3DS := TAll3DSMesh.Create( nil );
  Scene3DS.MasterScale := 0.25;
  Scene3DS.LoadFromFile( 'data/scene.3ds' );
  Create3DSCollisionTree( Scene3DS );
// Create some randomly positioned boxes at different heights
  for i := 0 to 2 do
    for j := 0 to 2 do
      for k := 0 to 2 do
      begin
        SetLength( NewtonBox, Length( NewtonBox ) + 1 );
        NewtonBox[ High( NewtonBox ) ] := TNewtonBox.Create( V3( 1, 1, 1 ), V3( 10 + i * ( 1.1 + Random / 10 ), 10 + j * 1.1, k * ( 1.1 + Random / 10 ) ), 10 );
      end;
// Materials
  Material.LevelID := NewtonMaterialGetDefaultGroupID( NewtonWorld );
  Material.PlayerID := NewtonMaterialCreateGroupID( NewtonWorld );
  NewtonMaterialSetDefaultFriction( NewtonWorld, Material.LevelID, Material.PlayerID, 0, 0 );
  NewtonMaterialSetDefaultElasticity( NewtonWorld, Material.LevelID, Material.PlayerID, 0 );
// Create character controller
  Character := TCharacterController.Create( V3( 1.5, 3.5, 1.5 ) );
  NewtonBodySetMaterialGroupID( Character.Body, Material.PlayerID );
end;

// =============================================================================
//  InitGL
// =============================================================================

procedure InitGL;
begin
  glEnable( GL_DEPTH_TEST );
  glEnable( GL_COLOR_MATERIAL );
  glDepthFunc( GL_LEQUAL );
  glClearColor( 0, 0, 0.3, 0 );
end;

// =============================================================================
//  Render
// =============================================================================

procedure Render;
const
  LightAmbient : array[ 0..2 ] of Single = ( 0.6, 0.6, 0.6 );
var
  i            : Integer;
begin
// Get input
  Character.ProcessInput;

// Accumulative time slicing
  AccTimeSlice := AccTimeSlice + ( SDL_GetTicks - TimeLastFrame );
  TimeLastFrame := SDL_GetTicks;

  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );
  glLightfv( GL_LIGHT0, GL_AMBIENT, @LightAmbient[ x ] );
  glEnable( GL_NORMALIZE );

  Character.SetCamera;

// Render boxes
  if Length( NewtonBox ) > 0 then
    for i := 0 to High( NewtonBox ) do
      NewtonBox[ i ].Render;

  glCullFace( GL_FRONT );
  Scene3DS.Render;
  glCullFace( GL_BACK );

  SDL_GL_SwapBuffers;

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
  Character.Free;
  if Scene3DS <> nil then
    Scene3DS.Free;
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
//  HandleKeyDown
// =============================================================================

procedure HandleKeyDown( keysym : PSDL_keysym );
begin
  case KeySym.Sym of
    SDLK_ESCAPE : TerminateApplication;
    SDLK_SPACE : Character.Jump;
  end;
  KeyState[ KeySym.Sym ] := 1;
end;

// =============================================================================
//  HandleKeyUp
// =============================================================================

procedure HandleKeyUp( keysym : PSDL_keysym );
begin
  KeyState[ KeySym.Sym ] := 0;
end;

// =============================================================================
//  HandleMouseMove
// =============================================================================

procedure HandleMouseMove( pEvent : TSDL_MouseMotionEvent );
begin
  with Character do
  begin
    Rotation[ 1 ] := Rotation[ 1 ] - ( pEvent.x - MousePos.x ) * 0.25;
    if Rotation[ 1 ] > 3600 then
      Rotation[ 1 ] := Rotation[ 1 ] - 360;
    if Rotation[ 1 ] < 0 then
      Rotation[ 1 ] := Rotation[ 1 ] + 360;

    Rotation[ 0 ] := Rotation[ 0 ] + ( pEvent.Y - MousePos.Y ) * 0.25;
    if Rotation[ 0 ] > 80 then
      Rotation[ 0 ] := 80;
    if Rotation[ 0 ] < -80 then
      Rotation[ 0 ] := -80;
  end;

  MousePos.x := pEvent.x;
  MousePos.y := pEvent.y;

  if pEvent.X < 15 then
  begin
    SDL_WarpMouse( ClientSize.x - 17, pEvent.y );
    MousePos.x := ClientSize.x - 17;
  end;
  if pEvent.X > ClientSize.x - 15 then
  begin
    SDL_WarpMouse( 17, pEvent.y );
    MousePos.x := 17
  end;
  if pEvent.Y < 15 then
  begin
    SDL_WarpMouse( pEvent.x, ClientSize.y - 17 );
    MousePos.y := ClientSize.y - 17;
  end;
  if pEvent.Y > ClientSize.y - 15 then
  begin
    SDL_WarpMouse( pEvent.x, 17 );
    MousePos.y := 17;
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
  ClientSize.x := Width;
  ClientSize.y := Height;
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
  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_TIMER ) < 0 then
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
  SDL_WM_SetCaption( 'Newton - Character Controller Demo (SDL) - 2005/06 by Sascha Willems', nil );
// Create SDL surface
  ClientSize.x := 640;
  ClientSize.y := 480;
  Surface := SDL_SetVideoMode( ClientSize.x, ClientSize.y, 32, Videoflags );
  if not Assigned( Surface ) then
    TerminateApplication;
// Grab input
  SDL_WM_GrabInput( SDL_GRAB_ON );
  SDL_ShowCursor( 0 );

  InitGL;
  glext_LoadExtension( 'GL_version_1_3' );
  ReSizeWindow( ClientSize.x, ClientSize.y );
  InitNewton;
  TimeLastFrame := SDL_GetTicks;
  Time := TimeLastFrame;
// Main loop
  Done := False;
  while not Done do
  begin
    while SDL_PollEvent( @Event ) = 1 do
    begin
      case event.type_ of
        SDL_QUITEV : Done := true;
        SDL_KEYDOWN : HandleKeyDown( @Event.Key.KeySym );
        SDL_KEYUP : HandleKeyUp( @Event.Key.KeySym );
        SDL_MOUSEMOTION : HandleMouseMove( Event.motion );
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

