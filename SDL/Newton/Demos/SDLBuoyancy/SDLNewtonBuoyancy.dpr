// =============================================================================
//
//  SDLNewtonBuoyancy.pas
//   Simple demo to show how to use newton's buoyancy feature to simulate bodies
//   immersed in a fluid.
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

program SDLNewtonBuoyancy;

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
  glext,
  glPrimitives in '..\common\glPrimitives.pas',
  glMatrixHelper in '..\common\glMatrixHelper.pas';

const
  Gravity      = -9.8;
  BasinSize    = 50;
  MaxBodyCount = 50;

var
  // Newton
  NewtonWorld  : PNewtonWorld;

  // Misc
  ShowDebug    : Boolean;
  MousePos     : TPoint;
  SceneRotation : TVector3f;
  SceneZoom    : Single = -125;
  BodyCount    : Integer;

  // Pick mode
  PickedParam  : Single;
  PickedForce  : TVector3f;
  AttachmentPoint : TVector3f;
  PickedBody   : PNewtonBody;
  MousePickMode : Boolean;
  v1, v2       : TVector3f;

  // Physics timing
  AccTimeSlice : Single;
  TimeLastFrame : Cardinal;

  // SDL
  Surface      : PSDL_Surface;

// *****************************************************************************
//  Newton callbacks
// *****************************************************************************

// =============================================================================
//  GetBuoyancyPlane
// =============================================================================
//  This callback is used to specify the plane equation of the buoyancy plane.
//  If you don't use this callback, bodies will always be fully in the fluid
// =============================================================================

function GetBuoyancyPlane( const collisionID : Int; context : Pointer; const globalSpaceMatrix : PFloat; globalSpacePlane : PFloat ) : Integer; cdecl;
var
  GSPlane      : TVector4f;
begin
  GSPlane := V4( 0, 1, 0, -25 );
  Move( GSPlane, globalSpacePlane^, SizeOf( TVector4f ) );
  Result := 0;
end;

// =============================================================================
//  PhysicsApplyGravityForce
// =============================================================================

procedure PhysicsApplyGravityForce( const Body : PNewtonBody ); cdecl;
var
  Mass         : Single;
  Inertia      : TVector3f;
  Force        : TVector3f;
  Gravity      : TVector3f;
  Matrix       : TMatrix4f;
begin
  NewtonBodyGetMassMatrix( Body, @Mass, @Inertia[ 0 ], @Inertia[ 1 ], @Inertia[ 2 ] );
  NewtonBodyGetMatrix( Body, @Matrix[ 0, 0 ] );
// We check (this is quick and dirty, but works for cubic fluid volumes) if the body is immersed in the fluid
  if ( Matrix[ 3, 1 ] < 25 ) and ( Matrix[ 3, 0 ] > -BasinSize ) and ( Matrix[ 3, 0 ] < BasinSize ) and ( Matrix[ 3, 2 ] > -BasinSize ) and ( Matrix[ 3, 2 ] < BasinSize ) then
  begin
 // Body is in fluid, so we add buoyancy forces
    Gravity := V3( 0, -1, 0 );
 // We use Newton's utility-function "NewtonBodyGetTotalVolume" to calculate the
 // body's total volume which is needed for correct body behaviour in a fluid
    NewtonBodyAddBuoyancyForce( Body, Mass / NewtonConvexCollisionCalculateVolume( NewtonBodyGetCollision( Body ) ), 0.9, 0.9, @Gravity[ 0 ], GetBuoyancyPlane, nil );
 // When in a fluid, we don't want our body to get autofreezed
    NewtonBodySetAutoFreeze( Body, 0 );
  end
  else
  begin
 // Body is not in fluid, so just apply normal gravity
    Force := V3( 0, Mass * -9.8, 0 );
    NewtonBodySetForce( Body, @Force[ 0 ] );
 // When outside the fluid, w e can use the autofreezing feature of newton
    NewtonBodySetAutoFreeze( Body, 0 );
  end;
end;

// =============================================================================
//  ScreenToWorld
// =============================================================================
//  Transform the given screen coordinates to world coordinates, used for the
//  picking forces
// =============================================================================

function ScreenToWorld( ScreenX, ScreenY, ScreenZ : Single ) : TVector3f;
var
  Viewport     : TViewPortArray;
  MVMat        : T16dArray;
  PJMat        : T16dArray;
  WinX         : Double;
  WinY         : Double;
  WinZ         : Double;
  ObjX         : Double;
  ObjY         : Double;
  ObjZ         : Double;
begin
// Retrieves the viewport
  glGetIntegerv( GL_VIEWPORT, @Viewport );
// Retrieve the matrices
  glGetDoublev( GL_MODELVIEW_MATRIX, @MVMat[ 0 ] );
  glGetDoublev( GL_PROJECTION_MATRIX, @PJMat[ 0 ] );
  WinX := ScreenX;
  WinY := ScreenY;
  WinZ := ScreenZ;
  WinY := Viewport[ 3 ] - WinY;
// Unproject screen coordinates
  gluUnProject( WinX, WinY, WinZ, MVMat, PJMat, Viewport, @objx, @objy, @objz );
  Result := V3( ObjX, ObjY, ObjZ );
end;

// =============================================================================
//  PhysicsApplyPickForce
// =============================================================================
//  Callback for when a body is picked. Adds picking forces when mouse moved
// =============================================================================

procedure PhysicsApplyPickForce( const Body : PNewtonBody ); cdecl;
var
  Mass         : Single;
  Ixx, Iyy, Izz : Single;
  Veloc        : TVector3f;
  Omega        : TVector3f;
  Matrix       : TMatrix4f;
  Force        : TVector3f;
  DampForce    : TVector3f;
  Point        : TVector3f;
  Torque       : TVector3f;
begin
  PhysicsApplyGravityForce( Body );
  NewtonBodyGetVelocity( Body, @Veloc[ 0 ] );
  NewtonBodyGetOmega( Body, @omega[ 0 ] );
  NewtonBodyGetVelocity( Body, @veloc[ 0 ] );
  NewtonBodyGetMassMatrix( Body, @mass, @Ixx, @Iyy, @Izz );
  DampForce := V3( Veloc[ 0 ] * 10 * Mass, Veloc[ 1 ] * 10 * Mass, Veloc[ 2 ] * 10 * Mass );
  Force := V3( PickedForce[ 0 ] * Mass * 100 - DampForce[ 0 ], PickedForce[ 1 ] * Mass * 100 - DampForce[ 1 ], PickedForce[ 2 ] * Mass * 100 - DampForce[ 2 ] );
  NewtonBodyGetMatrix( Body, @Matrix[ 0, 0 ] );
  Point := AttachmentPoint;
  Matrix_RotateVect( Matrix, Point );
  Torque := VCross( Point, Force );
  NewtonBodyAddForce( Body, @Force[ 0 ] );
  NewtonBodyAddTorque( Body, @Torque[ 0 ] );
end;

// =============================================================================
//  RayCastFilter
// =============================================================================
//  Callback that get's the rigid body hit by a raycast
// =============================================================================

function RayCastFilter( const Body : PNewtonBody; const Normal : PFloat; CollisionID : Int; UserData : Pointer; IntersectParam : Single ) : Single; cdecl;
var
  Mass         : Single;
  Ixx, Iyy, Izz : Single;
begin
  NewtonBodyGetMassMatrix( Body, @Mass, @Ixx, @Iyy, @Izz );
// skip statics rigid bodies (picking bodies with a mass of zero wouldn't make sense)
  if Mass > 0 then
    if ( IntersectParam < PickedParam ) then
    begin
      PickedParam := IntersectParam;
      PickedBody := PNewtonBody( Body );
    end;
  Result := IntersectParam;
end;

// =============================================================================
//  Debug_ShowGeometryCollision
// =============================================================================
//  Callback that shows collision geometry of a body
// =============================================================================

procedure Debug_ShowGeometryCollision( const Body : PNewtonBody; VertexCount : Integer; const FaceArray : PFloat; FaceId : int ); cdecl;
var
  i            : Integer;
  v0, v1       : array[ 0..2 ] of Single;
  vA           : array of Single;
begin
  if VertexCount = 0 then
    exit;
  SetLength( vA, VertexCount * 3 );
  Move( FaceArray^, vA[ 0 ], VertexCount * 3 * SizeOf( Single ) );
  v0[ 0 ] := vA[ ( VertexCount - 1 ) * 3 ];
  v0[ 1 ] := vA[ ( VertexCount - 1 ) * 3 + 1 ];
  v0[ 2 ] := vA[ ( VertexCount - 1 ) * 3 + 2 ];
  for i := 0 to VertexCount - 1 do
  begin
    v1[ 0 ] := vA[ i * 3 ];
    v1[ 1 ] := vA[ i * 3 + 1 ];
    v1[ 2 ] := vA[ i * 3 + 2 ];
    glVertex3f( v0[ 0 ], v0[ 1 ], v0[ 2 ] );
    glVertex3f( v1[ 0 ], v1[ 1 ], v1[ 2 ] );
    v0 := v1;
  end;
end;

// =============================================================================
//  Debug_ShowBodyCollision
// =============================================================================
//  Show collision geometry for all bodies in the current scene
// =============================================================================

procedure Debug_ShowBodyCollision( const Body : PNewtonBody ); cdecl;
begin
  NewtonBodyForEachPolygonDo( Body, Debug_ShowGeometryCollision );
end;

// =============================================================================
//  PhysicsSetTransform
// =============================================================================
//  Callback that's used to update the matrix of the graphic primitives
// =============================================================================

procedure PhysicsSetTransform( const Body : PNewtonBody; const Matrix : PFloat ); cdecl;
var
  Primitive    : TNewtonPrimitive;
begin
  if not Assigned( NewtonBodyGetUserData( Body ) ) then
    exit;
// Get the primitive by the user data assigned with this body
  Primitive := TNewtonPrimitive( NewtonBodyGetUserData( Body ) );
// Now update it's matrix
  Move( Matrix^, Primitive.Matrix, SizeOf( TMatrix4f ) );
end;

// =============================================================================
//  InitNewton
// =============================================================================

procedure InitNewton;
var
  FloorBody    : PNewtonBody;
  FloorCollider : PNewtonCollision;
  M            : TMatrix4f;
begin
  Randomize;
// Create the newton world
  NewtonWorld := NewtonCreate( nil, nil );
  NewtonSetPlatformArchitecture( NewtonWorld, 0 );
  NewtonSetMinimumFrameRate( NewtonWorld, 60 );
// Build the basin that surrounds our fluid volume
  Matrix_SetIdentity( M );
  FloorCollider := NewtonCreateBox( NewtonWorld, BasinSize * 2, 1, BasinSize * 2, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetTransform( M, V3( 0, -1, 0 ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  PrimitiveManager.SpawnBox( V3( BasinSize * 2, 1, BasinSize * 2 ), M );
// Create front
  FloorCollider := NewtonCreateBox( NewtonWorld, BasinSize * 2, 40, 1, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetTransform( M, V3( 0, 18, -BasinSize ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  PrimitiveManager.SpawnBox( V3( BasinSize * 2, 40, 1 ), M );
// Create back
  FloorCollider := NewtonCreateBox( NewtonWorld, BasinSize * 2, 40, 1, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetTransform( M, V3( 0, 18, BasinSize ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  PrimitiveManager.SpawnBox( V3( BasinSize * 2, 40, 1 ), M );
// Create left
  FloorCollider := NewtonCreateBox( NewtonWorld, 1, 40, BasinSize * 2, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetTransform( M, V3( BasinSize, 18, 0 ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  PrimitiveManager.SpawnBox( V3( 1, 40, BasinSize * 2 ), M );
// Create right
  FloorCollider := NewtonCreateBox( NewtonWorld, 1, 40, BasinSize * 2, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetTransform( M, V3( -BasinSize, 18, 0 ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  PrimitiveManager.SpawnBox( V3( 1, 40, BasinSize * 2 ), M );
// Release the collider, we don't need it anymore
  NewtonReleaseCollision( NewtonWorld, FloorCollider );
end;

// =============================================================================
//  InitGL
// =============================================================================

procedure InitGL;
begin
  glEnable( GL_DEPTH_TEST );
  glDepthFunc( GL_LEQUAL );
  glClearColor( 0, 0, 0.3, 0 );
  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );
  glEnable( GL_COLOR_MATERIAL );
  SceneRotation := V3( 45, 45, 0 );
end;

// =============================================================================
//  TerminateApplication
// =============================================================================

procedure TerminateApplication;
begin
  PrimitiveManager.Free;
// Destroy the Newton world
  NewtonDestroy( NewtonWorld );
  SDL_QUIT;
  Halt( 0 );
end;

// =============================================================================
//  SpawnBody
// =============================================================================

procedure SpawnBody;
var
  TmpBody      : PNewtonBody;
  Collider     : PNewtonCollision;
  Inertia      : TVector3f;
  TmpV         : TVector3f;
  Mass         : Single;
  TmpSize      : Single;
  TmpSizeB     : Single;
  M            : TMatrix4f;
begin
  if BodyCount >= MaxBodyCount then
    exit;
// Spawn some random primitive to interact with
  TmpSize := 1 + Random * 2;
  TmpSizeB := 1 + Random * 2;
  TmpBody := nil;
  Collider := nil;
  case Random( 3 ) of
    0 :
      begin
        Collider := NewtonCreateBox( NewtonWorld, TmpSize * 2, TmpSize * 2, TmpSize * 2, nil );
        TmpBody := NewtonCreateBody( NewtonWorld, Collider );
        NewtonBodySetUserData( TmpBody, PrimitiveManager.SpawnBox( V3( TmpSize * 2, TmpSize * 2, TmpSize * 2 ), M ) );
     // Calculate real moment of inertia
        Mass := 10 * TmpSize;
        Inertia := V3( Mass * ( TmpSize * 2 * TmpSize * 2 + TmpSize * 2 * TmpSize * 2 ) / 12,
          Mass * ( TmpSize * 2 * TmpSize * 2 + TmpSize * 2 * TmpSize * 2 ) / 12,
          Mass * ( TmpSize * 2 * TmpSize * 2 + TmpSize * 2 * TmpSize * 2 ) / 12 );
        NewtonBodySetMassMatrix( TmpBody, Mass, Inertia[ 0 ], Inertia[ 1 ], Inertia[ 2 ] );
      end;
    1 :
      begin
        Collider := NewtonCreateSphere( NewtonWorld, TmpSize, TmpSize, TmpSize, nil );
        TmpBody := NewtonCreateBody( NewtonWorld, Collider );
        NewtonBodySetUserData( TmpBody, PrimitiveManager.SpawnSphere( TmpSize, M ) );
     // Calculate real moment of inertia
        Mass := 10 * TmpSize;
        Inertia := V3( 2 / 5 * Mass * Sqr( TmpSize ), 2 / 5 * Mass * Sqr( TmpSize ), 2 / 5 * Mass * Sqr( TmpSize ) );
        NewtonBodySetMassMatrix( TmpBody, Mass, Inertia[ 0 ], Inertia[ 1 ], Inertia[ 2 ] );
      end;
    2 :
      begin
        Collider := NewtonCreateCone( NewtonWorld, TmpSize, TmpSizeB * 2, nil );
        TmpBody := NewtonCreateBody( NewtonWorld, Collider );
        NewtonBodySetUserData( TmpBody, PrimitiveManager.SpawnCone( TmpSize, TmpSizeB * 2, M ) );
     // Calculate real moment of inertia
        Mass := 10 * TmpSize;
        Inertia := V3( 3 / 80 * Mass * Sqr( TmpSizeB ) + 3 / 20 * Mass * Sqr( TmpSize ),
          3 / 80 * Mass * Sqr( TmpSizeB ) + 3 / 20 * Mass * Sqr( TmpSize ),
          3 / 80 * Mass * Sqr( TmpSizeB ) + 3 / 20 * Mass * Sqr( TmpSize ) );
        NewtonBodySetMassMatrix( TmpBody, Mass, Inertia[ 0 ], Inertia[ 1 ], Inertia[ 2 ] );
      end;
    3 :
      begin
        Collider := NewtonCreateCylinder( NewtonWorld, TmpSize, TmpSizeB, nil );
        TmpBody := NewtonCreateBody( NewtonWorld, Collider );
        NewtonBodySetUserData( TmpBody, PrimitiveManager.SpawnCylinder( TmpSize, TmpSizeB, M ) );
     // Calculate real moment of inertia
        Mass := 10 * TmpSize;
        Inertia := V3( 1 / 12 * Mass * Sqr( TmpSizeB ) + 1 / 4 * Mass * Sqr( TmpSize ),
          1 / 12 * Mass * Sqr( TmpSizeB ) + 1 / 4 * Mass * Sqr( TmpSize ),
          1 / 12 * Mass * Sqr( TmpSizeB ) + 1 / 4 * Mass * Sqr( TmpSize ) );
        NewtonBodySetMassMatrix( TmpBody, Mass, Inertia[ 0 ], Inertia[ 1 ], Inertia[ 2 ] );
      end;
  end;
  if not Assigned( TmpBody ) then
    exit;
  Matrix_SetIdentity( M );
// Set random spawn position
  Matrix_SetTransform( M, V3( Random( 40 ) - Random( 40 ), 60, Random( 40 ) - Random( 40 ) ) );
// Some random orientation
  Matrix_SetRotation( M, V3( Random( 360 ), Random( 360 ), Random( 360 ) ) );
// And some random angular velocity
  TmpV := V3( Random * 2 - Random * 2, Random * 2 - Random * 2, Random * 2 - Random * 2 );
  NewtonBodySetOmega( TmpBody, @TmpV[ 0 ] );
  NewtonBodySetMatrix( TmpBody, @M[ 0, 0 ] );
// Set callbacks
  NewtonBodySetForceAndTorqueCallBack( TmpBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( TmpBody, PhysicsSetTransform );
  if Assigned( Collider ) then
    NewtonReleaseCollision( NewtonWorld, Collider );
  inc( BodyCount );
end;

// =============================================================================
//  HandleKeyPress
// =============================================================================

procedure HandleKeyPress( KeySym : PSDL_keysym );
begin
  case KeySym.Sym of
    SDLK_ESCAPE : TerminateApplication;
    SDLK_D : ShowDebug := not ShowDebug;
  end;
end;

// =============================================================================
//  HandleMouseMove
// =============================================================================

procedure HandleMouseMove( pEvent : TSDL_MouseMotionEvent );
var
  p, p0, p1, p2 : TVector3f;
  Matrix       : TMatrix4f;
  MPos         : TPoint;
begin
// Rotate scene
  if ( pEvent.state and SDL_Button( SDL_BUTTON_RIGHT ) ) > 0 then
  begin
    MPos.x := pEvent.x;
    MPos.y := pEvent.y;
    SceneRotation[ 0 ] := SceneRotation[ 0 ] + ( MPos.Y - MousePos.Y ) * 0.25;
    SceneRotation[ 1 ] := SceneRotation[ 1 ] + ( MPos.X - MousePos.X ) * 0.25;
  end;
  MousePos.x := pEvent.x;
  MousePos.y := pEvent.y;
// Calculate mouse-inflicted force on picked body when in picking mode
  if ( MousePickMode ) and Assigned( PickedBody ) then
  begin
 // Get the current matrix of the picked body
    NewtonBodyGetMatrix( PickedBody, @Matrix[ 0, 0 ] );
 // Get current mouse position in world coordinates relative to the picked point on the body
    p0 := ScreenToWorld( pEvent.x, pEvent.y, 0 );
    p1 := ScreenToWorld( pEvent.x, pEvent.y, 1 );
    p2 := Matrix_TansformVector( Matrix, AttachmentPoint );
    p := V3( p0[ 0 ] + ( p1[ 0 ] - p0[ 0 ] ) * PickedParam, p0[ 1 ] + ( p1[ 1 ] - p0[ 1 ] ) * PickedParam, p0[ 2 ] + ( p1[ 2 ] - p0[ 2 ] ) * PickedParam );
    PickedForce := V3( ( P[ 0 ] - p2[ 0 ] ) * 0.1, ( P[ 1 ] - p2[ 1 ] ) * 0.1, ( P[ 2 ] - p2[ 2 ] ) * 0.1 );
  end;
end;

// =============================================================================
//  HandleMouseDown
// =============================================================================

procedure HandleMouseDown( pEvent : TSDL_MouseButtonEvent );
const
  Vzero        : array[ 0..2 ] of Single = ( 0, 0, 0 );
var
  p0, p1       : TVector3f;
  p            : TVector3f;
  Matrix       : TMatrix4f;
begin
  MousePickMode := False;
  if pEvent.button = SDL_BUTTON_LEFT then
  begin
    p0 := ScreenToWorld( pEvent.x, pEvent.y, 0 );
    p1 := ScreenToWorld( pEvent.x, pEvent.y, 1 );
    PickedBody := nil;
    PickedParam := 1.1;
    NewtonWorldRayCast( NewtonWorld, @p0[ 0 ], @p1[ 0 ], @RayCastFilter, nil, nil );
    v1 := p0;
    v2 := p1;
    if Assigned( PickedBody ) then
    begin
      MousePickMode := True;
      NewtonBodyGetMatrix( PickedBody, @Matrix[ 0, 0 ] );
      p := V3( p0[ 0 ] + ( p1[ 0 ] - p0[ 0 ] ) * PickedParam, p0[ 1 ] + ( p1[ 1 ] - p0[ 1 ] ) * PickedParam, p0[ 2 ] + ( p1[ 2 ] - p0[ 2 ] ) * PickedParam );
      AttachmentPoint := Matrix_UntransformVector( Matrix, P );
      NewtonBodySetForceAndTorqueCallback( PickedBody, PhysicsApplyPickForce );
      NewtonBodySetAutoFreeze( PickedBody, 0 );
      NewtonWorldUnfreezeBody( NewtonWorld, PickedBody );
    end;
  end;
end;

// =============================================================================
//  HandleMouseUp
// =============================================================================

procedure HandleMouseUp( pEvent : TSDL_MouseButtonEvent );
begin
  if pEvent.button = SDL_BUTTON_WHEELUP then
    SceneZoom := SceneZoom + 1;
  if pEvent.button = SDL_BUTTON_WHEELDOWN then
    SceneZoom := SceneZoom - 1;
  if pEvent.button = SDL_BUTTON_LEFT then
    if Assigned( PickedBody ) then
    begin
      NewtonWorldUnfreezeBody( NewtonWorld, PickedBody );
      NewtonBodySetAutoFreeze( PickedBody, 1 );
      NewtonBodySetForceAndTorqueCallback( PickedBody, PhysicsApplyGravityForce );
      PickedBody := nil;
      PickedForce := V3( 0, 0, 0 );
    end;
end;

// =============================================================================
//  ResizeWindow
// =============================================================================

function ResizeWindow( width : integer; height : integer ) : Boolean;
begin
  if Height = 0 then
    Height := 1;
  glViewport( 0, 0, width, height );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluPerspective( 60, Width / Height, 1, 256 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  Result := true;
end;

// =============================================================================
//  Render
// =============================================================================

procedure Render;
begin
  AccTimeSlice := AccTimeSlice + ( SDL_GetTicks - TimeLastFrame );
  TimeLastFrame := SDL_GetTicks;

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  glTranslatef( 0, 0, SceneZoom );
  glRotatef( SceneRotation[ 0 ], 1, 0, 0 );
  glRotatef( SceneRotation[ 1 ], 0, 1, 0 );
  glRotatef( SceneRotation[ 2 ], 0, 0, 1 );

// Render objects
  PrimitiveManager.Render;

  glDisable( GL_LIGHTING );

// Render debug output
  if ShowDebug then
  begin
    glBegin( GL_LINES );
    NewtonWorldForEachBodyDo( NewtonWorld, @Debug_ShowBodyCollision );
    glEnd;
  end;

// Render fluid plane
  glEnable( GL_BLEND );
  glDisable( GL_CULL_FACE );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glColor4f( 0.6, 0.6, 1, 0.5 );
  glPointSize( 100 );
  glBegin( GL_QUADS );
  glVertex3f( -BasinSize, 25, -BasinSize );
  glVertex3f( -BasinSize, 25, BasinSize );
  glVertex3f( BasinSize, 25, BasinSize );
  glVertex3f( BasinSize, 25, -BasinSize );
  glEnd;
  glColor3f( 1, 1, 1 );
  glDisable( GL_BLEND );
  glEnable( GL_LIGHTING );

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
//  Main
// =============================================================================
var
  VideoFlags   : UInt32;
  VideoInfo    : PSDL_VideoInfo;
  Event        : TSDL_Event;
  Done         : Boolean;
  LastTick     : Cardinal;
  SpawnTime    : Integer;
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
  SDL_WM_SetCaption( 'Newton - Buoyancy Demo (SDL) - 2005/06 by Sascha Willems', nil );
// Create SDL surface
  Surface := SDL_SetVideoMode( 640, 480, 32, Videoflags );
  if not Assigned( Surface ) then
    TerminateApplication;

  PrimitiveManager := TNewtonPrimitiveManager.Create;
  InitGL;
  ReSizeWindow( 640, 480 );
  InitNewton;
  TimeLastFrame := SDL_GetTicks;
  SpawnTime := 0;
  LastTick := SDL_GetTicks;
  Randomize;
// Main loop
  Done := False;
  while not Done do
  begin
    while SDL_PollEvent( @Event ) = 1 do
    begin
      case event.type_ of
        SDL_QUITEV : Done := true;
        SDL_KEYDOWN : HandleKeyPress( @Event.Key.KeySym );
        SDL_MOUSEMOTION : HandleMouseMove( Event.motion );
        SDL_MOUSEBUTTONDOWN : HandleMouseDown( Event.button );
        SDL_MOUSEBUTTONUP : HandleMouseUp( Event.button );
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
 // Spawn timer
    if SpawnTime <= 0 then
    begin
      SpawnBody;
      SpawnTime := 12500;
      LastTick := SDL_GetTicks;
    end
    else
      SpawnTime := SpawnTime - ( SDL_GetTicks - LastTick );
  end;
  TerminateApplication;
end.

