// =============================================================================
//
//  SDLNewtonJointDemo.pas
//   Simple demo to show off the different joint types that Newton offers. It
//   also shows how to use newton's raycast functions to influence bodies.
//
// =============================================================================
//
//   Copyright © 2005/06 by Sascha Willems (www.delphigl.de)
//   Newton Game Dynamics © 2003-2006 by Julio Jerez (www.newtondynamics.com)
//
// =============================================================================
//
//   Contents of this file are subject to the Mozilla Public Licencse 1.1
//   which can be obtained here : http://opensource.org/licenses/mozilla1.1.php
//
// =============================================================================

program SDLNewtonJointDemo;

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

var
  // Newton
  NewtonWorld  : PNewtonWorld;

  // Containers for the different joint types
  Joint        : array of PNewtonJoint;

  // Pick mode
  PickedParam  : Single;
  PickedForce  : TVector3f;
  AttachmentPoint : TVector3f;
  PickedBody   : PNewtonBody;
  MousePickMode : Boolean;
  v1, v2       : TVector3f;

  // Misc
  ShowDebug    : Boolean;
  MousePos     : TPoint;
  SceneRotation : TVector3f;
  SceneZoom    : Single = -25;

  // Physics timing
  AccTimeSlice : Single;
  TimeLastFrame : Cardinal;

  // SDL
  Surface      : PSDL_Surface;

// *****************************************************************************
//  Newton callbacks
// *****************************************************************************

// =============================================================================
//  PhysicsApplyGravityForce
// =============================================================================
//  Callback that applies force to a body
// =============================================================================

procedure PhysicsApplyGravityForce( const Body : PNewtonBody ); cdecl;
var
  mass         : Single;
  Ixx          : Single;
  Iyy          : Single;
  Izz          : Single;
  Force        : TVector3f;
begin
  NewtonBodyGetMassMatrix( Body, @mass, @Ixx, @Iyy, @Izz );
  Force := V3( 0, Mass * -10, 0 );
  NewtonBodySetForce( Body, @Force[ 0 ] );
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

// *****************************************************************************
//  Joint creating functions
// *****************************************************************************

// =============================================================================
//  CreateBallJoint
// =============================================================================
//  Creates a ball and socket joint. In this demo we build the
//  following joint :
//   - Base (static) is a sphere
//   - Child is a cone that's connected to the sphere
//   - Second Child is a cone that's connected to the above cone
// =============================================================================

function CreateBallJoint( pNewtonWorld : PNewtonWorld; pPivotPoint : TVector3f ) : PNewtonJoint;
var
  Collider     : PNewtonCollision;
  Matrix       : TMatrix4f;
  PinDir       : TVector3f;
  ParentBody   : PNewtonBody;
  ChildBody    : PNewtonBody;
begin
// Create the PARENT body for this joint =======================================
  Collider := NewtonCreateSphere( pNewtonWorld, 2, 2, 2, nil );
  ParentBody := NewtonCreateBody( pNewtonWorld, Collider );
// We give this body a mass of zero, cause the "root" of the joint shall not move
// note : if you don't tell newton the mass of a body, it's zero. But we still do it for learning purposes
  NewtonBodySetMassMatrix( ParentBody, 0, 0, 0, 0 );
  NewtonBodySetForceAndTorqueCallback( ParentBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ParentBody, PhysicsSetTransform );
// Offset the parent
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, pPivotPoint );
  NewtonBodySetMatrix( ParentBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ParentBody, PrimitiveManager.SpawnSphere( 2, Matrix ) );

// Create the first CHILD body for this joint ==================================
  Collider := NewtonCreateCapsule( pNewtonWorld, 1, 4, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
// Set some damping
  NewtonBodySetLinearDamping( ChildBody, 0.2 );
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Offset the child
  Matrix_SetIdentity( Matrix );
// Set callbacks for this body
  Matrix_SetTransform( Matrix, V3( pPivotPoint[ 0 ] + 0, pPivotPoint[ 1 ] - 4, pPivotPoint[ 2 ] + 0 ) );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnCapsule( 1, 4, Matrix ) );
// Now create the joint at the given pivot point
  Result := NewtonConstraintCreateBall( pNewtonWorld, @pPivotPoint[ 0 ], ChildBody, ParentBody );
// Set some limits for the joint so that it can't freely move around the parent sphere
  PinDir := V3( 0, 1, 0 );
  NewtonBallSetConeLimits( Result, @PinDir[ 0 ], 45 * PI / 180, 45 * PI / 180 );

// Create the second CHILD body for this joint =================================
// Adjust the pivot point
  pPivotPoint[ 1 ] := pPivotPoint[ 1 ] - 4;
  ParentBody := ChildBody;
// Create a new child
  Collider := NewtonCreateCapsule( pNewtonWorld, 1, 4, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
  NewtonBodySetLinearDamping( ChildBody, 0.2 );
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Offset the child
  Matrix_SetIdentity( Matrix );
// Set callbacks for this body
  Matrix_SetTransform( Matrix, V3( pPivotPoint[ 0 ] + 0, pPivotPoint[ 1 ] - 4, pPivotPoint[ 2 ] + 0 ) );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnCapsule( 1, 4, Matrix ) );
// Build the joint
  Result := NewtonConstraintCreateBall( pNewtonWorld, @pPivotPoint[ 0 ], ChildBody, ParentBody );
// Set some limits for the joint so that it can't freely move around it's parent
  PinDir := V3( 0, 1, 0 );
  NewtonBallSetConeLimits( Result, @PinDir[ 0 ], 25 * PI / 180, 25 * PI / 180 );
// Release the collider
  NewtonReleaseCollision( pNewtonWorld, Collider );
end;

// =============================================================================
//  CreateHingeJoint
// =============================================================================
//  Creates a hinge joint. In this example we just take a long rod
//  and "attach" a "door" to it that can then swing around the hinge axis.
// =============================================================================

function CreateHingeJoint( pNewtonWorld : PNewtonWorld; pPivotPoint : TVector3f ) : PNewtonJoint;
var
  Collider     : PNewtonCollision;
  Matrix       : TMatrix4f;
  PinDir       : TVector3f;
  ParentBody   : PNewtonBody;
  ChildBody    : PNewtonBody;
begin
// Create the PARENT body for this joint =======================================
  Collider := NewtonCreateCylinder( pNewtonWorld, 0.5, 5, nil );
  ParentBody := NewtonCreateBody( pNewtonWorld, Collider );
// We give this body a mass of zero, cause the "root" of the joint shall not move
  NewtonBodySetMassMatrix( ParentBody, 0, 0, 0, 0 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ParentBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ParentBody, PhysicsSetTransform );
// Offset the parent
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, pPivotPoint );
  NewtonBodySetMatrix( ParentBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ParentBody, PrimitiveManager.SpawnCylinder( 0.5, 5, Matrix ) );

// Create the CHILD body for this joint ========================================
  Collider := NewtonCreateBox( pNewtonWorld, 4, 0.5, 6, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
// Set some damping
  NewtonBodySetLinearDamping( ChildBody, 0.8 );
// Give the child some mass (since it shall move)
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Offset the child
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, V3( pPivotPoint[ 0 ], pPivotPoint[ 1 ], pPivotPoint[ 2 ] + 3 ) );
//Matrix_SetTransform(Matrix, pPivotPoint);
//Matrix_SetRotation(Matrix, V3(0,0,-90));
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnBox( V3( 4, 0.5, 6 ), Matrix ) );
// Now create the joint at the given pivot point
  PinDir := V3( 1, 0, 0 );
  Result := NewtonConstraintCreateHinge( pNewtonWorld, @pPivotPoint[ 0 ], @PinDir[ 0 ], ChildBody, ParentBody );
// Release the collider
  NewtonReleaseCollision( pNewtonWorld, Collider );
end;

// =============================================================================
//  CreateSliderJoint
// =============================================================================
//  Creates a slider joint. That's a joint that let's a body "slide" up and
//  down one single axis.
// =============================================================================

function CreateSliderJoint( pNewtonWorld : PNewtonWorld; pPivotPoint : TVector3f ) : PNewtonJoint;
var
  Collider     : PNewtonCollision;
  Matrix       : TMatrix4f;
  PinDir       : TVector3f;
  ParentBody   : PNewtonBody;
  ChildBody    : PNewtonBody;
begin
// Create the PARENT body for this joint =======================================
  Collider := NewtonCreateCylinder( pNewtonWorld, 0.5, 25, nil );
  ParentBody := NewtonCreateBody( pNewtonWorld, Collider );
// We give this body a mass of zero, cause the "root" of the joint shall not move (if you leave it out, it's also set to 0)
  NewtonBodySetMassMatrix( ParentBody, 0, 0, 0, 0 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ParentBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ParentBody, PhysicsSetTransform );
// Offset and rotate this joint
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, pPivotPoint );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ParentBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ParentBody, PrimitiveManager.SpawnCylinder( 0.5, 25, Matrix ) );

// Create the CHILD body for this joint ========================================
  Collider := NewtonCreateBox( pNewtonWorld, 6, 2, 2, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
// Set some damping (to avoid it to spin forever)
  NewtonBodySetLinearDamping( ChildBody, 0.8 );
// Since the child of this bone should be able to move, we need to assign a mass to it
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Offset and rotate the child
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, V3( pPivotPoint[ 0 ] + 0, pPivotPoint[ 1 ], pPivotPoint[ 2 ] + 0 ) );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnBox( V3( 6, 2, 2 ), Matrix ) );
// PinDir is the axis on which the slider can move
  PinDir := V3( 0, 1, 0 );
// Now create the joint at the given pivot point
  Result := NewtonConstraintCreateSlider( pNewtonWorld, @pPivotPoint[ 0 ], @PinDir[ 0 ], ChildBody, ParentBody );
// Release the collider
  NewtonReleaseCollision( pNewtonWorld, Collider );
end;

// =============================================================================
//  CreateCorkscrewJoint
// =============================================================================
//  Creates a corkrscew joint, which is actually an enhanched slider joint
//  that let's the body also rotate around the sliding axis.
// =============================================================================

function CreateCorkscrewJoint( pNewtonWorld : PNewtonWorld; pPivotPoint : TVector3f ) : PNewtonJoint;
var
  Collider     : PNewtonCollision;
  Matrix       : TMatrix4f;
  PinDir       : TVector3f;
  ParentBody   : PNewtonBody;
  ChildBody    : PNewtonBody;
begin
// Create the PARENT body for this joint =======================================
  Collider := NewtonCreateCylinder( pNewtonWorld, 0.5, 35, nil );
  ParentBody := NewtonCreateBody( pNewtonWorld, Collider );
// We give this body a mass of zero, cause the "root" of the joint shall not move
  NewtonBodySetMassMatrix( ParentBody, 0, 0, 0, 0 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ParentBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ParentBody, PhysicsSetTransform );
// Offset and rotate the parent
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, pPivotPoint );
  Matrix_SetRotation( Matrix, V3( -45, 0, -90 ) );
  NewtonBodySetMatrix( ParentBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ParentBody, PrimitiveManager.SpawnCylinder( 0.5, 35, Matrix ) );

// Create the CHILD body for this joint ========================================
  Collider := NewtonCreateBox( pNewtonWorld, 3, 2, 2, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
// Set some damping
  NewtonBodySetLinearDamping( ChildBody, 0.8 );
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Offset the child
  Matrix_SetIdentity( Matrix );
// Set callbacks for this body
  Matrix_SetTransform( Matrix, V3( pPivotPoint[ 0 ] + 0, pPivotPoint[ 1 ], pPivotPoint[ 2 ] + 0 ) );
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnBox( V3( 3, 2, 2 ), Matrix ) );
// PinDir is the axis on which the slider can move
  PinDir := V3( 0, 1, 1 );
// Now create the joint at the given pivot point
  Result := NewtonConstraintCreateCorkscrew( pNewtonWorld, @pPivotPoint[ 0 ], @PinDir[ 0 ], ChildBody, ParentBody );
// Release the collider
  NewtonReleaseCollision( pNewtonWorld, Collider );
end;

// =============================================================================
//  CreateUniversalJoint
// =============================================================================
//  Creates an universal joint, which is a joint type that can rotate around
//  two axis, in opposite to a hinge joint that only rotates on one axis.
// =============================================================================

function CreateUniversalJoint( pNewtonWorld : PNewtonWorld; pPivotPoint : TVector3f ) : PNewtonJoint;
var
  Collider     : PNewtonCollision;
  Matrix       : TMatrix4f;
  PinDir1      : TVector3f;
  PinDir2      : TVector3f;
  ParentBody   : PNewtonBody;
  ChildBody    : PNewtonBody;
begin
// Create the PARENT body for this joint =======================================
  Collider := NewtonCreateCylinder( pNewtonWorld, 0.5, 6, nil );
  ParentBody := NewtonCreateBody( pNewtonWorld, Collider );
// We give this body a mass of zero, cause the "root" of the joint shall not move
// note : if you don't tell newton the mass of a body, it's zero. But we still do it for learning purposes
  NewtonBodySetMassMatrix( ParentBody, 0, 0, 0, 0 );
  NewtonBodySetForceAndTorqueCallback( ParentBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ParentBody, PhysicsSetTransform );
// Offset and rotate the parent
  Matrix_SetIdentity( Matrix );
// Set callbacks for this body
  Matrix_SetTransform( Matrix, pPivotPoint );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ParentBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ParentBody, PrimitiveManager.SpawnCylinder( 0.5, 6, Matrix ) );

// Create the CHILD body for this joint ========================================
  Collider := NewtonCreateBox( pNewtonWorld, 2, 4, 2, nil );
  ChildBody := NewtonCreateBody( pNewtonWorld, Collider );
// Set some damping
  NewtonBodySetLinearDamping( ChildBody, 0.8 );
  NewtonBodySetMassMatrix( ChildBody, 10, 10, 10, 10 );
// Set callbacks for this body
  NewtonBodySetForceAndTorqueCallback( ChildBody, PhysicsApplyGravityForce );
  NewtonBodySetTransformCallBack( ChildBody, PhysicsSetTransform );
// Rotate the child
  Matrix_SetIdentity( Matrix );
  Matrix_SetTransform( Matrix, pPivotPoint );
  Matrix_SetRotation( Matrix, V3( 0, 0, -90 ) );
  NewtonBodySetMatrix( ChildBody, @Matrix[ 0, 0 ] );
// Create the graphics primitives and assign it to this body
  NewtonBodySetUserData( ChildBody, PrimitiveManager.SpawnBox( V3( 2, 4, 2 ), Matrix ) );
// This is the first axis of rotation of the universal joint
  PinDir1 := V3( 0, 1, 0 );
// This is the second axis of rotation of the universal joint
  PinDir2 := V3( 0, 0, 1 );
// Now create the joint at the given pivot point
  Result := NewtonConstraintCreateUniversal( pNewtonWorld, @pPivotPoint[ 0 ], @PinDir1[ 0 ], @PinDir2[ 0 ], ChildBody, ParentBody );
// Release the collider
  NewtonReleaseCollision( pNewtonWorld, Collider );
end;

// *****************************************************************************
//  Newton callbacks and helpers
// *****************************************************************************

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
//  InitNewton
// =============================================================================

procedure InitNewton;
var
  FloorBody    : PNewtonBody;
  FloorCollider : PNewtonCollision;
  M            : TMatrix4f;
begin
  Randomize;
// Create a newton world
  NewtonWorld := NewtonCreate( nil, nil );
  NewtonSetPlatformArchitecture( NewtonWorld, 0 );
  NewtonSetMinimumFrameRate( NewtonWorld, 120 );
// Generate the different joint types
  SetLength( Joint, 5 );
// Create objects
// Ball and socket joint
  Joint[ 0 ] := CreateBallJoint( NewtonWorld, V3( 5, 0, 0 ) );
  Joint[ 1 ] := CreateHingeJoint( NewtonWorld, V3( 0, 0, 0 ) );
  Joint[ 2 ] := CreateSliderJoint( NewtonWorld, V3( -5, 0, 0 ) );
  Joint[ 3 ] := CreateCorkscrewJoint( NewtonWorld, V3( -10, 0, 0 ) );
  Joint[ 4 ] := CreateUniversalJoint( NewtonWorld, V3( 10, 0, 0 ) );
// Create a floor
  FloorCollider := NewtonCreateBox( NewtonWorld, 40, 1, 20, nil );
  FloorBody := NewtonCreateBody( NewtonWorld, FloorCollider );
  Matrix_SetIdentity( M );
  Matrix_SetTransform( M, V3( 0, -12, 0 ) );
  NewtonBodySetMatrix( FloorBody, @M[ 0, 0 ] );
  NewtonReleaseCollision( NewtonWorld, FloorCollider );
  NewtonBodySetUserData( FloorBody, PrimitiveManager.SpawnBox( V3( 40, 1, 40 ), M ) );
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
end;

// =============================================================================
//  Render
// =============================================================================

procedure Render;
var
  M            : TMatrix4f;
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

// Show picked point if body is selected
  if ( MousePickMode ) and Assigned( PickedBody ) then
  begin
    glDisable( GL_LIGHTING );
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
    NewtonBodyGetMatrix( PickedBody, @M[ 0, 0 ] );
    glPushMatrix;
    glMultMatrixf( @M[ 0, 0 ] );
    glTranslatef( AttachmentPoint[ 0 ], AttachmentPoint[ 1 ], AttachmentPoint[ 2 ] );
    gluSphere( Q, 0.35, 6, 6 );
    glPopMatrix;
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
    glEnable( GL_LIGHTING );
  end;

// Render debug output
  if ShowDebug then
  begin
    glDisable( GL_LIGHTING );
    glBegin( GL_LINES );
    NewtonWorldForEachBodyDo( NewtonWorld, @Debug_ShowBodyCollision );
    glEnd;
    glEnable( GL_LIGHTING );
  end;

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
begin
  PrimitiveManager.Free;
// Destroy the Newton world
  NewtonDestroy( NewtonWorld );
  SDL_QUIT;
  Halt( 0 );
end;

// *****************************************************************************
//  Event handlers
// *****************************************************************************

// =============================================================================
//  HandleKeyPress
// =============================================================================

procedure HandleKeyPress( KeySym : PSDL_KeySym );
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

function ResizeWindow( Width : Integer; Height : Integer ) : Boolean;
begin
  PrimitiveManager.ClearListIDs;
  if Height = 0 then
    Height := 1;
  glViewport( 0, 0, Width, Height );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluPerspective( 60, width / height, 1, 100 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  Result := true;
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
  begin
    TerminateApplication;
  end;
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
  SDL_WM_SetCaption( 'Newton - Simple Joint Demo (SDL) - 2005/06 by Sascha Willems', nil );
// Create SDL surface
  Surface := SDL_SetVideoMode( 640, 480, 32, Videoflags );
  if not Assigned( Surface ) then
  begin
    TerminateApplication;
  end;

  InitGL;
  PrimitiveManager := TNewtonPrimitiveManager.Create;
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
        SDL_MOUSEMOTION : HandleMouseMove( Event.motion );
        SDL_MOUSEBUTTONDOWN : HandleMouseDown( Event.button );
        SDL_MOUSEBUTTONUP : HandleMouseUp( Event.button );
        SDL_VIDEORESIZE :
          begin
            Surface := SDL_SetVideoMode( event.resize.w, event.resize.h, 32, videoflags );
            if surface = nil then
            begin
              TerminateApplication;
            end;
            ReSizeWindow( event.resize.w, event.resize.h );
            InitGL;
          end;
      end;
    end;
    Render;
  end;
  TerminateApplication;
end.

