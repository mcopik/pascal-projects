// =============================================================================
//
//  SDLNewtonRagdoll.pas
//   This demo shows the ragdoll container of NGD and ragdoll collisions
//   with other primitives
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

program SDLNewtonRagdoll;

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
  glMatrixHelper in '..\common\glMatrixHelper.pas';

type
  TRagDollBonePrimitiveType = ( ptBox, ptCylinder, ptSphere );

  // TRagDollBone ==============================================================
  //  Ragdolls are made up of bones, so we use a class for the bones itself
  TRagDollBone = class;
  TRagDollBone = class
    Children : array of TRagDollBone;
    PrimitiveType : TRagDollBonePrimitiveType;
    Body : PNewtonBody;
    Size : TVector3f;
    Matrix : TMatrix4f;
    Color : TVector3f;
    ID : Integer;
    ListID : glUInt;
      Name : string;
    constructor Create( pPrimitiveType : TRagDollBonePrimitiveType );
    procedure Render;
  end;

  // TRagDoll ==================================================================
  //  Stores bones and other stuff. Note that this example is hard-coded for
  //  a humanoid ragdoll but can easily be changed into any ragdoll shape
  TRagDoll = class
  private
    procedure BuildRagDoll( pRagDoll : PNewtonRagDoll; pParentDollBone : PNewtonRagDollBone; pVelocity : TVector3f; var pBone : TRagDollBone );
  public
    RagDoll : PNewtonRagDoll;
    spine0_Bone : TRagDollBone;
    spine1_Bone : TRagDollBone;
    head_Bone : TRagDollBone;
    rUpperArm_Bone : TRagDollBone;
    lUpperArm_Bone : TRagDollBone;
    rLowerArm_Bone : TRagDollBone;
    lLowerArm_Bone : TRagDollBone;
    rUpperLeg_Bone : TRagDollBone;
    rLowerLeg_Bone : TRagDollBone;
    lUpperLeg_Bone : TRagDollBone;
    lLowerLeg_Bone : TRagDollBone;
    Bone : array of TRagDollBone;
    constructor Create( pPosition, pVelocity, pRotation : TVector3f; pHangByHead : Boolean = False );
    destructor Destroy; override;
    procedure Render;
  end;

  PBox = ^TBox;
  TBox = record
    Body : PNewtonBody;
    Size : TVector3f;
    Matrix : TMatrix4f;
    Color : TVector3f;
    ListID : glUInt;
  end;

  TBoxContainer = record
    Box : array of TBox;
  end;


const
  GLBoxData    : array[ 0..71 ] of Single =
    ( -1, 1, 1, 1, 1, 1, 1, -1, 1, -1, -1, 1,
    -1, 1, -1, 1, 1, -1, 1, -1, -1, -1, -1, -1,
    -1, 1, 1, -1, 1, -1, -1, -1, -1, -1, -1, 1,
    1, 1, 1, 1, 1, -1, 1, -1, -1, 1, -1, 1,
    -1, 1, 1, -1, 1, -1, 1, 1, -1, 1, 1, 1,
    -1, -1, 1, -1, -1, -1, 1, -1, -1, 1, -1, 1 );
  GLBoxNormal  : array[ 0..71 ] of Single =
    ( 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, -1, 0, 0, -1, 0, 0, -1,
    -1, 0, 0, -1, 0, 0, -1, 0, 0, -1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0,
    0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, -1, 0, 0, -1, 0, 0, -1, 0 );
  NewtonGravity = -9.8;

var
  NewtonWorld  : PNewtonWorld;
  RagDoll      : array of TRagDoll;
  BoxContainer : TBoxContainer;

  Q            : PgluQuadric;

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
  SceneZoom    : Single = -45;

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
//  AddBox
// =============================================================================
//  Add a static box the scene.
// =============================================================================

procedure AddBox( var pBoxContainer : TBoxContainer; pPosition, pSize, pColor : TVector3f );
var
  Collider     : PNewtonCollision;
begin
  SetLength( pBoxContainer.Box, Length( pBoxContainer.Box ) + 1 );
  with pBoxContainer.Box[ High( pBoxContainer.Box ) ] do
  begin
    Size := pSize;
    Matrix_SetIdentity( Matrix );
    Matrix_SetTransform( Matrix, V3( pPosition[ 0 ], pPosition[ 1 ], pPosition[ 2 ] ) );
    Collider := NewtonCreateBox( NewtonWorld, Size[ 0 ], Size[ 1 ], Size[ 2 ], nil );
    Body := NewtonCreateBody( NewtonWorld, Collider );
    Color := pColor;
    NewtonBodySetMatrix( Body, @Matrix );
  end;
end;

// =============================================================================
//  AddChildren
// =============================================================================

procedure AddChildren( var pParent : TRagDollBone; pChildren : TRagDollBone );
begin
  SetLength( pParent.Children, Length( pParent.Children ) + 1 );
  pParent.Children[ High( pParent.Children ) ] := pChildren;
end;

// =============================================================================
//  TRagDoll.Create
// =============================================================================

// =============================================================================
//  TRagDollBone.Create
// =============================================================================

constructor TRagDollBone.Create( pPrimitiveType : TRagDollBonePrimitiveType );
begin
  PrimitiveType := pPrimitiveType;
end;

// =============================================================================
//  TRagDollBone.Render
// =============================================================================

procedure TRagDollBone.Render;
var
  Matrix       : TMatrix4f;
  M            : TMatrix4f;
  V            : TVector3f;
  mH           : Single;
  i            : Integer;
begin
  if Body = nil then
    exit;
  if glIsList( ListID ) = GL_TRUE then
  begin
    glPushMatrix;
    NewtonBodyGetMatrix( Body, @Matrix[ 0, 0 ] );
    glMultMatrixf( @Matrix[ 0, 0 ] );
    glCallList( ListID );
    glPopMatrix;
    exit;
  end;
  ListID := glGenLists( 1 );
  glNewList( ListID, GL_COMPILE );
  glColor3f( Color[ 0 ], Color[ 1 ], Color[ 2 ] );
  case PrimitiveType of
    ptBox :
      begin
        glScalef( Size[ 0 ] / 2, Size[ 1 ] / 2, Size[ 2 ] / 2 );
        glBegin( GL_QUADS );
        for i := 0 to 23 do
        begin
          glNormal3f( GLBoxNormal[ i * 3 ], GLBoxNormal[ i * 3 + 1 ], GLBoxNormal[ i * 3 + 2 ] );
          glVertex3f( GLBoxData[ i * 3 ], GLBoxData[ i * 3 + 1 ], GLBoxData[ i * 3 + 2 ] );
        end;
        glEnd;
        glColor3f( 1, 1, 1 );
      end;
    ptSphere :
      begin
        gluSphere( Q, Size[ 0 ] / 2, 16, 16 );
      end;
    ptCylinder :
      begin
        mH := Size[ 0 ] - Size[ 1 ] / 2 * 2;
        M := Matrix_MakeYawMatrix( PI * 0.5 );
        V := V3( 0, 0, -mH * 0.5 );
        Matrix_RotateVect( M, V );
        Matrix_SetTransform( M, V );
        glPushMatrix;
        glMultMatrixf( @M[ 0, 0 ] );
        gluCylinder( Q, Size[ 1 ] / 2, Size[ 1 ] / 2, mH, 16, 1 );
        gluSphere( Q, Size[ 1 ] / 2, 16, 16 );
        glTranslatef( 0, 0, mH );
        gluSphere( Q, Size[ 1 ] / 2, 16, 16 );
        glPopMatrix;
      end;
  end;
  glEndList;
end;

// =============================================================================
//  TRagDoll
// =============================================================================

// =============================================================================
//  TRagDoll.Create
// =============================================================================

constructor TRagDoll.Create( pPosition, pVelocity, pRotation : TVector3f; pHangByHead : Boolean = False );
const
  HeadSize     = 1;
var
  SpineSize    : TVector3f;
  HeadBody     : PNewtonBody;
  HeadBone     : PNewtonRagDollBone;
  Mat          : TMatrix4f;
  Pivot        : TVector3f;
  MainColor    : TVector3f;
  SkinColor    : TVector3f;
begin
  MainColor := V3( Random, Random, Random );
  SkinColor := V3( 0.95, 0.8, 0.95 );
// set the lower spine bone (this is the root of the rag doll)
  Spine0_Bone := TRagDollBone.Create( ptBox );
  Spine0_Bone.Color := MainColor;
  SpineSize := V3( HeadSize * 1.2, HeadSize, HeadSize * 1.2 );
  Spine0_Bone.Size := SpineSize;
  Matrix_SetIdentity( Spine0_Bone.Matrix );
  Matrix_SetTransform( Spine0_Bone.Matrix, V3( pPosition[ 0 ], pPosition[ 1 ], pPosition[ 2 ] ) );
//Matrix_SetRotation(Spine0_Bone.Matrix, V3(90, 0, 90));
  Matrix_SetRotation( Spine0_Bone.Matrix, pRotation );
  Spine0_Bone.ID := 0;
  Spine0_Bone.Name := 'Spine0_Bone';

// set the upper spine bone
  Spine1_Bone := TRagDollBone.Create( ptBox );
  Spine1_Bone.Size := SpineSize;
  Spine1_Bone.Color := MainColor;
  Matrix_SetIdentity( Spine1_Bone.Matrix );
  Matrix_SetTransform( Spine1_Bone.Matrix, V3( SpineSize[ 0 ], 0, 0 ) );
  Matrix_SetRotation( Spine1_Bone.Matrix, V3( 0, 0, 0 ) );
  Spine1_Bone.ID := 1;
  Spine1_Bone.Name := 'Spine1_Bone';

// add the head
  head_Bone := TRagDollBone.Create( ptSphere );
  head_Bone.Color := SkinColor;
  head_Bone.Size := V3( HeadSize, HeadSize, HeadSize );
  Matrix_SetIdentity( head_Bone.Matrix );
  Matrix_SetTransform( head_Bone.Matrix, V3( SpineSize[ 0 ], 0, 0 ) );
  Matrix_SetRotation( head_Bone.Matrix, V3( 0, 0, 0 ) );
  head_Bone.ID := 2;
  head_Bone.Name := 'head_Bone';

// Right Upper arm
  rUpperArm_Bone := TRagDollBone.Create( ptCylinder );
  rUpperArm_Bone.Color := MainColor;
  rUpperArm_Bone.Size := V3( HeadSize * 1.5, HeadSize * 0.5, HeadSize * 0.5 );
  Matrix_SetIdentity( rUpperArm_Bone.Matrix );
  Matrix_SetTransform( rUpperArm_Bone.Matrix, V3( SpineSize[ 0 ], 0, SpineSize[ 0 ] * 0.55 ) );
  Matrix_SetRotation( rUpperArm_Bone.Matrix, V3( 90, -90, 90 ) );
  rUpperArm_Bone.ID := 3;
  rUpperArm_Bone.Name := 'rUpperArm_Bone';

// Right Lower arm
  rLowerArm_Bone := TRagDollBone.Create( ptCylinder );
  rLowerArm_Bone.Color := SkinColor;
  rLowerArm_Bone.Size := V3( HeadSize * 1.5, HeadSize * 0.5, HeadSize * 0.5 );
  Matrix_SetIdentity( rLowerArm_Bone.Matrix );
  Matrix_SetTransform( rLowerArm_Bone.Matrix, V3( HeadSize * 1.5, 0, 0 ) );
  Matrix_SetRotation( rLowerArm_Bone.Matrix, V3( 0, 0, 0 ) );
  rLowerArm_Bone.ID := 4;
  rLowerArm_Bone.Name := 'rLowerArm_Bone';

// Left Upper arm
  lUpperArm_Bone := TRagDollBone.Create( ptCylinder );
  lUpperArm_Bone.Color := MainColor;
  lUpperArm_Bone.Size := V3( HeadSize * 1.5, HeadSize * 0.5, HeadSize * 0.5 );
  Matrix_SetIdentity( lUpperArm_Bone.Matrix );
  Matrix_SetTransform( lUpperArm_Bone.Matrix, V3( SpineSize[ 0 ], 0, -SpineSize[ 0 ] * 0.55 ) );
  Matrix_SetRotation( lUpperArm_Bone.Matrix, V3( -90, 90, 90 ) );
  lUpperArm_Bone.ID := 5;
  lUpperArm_Bone.Name := 'lUpperArm_Bone';

// Left Lower arm
  lLowerArm_Bone := TRagDollBone.Create( ptCylinder );
  lLowerArm_Bone.Color := SkinColor;
  lLowerArm_Bone.Size := V3( HeadSize * 1.5, HeadSize * 0.5, HeadSize * 0.5 );
  Matrix_SetIdentity( lLowerArm_Bone.Matrix );
  Matrix_SetTransform( lLowerArm_Bone.Matrix, V3( HeadSize * 1.5, 0, 0 ) );
  Matrix_SetRotation( lLowerArm_Bone.Matrix, V3( 0, 0, 0 ) );
  lLowerArm_Bone.ID := 6;
  lLowerArm_Bone.Name := 'lLowerArm_Bone';

// Right Upper leg
  rUpperLeg_Bone := TRagDollBone.Create( ptCylinder );
  rUpperLeg_Bone.Color := MainColor;
  rUpperLeg_Bone.Size := V3( HeadSize * 2.0, HeadSize * 0.75, HeadSize * 0.75 );
  Matrix_SetIdentity( rUpperleg_Bone.Matrix );
  Matrix_SetTransform( rUpperleg_Bone.Matrix, V3( 0, 0, SpineSize[ 0 ] * 0.35 ) );
  Matrix_SetRotation( rUpperleg_Bone.Matrix, V3( 0, 0, 180 ) );
  rUpperleg_Bone.ID := 7;
  rUpperleg_Bone.Name := 'rUpperleg_Bone';

// Right Lower leg
  rLowerleg_Bone := TRagDollBone.Create( ptCylinder );
  rLowerleg_Bone.Color := SkinColor;
  rLowerleg_Bone.Size := V3( HeadSize * 2.0, HeadSize * 0.65, HeadSize * 0.65 );
  Matrix_SetIdentity( rLowerleg_Bone.Matrix );
  Matrix_SetTransform( rLowerleg_Bone.Matrix, V3( HeadSize * 2, 0, 0 ) );
  Matrix_SetRotation( rLowerleg_Bone.Matrix, V3( 0, 0, 0 ) );
  rLowerleg_Bone.ID := 8;
  rLowerleg_Bone.Name := 'rLowerleg_Bone';

// Left Upper leg
  lUpperleg_Bone := TRagDollBone.Create( ptCylinder );
  lUpperleg_Bone.Color := MainColor;
  lUpperleg_Bone.Size := V3( HeadSize * 2.0, HeadSize * 0.75, HeadSize * 0.75 );
  Matrix_SetIdentity( lUpperleg_Bone.Matrix );
  Matrix_SetTransform( lUpperleg_Bone.Matrix, V3( 0, 0, -SpineSize[ 0 ] * 0.35 ) );
  Matrix_SetRotation( lUpperleg_Bone.Matrix, V3( 0, 0, 180 ) );
  lUpperleg_Bone.ID := 9;
  lUpperleg_Bone.Name := 'lUpperleg_Bone';

// Left Lower leg
  lLowerleg_Bone := TRagDollBone.Create( ptCylinder );
  lLowerleg_Bone.Color := SkinColor;
  lLowerleg_Bone.Size := V3( HeadSize * 2.0, HeadSize * 0.65, HeadSize * 0.65 );
  Matrix_SetIdentity( lLowerleg_Bone.Matrix );
  Matrix_SetTransform( lLowerleg_Bone.Matrix, V3( HeadSize * 2, 0, 0 ) );
  Matrix_SetRotation( lLowerleg_Bone.Matrix, V3( 0, 0, 0 ) );
  lLowerleg_Bone.ID := 10;
  lLowerleg_Bone.Name := 'lLowerleg_Bone';

// Build hierarchy
  AddChildren( Spine0_Bone, Spine1_Bone );
  AddChildren( Spine1_Bone, Head_Bone );
  AddChildren( Spine1_Bone, rUpperArm_Bone );
  AddChildren( rUpperArm_Bone, rLowerArm_Bone );
  AddChildren( Spine1_Bone, lUpperArm_Bone );
  AddChildren( lUpperArm_Bone, lLowerArm_Bone );
  AddChildren( Spine0_Bone, rUpperleg_Bone );
  AddChildren( rUpperleg_Bone, rLowerleg_Bone );
  AddChildren( Spine0_Bone, lUpperleg_Bone );
  AddChildren( lUpperleg_Bone, lLowerleg_Bone );

// Store all bones in an array for easier access
  SetLength( Bone, 11 );
  Bone[ 0 ] := Spine0_Bone;
  Bone[ 1 ] := Spine1_Bone;
  Bone[ 2 ] := Head_Bone;
  Bone[ 3 ] := rUpperArm_Bone;
  Bone[ 4 ] := rLowerArm_Bone;
  Bone[ 5 ] := lUpperArm_Bone;
  Bone[ 6 ] := lLowerArm_Bone;
  Bone[ 7 ] := rUpperleg_Bone;
  Bone[ 8 ] := rLowerleg_Bone;
  Bone[ 9 ] := lUpperleg_Bone;
  Bone[ 10 ] := lLowerleg_Bone;

// Physics

//create a ragdoll container
  RagDoll := NewtonCreateRagDoll( NewtonWorld );
// begin adding bones to the rad doll
  NewtonRagDollBegin( RagDoll );
// set the force function
  NewtonRagDollSetForceAndTorqueCallback( RagDoll, PhysicsApplyGravityForce );
// Build ragdoll
  BuildRagDoll( RagDoll, nil, pVelocity, Spine0_Bone );
// finalize the rag doll
  NewtonRagDollEnd( RagDoll );
  if pHangByHead then
  begin
 // hang ragdoll by the head
 // the head bone is bone #2
    HeadBone := NewtonRagDollFindBone( RagDoll, 2 );
    HeadBody := NewtonRagDollBoneGetBody( HeadBone );
 // get the pivot point
    NewtonBodyGetMatrix( HeadBody, @Mat[ 0, 0 ] );
    Pivot := V3( Mat[ 3, 0 ], Mat[ 3, 1 ], Mat[ 3, 2 ] );
    NewtonConstraintCreateBall( NewtonWorld, @Pivot[ 0 ], HeadBody, nil );
  end;
end;

// =============================================================================
//  TRagDoll.BuildRagDoll
// =============================================================================
//  Build the bones of the ragdoll (recursive calls all childrens)
// =============================================================================

procedure TRagDoll.BuildRagDoll( pRagDoll : PNewtonRagDoll; pParentDollBone : PNewtonRagDollBone; pVelocity : TVector3f; var pBone : TRagDollBone );
var
  Body         : PNewtonBody;
  DollBone     : PNewtonRagDollBone;
  Collider     : PNewtonCollision;
  Size         : TVector3f;
  Matrix       : TMatrix4f;
  ConeDir      : TVector3f;
  LateralDir   : TVector3f;
  i            : Integer;
begin
  Size := pBone.Size;
  Matrix := pBone.Matrix;
// Create a collider for this bone. Note : It can any shape Newton has to offer,
// even convex hulls or compounds
  Collider := nil;
  case pBone.PrimitiveType of
    ptBox : Collider := NewtonCreateBox( NewtonWorld, Size[ 0 ], Size[ 1 ], Size[ 2 ], nil );
    ptCylinder : Collider := NewtonCreateCapsule( NewtonWorld, Size[ 1 ] / 2, Size[ 0 ], nil );
    ptSphere : Collider := NewtonCreateSphere( NewtonWorld, Size[ 0 ] / 2, Size[ 1 ] / 2, Size[ 2 ] / 2, nil );
  end;
// Create a rag doll bone at the origin of this bone
  DollBone := NewtonRagDollAddBone( pRagDoll, pParentDollBone, nil, 10, @Matrix[ 0 ], Collider, @Size[ 0 ] );
  NewtonRagDollBoneSetID( DollBone, pBone.ID );
// Release collider
  NewtonReleaseCollision( NewtonWorld, Collider );
// Set limits for the joints
// The x axis is usually the box main axis
  ConeDir := V3( 1, 0, 0 );
// The z axis is usually the box plane
  LateralDir := V3( 0, 0, 1 );
// The axis limits are specifiied in global space, rotate by the joint matrix
  Matrix_RotateVect( pBone.Matrix, ConeDir );
  Matrix_RotateVect( pBone.Matrix, LateralDir );
// Set the limis
  NewtonRagDollBoneSetLimits( DollBone, @ConeDir[ 0 ], -PI * 0.25, PI * 0.25, -PI * 0.5, @LateralDir[ 0 ], 0, 0 );
  Body := NewtonRagDollBoneGetBody( DollBone );
  pBone.Body := Body;
  if Length( pBone.Children ) > 0 then
    for i := 0 to High( pBone.Children ) do
      BuildRagDoll( pRagDoll, DollBone, pVelocity, pBone.Children[ i ] );
end;

// =============================================================================
//  TRagDoll.Destroy
// =============================================================================

destructor TRagDoll.Destroy;
var
  i            : Integer;
begin
  for i := 0 to High( Bone ) do
    Bone[ i ].Free;
end;

// =============================================================================
//  TRagDoll.Render
// =============================================================================

procedure TRagDoll.Render;
var
  i            : Integer;
begin
  for i := 0 to High( Bone ) do
    Bone[ i ].Render;
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
  x, y         : Integer;
begin
  Randomize;
// Create a newton world
  NewtonWorld := NewtonCreate( nil, nil );
  NewtonSetPlatformArchitecture( NewtonWorld, 0 );
  NewtonSetMinimumFrameRate( NewtonWorld, 60 );
// Spawn some random ragdolls
  SetLength( RagDoll, 5 );
  for x := 0 to High( RagDoll ) do
  begin
// RagDoll[0] := TRagDoll.Create(V3(0,15,0), V3(1,1,1), V3(Random(360), Random(360), Random(360)), False);
    RagDoll[ x ] := TRagDoll.Create( V3( Random * 10 - Random * 10, 30 + Random( 20 ), Random * 10 - Random * 10 ), V3( 1, 1, 1 ), V3( Random( 360 ), Random( 360 ), Random( 360 ) ), False );
  end;

// Create a floor
  AddBox( BoxContainer, V3( 0, -5, 0 ), V3( 60, 1, 60 ), V3( 0.5, 0.5, 0.5 ) );
  for x := 0 to 5 do
    for y := 0 to 3 do
      AddBox( BoxContainer, V3( 0, y * 5 + ( ( x mod 2 ) * 2.5 ), -15 + x * 5 + ( ( y mod 2 ) * 2.5 ) ), V3( 12, 0.5, 0.5 ), V3( 0.7, 0.7, 0.7 ) );
end;

// =============================================================================
//  InitGL
// =============================================================================

procedure InitGL;
begin
// Set up states
  glEnable( GL_DEPTH_TEST );
  glEnable( GL_COLOR_MATERIAL );
  glEnable( GL_NORMALIZE );
  glDepthFunc( GL_LESS );
  glClearColor( 0, 0, 0.3, 0 );
end;

// =============================================================================
//  Render
// =============================================================================

procedure Render;
var
  M            : TMatrix4f;
  i, j         : Integer;
begin
  AccTimeSlice := AccTimeSlice + ( SDL_GetTicks - TimeLastFrame );
  TimeLastFrame := SDL_GetTicks;

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

// Render the scene

  glTranslatef( 0, 0, SceneZoom );
  glRotatef( SceneRotation[ 0 ], 1, 0, 0 );
  glRotatef( SceneRotation[ 1 ], 0, 1, 0 );
  glRotatef( SceneRotation[ 2 ], 0, 0, 1 );


  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );
//glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);

// Render all ragdolls
  if Length( RagDoll ) > 0 then
    for i := 0 to High( RagDoll ) do
      RagDoll[ i ].Render;

// Render all static boxes
  if Length( BoxContainer.Box ) > 0 then
    for i := 0 to High( BoxContainer.Box ) do
      with BoxContainer.Box[ i ] do
      begin
        if glIsList( ListID ) = GL_TRUE then
        begin
          NewtonBodyGetMatrix( Body, @Matrix );
          glPushMatrix;
          glMultMatrixf( @Matrix );
          glCallList( ListID );
          glPopMatrix;
        end
        else
        begin
          ListID := glGenLists( 1 );
          glNewList( ListID, GL_COMPILE );
          glColor3f( Color[ 0 ], Color[ 1 ], Color[ 2 ] );
          glScalef( Size[ 0 ] / 2, Size[ 1 ] / 2, Size[ 2 ] / 2 );
          glBegin( GL_QUADS );
          for j := 0 to 23 do
          begin
            glNormal3f( GLBoxNormal[ j * 3 ], GLBoxNormal[ j * 3 + 1 ], GLBoxNormal[ j * 3 + 2 ] );
            glVertex3f( GLBoxData[ j * 3 ], GLBoxData[ j * 3 + 1 ], GLBoxData[ j * 3 + 2 ] );
          end;
          glEnd;
          glColor3f( 1, 1, 1 );
          glEndList;
        end;
      end;

  glDisable( GL_LIGHTING );

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
var
  i            : Integer;
begin
  gluDeleteQuadric( Q );
  if Length( RagDoll ) > 0 then
    for i := 0 to High( RagDoll ) do
      RagDoll[ i ].Free;
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
    SDLK_RETURN :
      begin
        if Length( RagDoll ) > 100 then
          exit;
        SetLength( RagDoll, Length( RagDoll ) + 1 );
        RagDoll[ High( RagDoll ) ] := TRagDoll.Create( V3( Random * 10 - Random * 10, 30, Random * 10 - Random * 10 ), V3( 1, 1, 1 ), V3( Random( 360 ), Random( 360 ), Random( 360 ) ), False );
      end;
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
    PickedForce := V3( ( P[ 0 ] - p2[ 0 ] ) * 0.4, ( P[ 1 ] - p2[ 1 ] ) * 0.4, ( P[ 2 ] - p2[ 2 ] ) * 0.4 );
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
  SDL_WM_SetCaption( 'Newton - RagDoll Demo (SDL) - 2005/06 by Sascha Willems', nil );
// Create SDL surface
  Surface := SDL_SetVideoMode( 640, 480, 32, Videoflags );
  if not Assigned( Surface ) then
  begin
    TerminateApplication;
  end;

  InitGL;
  ReSizeWindow( 640, 480 );
  InitNewton;
  TimeLastFrame := SDL_GetTicks;
  SceneRotation := V3( 45, 45, 0 );
  Q := gluNewQuadric;
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

