// =============================================================================
//
//  NewtonPlayGround_Primitives.pas
//   Unit to load and manage all collision primitives newton has to offer
//
// =============================================================================
//
//   Copyright © 2004 by Sascha Willems (http://www.delphigl.de)
//
// =============================================================================
//
//   Contents of this file are subject to the GNU Public License (GPL) which can
//   be obtained here : http://opensource.org/licenses/gpl-license.php
//
// =============================================================================

unit glPrimitives;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
 glMatrixHelper,
 gl,
 glu,
 glext;

type
 // TNewtonPrimitive ===========================================================
 //  Base class for all wrapped collision primitives
 TNewtonPrimitive = class(TObject)
   Matrix      : TMatrix4f;
   StartMatrix : TMatrix4f;
   MassMatrix  : TVector4f;
   Color       : TVector3f;
   Rotation    : TVector3f;
   ListID      : glUInt;
   ID          : Cardinal;
   constructor Create;
   destructor Destroy; override;
   procedure Render; virtual; abstract;
   procedure SetMatrix(pNewMatrix : TMatrix4f);
  end;

 // TNewtonSphere ==============================================================
 TNewtonSphere = class(TNewtonPrimitive)
   Size : Single;
   constructor Create(pSize : Single; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonBox =================================================================
 TNewtonBox = class(TNewtonPrimitive)
   Size : TVector3f;
   constructor Create(pSize : TVector3f; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonCylinder ============================================================
 TNewtonCylinder = class(TNewtonPrimitive)
   Radius : Single;
   Height : Single;
   constructor Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonCone ================================================================
 TNewtonCone = class(TNewtonPrimitive)
   Radius : Single;
   Height : Single;
   constructor Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonCapsule =============================================================
 TNewtonCapsule = class(TNewtonPrimitive)
   Radius : Single;
   Height : Single;
   constructor Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonChamferCylinder =====================================================
 TNewtonChamferCylinder = class(TNewtonPrimitive)
   Radius     : Single;
   Height     : Single;
   RadiusSave : Single;
   HeightSave : Single;
   constructor Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
   procedure Render; override;
  end;

 // TNewtonPrimitiveManager ====================================================
 //  Spawns and manages all primitives
 TNewtonPrimitiveManager = class
   Primitive     : array of TNewtonPrimitive;
   constructor Create;
   destructor Destroy; reintroduce;
   function GetPrimitiveCount : Cardinal;
   procedure Render;
   procedure ClearAll(pTotalClean : Boolean = False);
   procedure ClearListIDs;
   function SpawnSphere(pSize : Single; pMatrix : TMatrix4f) : TNewtonSphere;
   function SpawnBox(pSize : TVector3f; pMatrix : TMatrix4f) : TNewtonBox;
   function SpawnCylinder(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCylinder;
   function SpawnCone(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCone;
   function SpawnCapsule(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCapsule;
   function SpawnChamferCylinder(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonChamferCylinder;
  end;

var
 PrimitiveManager : TNewtonPrimitiveManager;
 Q                : PGLUQuadric;

implementation


// *****************************************************************************
//  TNewtonPrimitive
// *****************************************************************************

// =============================================================================
//  TNewtonPrimitive.Create
// =============================================================================
constructor TNewtonPrimitive.Create;
begin
Color := V3(0.8, 0.8, 0.8);
Color := V3(0.2+Random, 0.2+Random, 0.2+Random);
end;

// =============================================================================
//  TNewtonPrimitive.Destroy
// =============================================================================
destructor TNewtonPrimitive.Destroy;
begin
if ListID > 0 then
 glDeleteLists(ListID, 1);
inherited Destroy;
end;

// =============================================================================
//  TNewtonPrimitive.SetMatrix
// =============================================================================
procedure TNewtonPrimitive.SetMatrix(pNewMatrix : TMatrix4f);
begin
Matrix := pNewMatrix;
end;

// *****************************************************************************
//  TNewtonSphere
// *****************************************************************************

// =============================================================================
//  TNewtonSphere.Create
// =============================================================================
constructor TNewtonSphere.Create(pSize : Single; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Size        := pSize;
inherited Create;
end;

// =============================================================================
//  TNewtonSphere.Render
// =============================================================================
procedure TNewtonSphere.Render;
begin
if ListID > 0 then
 begin
 glPushMatrix;
  glMultMatrixf(@Matrix[0,0]);
  glRotatef(1, Rotation[0], 0, 0);
  glRotatef(1, 0, Rotation[1], 0);
  glRotatef(1, 0, 0, Rotation[2]);
  glCallList(ListID);
 glPopMatrix;
 exit;
 end;
ListID := glGenLists(1);
glNewList(ListID, GL_COMPILE);
 glColor3f(Color[0], Color[1], Color[2]);
 glPushMatrix;
  glScalef(Size, Size, Size);
  gluSphere(Q, 1, 20, 20);
 glPopMatrix;
 glColor3f(1,1,1);
glEndList;
end;

// *****************************************************************************
//  TNewtonBox
// *****************************************************************************

// =============================================================================
//  TNewtonBox.Create
// =============================================================================
constructor TNewtonBox.Create(pSize : TVector3f; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Size        := pSize;
inherited Create;
end;

// =============================================================================
//  TNewtonBox.Render
// =============================================================================
procedure TNewtonBox.Render;
const
 Box : array[0..143] of Single =
  (-1,-1,1, 1,-1,1, 1,1,1, -1,1,1,
   -1,1,-1, 1,1,-1, 1,-1,-1, -1,-1,-1,
   -1,1,1, -1,1,-1, -1,-1,-1, -1,-1,1,
   1,-1,1, 1,-1,-1, 1,1,-1, 1,1,1,
   1,1,1, 1,1,-1, -1,1,-1, -1,1,1,
   -1,-1,1, -1,-1,-1, 1,-1,-1, 1,-1,1,
   0,0,1,0,0,1,0,0,1,0,0,1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,
   -1,0,0,-1,0,0,-1,0,0,-1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,
   0,1,0,0,1,0,0,1,0, 0,1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0);
var
 i : Integer;
begin
if ListID > 0 then
 begin
 glColor3f(Color[0], Color[1], Color[2]);
 glPushMatrix;
  glMultMatrixf(@Matrix[0,0]);
  glRotatef(Rotation[0], 1, 0, 0);
  glRotatef(Rotation[1], 0, 1, 0);
  glRotatef(Rotation[2], 0, 0, 1);
  glCallList(ListID);
 glPopMatrix;
 glColor3f(1,1,1);
 end;
ListID := glGenLists(1);
glNewList(ListID, GL_COMPILE);
 glScalef(Size[0]/2, Size[1]/2, Size[2]/2);
 glBegin(GL_QUADS);
  for i := 0 to 23 do
   begin
   glNormal3f(Box[i*3+72], Box[i*3+73], Box[i*3+74]);
   glVertex3f(Box[i*3], Box[i*3+1], Box[i*3+2]);
   end;
 glEnd;
glEndList;
end;

// *****************************************************************************
//  TNewtonCylinder
// *****************************************************************************

// =============================================================================
//  TNewtonCylinder.Create
// =============================================================================
constructor TNewtonCylinder.Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Radius      := pRadius;
Height      := pHeight;
inherited Create;
end;

// =============================================================================
//  TNewtonCylinder.Render
// =============================================================================
procedure TNewtonCylinder.Render;
var
 M : TMatrix4f;
 V : TVector3f;
begin
if ListID > 0 then
 begin
 glPushMatrix;
  glMultMatrixf(@Matrix[0,0]);
  glRotatef(Rotation[0], 1, 0, 0);
  glRotatef(Rotation[1], 0, 1, 0);
  glRotatef(Rotation[2], 0, 0, 1);
  glCallList(ListID);
 glPopMatrix;
 exit;
 end;
ListID := glGenLists(1);
glNewList(ListID, GL_COMPILE);
 M := Matrix_MakeYawMatrix(PI * 0.5);
 V := V3(0, 0, -Height*0.5);
 Matrix_RotateVect(M, V);
 Matrix_SetTransform(M, V);
 glColor3f(Color[0], Color[1], Color[2]);
 glPushMatrix;
  glMultMatrixf(@M[0,0]);
  gluCylinder(Q, Radius, Radius, Height, 24, 1);
  gluQuadricOrientation(Q, GLU_INSIDE);
  gluDisk(Q, 0, Radius, 24, 1);
  gluQuadricOrientation(Q, GLU_OUTSIDE);
  glTranslatef(0, 0, Height);
  gluDisk(Q, 0, Radius, 24, 1);
 glPopMatrix;
 glColor3f(1,1,1);
glEndList;
end;

// *****************************************************************************
//  TNewtonCone
// *****************************************************************************

// =============================================================================
//  TNewtonCone.Create
// =============================================================================
constructor TNewtonCone.Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Radius      := pRadius;
Height      := pHeight;
inherited Create;
end;

// =============================================================================
//  TNewtonCone.Render
// =============================================================================
procedure TNewtonCone.Render;
var
 M : TMatrix4f;
 V : TVector3f;
begin
if ListID > 0 then
 begin
 glPushMatrix;
  glMultMatrixf(@Matrix[0,0]);
  glRotatef(Rotation[0], 1, 0, 0);
  glRotatef(Rotation[1], 0, 1, 0);
  glRotatef(Rotation[2], 0, 0, 1);
  glCallList(ListID);
 glPopMatrix;
 exit;
 end;
ListID := glGenLists(1);
glNewList(ListID, GL_COMPILE);
 M := Matrix_MakeYawMatrix(PI * 0.5);
 V := V3(0, 0, -Height*0.5);
 Matrix_RotateVect(M, V);
 Matrix_SetTransform(M, V);
 glColor3f(Color[0], Color[1], Color[2]);
 glPushMatrix;
  glMultMatrixf(@M[0,0]);
  gluCylinder(Q, 0, Radius, Height, 24, 1);
  glTranslatef(0, 0, Height);
  gluDisk(Q, 0, Radius, 24, 1);
 glPopMatrix;
 glColor3f(1,1,1);
glEndList;
end;

// *****************************************************************************
//  TNewtonCapsule
// *****************************************************************************

// =============================================================================
//  TNewtonCapsule.Create
// =============================================================================
constructor TNewtonCapsule.Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Radius      := pRadius;
Height      := pHeight;
inherited Create;
end;

// =============================================================================
//  TNewtonCapsule.Render
// =============================================================================
procedure TNewtonCapsule.Render;
var
 M  : TMatrix4f;
 V  : TVector3f;
 mH : Single;
begin
mH := Height - Radius*2;
M := Matrix_MakeYawMatrix(PI * 0.5);
V := V3(0, 0, -mH*0.5);
Matrix_RotateVect(M, V);
Matrix_SetTransform(M, V);
M := Matrix_Multiply(M, Matrix);
glColor3f(Color[0], Color[1], Color[2]);
glPushMatrix;
 glMultMatrixf(@M[0,0]);
 glRotatef(Rotation[0], 1, 0, 0);
 glRotatef(Rotation[1], 0, 1, 0);
 glRotatef(Rotation[2], 0, 0, 1);
 gluCylinder(Q, Radius, Radius, mH, 20, 2);
 gluSphere(Q, Radius, 20, 20);
 glTranslatef(0, 0, mH);
 gluSphere(Q, Radius, 20, 20);
glPopMatrix;
glColor3f(1,1,1);
end;

// *****************************************************************************
//  TNewtonChamferCylinder
// *****************************************************************************

// =============================================================================
//  TNewtonChamferCylinder.Create
// =============================================================================
constructor TNewtonChamferCylinder.Create(pRadius, pHeight : Single; pMatrix : TMatrix4f);
begin
StartMatrix := pMatrix;
Matrix      := pMatrix;
Radius      := pRadius;
Height      := pHeight;
RadiusSave  := pRadius;
HeightSave  := pHeight;
inherited Create;
end;

// =============================================================================
//  TNewtonChamferCylinder.Render
// =============================================================================
procedure TNewtonChamferCylinder.Render;
const
 Slices     = 12;
 Breaks     = Slices*2;
var
 i,j        : Integer;
 SliceStep  : Single;
 SliceAngle : Single;
 Rot        : TMatrix4f;
 p0,p1      : TVector3f;
 q0,q1      : TVector3f;
 Normal     : TVector3f;
 tmp        : Single;
begin
if ListID > 0 then
 begin
 glPushMatrix;
  glMultMatrixf(@Matrix[0,0]);
  glRotatef(Rotation[0], 1, 0, 0);
  glRotatef(Rotation[1], 0, 1, 0);
  glRotatef(Rotation[2], 0, 0, 1);
  glCallList(ListID);
 glPopMatrix;
 exit;
 end;

SliceStep  := PI/Slices;
SliceAngle := 0;

ListID  := glGenLists(1);
Rot     := Matrix_MakePitchMatrix(SliceStep);
Height  := HeightSave * 0.5;
Radius  := RadiusSave * 0.5;

glNewList(ListID, GL_COMPILE);
 glColor3f(Color[0], Color[1], Color[2]);
 for j := 0 to Slices-1 do
  begin
  p0 := V3(Height*Cos(SliceAngle), 0, Radius+Height*Sin(SliceAngle));
  p1 := V3(Height*Cos(SliceAngle+SliceStep), 0, Radius+Height*Sin(SliceAngle+SliceStep));
  for i := 0 to Breaks-1 do
   begin
   q0 := p0;
   Matrix_RotateVect(Rot, q0);
   q1 := p1;
   Matrix_RotateVect(Rot, q1);
   glBegin(GL_QUADS);
    Normal := VCross(VSub(p0,q0), VSub(q1,q0));
    tmp    := Sqrt(Normal[0]*Normal[0] + Normal[1]*Normal[1] + Normal[2]*Normal[2]);
    glNormal3f(-Normal[0]/tmp, -Normal[1]/tmp, -Normal[2]/tmp);
  	glVertex3fv(@q0[0]);
	  glVertex3fv(@q1[0]);
  	glVertex3fv(@p1[0]);
  	glVertex3fv(@p0[0]);
	 glEnd;
	 p0 := q0;
	 p1 := q1;
   end;
  SliceAngle := SliceAngle+SliceStep;
  end;
 glBegin(GL_TRIANGLE_FAN);
  glNormal3f(-1, 0, 0);
  p0 := V3(-Height, 0, Radius);
  for i := 0 to Breaks-1 do
   begin
   glVertex3fv(@p0[0]);
   Matrix_RotateVect(Rot, p0);
   end;
 glEnd;
 glFrontFace(GL_CW);
 glBegin(GL_TRIANGLE_FAN);
  glNormal3f(1, 0, 0);
  p1 := V3(Height, 0, Radius);
  for i := 0 to Breaks-1 do
   begin
   glVertex3fv(@p1[0]);
   Matrix_RotateVect(Rot, p1);
   end;
 glEnd;
 glFrontFace(GL_CCW);
 glColor3f(1,1,1);
glEndList;
end;

// *****************************************************************************
//  TNewtonPrimitiveManager
// *****************************************************************************

// =============================================================================
//  TNewtonPrimitiveManager.Destroy
// =============================================================================
destructor TNewtonPrimitiveManager.Destroy;
var
 i : Integer;
begin
gluDeleteQuadric(Q);
if Length(Primitive) > 0 then
 for i := 0 to High(Primitive) do
  Primitive[i].Free;
end;

// =============================================================================
//  TNewtonPrimitiveManager.ClearAll
// =============================================================================
procedure TNewtonPrimitiveManager.ClearAll(pTotalClean : Boolean = False);
var
 i : Integer;
 s : Integer;
begin
if pTotalClean then
 s := 0
else
 s := 1;
if Length(Primitive) > s then
 for i := s to High(Primitive) do
  Primitive[i].Free;
SetLength(Primitive, s);
end;

// =============================================================================
//  TNewtonPrimitiveManager.ClearListIDs
// =============================================================================
procedure TNewtonPrimitiveManager.ClearListIDs;
var
 i : Integer;
begin
if Length(Primitive) > 0 then
 for i := 0 to High(Primitive) do
  begin
  glDeleteLists(Primitive[i].ListID, 1);
  Primitive[i].ListID := 0;
  end;
end;

// =============================================================================
//  TNewtonPrimitiveManager.Create
// =============================================================================
constructor TNewtonPrimitiveManager.Create;
begin
Q := gluNewQuadric;
end;

// =============================================================================
//  TNewtonPrimitiveManager.GetPrimitiveCount
// =============================================================================
function TNewtonPrimitiveManager.GetPrimitiveCount : Cardinal;
begin
Result := Length(Primitive);
end;

// =============================================================================
//  TNewtonPrimitiveManager.Render
// =============================================================================
procedure TNewtonPrimitiveManager.Render;
var
 i : Integer;
begin
glEnable(GL_CULL_FACE);
glCullFace(GL_BACK);
if Length(Primitive) > 0 then
 for i := 0 to High(Primitive) do
  if Assigned(Primitive[i]) then
   Primitive[i].Render;
glDisable(GL_CULL_FACE);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnSphere
// =============================================================================
function TNewtonPrimitiveManager.SpawnSphere(pSize : Single; pMatrix : TMatrix4f) : TNewtonSphere;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonSphere.Create(pSize, pMatrix);
Primitive[High(Primitive)].ID := High(Primitive);
Result := (Primitive[High(Primitive)] as TNewtonSphere);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnBox
// =============================================================================
function TNewtonPrimitiveManager.SpawnBox(pSize : TVector3f; pMatrix : TMatrix4f) : TNewtonBox;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonBox.Create(pSize, pMatrix);
Primitive[High(Primitive)].ID := High(Primitive);
Result := (Primitive[High(Primitive)] as TNewtonBox);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnCylinder
// =============================================================================
function TNewtonPrimitiveManager.SpawnCylinder(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCylinder;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonCylinder.Create(pRadius, pHeight, pMatrix);
Primitive[High(Primitive)].ID := High(Primitive);
Result := (Primitive[High(Primitive)] as TNewtonCylinder);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnCone
// =============================================================================
function TNewtonPrimitiveManager.SpawnCone(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCone;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonCone.Create(pRadius, pHeight, pMatrix);
Result := (Primitive[High(Primitive)] as TNewtonCone);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnChamferCylinder
// =============================================================================
function TNewtonPrimitiveManager.SpawnChamferCylinder(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonChamferCylinder;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonChamferCylinder.Create(pRadius, pHeight, pMatrix);
Primitive[High(Primitive)].ID := High(Primitive);
Result := (Primitive[High(Primitive)] as TNewtonChamferCylinder);
end;

// =============================================================================
//  TNewtonPrimitiveManager.SpawnCapsule
// =============================================================================
function TNewtonPrimitiveManager.SpawnCapsule(pRadius, pHeight : Single; pMatrix : TMatrix4f) : TNewtonCapsule;
begin
SetLength(Primitive, Length(Primitive)+1);
Primitive[High(Primitive)] := TNewtonCapsule.Create(pRadius, pHeight, pMatrix);
Primitive[High(Primitive)].ID := High(Primitive);
Result := (Primitive[High(Primitive)] as TNewtonCapsule);
end;


end.
