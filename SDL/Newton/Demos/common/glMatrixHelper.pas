// =============================================================================
//
//  glMatrixHelper
//   USmall unit with some matrix functions usefull for Newton and the GL
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
//
//   Some of the more complex matrix routines (like inverse, transpose) were
//   taken from Mike Lischke's geometry.pas, so credits for those functions go
//   out to him!
//
// =============================================================================

unit glMatrixHelper;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
 Math,
 gl,
 glu,
 glext;

type
 TVector3f = array[0..2] of Single;
 TVector4f = array[0..3] of Single;
 TPlane4f  = array[0..3] of Single;
 TMatrix4f = array[0..3, 0..3] of Single;

var
 NullMatrix4f   : TMatrix4f = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
 IdentityMatrix : TMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

const
  X = 0; A = 0;
  Y = 1; B = 1;
  Z = 2; C = 2;
  W = 3; D = 3;
  Epsilon = 0.0001;

function Matrix_MakeYawMatrix(Angle : Single) : TMatrix4f;
function Matrix_MakeRollMatrix(Angle : Single) : TMatrix4f;
function Matrix_MakePitchMatrix(Angle : Single) : TMatrix4f;
function Matrix_Multiply(m1 : TMatrix4f; m2 : TMatrix4f) : TMatrix4f;
function Matrix_Transpose(M : TMatrix4f) : TMatrix4f;
function Matrix_Changed(M1, M2 : TMatrix4f) : Boolean;
procedure Matrix_SetIdentity(var M : TMatrix4f);
procedure Matrix_SetTransform(var M : TMatrix4f; V : TVector3f);
procedure Matrix_SetRotation(var M : TMatrix4f; V : TVector3f);
procedure Matrix_RotateVect(const M : TMatrix4f; var pVect : TVector3f);
procedure Matrix_Inverse(var M: TMatrix4f);
function Matrix_TansformVector(const M : TMatrix4f; V : TVector3f) : TVector3f;
function Matrix_UntransformVector(const M : TMatrix4f; V : TVector3f) : TVector3f;
function Matrix_UnRotateVect(const M : TMatrix4f; pVect : TVector3f) : TVector3f;

function V4(x,y,z : Single) : TVector4f; overload;
function V4(x,y,z,w : Single) : TVector4f; overload;
function V3(x,y,z : Single) : TVector3f;
function VCross(pV1, pV2 : TVector3f) : TVector3f;
function VDot(pV1, pV2 : TVector3f) : Single;
function VTransform(pV1 : TVector3f; pM : TMatrix4f) : TVector3f;
function VSub(pV1, pV2 : TVector3f) : TVector3f;

implementation

// =============================================================================
//  V4
// =============================================================================
function V4(x,y,z : Single) : TVector4f;
begin
Result[0] := x;
Result[1] := y;
Result[2] := z;
Result[3] := 0;
end;

function V4(x,y,z,w : Single) : TVector4f;
begin
Result[0] := x;
Result[1] := y;
Result[2] := z;
Result[3] := w;
end;

// =============================================================================
//  V3
// =============================================================================
function V3(x,y,z : Single) : TVector3f;
begin
Result[0] := x;
Result[1] := y;
Result[2] := z;
end;

// =============================================================================
//  VCross
// =============================================================================
function VCross(pV1, pV2 : TVector3f) : TVector3f;
begin
Result[0] := (pV1[1]*pV2[2]) - (pV1[2]*pV2[1]);
Result[1] := (pV1[2]*pV2[0]) - (pV1[0]*pV2[2]);
Result[2] := (pV1[0]*pV2[1]) - (pV1[1]*pV2[0]);
end;

// =============================================================================
//  VDot
// =============================================================================
function VDot(pV1, pV2 : TVector3f) : Single;
begin
Result := (pV1[0]*pV2[0]) + (pV1[1]*pV2[1]) + (pV1[2]*pV2[2]);
end;

function VTransform(pV1 : TVector3f; pM : TMatrix4f) : TVector3f;
var
 TV : TVector3f;
begin
TV[X] := pV1[X] * pM[X, X] + pV1[Y] * pM[Y, X] + pV1[Z] * pM[Z, X] + pM[W, X];
TV[Y] := pV1[X] * pM[X, Y] + pV1[Y] * pM[Y, Y] + pV1[Z] * pM[Z, Y] + pM[W, Y];
TV[Z] := pV1[X] * pM[X, Z] + pV1[Y] * pM[Y, Z] + pV1[Z] * pM[Z, Z] + pM[W, Z];
//TV[W] := V[X] * M[X, W] + V[Y] * M[Y, W] + V[Z] * M[Z, W] + V[W] * M[W, W];
Result := TV
end;

// =============================================================================
//  VSub
// =============================================================================
function VSub(pV1, pV2 : TVector3f) : TVector3f;
begin
Result := V3(pV1[0]-pV2[0], pV1[1]-pV2[1], pV1[2]-pV2[2]);
end;

// =============================================================================
//  Matrix_SetIdentity
// =============================================================================
procedure Matrix_SetIdentity(var M : TMatrix4f);
begin
M[0,0] := 1; M[1,0] := 0; M[2,0] := 0; M[3,0] := 0;
M[0,1] := 0; M[1,1] := 1; M[2,1] := 0; M[3,1] := 0;
M[0,2] := 0; M[1,2] := 0; M[2,2] := 1; M[3,2] := 0;
M[0,3] := 0; M[1,3] := 0; M[2,3] := 0; M[3,3] := 1;
end;

// =============================================================================
//  Matrix_SetTransform
// =============================================================================
procedure Matrix_SetTransform(var M : TMatrix4f; V : TVector3f);
begin
M[3][0] := V[0];
M[3][1] := V[1];
M[3][2] := V[2];
end;

// =============================================================================
//  Matrix_SetRotation
// =============================================================================
procedure Matrix_SetRotation(var M : TMatrix4f; V : TVector3f);
var
 cr , sr , cp , sp , cy , sy , srsp , crsp : single;
begin
V[0] := DegToRad(V[0]);
V[1] := DegToRad(V[1]);
V[2] := DegToRad(V[2]);

cr := cos(V[0]);
sr := sin(V[0]);
cp := cos(V[1]);
sp := sin(V[1]);
cy := cos(V[2]);
sy := sin(V[2]);

M[0,0] := cp*cy;
M[1,0] := cp*sy;
M[2,0] := -sp;

srsp := sr*sp;
crsp := cr*sp;

M[0,1] := srsp*cy-cr*sy ;
M[1,1] := srsp*sy+cr*cy ;
M[2,1] := sr*cp ;
M[0,2] := crsp*cy+sr*sy ;
M[1,2] := crsp*sy-sr*cy ;
M[2,2] := cr*cp ;
end;

// =============================================================================
//  Matrix_RotateVect
// =============================================================================
procedure Matrix_RotateVect(const M : TMatrix4f; var pVect : TVector3f);
var
 vec : array [0..2] of single;
begin
vec[0] := pVect[0]*M[0,0] + pVect[1]*M[1,0] + pVect[2]*M[2,0];
vec[1] := pVect[0]*M[0,1] + pVect[1]*M[1,1] + pVect[2]*M[2,1];
vec[2] := pVect[0]*M[0,2] + pVect[1]*M[1,2] + pVect[2]*M[2,2];
//vec[0] := pVect[0]*M[0,0] + pVect[1]*M[0,1] + pVect[2]*M[0,2];
//vec[1] := pVect[0]*M[1,0] + pVect[1]*M[1,1] + pVect[2]*M[1,2];
//vec[2] := pVect[0]*M[2,0] + pVect[1]*M[2,1] + pVect[2]*M[2,2];
pVect[0] := vec[0];
pVect[1] := vec[1];
pVect[2] := vec[2];
end;

// =============================================================================
//  Matrix_UnRotateVect
// =============================================================================
function Matrix_UnRotateVect(const M : TMatrix4f; pVect : TVector3f) : TVector3f;
var
 V : TVector3f;
begin
V := V3(VDot(V, V3(M[0,0], M[0,1], M[0,2])),
        VDot(V, V3(M[1,0], M[1,1], M[1,2])),
        VDot(V, V3(M[2,0], M[2,1], M[2,2])));
Result := V;
end;

// =============================================================================
//  Matrix_MakeYawMatrix
// =============================================================================
function Matrix_MakeYawMatrix(Angle : Single) : TMatrix4f;
var
 CA : Single;
 SA : Single;
 M  : TMatrix4f;
begin
SA := Sin(Angle);
CA := Cos(Angle);
M[0,0] := CA; M[1,0] := 0; M[2,0] := -SA; M[3,0] := 0;
M[0,1] := 0;  M[1,1] := 1; M[2,1] := 0;   M[3,1] := 0;
M[0,2] := SA; M[1,2] := 0; M[2,2] := CA;  M[3,2] := 0;
M[0,3] := 0;  M[1,3] := 0; M[2,3] := 0;   M[3,3] := 1;
Result := M;
end;

// =============================================================================
//  Matrix_MakeRollMatrix
// =============================================================================
function Matrix_MakeRollMatrix(Angle : Single) : TMatrix4f;
var
 CA : Single;
 SA : Single;
 M  : TMatrix4f;
begin
SA := Sin(Angle);
CA := Cos(Angle);
M[0,0] := CA;  M[1,0] := SA; M[2,0] := 0; M[3,0] := 0;
M[0,1] := -SA; M[1,1] := CA; M[2,1] := 0; M[3,1] := 0;
M[0,2] := 0;   M[1,2] := 0;  M[2,2] := 1; M[3,2] := 0;
M[0,3] := 0;   M[1,3] := 0;  M[2,3] := 0; M[3,3] := 1;
Result := M;
end;

// =============================================================================
//  Matrix_MakePitchMatrix
// =============================================================================
function Matrix_MakePitchMatrix(Angle : Single) : TMatrix4f;
var
 CA : Single;
 SA : Single;
 M  : TMatrix4f;
begin
SA := Sin(Angle);
CA := Cos(Angle);
M[0,0] := 1; M[1,0] := 0;   M[2,0] := 0;  M[3,0] := 0;
M[0,1] := 0; M[1,1] := CA;  M[2,1] := SA; M[3,1] := 0;
M[0,2] := 0; M[1,2] := -SA; M[2,2] := CA; M[3,2] := 0;
M[0,3] := 0; M[1,3] := 0;   M[2,3] := 0;  M[3,3] := 1;
Result := M;
end;

// =============================================================================
//  Matrix_Multiply
// =============================================================================
function Matrix_Multiply(m1 : TMatrix4f; m2 : TMatrix4f) : TMatrix4f;
var
  r, c, i: Byte;
  t: TMatrix4f;
begin
// Multiply two matrices.
t := NullMatrix4f;
for r := 0 to 3 do
 for c := 0 to 3 do
  for i := 0 to 3 do
   t[r,c] := t[r,c] + (m1[r,i]*m2[i,c]);
Result := t;
end;

// =============================================================================
//  Matrix_Transpose
// =============================================================================
function Matrix_Transpose(M : TMatrix4f) : TMatrix4f;
var
 i,j : Integer;
 TM  : TMatrix4f;
begin
for I := 0 to 3 do
 for J := 0 to 3 do
  TM[J, I] := M[I, J];
Result := TM;
end;

// internal version for the determinant of a 3x3 matrix
function MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
Result := a1 * (b2 * c3 - b3 * c2) -
          b1 * (a2 * c3 - a3 * c2) +
          c1 * (a2 * b3 - a3 * b2);
end;

// Determinant of a 4x4 matrix
function MatrixDeterminant(M: TMatrix4f): Single; register;
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;
begin
  a1 := M[X, X];  b1 := M[X, Y];  c1 := M[X, Z];  d1 := M[X, W];
  a2 := M[Y, X];  b2 := M[Y, Y];  c2 := M[Y, Z];  d2 := M[Y, W];
  a3 := M[Z, X];  b3 := M[Z, Y];  c3 := M[Z, Z];  d3 := M[Z, W];
  a4 := M[W, X];  b4 := M[W, Y];  c4 := M[W, Z];  d4 := M[W, W];
  Result := a1 * MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

// Adjoint of a 4x4 matrix - used in the computation of the inverse
// of a 4x4 matrix
procedure MatrixAdjoint(var M: TMatrix4f); register;
var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: Single;
begin
    a1 :=  M[X, X]; b1 :=  M[X, Y];
    c1 :=  M[X, Z]; d1 :=  M[X, W];
    a2 :=  M[Y, X]; b2 :=  M[Y, Y];
    c2 :=  M[Y, Z]; d2 :=  M[Y, W];
    a3 :=  M[Z, X]; b3 :=  M[Z, Y];
    c3 :=  M[Z, Z]; d3 :=  M[Z, W];
    a4 :=  M[W, X]; b4 :=  M[W, Y];
    c4 :=  M[W, Z]; d4 :=  M[W, W];
    // row column labeling reversed since we transpose rows & columns
    M[X, X] :=  MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X] :=  MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
    M[X, Y] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y] :=  MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y] :=  MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);
    M[X, Z] :=  MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z] :=  MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);
    M[X, W] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W] :=  MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W] :=  MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

// multiplies all elements of a 4x4 matrix with a factor
procedure MatrixScale(var M: TMatrix4f; Factor: Single); register;
var
 I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do M[I, J] := M[I, J] * Factor;
end;

// finds the inverse of a 4x4 matrix
procedure Matrix_Inverse(var M: TMatrix4f); register;
var
 Det: Single;
begin
  Det := MatrixDeterminant(M);
  if Abs(Det) < EPSILON then M := IdentityMatrix
                        else
  begin
    MatrixAdjoint(M);
    MatrixScale(M, 1 / Det);
  end;
end;

function Matrix_Changed(M1, M2 : TMatrix4f) : Boolean;
var
 I, J: Integer;
begin
Result := True;
for i := 0 to 3 do
 for j := 0 to 3 do
  if M1[i,j] <> M2[i,j] then
   exit;
Result := False;
end;

// =============================================================================
//  Matrix_TansformVector
// =============================================================================
function Matrix_TansformVector(const M : TMatrix4f; V : TVector3f) : TVector3f;
begin
Matrix_RotateVect(M, V);
Result := V3(V[0]+M[3,0], V[1]+M[3,1], V[2]+M[3,2]);
end;

// =============================================================================
//  Matrix_UntransformVector
// =============================================================================
function Matrix_UntransformVector(const M : TMatrix4f; V : TVector3f) : TVector3f;
begin
V := V3(V[0]-M[3,0], V[1]-M[3,1], V[2]-M[3,2]);
V := V3(VDot(V, V3(M[0,0], M[0,1], M[0,2])),
        VDot(V, V3(M[1,0], M[1,1], M[1,2])),
        VDot(V, V3(M[2,0], M[2,1], M[2,2])));
Result := V;
end;


end.

