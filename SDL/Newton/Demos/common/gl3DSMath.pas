unit gl3DSMath;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses gl, glu, glext, glmatrix;

type
  //generic opengl color record (should not be here)
  TGLColor=packed record
    red,green,blue,alpha: GLclampf;
  end;

  T3dpoint     = packed record
    x, y, z        :Single;
  end;

var
   hastexture: GLint;  

function Point3D(x,y,z : Single) : T3DPoint;
function Normalize(v: T3dPoint): T3dPoint;
function CalcNormalVector(const Point1,Point2,Point3:T3DPoint):T3DPoint;
function MatrixTransform(matrix: clsMatrix; vertex: T3DPoint): T3DPoint;
function VectorSubtract(V1, V2: T3dPoint): T3dPoint; register;
function VectorTransform(V: T3DPoint; M: TMatrix): T3dPoint;
function VCross(pV1, pV2 : T3dPoint) : T3dPoint;
function VDot(pV1, pV2 : T3dPoint) : Single;

implementation

function VCross(pV1, pV2 : T3dPoint) : T3dPoint;
begin
Result.x := (pV1.y*pV2.z) - (pV1.z*pV2.y);
Result.y := (pV1.z*pV2.x) - (pV1.x*pV2.z);
Result.z := (pV1.x*pV2.y) - (pV1.y*pV2.x);
end;

function VDot(pV1, pV2 : T3dPoint) : Single;
begin
Result := (pV1.x*pV2.x) + (pV1.y*pV2.y) + (pV1.z*pV2.z);
end;

function Point3D(x,y,z : Single) : T3DPoint;
begin
Result.x := x;
Result.y := y;
Result.z := z;
end;

function Normalize(v: T3dPoint): T3dPoint;
var
 longi:single;
begin
longi:=sqrt(sqr (v.x) + sqr(v.y) + sqr(v.z));
     If longi>0 then begin result.x:=v.x/longi; result.y:=v.y/longi; result.z:=v.z/longi; end;
end;

// returns the difference of two vectors
function VectorSubtract(V1, V2: T3dPoint): T3dPoint; register;
begin
  Result.x := V1.x - V2.x;
  Result.y := V1.y - V2.y;
  Result.z := V1.z - V2.z;
end;

// transforms an affine vector by multiplying it with a (homogeneous) matrix
function  VectorTransform(V: T3DPoint; M: TMatrix): T3dPoint;
var
  TV: T3dPoint;
begin
  TV.x := V.x * M[0, 0] + V.y * M[1, 0] + V.z * M[2, 0] + M[3, 0];
  TV.y := V.x * M[0, 1] + V.y * M[1, 1] + V.z * M[2, 1] + M[3, 1];
  TV.z := V.x * M[1, 2] + V.y * M[1, 2] + V.z * M[2, 2] + M[3, 2];
  Result := TV;
end;

function MatrixTransform(matrix: clsMatrix; vertex: T3DPoint): T3DPoint;
var
   tempmatrix : array [0..15] of single;

begin
  matrix.getMatrix(tempmatrix);
  result.x := vertex.x*tempmatrix[0]+vertex.y*tempmatrix[4]+vertex.z*tempmatrix[8]+tempmatrix[12];
  result.y := vertex.x*tempmatrix[1]+vertex.y*tempmatrix[5]+vertex.z*tempmatrix[9]+tempmatrix[13];
  result.z := vertex.x*tempmatrix[2]+vertex.y*tempmatrix[6]+vertex.z*tempmatrix[10]+tempmatrix[14];
end;

function CalcNormalVector(const Point1,Point2,Point3:T3DPoint):T3DPoint;
{var longi:single;
    vx1,vx2,vy1,vy2,vz1,vz2:Single;
begin
  vx1:=Point1.x-Point2.x;
  vy1:=Point1.y-Point2.y;
  vz1:=Point1.z-Point2.z;
  vx2:=Point2.x-Point3.x;
  vy2:=Point2.y-Point3.y;
  vz2:=Point2.z-Point3.z;
  with Result do begin
     x:=vy1*vz2 - vz1*vy2;
     y:=vz1*vx2 - vx1*vz2;
     z:=vx1*vy2 - vy1*vx2;
     longi:=sqrt(sqr (x) + sqr(y) + sqr(z));
     If longi>0 then begin x:=x/longi; y:=y/longi; z:=z/longi; end;
  end;
  result.x:=-result.x;
  result.y:=-result.y;
  result.z:=-result.z;}

var
  temppoint1, temppoint2: t3dpoint;
  temp: t3dpoint;
begin
temppoint1:=vectorsubtract(point1,point2);
temppoint2:=vectorsubtract(point1,point3);
temp:=VCross(temppoint1,temppoint2);
result:=normalize(temp);
  result.x:=-result.x;
  result.y:=-result.y;
  result.z:=-result.z;
end;



end.
