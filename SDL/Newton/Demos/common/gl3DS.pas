//TODO:
//  save skeleton msa and bin
//  save skeleton animations msa and bin
//  load skeleton (with animations) bin
//  clean up code...
//  example load save example (converter)
//  example vertex picking (selection of face)... And assign bone to it...
//  selected propertie for mesh, face, vertex and bone

//02.08.2004 by Noeska Software (now uses glbitmap.pas instead of textures.pas)
//01.08.2004 by Noeska Software (bump mapping)
//27.06.2004 by Noeska Software (do not calculate bounding boxes for non mesh objects)
//20.06.2004 by Noeska Software (bounding boxes per submesh)
//15.06.2004 by Noeska Software (bugfixed: cw to ccw, color loading in msa, transparency)
//14.06.2004 by Noeska Software (changed rendering from cw to ccw, transparency with msa and bin)
//14.06.2004 by SOS (two sided materials)
//15.04.2004 by Noeska Software (bin save and load)
//14.04.2004 by Noeska Software (allow saving of milkshape ascii files)
//12.04.2004 by Noeska Software (seperated bones from mesh using a skeleton class, allows ussage of multiple different bone animations per mesh)
//??.03.2004 by Noeska Software (fixed milkshape ascii support, now works again)
//14.02.2004 by Noeska Software (allow adding faces and assign material to a mesh, not yet optimized)
//08.02.2004 by Noeska Software (allow adding materials and meshes...)
//26.01.2004 by Noeska Software (improved transparency mesh order, corrected skipping light and camera chunks)
//25.01.2004 by Noeska Software (transparency added)
//15.01.2004 by Noeska Software (rewrote (some) source to borland standard, added displaylist renderer)
//06.10.2003 by Noeska Software (ms3d ascii bone animation, thanks to Maarten "McCLaw" Kronberger)
//05.10.2003 by Noeska Software (3ds meshes with no material and no texture coordinates)
//02.10.2003 by Noeska Software (ms3d ascii loader)
//01.10.2003 by Noeska Software (textures are back, added some more properties, SwitchAxesP is not used anymore)
//30.09.2003 by Noeska Software (converted to classes, optimized setting matarial on rendering)
//24.09.2003 by Noeska Software (uses pivot points to draw meshes when needed, removed matrix)
//21.09.2003 by Noeska Software (more then 1 material per submesh, smoothed normals, dummy submeshes)
//18.09.2003 by Noeska Software (rewrite to 1 chunk parser) now it also reads non-standard 3ds files.
//16.09.2003 by Noeska Software
//08.09.2002 by Noeska Software
//01.05.2002 by Jan Michalowsky
//Thx a lot  Prometheus,Poetikus
//Version 2.4a

unit gl3DS;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
 Classes,
 SysUtils,
 gl,
 glu,
 glext,
 GL3DSMath,
 Math,
 glMatrix;

const
  //numbering may change so use names instead of numbers
  MeshBin = 64;   //own bin format...
  MeshMs3d = 16;  //bin milkshape mesh and/or animation
  MeshMsa = 8;    //ascii milkshape mesh and/or animation
  Mesh3dsBvh = 4; //3ds with bvh animation (does not work yet)
  Mesh3dsa = 2;   //3ds with keyframe animation
  Mesh3ds = 1;    //3ds

  //channel types for milkshape bone animation
  ChanneltypeNone = 0;                 //no channel applied
  ChanneltypeScaleRotateTranslate = 1; //scale rotation and translation (bva)
  ChanneltypeTranslate = 2;            //translation x y z
  ChanneltypeRxyz = 4;                 //rotation x y z order
  ChanneltypeRzxy = 8;                 //rotation z x y order
  ChanneltypeRyzx = 16;                //rotation y z x order
  ChanneltypeRzyx = 32;                //rotation z y x order
  ChanneltypeRxzy = 64;                //rotation x z y order
  ChanneltypeRyxz = 128;               //rotation y x z order
  ChanneltypeScale = 256;              //scale only
  ChanneltypeTranslateXyz = 512;       //translate x y z only
  channeltypeInterleaved = 1024;       //multiple channels?

type
  //texturemapping coords
  TMap = packed record
    tu: single;
    tv: single;
  end;

  //keyframe data
  TKeyFrame = packed record
    time: integer;
    Value: T3dPoint;
    bvh_value: T3dPoint;
  end;

  //bone data
  T3dsBone = class(TComponent)
  private
    FName: string;
    FParentName: string;
    FParent: T3dsBone;                   //direct access to the parent bone
    FTranslate: T3dPoint;
    FRotate: T3dPoint;
    FBvhChanneltype: integer;
    FMatrix: ClsMatrix;                  //absolute in accordance to animation
    FNumTranslateFrames: integer;
    FNumRotateFrames: integer;
    FTranslateFrame: array of TKeyFrame;
    FRotateFrame: array of TKeyFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Render;
    procedure AdvanceAnimation;
    property Name: string read FName;
    property ParentName: string read FParentName;
    property Translate: T3DPoint read FTranslate;
    property Rotate: T3DPoint read FRotate;
    //TODO: add access to keyframes data
  end;

  T3dsMesh = class(TComponent)
  private
    Fname: string;                 //meshname
    Fid: integer;                  //id for picking...
    Fmatname: array of string;     //array of material names.
    Fnumvertex: integer;           //number of vertexes
    Fnumindices: integer;          //number of faces
    Fpivot: T3DPoint;              //where to draw submesh (also for anim?)
    Fboneid: array of integer;     //boneid for a vertex
    Fvnormal: array of T3dPoint;   //vertex normals for smoother meshes
    Fmapping: array of TMap;       //uvmapping
    Fmatid: array of word;         //material for a face
    FSmoothingGroup : array of Cardinal; // Smoothing group ids for a face (bitmask)
    Findices: array of word;       //faces
    FDisplaylist: integer;         //displaylist id
    FMinimum: T3dPoint;            //min pos
    FMaximum: T3dPoint;            //max pos
{!} FLocalCoords : T3DPoint;       // Local coordinate system (origin)
    function GetVertex(Index: integer): T3dPoint;
    function GetNormal(Index: integer): T3dPoint;
    function GetMapping(Index: integer): TMap;
    procedure SetMapping(Index: integer; Value: TMap);
    procedure SetVertex(Index : Integer; Value : T3DPoint);
    function GetMatID(Index: integer): word;
    function GetFace(Index: integer): word;
  public
{!} LocalAxis : array[0..2] of T3DPoint; // Local coordinate system (3 axis)
    Fnormalindices: array of word; //normal index
{!} Facenormal     : array of T3DPoint;       // Made public
    Fvertex: array of T3dpoint;    //vertexes
    destructor Destroy; override;
    procedure Render;
    procedure RenderBoundBox;
    procedure CalculateSize;
    procedure BuildDisplayList;
    procedure AddFace(v1, v2, v3: T3DPoint; matname: string);
    property Name: string read FName write FName;
    property Id: integer read FId write FId;
    property NumVertex: integer read FNumVertex;
    property NumFaces: integer read FNumIndices;
    property Vertex[Index: integer]: T3dPoint read GetVertex write SetVertex;
    property Normal[Index: integer]: T3dPoint read GetNormal;
    property Mapping[Index: integer]: TMap read GetMapping write SetMapping;
    property MatID[Index: integer]: word read GetMatID;
    property Face[Index: integer]: word read GetFace;
    property Maximum: T3dPoint read FMaximum;
    property Minimum: T3dPoint read FMinimum;
{!} property LocalCoords : T3DPoint read FLocalCoords;
  end;

  T3dsMaterial = class(TComponent)
  private
    FHastexturemap: boolean;
    FHasbumpmap: boolean;
    FHasmaterial: boolean;
    FIsAmbient: boolean;
    FIsDiffuse: boolean;
    FIsSpecular: boolean;
    FTwoSided: boolean;
    FName: string;
    FFileName: string;
    FBumpMapFileName: string;
    FUs: single;
    FVs: single;
    FUoff: single;
    FVoff: single;
    FRot: single;
    FAmbR: single;
    FAmbG: single;
    FAmbB: single;
    FDifR: single;
    FDifG: single;
    FDifB: single;
    FSpcR: single;
    FSpcG: single;
    FSpcB: single;
    FTransparency: single;
    FBumpMapStrength: single;
    FTexId: integer;
    FTexture: glUInt;
    FDetailTex : glUInt;
  public
    Format : glUInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply;
    procedure Updatetexture;
    property Name: string read FName write FName;
    property HasMaterial: boolean read FHasMaterial write FHasMaterial;
    property IsAmbient: boolean read FIsambient write FIsambient;
    property IsDiffuse: boolean read FIsdiffuse write FIsdiffuse;
    property IsSpecular: boolean read FIsSpecular write FIsSpecular;
    property TwoSided: boolean read FTwoSided write FTwoSided;
    property HasTexturemap: boolean read FHasTexturemap write FHasTexturemap;
    property HasBumpmap: boolean read FHasBumpmap write FHasBumpmap;
    property TextureFilename: string read FFileName write FFileName;
    property BumpMapFilename: string read FBumpMapFileName write FBumpMapFileName;
    property TextureID: glUint read FTexture;
    property Us: single read FUs write FUs;
    property Vs: single read FVs write FVs;
    property Uoff: single read FUoff write FUoff;
    property Voff: single read FVoff write FVoff;
    property Rotate: single read FRot write FRot;
    property AmbientRed: single read FAmbr write FAmbr;
    property AmbientGreen: single read FAmbg write FAmbg;
    property AmbientBlue: single read FAmbb write FAmbb;
    property DiffuseRed: single read FDifr write FDifr;
    property DiffuseGreen: single read FDifg write FDifg;
    property DiffuseBlue: single read FDifb write FDifb;
    property SpecularRed: single read FSpcr write FSpcr;
    property SpecularGreen: single read FSpcg write FSpcg;
    property SpecularBlue: single read FSpcb write FSpcb;
    property Transparency: single read FTransparency write FTransparency;
    property Bumpmapstrength: single read FBumpmapstrength write FBumpMapStrength;
    property TexID: integer read FTexId write FTexID;
    property DetailID: glUInt read FDetailTex;
  end;

  TSkeleton = class(TComponent)
  private
    FName: string;
    FBone: array of T3dsBone;
    FNumBones: integer;
    FNumFrames: integer;
    FCurrentFrame: integer;
    function GetBone(Index: integer): T3dsBone;
    procedure LoadBvhFromFile(Filename: string);
    procedure LoadBvhFromStream(Stream: TStream);
    procedure LoadMsaFromFile(filename: string);
    procedure LoadMsaFromStream(stream: Tstream);
  public
    destructor Destroy; override;
    property Name: string read FName write FName;
    property NumBones: integer read FNumBones;
    property NumFrames: integer read FNumFrames;
    property CurrentFrame: integer read FCurrentFrame write FCurrentFrame;
    property Bone[Index: integer]: T3dsBone read GetBone;
    function GetBoneByName(s: string): T3dsBone;
    procedure AdvanceAnimation;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
  end;

  TAll3dsMesh = class(TComponent)
  private
    FName: string;
    FType: integer;
    FMasterScale: single;
    FDisplayList: GlInt;
    FMesh: array of T3dsMesh;
    FRenderOrder: array of integer;
    FMaterial: array of T3dsMaterial;
    FSkeleton: array of TSkeleton;
    FNumSkeletons: integer;
    FCurrentSkeleton: integer;
    FLoadSkeleton: boolean;
    FMinimum: T3dPoint;
    FMaximum: T3dPoint;
    FVersion: integer;
    FSubVersion: integer;
    FNumMeshes: integer;
    FNumMaterials: integer;
    FTexturePath: string;
    function GetMesh(Index: integer): T3dsMesh;
    function GetMaterial(Index: integer): T3dsMaterial;
    function GetMaterialIdByName(s: string): integer;
    function GetSkeleton(Index: integer): TSkeleton;
    procedure MoveToPivot;
    procedure CalculateScale;
    procedure CalculateRenderOrder;
    procedure LoadMsaFromFile(filename: string);
    procedure LoadMsaFromStream(stream: Tstream);
    procedure Load3dsFromFile(filename: string);
    procedure Load3dsFromStream(stream: Tstream);
    procedure LoadBinFromFile(filename: string);
    procedure LoadBinFromStream(stream: Tstream);
    procedure SaveMsaToFile(filename: string);
    procedure SaveMsaToStream(stream: Tstream);
    procedure SaveBinToFile(filename: string);
    procedure SaveBinToStream(stream: Tstream);
  public
    destructor Destroy; override;
    property Name: string read FName write FName;
    property FileType: integer read FType write FType;
    property MasterScale: single read FMasterScale write FMasterScale;
    property Version: integer read FVersion;
    property SubVersion: integer read FSubVersion;
    property NumMeshes: integer read FNumMeshes;
    property NumMaterials: integer read FNumMaterials;
    property Maximum: T3dPoint read FMaximum;
    property Minimum: T3dPoint read FMinimum;
    property TexturePath: string read FTexturePath write FTexturePath;
    property Mesh[Index: integer]: T3dsMesh read GetMesh;
    property Material[Index: integer]: T3dsMaterial read GetMaterial;
    property Skeleton[Index: integer]: TSkeleton read GetSkeleton;
    property NumSkeletons: integer read FNumSkeletons;
    property CurrentSkeleton: integer read FCurrentSkeleton write FCurrentSkeleton;
    procedure Render;
    procedure RenderBoundBox;
    procedure CalculateSize;
    procedure BuildDisplayList;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(Stream: TStream);
    function GetMeshByName(s: string): T3dsMesh;
    function GetMaterialByName(s: string): T3dsMaterial;
    procedure InitSkin;
    procedure CalcVnormals;
    procedure AddMesh;
    procedure AddMaterial;
    procedure AddSkeleton;
  end;

procedure startpickmode;
procedure endpickmode;

var
  pickmode: boolean;
  pickmesh: boolean;
  pickface: boolean;
  pickvertex: boolean;

  //TODO: also read lights form 3ds file
  //TODO: also make light class...
  objlightpos: T3dPoint; //lightpos needed for bumpmap calculations

{!} IgnoreMaterials  : Boolean = False;
{!} DontLoadTextures : Boolean = False;

implementation

const
  OVersion = 3; //expects 3ds version 3 files

  //Chunk ID's
  //  INT_PERCENTAGE = $0030; //hm transparency always has this subchunck
  MAIN3DS = $4D4D;
  VERS3DS = $0002;
  EDIT3DS = $3D3D;
  KEYF3DS = $B000;

  EDIT_OBJECT = $4000;

  MASTER_SCALE = $0100;

  OBJ_HIDDEN = $4010;
  OBJ_VERINFO = $3D3E;
  OBJ_TRIMESH = $4100;
  OBJ_LIGHT = $4600;
  OBJ_CAMERA = $4700;

  TRI_VERTEXL = $4110;
  TRI_FACEL1 = $4120;
  TRI_MATERIAL = $4130;
  TRI_MAPPINGCOORDS = $4140;
  TRI_MATRIX = $4160;


  MAT_MATRIAL = $AFFF;
  MAT_MATNAME = $A000;
  MAT_AMBIENT = $A010;
  MAT_DIFFUSE = $A020;
  MAT_SPECULAR = $A030;
  MAT_TRANSPARENCY = $A050; // Transparency Material
  MAT_TEXTURE = $A200;  //texmap
  MAT_BUMPMAP = $A230;
  MAT_MAPFILE = $A300;
  MAT_VSCALE = $A354;
  MAT_USCALE = $A356;
  MAT_VOFF = $A35A;
  MAT_UOFF = $A358;
  MAT_TEXROT = $A35C;
  MAT_COLOR = $0010;
  MAT_COLOR24 = $0011;
  MAT_TWO_SIDE = $A081; //thanks sos

  KEYF_OBJDES = $B002;
  KEYF_OBJHIERARCH = $B010;
  KEYF_OBJPIVOT = $B013;

type
  T3DSColor = packed record
    r, g, b: single;
  end;

  rgb = packed record
    r, g, b: byte
  end;

  TChunkHdr = packed record
    chunkID: word;
    chunkLE: longword;
  end;

procedure startpickmode;
begin
  pickmode:=true;
  glDisable(GL_LIGHTING);
  glDisable(GL_DITHER);
end;

procedure endpickmode;
begin
  pickmode:=false;
  glEnable(GL_LIGHTING);
  glEnable(GL_DITHER);
end;

//T3DSBone routines ...
constructor T3DSBone.Create(AOwner: TComponent);
begin
  inherited Create(AOWner);
  FMatrix := clsMatrix.Create;
end;

destructor T3DSBone.Destroy;
begin
  FMatrix.Free;
  SetLength(FTranslateFrame, 0);
  SetLength(FRotateFrame, 0);
  //free parent also?
  inherited Destroy;
end;

procedure T3DSBone.init;
var
  m_rel: clsMatrix;
  tempm: array [0..15] of single;
  tempv: array [0..2] of single;
begin
  glpushmatrix();

  //init parent bone direct access
  FParent := nil;
  if FParentName > '' then
    FParent := TSkeleton(owner).GetBoneByName(FParentName);

  //calculate the matrix for the bone
  m_rel := clsMatrix.Create;
  //m_rel.loadIdentity;
  // Create a transformation matrix from the position and rotation
  tempv[0] := FRotate.x;
  tempv[1] := FRotate.y;
  tempv[2] := FRotate.z;
  m_rel.setRotationRadians(tempv);


  tempv[0] := FTranslate.x;
  tempv[1] := FTranslate.y;
  tempv[2] := FTranslate.z;
  m_rel.setTranslation(tempv);

  // Each bone's final matrix is its relative matrix concatenated onto its
  // parent's final matrix (which in turn is ....)
  if (FParent = nil) then
  begin
    m_rel.getMatrix(tempm);
    FMatrix.setMatrixValues(tempm);
  end
  else
  begin
    FParent.FMatrix.getMatrix(tempm);
    FMatrix.setMatrixValues(tempm);
    FMatrix.postMultiply(m_rel);
  end;

  m_rel.Free;

  glpopmatrix();
end;

procedure T3DSBone.render;
var
  parentvertex, vertex: T3DPoint;

begin
  vertex.x := 0;
  vertex.y := 0;
  vertex.z := 0;
  vertex := MatrixTransform(FMatrix, Vertex);

  if FParent <> nil then
  begin
    parentvertex.x := 0;
    parentvertex.y := 0;
    parentvertex.z := 0;
    parentvertex := MatrixTransform(FParent.FMatrix, ParentVertex);
  end;

  glPointSize(4.0);
  glBegin(GL_POINTS);
  glvertex3fv(@vertex);
  glend;

  if FParent <> nil then
  begin
    glBegin(GL_LINES);
    glvertex3fv(@vertex);
    glvertex3fv(@parentvertex);
    glend;

    glPointSize(4.0);
    glBegin(GL_POINTS);
    glvertex3fv(@parentvertex);
    glend;
  end;
end;

procedure T3DSBone.AdvanceAnimation;
var
  i: integer;
  deltaTime: single;
  fraction: single;
  Position: array [0..2] of single;
  Rotation: array [0..2] of single;
  m_rel, m_frame: clsMatrix;
  tempm: array [0..15] of single;
  tvec: array [0..2] of single;
begin
  // Position

  // Find appropriate position key frame
  i := 0;
  while ((i < FNumTranslateFrames - 1) and (FTranslateFrame[i].Time <
    TSkeleton(owner).FCurrentFrame)) do
    i := i + 1;

  if (i > 0) then
  begin
    // Interpolate between 2 key frames

    // time between the 2 key frames
    deltaTime := FTranslateFrame[i].Time - FTransLateFrame[i - 1].Time;

    // relative position of interpolation point to the keyframes [0..1]
    fraction := (TSkeleton(owner).FCurrentFrame - FTransLateFrame[i - 1].Time) / deltaTime;

    Position[0] := FTransLateFrame[i - 1].Value.x + fraction *
      (FTransLateFrame[i].Value.x - FTransLateFrame[i - 1].Value.x);
    Position[1] := FTransLateFrame[i - 1].Value.y + fraction *
      (FTransLateFrame[i].Value.y - FTransLateFrame[i - 1].Value.y);
    Position[2] := FTransLateFrame[i - 1].Value.z + fraction *
      (FTransLateFrame[i].Value.z - FTransLateFrame[i - 1].Value.z);
  end
  else
  begin
    Position[0] := FTransLateFrame[i].Value.x;
    Position[1] := FTransLateFrame[i].Value.y;
    Position[2] := FTransLateFrame[i].Value.z;
  end;

  // Rotation

  // Find appropriate rotation key frame
  i := 0;
  while ((i < FNumRotateFrames - 1) and (FRotateFrame[i].Time <
    TSkeleton(owner).CurrentFrame)) do
    i := i + 1;

  if (i > 0) then
  begin
    // Interpolate between 2 key frames

    // time between the 2 key frames
    deltaTime := FRotateFrame[i].Time - FRotateFrame[i - 1].Time;

    // relative position of interpolation point to the keyframes [0..1]
    fraction := ({TAll3DSMesh}TSkeleton(Owner).CurrentFrame - FRotateFrame[i - 1].Time) / deltaTime;

    Rotation[0] := FRotateFrame[i - 1].Value.x + fraction *
      (FRotateFrame[i].Value.x - FRotateFrame[i - 1].Value.x);
    Rotation[1] := FRotateFrame[i - 1].Value.y + fraction *
      (FRotateFrame[i].Value.y - FRotateFrame[i - 1].Value.y);
    Rotation[2] := FRotateFrame[i - 1].Value.z + fraction *
      (FRotateFrame[i].Value.z - FRotateFrame[i - 1].Value.z);
  end
  else
  begin
    Rotation[0] := FRotateFrame[i].Value.x;
    Rotation[1] := FRotateFrame[i].Value.y;
    Rotation[2] := FRotateFrame[i].Value.z;
  end;

  // Now we know the position and rotation for this animation frame.
  // Let's calculate the transformation matrix (_matrix) for this bone...

  m_rel := clsMatrix.Create;
  m_frame := clsMatrix.Create;

  // Create a transformation matrix from the position and rotation of this
  // joint in the rest position
  tvec[0] := FRotate.x;
  tvec[1] := FRotate.y;
  tvec[2] := FRotate.z;

  m_rel.setRotationRadians(tvec);

  tvec[0] := FTranslate.x;
  tvec[1] := FTranslate.y;
  tvec[2] := FTranslate.z;

  m_rel.setTranslation(tvec);

  // Create a transformation matrix from the position and rotation
  // m_frame: additional transformation for this frame of the animation

  m_frame.setRotationRadians(Rotation);

  m_frame.setTranslation(Position);

  // Add the animation state to the rest position
  m_rel.postMultiply(m_frame);

  if (FParent = nil) then // this is the root node
  begin
    m_rel.getMatrix(tempm);
    FMatrix.setMatrixValues(tempm);  // _matrix := m_rel
  end
  else                  // not the root node
  begin
    // _matrix := parent's _matrix * m_rel (matrix concatenation)
    FParent.FMatrix.getMatrix(tempm);
    FMatrix.setMatrixValues(tempm);
    FMatrix.postMultiply(m_rel);
  end;

  m_frame.Free;
  m_rel.Free;
end;

//TSkeleton routines ...
procedure TSkeleton.LoadFromFile(filename: string);
begin
  fname:=filename;
  if LowerCase(copy(filename, length(filename) - 3, 4)) = '.bvh' then
    LoadBVHFromFile(Filename);
  if LowerCase(copy(filename, length(filename) - 3, 4)) = '.txt' then
    LoadMSAFromFile(Filename);
end;

procedure TSkeleton.LoadFromStream(stream: Tstream);
begin
  if TAll3dsMesh(owner).FType = Mesh3dsBVH then
    LoadBVHFromStream(stream);
  if TAll3dsMesh(owner).FType = MeshMsa then
    LoadMSAFromStream(stream);
end;

destructor TSkeleton.Destroy;
begin
  inherited Destroy; //this will automaticaly free the meshes, materials, bones...
  //however do free the dynamic arrays used
  SetLength(FBone, 0);
end;

function TSkeleton.GetBoneByName(s: string): T3DSBone;
var
  i: word;
begin
  Result := nil;
  for i := 0 to High(FBone) do
    if uppercase(FBone[i].FName) = uppercase(s) then
    begin
      Result := FBone[i];
      break;
    end;
end;

procedure TSkeleton.AdvanceAnimation;
var
  m: integer;
begin
  //increase the currentframe
  FCurrentFrame := FCurrentFrame + 1;
  if FCurrentFrame > FNumFrames then FCurrentFrame := 1; //reset when needed

  //set the bones to their new positions
  if FNumBones > 0 then
    for m := 0 to FNumBones - 1 do
    begin
      FBone[m].AdvanceAnimation;
    end;
end;

function TSkeleton.GetBone(Index: integer): T3DSBone;
begin
  Result := FBone[index];
end;

procedure TSkeleton.LoadBvhFromFile(filename: string);
var
  stream: TFileStream;
begin
  TAll3dsMesh(owner).FType := Mesh3dsBvh;

  stream := TFilestream.Create(Filename, fmOpenRead);
  LoadBvhFromStream(stream);
  stream.Free;
end;

procedure TSkeleton.LoadBvhFromStream(stream: Tstream);
var
  mem: TStringList;
  parseline: TStringList;
  loop, parselineloop: integer;
  line: string;
  bonelevel, maxbonelevel : integer;
  endsite: boolean;
  tempparentname: string;
  m: integer;
  downlevel: boolean;

  mybone: integer;
begin
  TAll3dsMesh(owner).FType := Mesh3dsBvh;

  // Create a StringList and load in BVH file
  mem := TStringList.Create;
  mem.loadfromstream(stream);

//  channelcount := 0;

  bonelevel := 0;
  downlevel := False;
//  tempbonelevel := 0;
  maxbonelevel := 0;
//  nbl := 0;
  endsite := False;

  // Parse through the whole BVH file
  for loop := 0 to mem.Count - 1 do
  begin
    line := mem.Strings[loop];
    if Line <> '' then
    begin
      //Set bone hierarchie level up
      if Pos('{', lowercase(line)) > 0 then
      begin
        bonelevel := bonelevel + 1; //points to parent bone id
        //downlevel:=false;
      end;

      //Set bone hierarchie level down
      if Pos('}', lowercase(line)) > 0 then
      begin
        bonelevel := bonelevel - 1;
        downlevel := True;
      end;

      //Find End Site joint
      if Pos('end site', lowercase(line)) > 0 then
      begin
        endsite := True;
        //ignore it for the time being (later make it a bone?)
        //treat it like a bone with no anim data put with a position?
      end;

      //Find root bone
      if Pos('root', lowercase(line)) > 0 then
      begin
        maxbonelevel := maxbonelevel + 1;
        FNumBones := maxbonelevel;
        setlength(FBone, maxbonelevel);
        FBone[maxbonelevel - 1] := t3dsbone.Create(self);
        FBone[maxbonelevel - 1].FName :=
          trim(copy(line, Pos('root', lowercase(line)) + 5,Length(line) - Pos('root',
          lowercase(line)) + 5));
        FBone[maxbonelevel - 1].FParentname := ''; //no parent;

        tempparentname := FBone[maxbonelevel - 1].FName;
      end;

      //Find child bone
      if Pos('joint', lowercase(line)) > 0 then
      begin
        maxbonelevel := maxbonelevel + 1;
        FNumBones := maxbonelevel;

        setlength(FBone, maxbonelevel);
        FBone[maxbonelevel - 1] := t3dsbone.Create(self);

        FBone[maxbonelevel - 1].FName :=
          trim(copy(line, Pos('joint', lowercase(line)) + 5,Length(line) - Pos('root',
          lowercase(line)) + 5));
        FBone[maxbonelevel - 1].FParentname := tempparentname;

        tempparentname := FBone[maxbonelevel - 1].FName;

        if downlevel then
        begin

          FBone[maxbonelevel - 1].FParentname :=
            FBone[bonelevel - 1].FName;
          downlevel := False;
        end;

      end;

      //Find the offset (translate) for the current bone
      if Pos('offset', lowercase(line)) > 0 then
      begin
        if endsite = False then
        begin
          //create a stringlist for current frame values
          parseline := TStringList.Create;

          //sepperate the values frim line in a list
          parseline.CommaText :=
            trim(copy(line, Pos('offset', lowercase(line)) + 7,Length(line) - Pos('offset',
            lowercase(line)) + 7));;

          FBone[maxbonelevel - 1].FTranslate.x :=
            StrToFloat(ParseLine.Strings[0]);
          FBone[maxbonelevel - 1].FTranslate.y :=
            StrToFloat(ParseLine.Strings[1]);
          FBone[maxbonelevel - 1].FTranslate.z :=
            StrToFloat(ParseLine.Strings[2]);

          //no rotation in the skeleton definition of bvh
          FBone[maxbonelevel - 1].FRotate.x := 0.0;
          FBone[maxbonelevel - 1].FRotate.y := 0.0;
          FBone[maxbonelevel - 1].FRotate.z := 0.0;

          //destroy the stringlist for parsing the currentframe
          parseline.Clear;
          parseline.Free;
        end
        else
        begin
          endsite := False;
        end;
      end;

      //Find the channels (like xrot, xpos, yrot ...)
      if Pos('channels', lowercase(line)) > 0 then
      begin
        if endsite = False then
        begin
          //create a stringlist for current frame values
          parseline := TStringList.Create;

          //sepperate the values frim line in a list
          parseline.CommaText :=
            trim(copy(lowercase(line), Pos('channels', lowercase(line)) + 9,Length(line)
            - Pos('channels', lowercase(line)) + 9));

          //read in the frametypes
          if bonelevel > 1 then
          begin
            //for child bones
//            channelcount := channelcount + StrToInt(ParseLine.Strings[0]);

            //find rotatation order...
            if ParseLine.Strings[1] = 'xposition' then
            begin
              FBone[maxbonelevel - 1].FBvhChanneltype :=
                ChanneltypeTranslate;
              if (ParseLine.Strings[4] = 'xrotation') and
                (ParseLine.Strings[5] = 'yrotation') and (ParseLine.Strings[6] = 'zrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := Channeltypetranslate + ChanneltypeRxyz;
              if (ParseLine.Strings[4] = 'zrotation') and
                (ParseLine.Strings[5] = 'xrotation') and (ParseLine.Strings[6] = 'yrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRzxy;
              if (ParseLine.Strings[4] = 'yrotation') and
                (ParseLine.Strings[5] = 'zrotation') and (ParseLine.Strings[6] = 'xrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRyzx;
              if (ParseLine.Strings[4] = 'zrotation') and
                (ParseLine.Strings[5] = 'yrotation') and (ParseLine.Strings[6] = 'xrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRzyx;
              if (ParseLine.Strings[4] = 'xrotation') and
                (ParseLine.Strings[5] = 'zrotation') and (ParseLine.Strings[6] = 'yrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRxzy;
              if (ParseLine.Strings[4] = 'yrotation') and
                (ParseLine.Strings[5] = 'xrotation') and (ParseLine.Strings[6] = 'zrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := channeltypeTranslate + ChanneltypeRyxz;
            end;
            if (ParseLine.Strings[1] = 'xrotation') and
              (ParseLine.Strings[2] = 'yrotation') and (ParseLine.Strings[3] = 'zrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRxyz;
            if (ParseLine.Strings[1] = 'zrotation') and
              (ParseLine.Strings[2] = 'xrotation') and (ParseLine.Strings[3] = 'yrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRzxy;
            if (ParseLine.Strings[1] = 'yrotation') and
              (ParseLine.Strings[2] = 'zrotation') and (ParseLine.Strings[3] = 'xrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRyzx;
            if (ParseLine.Strings[1] = 'zrotation') and
              (ParseLine.Strings[2] = 'yrotation') and (ParseLine.Strings[3] = 'xrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRzyx;
            if (ParseLine.Strings[1] = 'xrotation') and
              (ParseLine.Strings[2] = 'zrotation') and (ParseLine.Strings[3] = 'yrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRxzy;
            if (ParseLine.Strings[1] = 'yrotation') and
              (ParseLine.Strings[2] = 'xrotation') and (ParseLine.Strings[3] = 'zrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRyxz;
          end
          else
          begin
            // for the root bone
//            channelcount := channelcount + StrToInt(ParseLine.Strings[0]);

            //find rotatation order...
            if ParseLine.Strings[1] = 'xposition' then
            begin
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate;
              if (ParseLine.Strings[4] = 'xrotation') and
                (ParseLine.Strings[5] = 'yrotation') and (ParseLine.Strings[6] = 'zrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRxyz;
              if (ParseLine.Strings[4] = 'zrotation') and
                (ParseLine.Strings[5] = 'xrotation') and (ParseLine.Strings[6] = 'yrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRzxy;
              if (ParseLine.Strings[4] = 'yrotation') and
                (ParseLine.Strings[5] = 'zrotation') and (ParseLine.Strings[6] = 'xrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRyzx;
              if (ParseLine.Strings[4] = 'zrotation') and
                (ParseLine.Strings[5] = 'yrotation') and (ParseLine.Strings[6] = 'xrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRzyx;
              if (ParseLine.Strings[4] = 'xrotation') and
                (ParseLine.Strings[5] = 'zrotation') and (ParseLine.Strings[6] = 'yrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRxzy;
              if (ParseLine.Strings[4] = 'yrotation') and
                (ParseLine.Strings[5] = 'xrotation') and (ParseLine.Strings[6] = 'zrotation') then
                FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeTranslate + ChanneltypeRyxz;
            end;
            if (ParseLine.Strings[1] = 'xrotation') and
              (ParseLine.Strings[2] = 'yrotation') and (ParseLine.Strings[3] = 'zrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRxyz;
            if (ParseLine.Strings[1] = 'zrotation') and
              (ParseLine.Strings[2] = 'xrotation') and (ParseLine.Strings[3] = 'yrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRzxy;
            if (ParseLine.Strings[1] = 'yrotation') and
              (ParseLine.Strings[2] = 'zrotation') and (ParseLine.Strings[3] = 'xrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRyzx;
            if (ParseLine.Strings[1] = 'zrotation') and
              (ParseLine.Strings[2] = 'yrotation') and (ParseLine.Strings[3] = 'xrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRzyx;
            if (ParseLine.Strings[1] = 'xrotation') and
              (ParseLine.Strings[2] = 'zrotation') and (ParseLine.Strings[3] = 'yrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRxzy;
            if (ParseLine.Strings[1] = 'yrotation') and
              (ParseLine.Strings[2] = 'xrotation') and (ParseLine.Strings[3] = 'zrotation') then
              FBone[maxbonelevel - 1].FBvhChanneltype := ChanneltypeRyxz;
          end;
          //destroy the stringlist for parsing the current bone frametypes
          parseline.Clear;
          parseline.Free;
        end
        else
        begin
          endsite := False;
        end;
      end;

      //Find number of Frames
      if Pos('frames', lowercase(line)) > 0 then
      begin
        FNumFrames := StrToInt(trim(copy(line,
          Pos(':', lowercase(line)) + 1,Length(line) - Pos(':', lowercase(line)) + 1)));
        FCurrentFrame := 0; //reset currentframe!
      end;

      //read in animation
      if (Pos('end site', lowercase(line)) = 0) and
        (Pos('{', lowercase(line)) = 0) and (Pos('}', lowercase(line)) = 0) and
        (Pos('root', lowercase(line)) = 0) and (Pos('joint', lowercase(line)) = 0) and
        (Pos('offset', lowercase(line)) = 0) and (Pos('hierarchy', lowercase(line)) = 0) and
        (Pos('motion', lowercase(line)) = 0) and (Pos('frame time', lowercase(line)) = 0) and
        (Pos('frames', lowercase(line)) = 0) and (Pos('channels', lowercase(line)) = 0) then
      begin
        FCurrentFrame := FCurrentFrame + 1;

        //create a stringlist for current frame values
        parseline := TStringList.Create;

        //sepperate the values frim line in a list
        parseline.CommaText := line;

        parselineloop := 0;
        mybone := 0;

        for m := 0 to FNumBones - 1 do
        begin
          if mybone <> m then
          begin
            mybone := m;
            //parselineloop:=0;
          end;

          FBone[m].FNumTranslateFrames := FNumFrames;
          FBone[m].FNumRotateFrames := FNumFrames;
          SetLength(FBone[m].FTranslateFrame, FNumframes);
          SetLength(FBone[m].FRotateFrame, FNumframes);

          if FBone[m].FBvhChanneltype = ChanneltypeTranslate + ChanneltypeRxzy then
          begin
            FBone[m].FTranslateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 3]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 4]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 5]);

            parselineloop := parselineloop + 6;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRxzy then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 2]);

            parselineloop := parselineloop + 3;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRxyz then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            parselineloop := parselineloop + 3;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRzxy then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            parselineloop := parselineloop + 3;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRyzx then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            parselineloop := parselineloop + 3;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRzyx then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := CurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            parselineloop := parselineloop + 3;
          end;

          if FBone[m].FBvhChanneltype = ChanneltypeRyxz then
          begin
            FBone[m].FRotateframe[FCurrentFrame - 1].time := FCurrentFrame;
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y :=
              StrToFloat(parseline.strings[parselineloop + 0]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x :=
              StrToFloat(parseline.strings[parselineloop + 1]);
            FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z :=
              StrToFloat(parseline.strings[parselineloop + 2]);
            parselineloop := parselineloop + 3;
          end;

          FBone[m].FRotateframe[FCurrentFrame - 1].Value.x :=
            degtorad(FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.x);
          FBone[m].FRotateframe[FCurrentFrame - 1].Value.y :=
            degtorad(FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.y);
          FBone[m].FRotateframe[FCurrentFrame - 1].Value.z :=
            degtorad(FBone[m].FRotateframe[FCurrentFrame - 1].bvh_value.z);

          FBone[m].FTranslateframe[FCurrentFrame - 1].Value.x :=
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.x;
          FBone[m].FTranslateframe[FCurrentFrame - 1].Value.y :=
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.y;
          FBone[m].FTranslateframe[FCurrentFrame - 1].Value.z :=
            FBone[m].FTranslateframe[FCurrentFrame - 1].bvh_value.z;
        end;
        parseline.Free;
      end;
    end;
  end;

  FCurrentFrame := 0;

  line := '';
  mem.Clear;
  mem.Free;

  //init bone structure
  for m := 0 to FNumBones - 1 do
  begin
    FBone[m].Init;
  end;
end;

procedure TSkeleton.LoadmsaFromFile(filename: string);
var
  stream: TFileStream;
begin
  TAll3dsMesh(owner).FType := MeshMsa;
  stream := TFilestream.Create(Filename, fmOpenRead);
  LoadmsaFromStream(stream);
  stream.Free;
end;

procedure TSkeleton.LoadmsaFromStream(stream: Tstream);
var
  sl, tsl: TStringList;
  l: integer;
  line: string;
  strtemp: string;
  tcount: longword;
//  acount: longword;
//  mcount: longword;
  bcount: longword;

  Count{, loop}, floop: longword;
//  matid: integer;
  m: longword;
begin
  sl := TStringList.Create;
  sl.LoadFromStream(stream);
  l := 0;
  while l < sl.Count - 1 do
  begin
    line := sl.Strings[l];

    //read in frames data...
    if (pos('Frames: ', line) = 1) then
    begin
      FNumFrames := StrToInt(StringReplace(Line, 'Frames: ', '', [rfReplaceAll]));
    end;

    //read in frames data...
    if (pos('Frame: ', line) = 1) then
    begin
      FCurrentFrame := StrToInt(StringReplace(Line, 'Frame: ', '', [rfReplaceAll]));
    end;

    //read in bone data...
    if (pos('Bones: ', line) = 1) then
    begin
      bcount := StrToInt(StringReplace(Line, 'Bones: ', '', [rfReplaceAll]));

      setlength(FBone, bcount);
      FNumBones := bcount;

      if FNumBones > 0 then
      for tcount := 0 to bcount - 1 do
      begin
        FBone[tcount] := T3DSBone.Create(self);

        //read bone name
        l := l + 1;
        line := sl.Strings[l];
        strtemp := line;
        FBone[tcount].FName := StringReplace(strtemp, '"', '', [rfReplaceAll]);

        //read parent bone name
        l := l + 1;
        line := sl.Strings[l];
        strtemp := line;
        FBone[tcount].FParentName := StringReplace(strtemp, '"', '', [rfReplaceAll]);

        //read bone translate and rotate...
        l := l + 1;
        line := sl.Strings[l];
        tsl := TStringList.Create;
        tsl.CommaText := line;

        FBone[tcount].FTranslate.x := StrToFloat(tsl.strings[1]);
        FBone[tcount].FTranslate.y := StrToFloat(tsl.strings[2]);
        FBone[tcount].FTranslate.z := StrToFloat(tsl.strings[3]);

        FBone[tcount].FRotate.x := StrToFloat(tsl.strings[4]);
        FBone[tcount].FRotate.y := StrToFloat(tsl.strings[5]);
        FBone[tcount].FRotate.z := StrToFloat(tsl.strings[6]);

        tsl.Free;

        //read translate frames for bone
        l := l + 1;
        line := sl.Strings[l];
        Count := StrToInt(line);
        FBone[tcount].FNumTranslateFrames := Count;
        setlength(FBone[tcount].FTranslateFrame, Count);

        for floop := 0 to Count - 1 do
        begin
          l := l + 1;
          line := sl.Strings[l];
          tsl := TStringList.Create;
          tsl.CommaText := line;
          FBone[tcount].FTranslateFrame[floop].time := Round(StrToFloat(tsl.strings[0]));
          FBone[tcount].FTranslateFrame[floop].Value.x := StrToFloat(tsl.strings[1]);
          FBone[tcount].FTranslateFrame[floop].Value.y := StrToFloat(tsl.strings[2]);
          FBone[tcount].FTranslateFrame[floop].Value.z := StrToFloat(tsl.strings[3]);
          tsl.Free;
        end;

        //read rotate frames for bone
        l := l + 1;
        line := sl.Strings[l];
        Count := StrToInt(line);
        FBone[tcount].FNumRotateFrames := Count;
        setlength(FBone[tcount].FRotateFrame, Count);

        for floop := 0 to Count - 1 do
        begin
          l := l + 1;
          line := sl.Strings[l];
          tsl := TStringList.Create;
          tsl.CommaText := line;
          FBone[tcount].FRotateFrame[floop].time := Round(StrToFloat(tsl.strings[0]));
          FBone[tcount].FRotateFrame[floop].Value.x := StrToFloat(tsl.strings[1]);
          FBone[tcount].FRotateFrame[floop].Value.y := StrToFloat(tsl.strings[2]);
          FBone[tcount].FRotateFrame[floop].Value.z := StrToFloat(tsl.strings[3]);
          tsl.Free;
        end;
      end;
    end;

    l := l + 1;
  end;
  sl.Free;

  //init bone structure
  If FNumBones > 0 then
  for m := 0 to FNumBones - 1 do
  begin
    FBone[m].Init;
  end;

end;

//TAll3DSMesh routines ...
destructor TAll3DSMesh.Destroy;
begin
  if FDisplaylist > 0 then gldeletelists(FDisplaylist, 1);
  FName := '';
  inherited Destroy; //this will automaticaly free the meshes, materials, bones...
  //however do free the dynamic arrays used
  SetLength(FMesh, 0);
  SetLength(FMaterial, 0);
  SetLength(FSkeleton, 0);
end;

procedure TAll3dsMesh.AddMesh;
begin
  FNumMeshes := FNumMeshes + 1;
  SetLength(FMesh, FNumMeshes);
  FMesh[FNumMeshes - 1] := T3dsMesh.Create(self);
end;

procedure TAll3dsMesh.AddSkeleton;
begin
  FNumSkeletons := FNumSkeletons + 1;
  SetLength(FSkeleton, FNumSkeletons);
  FSkeleton[FNumSkeletons - 1] := TSkeleton.Create(self);
end;

procedure TAll3dsMesh.AddMaterial;
begin
  FNumMaterials := FNumMaterials + 1;
  SetLength(FMaterial, FNumMaterials);
  FMaterial[FNumMaterials - 1] := T3DSMaterial.Create(self);
end;

function TAll3DSMesh.GetSkeleton(Index: integer): TSkeleton;
begin
  Result := FSkeleton[index];
end;

function TAll3DSMesh.GetMesh(Index: integer): T3DSMesh;
begin
  Result := FMesh[index];
end;

function TAll3DSMesh.GetMaterial(Index: integer): T3DSMaterial;
begin
  Result := FMaterial[index];
end;

procedure TAll3DSMesh.BuildDisplayList;
begin
FDisplaylist := glGenLists(1);
glNewList(FDisplaylist, GL_COMPILE);
 Render;
glEndList;
end;

procedure TAll3DSMesh.render;
var
  m: integer;
begin
if FDisplaylist > 0 then
 glcalllist(FDisplaylist)
else
 for m := 0 to FNumMeshes - 1 do
  FMesh[FRenderOrder[m]].render;
end;

procedure TAll3DSMesh.LoadFromFile(filename: string);
begin
  fname:=filename;
  if LowerCase(copy(filename, length(filename) - 3, 4)) = '.3ds' then
    Load3DSFromFile(Filename);
  if LowerCase(copy(filename, length(filename) - 3, 4)) = '.txt' then
    LoadMSAFromFile(Filename);
  if LowerCase(copy(filename, length(filename) - 3, 4)) = '.bin' then
    LoadBINFromFile(Filename);
end;

procedure TAll3DSMesh.LoadFromStream(stream: Tstream);
begin
  if FType = Mesh3ds then
    Load3DSFromStream(stream);
  if FType = MeshMsa then
    LoadMSAFromStream(stream);
end;

procedure TAll3DSMesh.SaveToFile(filename: string);
begin
  if LowerCase(copy(filename, length(filename)-3, 4)) = '.txt' then
    SaveMSAToFile(Filename);
  if LowerCase(copy(filename, length(filename)-3, 4)) = '.bin' then
    SaveBinToFile(Filename);
end;

procedure TAll3DSMesh.SaveToStream(stream: Tstream);
begin
  if FType = MeshMsa then
    SaveMSAToStream(stream);
  if FType = MeshBin then
    SaveBinToStream(stream);
end;

procedure TAll3DSMesh.LoadmsaFromFile(filename: string);
var
  stream: TFileStream;
begin
  FType := MeshMsa;
  stream := TFilestream.Create(Filename, fmOpenRead);
  LoadmsaFromStream(stream);
  stream.Free;

  //also load skeleton if needed (this means that when loading from stream only
  //  this is not done)
  if floadskeleton then
  begin
  fnumskeletons:=fnumskeletons+1;
  setlength(fskeleton, fnumskeletons);
  fskeleton[fnumskeletons-1]:=Tskeleton.Create(self);
  fskeleton[fnumskeletons-1].LoadFromFile(fname);
  //init skin
  InitSkin;
  end;

end;

procedure TAll3DSMesh.LoadmsaFromStream(stream: Tstream);
var
  sl, tsl: TStringList;
  l: integer;
  line: string;
  strtemp: string;
  tcount: longword;
  acount: longword;
  mcount: longword;
  bcount: longword;

  Count, loop, floop: longword;
  matid: integer;
  m: longword;
begin
  floadskeleton:=false;
  sl := TStringList.Create;
  sl.LoadFromStream(stream);
  l := 0;
  while l < sl.Count - 1 do
  begin
    line := sl.Strings[l];

    //read in mesh data...
    if (pos('Meshes: ', line) = 1) then
    begin
      acount := StrToInt(StringReplace(Line, 'Meshes: ', '', [rfReplaceAll]));

      FNumMeshes := acount;
      SetLength(FMesh, acount);
      SetLength(FRenderOrder, acount);
      for tcount := 0 to acount - 1 do
      begin
        FMesh[tcount] := T3DSMesh.Create(self);
        FRenderOrder[tcount] := tcount;
        //read in mesh name and the id of the material for the mesh (only one material?)
        l := l + 1;
        line := sl.Strings[l];
        strtemp := copy(line, 0,pos(' ', line) - 1);
        Fmesh[tcount].FName := StringReplace(strtemp, '"', '', [rfReplaceAll]);

        strTemp := StringReplace(line, '"', '', [rfReplaceAll]);
        strTemp := copy(strTemp, pos(' ', strTemp) + 1,length(strTemp));
        matid := StrToInt(copy(strTemp, pos(' ', strTemp) + 1,length(strTemp)));

        SetLength(FMesh[tcount].FMatName, 1);
        if matid = -1 then FMesh[tcount].FMatName[0] := ''
        else
          FMesh[tcount].FMatName[0] := IntToStr(matid);

        //read in vertex data, texture u and v and the bone applied to the vertex
        l := l + 1;
        line := sl.Strings[l];
        Count := StrToInt(line);
        setlength(FMesh[tcount].FVertex, Count);
        setlength(FMesh[tcount].FMapping, Count);
        setlength(FMesh[tcount].FBoneid, Count);
        FMesh[tcount].FNumVertex := Count;

        if Count > 0 then
          for loop := 0 to count-1 do
          begin
            l := l + 1;
            line := sl.Strings[l];
            tsl := TStringList.Create;
            tsl.CommaText := line;

            FMesh[tcount].FVertex[loop].x := strtofloat(tsl.Strings[1]);
            FMesh[tcount].FVertex[loop].y := strtofloat(tsl.Strings[2]);
            FMesh[tcount].FVertex[loop].z := strtofloat(tsl.Strings[3]);

            FMesh[tcount].FMapping[loop].tu := strtofloat(tsl.Strings[4]);
            FMesh[tcount].FMapping[loop].tv := strtofloat(tsl.Strings[5]);
            //adjust texture coord v? when and when not?
            FMesh[tcount].FMapping[loop].tv := 1.0 - FMesh[tcount].FMapping[loop].tv;

            FMesh[tcount].FBoneId[loop] := StrToInt(tsl.Strings[6]);

            tsl.Free;
          end;

        //read in the normals
        l := l + 1;
        line := sl.Strings[l];
        Count := StrToInt(line);
        setlength(FMesh[tcount].FVnormal, Count);

        if Count > 0 then
          for loop := 0 to count-1 do
          begin
            l := l + 1;
            line := sl.Strings[l];
            tsl := TStringList.Create;
            tsl.CommaText := line;

            FMesh[tcount].FVnormal[loop].x := strtofloat(tsl.Strings[0]);
            FMesh[tcount].FVnormal[loop].y := strtofloat(tsl.Strings[1]);
            FMesh[tcount].FVnormal[loop].z := strtofloat(tsl.Strings[2]);

            tsl.Free;
          end;

        //read in the indices (faces)
        l := l + 1;
        line := sl.Strings[l];
        Count := StrToInt(line);
        setlength(FMesh[tcount].FIndices, Count * 3);
        setlength(FMesh[tcount].FNormalIndices, Count * 3);
        FMesh[tcount].FNumIndices := Count * 3;

        if Count > 0 then
          for loop := 0 to Count - 1 do
          begin
            l := l + 1;
            line := sl.Strings[l];
            tsl := TStringList.Create;
            tsl.CommaText := line;

            for floop := 1 to 3 do
            begin
              FMesh[tcount].FIndices[loop * 3 + floop - 1] := StrToInt(tsl.Strings[floop]);
              FMesh[tcount].FNormalindices[loop * 3 + floop - 1] := StrToInt(tsl.Strings[floop + 3]);
            end;
            //maybe also read in the normals indices?
            tsl.Free;
          end;

        //set matid for every vertex (make compatible with 3ds render)
        if Count > 0 then
        begin
          SetLength(FMesh[tcount].FMatId, FMesh[tcount].FNumIndices div 3);
          for loop := 0 to (FMesh[tcount].FNumIndices div 3) - 1 do
          begin
            FMesh[tcount].FMatId[loop] := matid{+ 1};
          end;
        end;
      end;
    end;

    //read in material data...
    if (pos('Materials: ', line) = 1) then
    begin

      mcount := StrToInt(StringReplace(Line, 'Materials: ', '', [rfReplaceAll]));
      setlength(FMaterial, mcount + 1);
      FNumMaterials := mcount;
      if FNumMaterials > 0 then
      begin
      for tcount := 0 to mcount - 1 do
      begin
        FMaterial[tcount] := T3DSMaterial.Create(self);

        //read material name
        l := l + 1;
        line := sl.Strings[l];
        strtemp := line;
        FMaterial[tcount].FName := StringReplace(strtemp, '"', '', [rfReplaceAll]);

        //read ambient color data
        l := l + 1;
        line := sl.Strings[l];
        tsl := TStringList.Create;
        tsl.CommaText := line;
        FMaterial[tcount].FIsAmbient := False;
        FMaterial[tcount].FAmbR := StrToFloat(tsl.strings[0]);
        FMaterial[tcount].FAmbG := StrToFloat(tsl.strings[1]);
        FMaterial[tcount].FAmbB := StrToFloat(tsl.strings[2]);
        if (FMaterial[tcount].FAmbR<>0) or (FMaterial[tcount].FAmbR<>0) or (FMaterial[tcount].FAmbR<>0) then FMaterial[tcount].FIsAmbient := True;
        tsl.Free;

        //read diffuse color data
        l := l + 1;
        line := sl.Strings[l];
        tsl := TStringList.Create;
        tsl.CommaText := line;
        FMaterial[tcount].FIsDiffuse := False;
        FMaterial[tcount].FDifR := StrToFloat(tsl.strings[0]);
        FMaterial[tcount].FDifG := StrToFloat(tsl.strings[1]);
        FMaterial[tcount].FDifB := StrToFloat(tsl.strings[2]);
        FMaterial[tcount].FTransparency := StrToFloat(tsl.strings[3]);
        if (FMaterial[tcount].FDifR<>0) or (FMaterial[tcount].FDifR<>0) or (FMaterial[tcount].FDifR<>0) then FMaterial[tcount].FIsDiffuse := True;
        tsl.Free;

        //read specular color data
        l := l + 1;
        line := sl.Strings[l];
        tsl := TStringList.Create;
        tsl.CommaText := line;
        FMaterial[tcount].FIsSpecular := False;
        FMaterial[tcount].FSpcR := StrToFloat(tsl.strings[0]);
        FMaterial[tcount].FSpcG := StrToFloat(tsl.strings[1]);
        FMaterial[tcount].FSpcB := StrToFloat(tsl.strings[2]);
        if (FMaterial[tcount].FSpcR<>0) or (FMaterial[tcount].FSpcR<>0) or (FMaterial[tcount].FSpcR<>0) then FMaterial[tcount].FIsSpecular := True;
        tsl.Free;

        l := l + 3; //skip emissive, shininess, transperancy (implement later)

        line:=sl.Strings[l];
        FMaterial[tcount].FBumpMapStrength := StrToFloat(line);

        l:=l+1;
        //read texture filename
        line := sl.Strings[l];
        strtemp := line;
        FMaterial[tcount].FFileName := '';
        FMaterial[tcount].FFileName := StringReplace(strtemp, '"', '', [rfReplaceAll]);
        if FMaterial[tcount].FFilename <> '' then
          FMaterial[tcount].FHastexturemap := True;
        //skip second texture filename? alpha?
        l := l + 1;
        //read bumpmap filename
        line := sl.Strings[l];
        strtemp := line;
        FMaterial[tcount].FBumpMapFileName := '';
        FMaterial[tcount].FBumpMapFileName := StringReplace(strtemp, '"', '', [rfReplaceAll]);
        if FMaterial[tcount].FBumpMapFileName <> '' then
          FMaterial[tcount].FHasbumpmap := True;
//        l:= l + 1;
      end;
    end;
    end;

    //read in bone data...
    if (pos('Bones: ', line) = 1) then
    begin
      bcount := StrToInt(StringReplace(Line, 'Bones: ', '', [rfReplaceAll]));
      //if there are bones
      if bcount >= 1 then floadskeleton:=true;
    end;

    l := l + 1;
  end;
  sl.Free;

  //fill matnames into meshes
  If FnumMeshes > 0 then
  for m:= 0 to FNumMeshes -1 do
  begin
    if FMesh[m].FMatName[0] <> '0' then
    begin
      FMesh[m].FMatID[0] := StrToInt(FMesh[m].FMatName[0]);
      FMesh[m].FMatName[0] := FMaterial[StrToInt(FMesh[m].FMatName[0])].FName;
    end;
  end;

  Calculatesize;        //calculate min and max size
  CalculateRenderOrder; //set transparency order...

  //preload textures...
  If FNumMaterials > 0 then
  for m := 0 to FNumMaterials - 1 do
  begin
    FMaterial[m].updatetexture;
  end;

end;

procedure TAll3DSMesh.SaveBINToFile(filename: string);
var
  stream: TFilestream;
begin
  FType := MeshBin;
  stream := TFilestream.Create(Filename, fmCreate);
  SaveBINToStream(stream);
  stream.Free;
end;

procedure TAll3DSMesh.SaveMsaToFile(filename: string);
var
  stream: TFilestream;
begin
  FType := MeshMSA;
  stream := TFilestream.Create(Filename, fmCreate);
  SaveMSAToStream(stream);
  stream.Free;
end;

procedure TAll3DSMesh.SaveMSAToStream(stream: Tstream);
var ms: TStringList;
    saveloop: integer;
    subsaveloop: integer;
    tempstring: string;
begin
  //this saves meshes and materials to a milkshape ascii file
  ms:=TStringList.Create;

  ms.Add('// MilkShape 3D ASCII');
  ms.Add('');
  ms.Add('Frames: 0');
  ms.Add('Frame: 0');
  ms.Add('');

  //save mesh data
  ms.Add('Meshes: '+IntToStr(FNumMeshes));

  for saveloop:=0 to FNumMeshes-1 do
  begin
    tempstring:=StringReplace(fmesh[saveloop].fname, ' ', '_', [rfReplaceAll]);
    if fmesh[saveloop].fmatid<>nil then
       ms.Add('"'+tempstring+'"'+' 0'+' '+inttostr(fmesh[saveloop].fmatid[0]))
    else
       ms.Add('"'+tempstring+'"'+' 0'+' 0');
    //save vertexes
    ms.Add(inttostr(fmesh[saveloop].Fnumvertex));
    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do
    begin
      if fmesh[saveloop].fboneid <> nil then
      ms.Add('0'+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].x)+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].y)+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].z)+' '+floattostr(fmesh[saveloop].Mapping[subsaveloop].tu)+' '+
      floattostr(1.0-fmesh[saveloop].Mapping[subsaveloop].tv)+' '+inttostr(fmesh[saveloop].fboneid[subsaveloop]))
      else
      ms.Add('0'+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].x)+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].y)+' '+floattostr(fmesh[saveloop].Vertex[subsaveloop].z)+' '+floattostr(fmesh[saveloop].Mapping[subsaveloop].tu)+' '+
      floattostr(1.0-fmesh[saveloop].Mapping[subsaveloop].tv)+' -1');
    end;
    //save normals
    ms.Add(inttostr(fmesh[saveloop].Fnumvertex));
    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do //should i use seperate Fnumnormals??
    begin
      ms.Add(floattostr(fmesh[saveloop].Normal[subsaveloop].x)+' '+floattostr(fmesh[saveloop].Normal[subsaveloop].y)+' '+floattostr(fmesh[saveloop].Normal[subsaveloop].z));
    end;
    //save faces (indices)
    ms.Add(inttostr(fmesh[saveloop].Fnumindices div 3));
    for subsaveloop:=0 to (fmesh[saveloop].Fnumindices div 3) -1 do
    begin
     ms.Add('0 '
       +IntToStr(fmesh[saveloop].Findices[subsaveloop*3])+' '+IntToStr(fmesh[saveloop].Findices[subsaveloop*3+1])+' '+IntToStr(fmesh[saveloop].Findices[subsaveloop*3+2])+' '
       +IntToStr(fmesh[saveloop].Fnormalindices[subsaveloop*3])+' '+IntToStr(fmesh[saveloop].Fnormalindices[subsaveloop*3+1])+' '+IntToStr(fmesh[saveloop].Fnormalindices[subsaveloop*3+2])
       +' 1');
    end;
  end;

  ms.Add('');
  //save material data
  ms.Add('Materials: '+IntToStr(FNumMaterials));
  for saveloop:=0 to FNumMaterials-1 do
  begin
    ms.Add('"'+FMaterial[saveloop].fname+'"');
    ms.Add(FloatToStr(FMaterial[saveloop].FAmbR)+' '+FloatToStr(FMaterial[saveloop].FAmbG)+' '+FloatToStr(FMaterial[saveloop].FAmbB)+' 1.0');
    ms.Add(FloatToStr(FMaterial[saveloop].FDifR)+' '+FloatToStr(FMaterial[saveloop].FDifG)+' '+FloatToStr(FMaterial[saveloop].FDifB)+' '+FloatToStr(FMaterial[saveloop].FTransparency));
    ms.Add(FloatToStr(FMaterial[saveloop].FSpcR)+' '+FloatToStr(FMaterial[saveloop].FSpcG)+' '+FloatToStr(FMaterial[saveloop].FSpcB)+' 1.0');
    ms.Add('0.0 0.0 0.0 1.0');
    ms.Add('0.0');
    ms.Add(FloatToStr(FMaterial[saveloop].fbumpmapstrength));
    ms.Add('"'+FMaterial[saveloop].ffilename+'"');
    ms.Add('"'+FMaterial[saveloop].fbumpmapfilename+'"');
  end;

  //fake save bones
  ms.Add('');
  ms.Add('Bones: 0');
  ms.Add('');
  ms.Add('');

  ms.SaveToStream(stream);
  ms.Free;
end;


procedure TAll3DSMesh.SaveBINToStream(stream: Tstream);
var ms: TMemoryStream;
    saveloop: integer;
    subsaveloop: integer;
    buf: pointer;
    tempstr: string;
    tempint: integer;
    tempfloat: single;
//    tempbyte: byte;
begin
  //this saves meshes and materials to a bin file
  ms:=TMemoryStream.Create;

  tempstr:='nsk';
  buf:=pchar(tempstr);
  ms.Write(buf^,length(tempstr));

  //save mesh data
  //meshes:
  tempint:=FNumMeshes;
  buf:=@tempint;
  ms.Write(buf^,sizeof(tempint));

  for saveloop:=0 to FNumMeshes-1 do
  begin
    tempint:=length(fmesh[saveloop].fname); //save stringlength first
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    tempstr:=fmesh[saveloop].fname;
    buf:=pchar(tempstr);
    ms.Write(buf^,length(tempstr));

    if fmesh[saveloop].fmatid<>nil then
    tempint:=fmesh[saveloop].fmatid[0]
    else
    tempint:=-1;

    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    //save vertexes
    tempint:=fmesh[saveloop].Fnumvertex;
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));

    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do
    begin
      tempfloat:=fmesh[saveloop].Vertex[subsaveloop].x;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Vertex[subsaveloop].y;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Vertex[subsaveloop].z;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Mapping[subsaveloop].tu;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Mapping[subsaveloop].tv;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));

      tempint:=-1;
      if fmesh[saveloop].FBoneID<>nil then
      tempint:=fmesh[saveloop].FBoneID[subsaveloop]; //bone id
      buf:=@tempint;
      ms.Write(buf^,sizeof(tempint));
    end;
    //save normals
    tempint:=fmesh[saveloop].Fnumvertex;
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do //should i use seperate Fnumnormals??
    begin
      tempfloat:=fmesh[saveloop].Normal[subsaveloop].x;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Normal[subsaveloop].y;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
      tempfloat:=fmesh[saveloop].Normal[subsaveloop].z;
      buf:=@tempfloat;
      ms.Write(buf^,sizeof(tempfloat));
    end;
    //save faces (indices)
    tempint:=fmesh[saveloop].Fnumindices;
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));

    for subsaveloop:=0 to fmesh[saveloop].Fnumindices -1 do
    begin
      //indice
      tempint:=fmesh[saveloop].Findices[subsaveloop];
      buf:=@tempint;
      ms.Write(buf^,sizeof(tempint));
      //normal
      tempint:=fmesh[saveloop].Fnormalindices[subsaveloop];
      buf:=@tempint;
      ms.Write(buf^,sizeof(tempint));
    end;

    for subsaveloop:=0 to fmesh[saveloop].Fnumindices div 3 -1 do
    begin
      //matid
      tempint:=fmesh[saveloop].FMatID[subsaveloop]; //mat id
      buf:=@tempint;
      ms.Write(buf^,sizeof(tempint));
    end;
  end;

  //save material data
  tempint:=FNumMaterials;
  buf:=@tempint;
  ms.Write(buf^,sizeof(tempint));
  for saveloop:=0 to FNumMaterials-1 do
  begin
    tempint:=length(FMaterial[saveloop].fname);
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    tempstr:=FMaterial[saveloop].fname;
    buf:=pchar(tempstr);
    ms.Write(buf^,length(tempstr));
    tempfloat:=FMaterial[saveloop].FAmbR;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FAmbG;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FAmbB;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));

    tempfloat:=FMaterial[saveloop].FDifR;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FDifG;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FDifB;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FTransparency;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));

    tempfloat:=FMaterial[saveloop].FSpcR;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FSpcG;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    tempfloat:=FMaterial[saveloop].FSpcB;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
    //write texture name
    tempint:=length(FMaterial[saveloop].ffilename);
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    tempstr:=FMaterial[saveloop].ffilename;
    buf:=pchar(tempstr);
    ms.Write(buf^,length(tempstr));
    //write bumpmap name
    tempint:=length(FMaterial[saveloop].fbumpmapfilename);
    buf:=@tempint;
    ms.Write(buf^,sizeof(tempint));
    tempstr:=FMaterial[saveloop].fbumpmapfilename;
    buf:=pchar(tempstr);
    ms.Write(buf^,length(tempstr));
    //write bumpmap strength
    tempfloat:=FMaterial[saveloop].fbumpmapstrength;
    buf:=@tempfloat;
    ms.Write(buf^,sizeof(tempfloat));
  end;

  ms.SaveToStream(stream);
  ms.Free;
end;

procedure TAll3DSMesh.LoadBinFromFile(filename: string);
var
  stream: TFilestream;
begin
  FType := MeshBin;
  stream := TFilestream.Create(Filename, $0000);
  LoadBinFromStream(stream);
  stream.Free;
end;

procedure TAll3DSMesh.LoadBINFromStream(stream: Tstream);
var ms: TMemoryStream;
    saveloop: integer;
    subsaveloop: integer;
    buf: pointer;
    tempstr: string;
    tempint: integer;
    tempfloat: single;
//    tempbyte: byte;
begin
  //this loads meshes and materials from bin
  ms:=TMemoryStream.Create;
  ms.LoadFromStream(stream);
  ms.Position:=0;

  //check header...
  tempstr:='kns';
  ms.Read(tempstr[1],length(tempstr));

  if tempstr = 'nsk' then
  begin
  //meshes:
  tempint:=FNumMeshes;
  buf:=@tempint;
  ms.Read(buf^,sizeof(tempint));
  FNumMeshes:=tempint;

  setlength(FMesh, FNumMeshes);
  setlength(FrenderOrder, FNumMeshes);

  for saveloop:=0 to FNumMeshes-1 do
  begin
    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    setlength(tempstr,tempint);
    ms.Read(tempstr[1],length(tempstr));

    Fmesh[saveloop]:=T3dsMesh.Create(self);
    Fmesh[saveloop].Fname:=tempstr;
    FrenderOrder[saveloop]:=saveloop;

    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    setlength(fmesh[saveloop].Fmatid,1);
    fmesh[saveloop].Fmatid[0]:=tempint;

    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    fmesh[saveloop].Fnumvertex:=tempint;

    setlength(fmesh[saveloop].Fvertex, fmesh[saveloop].FNumVertex);
    setlength(fmesh[saveloop].Fmapping, fmesh[saveloop].FNumVertex);
    setlength(fmesh[saveloop].FBoneId, fmesh[saveloop].FNumVertex);

    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do
    begin
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVertex[subsaveloop].x:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVertex[subsaveloop].y:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVertex[subsaveloop].z:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FMapping[subsaveloop].tu:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FMapping[subsaveloop].tv:=tempfloat;

      tempint:=0;
      buf:=@tempint;
      ms.Read(buf^,sizeof(tempint));
      fmesh[saveloop].Fboneid[subsaveloop]:=tempint;
    end;

    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));

    setlength(fmesh[saveloop].Fvnormal, fmesh[saveloop].FNumVertex); //should use the above..

    for subsaveloop:=0 to fmesh[saveloop].Fnumvertex -1 do
    begin
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVNormal[subsaveloop].x:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVNormal[subsaveloop].y:=tempfloat;
      tempfloat:=0;
      buf:=@tempfloat;
      ms.Read(buf^,sizeof(tempfloat));
      fmesh[saveloop].FVNormal[subsaveloop].z:=tempfloat;
    end;

    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));

    fmesh[saveloop].Fnumindices:=tempint;
    setlength(fmesh[saveloop].Findices, fmesh[saveloop].Fnumindices);
    setlength(fmesh[saveloop].Fnormalindices, fmesh[saveloop].Fnumindices);
    setlength(fmesh[saveloop].FMatId, fmesh[saveloop].Fnumindices div 3);

    for subsaveloop:=0 to fmesh[saveloop].Fnumindices -1 do
    begin
      tempint:=0;
      buf:=@tempint;
      ms.Read(buf^,sizeof(tempint));
      fmesh[saveloop].Findices[subsaveloop]:=tempint;
      tempint:=0;
      buf:=@tempint;
      ms.Read(buf^,sizeof(tempint));
      fmesh[saveloop].Fnormalindices[subsaveloop]:=tempint;
    end;

    for subsaveloop:=0 to fmesh[saveloop].Fnumindices div 3 -1 do
    begin
      tempint:=0;
      buf:=@tempint;
      ms.Read(buf^,sizeof(tempint));
      fmesh[saveloop].FMatid[subsaveloop]:=tempint;
    end;
  end; //temporary end for testing....

  tempint:=0;
  buf:=@tempint;
  ms.Read(buf^,sizeof(tempint));
  FNumMaterials:=tempint;
  SetLength(FMaterial,FNumMaterials);

  for saveloop:=0 to FNumMaterials-1 do
  begin
    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    setlength(tempstr,tempint);
    ms.Read(tempstr[1],length(tempstr));


    FMaterial[saveloop]:=T3dsMaterial.Create(self);
    FMaterial[saveloop].Fname:=tempstr;

    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FAmbR:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FAmbG:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FAmbB:=tempfloat;

    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FDifR:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FDifG:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FDifB:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FTransparency:=tempfloat;

    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FSpcR:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FSpcG:=tempfloat;
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FSpcB:=tempfloat;

    //load texture filename
    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    if tempint > 1 then
    begin
      FMaterial[saveloop].fhastexturemap:=true;
      setlength(tempstr,tempint);
      ms.Read(tempstr[1],length(tempstr));
      FMaterial[saveloop].ffilename:=tempstr;
    end;

    //load bumpmap filename
    tempint:=0;
    buf:=@tempint;
    ms.Read(buf^,sizeof(tempint));
    if tempint > 1 then
    begin
      FMaterial[saveloop].fhasbumpmap:=true;
      setlength(tempstr,tempint);
      ms.Read(tempstr[1],length(tempstr));
      FMaterial[saveloop].fbumpmapfilename:=tempstr;
    end;

    //load bumpmap strength
    tempfloat:=0;
    buf:=@tempfloat;
    ms.Read(buf^,sizeof(tempfloat));
    FMaterial[saveloop].FBumpMapStrength:=tempfloat;

  end;

  Calculatesize;        //calculate min and max size
  CalculateRenderOrder; //for transparency

  //preload textures...
  If FNumMaterials > 0 then
  for saveloop := 0 to FNumMaterials - 1 do
  begin
    FMaterial[saveloop].updatetexture;
  end;
  end;
  ms.Free;
end;

procedure TAll3DSMesh.Load3dsFromFile(filename: string);
var
  stream: TFilestream;
begin
  FType := Mesh3ds;
  stream := TFilestream.Create(Filename, $0000);
  Load3dsFromStream(stream);
  //if _Version <> 3 then raise Exception.Create('Wrong 3DS Version');
  stream.Free;
end;

procedure TAll3DSMesh.Load3dsFromStream(stream: Tstream);
var
  buffer: TChunkHdr;
  chrbuf: byte;
  color: t3dscolor;
  rgbcolor: rgb;
  Count, i, dummy: word;
  tv: T3dPoint;
  tm: TMap;
  matarr: byte;
  transp: shortInt;
  bump: shortInt;
  tempscale: single;
  matcount, matcountloop, mati, keyfcount: word;
  m: longword;
  bumpmap: boolean;
  acount: longword;
  mcount: longword;
  mapname: string;
begin
  //init hchunk
  FName := '';
  FMesh := nil;
  FMaterial := nil;
  FNumMeshes := 0; // set number of meshes to zero
  FNumMaterials := 0; // set number of materials to zero
  if fmasterscale = 0 then
   fmasterscale := 1;

  acount := 0;
  mcount := 0;
  keyfcount := 0;
  matarr := 0;
  bumpmap := false;

  //start reading chunks
  //now all is read in once... This helps for some none standard 3ds files like those generated by cinema4d.
  while stream.Position < stream.Size do
  begin
    stream.Read(buffer, sizeof(TChunkHdr));
    case buffer.chunkID of
      MAIN3DS:
      begin
        //No nothing seems to be needed here...
      end;
      VERS3DS:
      begin
        stream.Read(FVersion, 4);
        if not (FVersion = 3) then
        begin
          stream.Seek(0,3);
          //???????
        end;
      end;
      EDIT3DS:
      begin
        //No nothing seems to be needed here...
      end;

      MASTER_SCALE:
      begin
        stream.Read(tempscale, sizeof(tempscale));
        if fmasterscale = 0 then
         fmasterscale:=tempscale;
      end;

      //read matrial data
      MAT_MATRIAL:
      begin
        //inc(mcount);
        //setlength(HChunk.Material,mcount);
        //moved to MAT_MATNAME (thanks to non-standard 3ds files)
      end;
      MAT_MATNAME:
      begin
        inc(mcount);
        setlength(FMaterial, mcount);
        FNumMaterials := FNumMaterials + 1;
        FMaterial[mcount - 1] := T3DSMaterial.Create(self);
        FMaterial[mcount - 1].FName := '';
        FMaterial[mcount - 1].FTransparency := 1.0;
        chrbuf := 1;
        while chrbuf <> 0 do
        begin
          stream.Read(chrbuf, 1);
          FMaterial[mcount - 1].FName :=
            FMaterial[mcount - 1].FName + chr(chrbuf);
        end;
        Delete(FMaterial[mcount - 1].FName,
        length(FMaterial[mcount - 1].FName), 1);
      end;
      MAT_AMBIENT:
      begin
        with FMaterial[mcount - 1] do
        begin
          FIsAmbient := True;
          FIsDiffuse := False;
          FIsSpecular := False;
          Fhasmaterial := True;
        end;
      end;
      MAT_DIFFUSE:
      begin
        with FMaterial[mcount - 1] do
        begin
          FIsAmbient := False;
          FIsDiffuse := True;
          FIsSpecular := False;
          Fhasmaterial := True;
        end;
      end;
      MAT_SPECULAR:
      begin
        with FMaterial[mcount - 1] do
        begin
          FIsAmbient := False;
          FIsDiffuse := False;
          FIsSpecular := True;
          Fhasmaterial := True;
        end;
      end;
      MAT_TEXTURE:
      begin
        bumpmap:=false;
        with FMaterial[mcount - 1] do
        begin
          FHasTexturemap := True;
          FUs := 0;
          FVs := 0;
          FUoff := 0;
          FVoff := 0;
        end;
      end;
      MAT_BUMPMAP:
      begin
        bumpmap:=true;
        bump := 100;
        stream.Read(dummy, sizeof(TChunkHdr));
        //percentage chunk header overslaan...
        stream.Read(bump, 2);
        FMaterial[mcount - 1].FBumpmapStrength := bump / 10000.0;
      end;
      MAT_MAPFILE:
      begin
        chrbuf := 1;
        mapname:='';
        while chrbuf <> 0 do
        begin
          stream.Read(chrbuf, 1);
          MapName := MapName + chr(chrbuf);
        end;
        Delete(MapName, length(MapName), 1);

        If Not BumpMap then
        begin
          //texture filename
          FMaterial[mcount - 1].FFileName := mapname;

       end
        else
        begin
          //bumpmap filename
          FMaterial[mcount - 1].FBumpMapFileName := mapname;
          FMaterial[mcount - 1].FHasBumpMap := true;
          bumpmap:=false;
        end;
      end;
      MAT_VSCALE: stream.Read(FMaterial[mcount - 1].FVs, 4);
      MAT_USCALE: stream.Read(FMaterial[mcount - 1].FUs, 4);
      MAT_VOFF: stream.Read(FMaterial[mcount - 1].FVoff, 4);
      MAT_UOFF: stream.Read(FMaterial[mcount - 1].FUoff, 4);
      MAT_TEXROT: stream.Read(FMaterial[mcount - 1].FRot, 4);
      MAT_COLOR24:
      begin
        stream.Read(rgbcolor, sizeof(rgbcolor));
        if FMaterial[mcount - 1].FIsDiffuse then
        begin
          with FMaterial[mcount - 1] do
          begin
            FDifr := rgbcolor.r / 255;
            FDifg := rgbcolor.g / 255;
            FDifb := rgbcolor.b / 255;
          end;
        end;
        if FMaterial[mcount - 1].FIsAmbient then
        begin
          with FMaterial[mcount - 1] do
          begin
            FAmbr := rgbcolor.r / 255;
            FAmbg := rgbcolor.g / 255;
            FAmbb := rgbcolor.b / 255;
          end;
        end;
        if FMaterial[mcount - 1].FIsSpecular then
        begin
          with FMaterial[mcount - 1] do
          begin
            FSpcr := rgbcolor.r / 255;
            FSpcg := rgbcolor.g / 255;
            FSpcb := rgbcolor.b / 255;
          end;
        end;
      end;
      MAT_COLOR:
      begin
        stream.Read(color, sizeof(t3dscolor));
        if FMaterial[mcount - 1].FIsDiffuse then
        begin
          with FMaterial[mcount - 1] do
          begin
            FDifr := color.r;
            FDifg := color.g;
            FDifb := color.b;
          end;
        end;
        if FMaterial[mcount - 1].FIsAmbient then
        begin
          with FMaterial[mcount - 1] do
          begin
            FAmbr := color.r;
            FAmbg := color.g;
            FAmbb := color.b;
          end;
        end;
        if FMaterial[mcount - 1].FIsSpecular then
        begin
          with FMaterial[mcount - 1] do
          begin
            FSpcr := color.r;
            FSpcg := color.g;
            FSpcb := color.b;
          end;
        end;
      end;
      MAT_TRANSPARENCY:
      begin
        transp := 100;
        stream.Read(dummy, sizeof(TChunkHdr));
        //percentage chunk header overslaan...
        stream.Read(transp, 2);
        FMaterial[mcount - 1].FTransparency := 1.0 - transp / 100.0;
      end;
      MAT_TWO_SIDE :
      begin
        // This chunk contains nothing but the header. If it's present,
        // the current material is two-sided and won't get backface-culled
        FMaterial[mcount-1].FTwoSided := True;
      end;

      //read submeshes (objects) ...
      EDIT_OBJECT{, OBJ_HIDDEN}:
      begin
        inc(acount);
        setlength(FMesh, acount);
        setlength(FRenderOrder, acount);
        chrbuf := 1;
        FMesh[acount - 1] := T3DSMesh.Create(self); //create
        FMesh[acount - 1].FName := '';
        Fmesh[acount - 1].Fid:=acount; //store an id per mesh...
        FRenderOrder[acount - 1] := acount - 1;
        while chrbuf <> 0 do
        begin
          stream.Read(chrbuf, 1);
          FMesh[acount - 1].FName := FMesh[acount - 1].FName + chr(chrbuf);
        end;
        Delete(FMesh[acount - 1].FName, length(FMesh[acount - 1].FName), 1);

        //set dummy material
        setlength(FMesh[acount - 1].FMatName, 1);
        FMesh[acount - 1].FMatName[0] := '';
      end;
      OBJ_TRIMESH:
      begin
        matarr := 0; //reset matarr to 0 for every submesh
        FNumMeshes := FNumMeshes + 1;
      end;
      OBJ_LIGHT:
      begin
        //do nothing yet skip it
        Stream.Seek(Buffer.ChunkLE - sizeof(TChunkHdr), soFromCurrent);
        FNumMeshes := FNumMeshes + 1;
      end;
      OBJ_CAMERA:
      begin
        //do nothing yet skip it
        Stream.Seek(Buffer.ChunkLE - sizeof(TChunkHdr), soFromCurrent);
        FNumMeshes := FNumMeshes + 1;
      end;

      //ReadVertices...
      TRI_VERTEXL:
      begin
        stream.Read(Count, 2);

        if Count > 0 then
        begin
          FMesh[acount - 1].FNumVertex := Count;
          SetLength(FMesh[acount - 1].FVertex, Count);
          SetLength(FMesh[acount - 1].FBoneId, Count);
          SetLength(FMesh[acount - 1].FMapping, Count);
          for i := 0 to Count - 1 do
          begin
            stream.Read(tv, 12);
            FMesh[acount - 1].FVertex[i] := tv;
            FMesh[acount - 1].FVertex[i].y := tv.z;
            FMesh[acount - 1].FVertex[i].z := tv.y;
            FMesh[acount - 1].FBoneId[i] := -1;
            FMesh[acount - 1].FMapping[i].tu := 0;
            FMesh[acount - 1].FMapping[i].tv := 0;
          end;
        end
        else
        begin
          FMesh[acount - 1].FNumVertex := 0;
        end;
      end;
      TRI_MAPPINGCOORDS:
      begin
        stream.Read(Count, 2);
        SetLength(FMesh[acount - 1].FMapping, Count);
        for i := 0 to Count - 1 do
        begin
          stream.Read(tm, 8);
          FMesh[acount - 1].FMapping[i] := tm;
        end;
      end;
      TRI_MATERIAL:
      begin
        chrbuf := 1;
        inc(matarr);
        setlength(FMesh[acount - 1].FMatName, matarr);
        FMesh[acount - 1].FMatName[matarr - 1] := '';
        while chrbuf <> 0 do
        begin
          stream.Read(chrbuf, 1);
          FMesh[acount - 1].FMatName[matarr - 1] :=
            FMesh[acount - 1].FMatName[matarr - 1] + chr(chrbuf);
        end;
        Delete(FMesh[acount - 1].FMatName[matarr - 1],
          length(FMesh[acount - 1].FMatName[matarr - 1]), 1);

        //look up and set matid for vertices with this material
        stream.Read(matcount, 2);
        if matcount > 0 then //hmm matcount should be higher then 0????
        begin
          SetLength(FMesh[acount - 1].FMatId,
            FMesh[acount - 1].FNumIndices div 3);
          for matcountloop := 0 to matcount - 1 do
          begin
            stream.Read(mati, 2);
            FMesh[acount - 1].FMatId[mati] :=
              GetMaterialIDbyName(FMesh[acount - 1].FMatName[matarr - 1]);
          end;
        end;
      end;
      TRI_FACEL1:
      begin
        stream.Read(Count, 2);
        SetLength(FMesh[acount - 1].FIndices, Count * 3);
        SetLength(FMesh[acount - 1].FNormalIndices, Count * 3);
        FMesh[acount - 1].FNumIndices := Count * 3;
        for i := 0 to Count-1 do
        begin
          stream.Read(FMesh[acount - 1].FIndices[i * 3], 2);
          stream.Read(FMesh[acount - 1].FIndices[i * 3 + 1], 2);
          stream.Read(FMesh[acount - 1].FIndices[i * 3 + 2], 2);

          //copy vertex to normal data
          FMesh[acount - 1].FNormalIndices[i * 3] :=
            FMesh[acount - 1].FIndices[i * 3];
          FMesh[acount - 1].FNormalIndices[i * 3 + 1] :=
            FMesh[acount - 1].FIndices[i * 3 + 1];
          FMesh[acount - 1].FNormalIndices[i * 3 + 2] :=
            FMesh[acount - 1].FIndices[i * 3 + 2];

          stream.Read(dummy, 2);
        end;
      end;
      TRI_MATRIX :
       begin
       Stream.Read(FMesh[acount-1].LocalAxis[0], 12);
       Stream.Read(FMesh[acount-1].LocalAxis[1], 12);
       Stream.Read(FMesh[acount-1].LocalAxis[2], 12);
       Stream.Read(FMesh[acount-1].FLocalCoords, 12);
       end;
      $4150 : // Smoothing group (child of $4120)
       begin
       SetLength(FMesh[acount - 1].FSmoothingGroup, FMesh[acount - 1].FNumIndices div 3);
//       raise Exception.Create(FMesh[acount - 1].Name+' '+ IntToStr(FMesh[acount - 1].FNumIndices div 3)+' '+IntToStr(buffer.chunkLE- sizeof(TChunkHdr)));
       for i := 0 to FMesh[acount - 1].FNumIndices div 3 - 1 do
        Stream.Read(FMesh[acount - 1].FSmoothingGroup[i], SizeOf(Cardinal));
       end;

      // read in keyframe data if available (for now only for pivot)
      KEYF3DS:
      begin
        //No nothing seems to be needed here...
      end;
      KEYF_OBJDES:
      begin
        //if all is ok then for every submesh a keyf_objdes exists
        inc(keyfcount);
      end;
      KEYF_OBJPIVOT:
      begin
        //read in pivot point
        stream.Read(tv, 12);
        if keyfcount <= FNumMeshes then
          //stupid way to do, but some 3ds files have less meshes then 'bones'
          FMesh[keyfcount - 1].FPivot := tv;
      end;
      else
        stream.Seek(buffer.chunkLE - sizeof(TChunkHdr), soFromCurrent);
    end;
  end;

  //calculate some things afterwards...
  movetopivot;                   //if pivots are used in 3ds move submeshes
//  fmasterscale:=fmasterscale/10; //?? Am i allowed to do this?
  CalculateScale;                //calculate new vertexes with masterscale
  calcvnormals;                  //calculate vertex normals for a smooth looking 3ds
  CalculateSize;                 //calculate min and max size
  CalculateRenderOrder;          //transparent items should be rendered last

  //preload textures...
  for m := 0 to FNumMaterials - 1 do
  begin
    FMaterial[m].updatetexture;
  end;

  //set id's
  for m := 0 to FNumMeshes - 1 do
  begin
    FMesh[m].id:=m+1;
  end;

end;

//places transparent meshes to be rendered last
//TODO: if transparent material is not the first material for a mesh it does not work.
procedure TAll3DSMesh.CalculateRenderOrder;
var
  m: integer;
  found: integer;
  matloop: integer;
begin
  found := 0;
  m := 0;
  while m < FNumMeshes - 1 do
  begin
    if m < FNumMeshes - found then
    begin
      for matloop := 0 to High(FMesh[FrenderOrder[m]].FmatID) do
      begin
       if matloop > High(FMesh[FrenderOrder[m]].Fmatid) then
        raise Exception.Create('"'+Name+'"');
        if FMaterial[FMesh[FrenderOrder[m]].Fmatid[matloop]].Ftransparency < 1.0 then
        begin
          found := found + 1;
          FRenderOrder[m] := FNumMeshes - Found;
          FRenderOrder[FNumMeshes - Found] := m;
          m := m - 1; //a new mesh is placed at renderorder so it has to be checked again...
        end;
      end;
    end;
    m := m + 1;
  end;
end;

procedure TAll3DSMesh.InitSkin;
var
  f, m: integer;
  v: array [0..2] of single;
  matrix: clsMatrix;
  tempbone: integer;
begin
  for m := 0 to FNumMeshes - 1 do
  begin
    if FMesh[m].FNumIndices > 0 then
    begin
      f := 0;
      while f < FMesh[m].FNumVertex do // go through all vertexes and
      begin
        tempbone := FMesh[m].FBoneId[f];

        if tempbone<>-1 then
        begin
          matrix := FSkeleton[FCurrentSkeleton].FBone[tempbone].FMatrix;
          v[0] := FMesh[m].FVertex[f].x;
          v[1] := FMesh[m].FVertex[f].y;
          v[2] := FMesh[m].FVertex[f].z;
          matrix.InverseTranslateVect(v);
          matrix.InverseRotateVect(v);
          FMesh[m].FVertex[f].x := v[0];
          FMesh[m].FVertex[f].y := v[1];
          FMesh[m].FVertex[f].z := v[2];
        end;

        f := f + 1;
      end;
    end;
  end;
end;

procedure TAll3DSMesh.calcvnormals;
var
  f, m: integer;
  vertexn: t3dpoint;
begin
//get basic normals
for m := 0 to FNumMeshes - 1 do
 if FMesh[m].FNumIndices > 0 then
  with FMesh[m] do
   begin
   SetLength(FVnormal, FNumVertex);
   SetLength(FNormalIndices, FNumIndices);
   f := 0;
   while f < FMesh[m].FNumIndices  do // go through all vertexes and
    begin
    vertexn := CalcNormalVector(FVertex[FIndices[f]], FVertex[FIndices[f+1]], FVertex[FIndices[f+2]]);
    FNormalIndices[f]       := FIndices[f];
    FNormalIndices[f+1]     := FIndices[f+1];
    FNormalIndices[f+2]     := FIndices[f+2];
    FVnormal[FIndices[f]]   := vertexn;
    FVnormal[FIndices[f+1]] := vertexn;
    FVnormal[FIndices[f+2]] := vertexn;
    inc(f,3);
//    if (vertexn.x = 0) and (vertexn.y = 0) and (vertexn.z = 0) then
//     raise Exception.Create('Zero normalvector encountered, in mesh "'+Name+'" at face no.'+IntToStr(f div 3));
    end;
   end;

// Face normals (Sascha Willems)
for m := 0 to FNumMeshes - 1 do
 if FMesh[m].FNumIndices > 0 then
  with FMesh[m] do
   begin
   SetLength(FaceNormal, FNumIndices div 3);
   f := 0;
   while f < FMesh[m].FNumIndices  do
    begin
    FaceNormal[f div 3] := CalcNormalVector(FVertex[FIndices[f]], FVertex[FIndices[f+1]], FVertex[FIndices[f+2]]);
    inc(f,3);
    end;
   end;

{  //real smooth normals...
  // 15.09.2004 - Sascha Willems :
  //  Now takes the smoothing group of a face into account. Faces are only smoothed
  //  if they belong to the same smoothing group. This should make the normals correct
  //  for all meshes.
  vnormal.x:=0.0;
  vnormal.y:=0.0;
  vnormal.z:=0.0;
  shared:=0;
  for m := 0 to FNumMeshes - 1 do
  begin
    if FMesh[m].FNumIndices > 0 then
    begin
      f := 0;
      while f < FMesh[m].FNumIndices - 1 do // go through all vertexes and
       begin
       Find:=FMesh[m].FVertex[FMesh[m].FIndices[f]];
       for J:=0 to FMesh[m].FNumIndices-1 do
        if FMesh[m].FSmoothingGroup[FMesh[m].FIndices[j div 3]] = FMesh[m].FSmoothingGroup[FMesh[m].FIndices[f div 3]] then
         if (FMesh[m].FVertex[FMesh[m].FIndices[j]].x=Find.x) or (FMesh[m].FVertex[FMesh[m].FIndices[j]].y=Find.y) or (FMesh[m].FVertex[FMesh[m].FIndices[j]].z=Find.z) then
          begin
          vnormal.x:=vnormal.x+FMesh[m].FVNormal[FMesh[m].FIndices[j]].x;
          vnormal.y:=vnormal.y+FMesh[m].FVNormal[FMesh[m].FIndices[j]].y;
          vnormal.z:=vnormal.z+FMesh[m].FVNormal[FMesh[m].FIndices[j]].z;
          Inc(Shared);
          end;

        shared := 1;
        vertexn.x:=vnormal.x / shared;
        vertexn.y:=vnormal.y / shared;
        vertexn.z:=vnormal.z / shared;

        L:=sqrt(sqr (vertexn.x) + sqr(vertexn.y) + sqr(vertexn.z));
        if L>0 then
        begin
         vertexn.x:=vertexn.x/L;
         vertexn.y:=vertexn.y/L;
         vertexn.z:=vertexn.z/L;
         FMesh[m].FVnormal[FMesh[m].FIndices[f]] := vertexn;
        end;

        Shared:=0;

        vnormal.x:=0;
        vnormal.y:=0;
        vnormal.z:=0;

        f := f + 1;
      end;
    end;
  end;  }
end;

procedure TAll3dsMesh.RenderBoundBox;
var
  loop: integer;
begin
  if fnummeshes>0 then
    for loop:=0 to fnummeshes-1 do
    begin
      fmesh[loop].RenderBoundBox;
    end;
  glBegin(GL_LINE_LOOP);
    glVertex3f(minimum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, maximum.y, minimum.z);
    glVertex3f(minimum.x, maximum.y, minimum.z);
  glEnd;
  glBegin(GL_LINE_LOOP);
    glVertex3f(minimum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, maximum.y, maximum.z);
    glVertex3f(minimum.x, maximum.y, maximum.z);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(minimum.x, minimum.y, minimum.z);
    glVertex3f(minimum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, maximum.y, minimum.z);
    glVertex3f(maximum.x, maximum.y, maximum.z);
    glVertex3f(minimum.x, maximum.y, minimum.z);
    glVertex3f(minimum.x, maximum.y, maximum.z);
  glEnd;
end;

procedure TAll3dsMesh.calculatescale;
var
  f, m: integer;
begin
  for m := 0 to FNumMeshes - 1 do
  begin
    if FMesh[m].FNumVertex > 0 then
    begin
      f := 0;
      while f < FMesh[m].FNumVertex do // go through all vertexes and
      begin
        FMesh[m].FVertex[f].x := FMesh[m].FVertex[f].x * fmasterscale;
        FMesh[m].FVertex[f].y := FMesh[m].FVertex[f].y * fmasterscale;
        FMesh[m].FVertex[f].z := FMesh[m].FVertex[f].z * fmasterscale;

        f := f + 1;
      end;
    end;
  end;
end;

procedure TAll3dsMesh.movetopivot;
var
  f, m: integer;
  pivot: t3dpoint;
begin
  for m := 0 to FNumMeshes - 1 do
  begin
    if FMesh[m].FNumVertex > 0 then
    begin
      f := 0;
      while f < FMesh[m].FNumVertex do // go through all vertexes and
      begin
        pivot.x := FMesh[m].FPivot.x;
        pivot.y := FMesh[m].FPivot.y;
        pivot.z := FMesh[m].FPivot.z;

        FMesh[m].FVertex[f].x := FMesh[m].FVertex[f].x - pivot.x;
        FMesh[m].FVertex[f].y := FMesh[m].FVertex[f].y - pivot.y;
        FMesh[m].FVertex[f].z := FMesh[m].FVertex[f].z - pivot.z;

        f := f + 1;
      end;
    end;
  end;
end;

procedure TAll3DSMesh.Calculatesize;
var
  m{, f}: integer;
  x, y, z: single;
begin
  FMinimum.x := 0;
  FMinimum.y := 0;
  FMinimum.z := 0;
  FMaximum.x := 0;
  FMaximum.y := 0;
  FMaximum.z := 0;
  for m := 0 to FNumMeshes - 1 do
    if FMesh[m].FNumIndices > 0 then
    begin
      FMesh[m].CalculateSize;
      x := FMesh[m].Fminimum.x;
      y := FMesh[m].Fminimum.y;
      z := FMesh[m].Fminimum.z;
      if x < FMinimum.x then FMinimum.x := x;
      if y < FMinimum.y then FMinimum.y := y;
      if z < FMinimum.z then FMinimum.z := z;
      if x > FMaximum.x then FMaximum.x := x;
      if y > FMaximum.y then FMaximum.y := y;
      if z > FMaximum.z then FMaximum.z := z;
      x := FMesh[m].Fmaximum.x;
      y := FMesh[m].Fmaximum.y;
      z := FMesh[m].Fmaximum.z;
      if x < FMinimum.x then FMinimum.x := x;
      if y < FMinimum.y then FMinimum.y := y;
      if z < FMinimum.z then FMinimum.z := z;
      if x > FMaximum.x then FMaximum.x := x;
      if y > FMaximum.y then FMaximum.y := y;
      if z > FMaximum.z then FMaximum.z := z;
    end;
end;

function TAll3DSMesh.GetMeshByName(s: string): T3DSMesh;
var
  i: word;
begin
  Result := nil;
  for i := 0 to High(FMesh) do
    if uppercase(FMesh[i].FName) = uppercase(s) then
    begin
      Result := FMesh[i];
      break;
    end;
end;

function TAll3DSMesh.GetMaterialByName(s: string): T3DSMaterial;
var
  i: integer;
begin
  Result := nil;
  if FNumMaterials > 0 then
  begin
    for i := 0 to High(FMaterial) do
    begin
      if uppercase(FMaterial[i].FName) = uppercase(s) then
      begin
        Result := FMaterial[i];
        break;
      end;
    end;
  end;
end;

function TAll3DSMesh.GetMaterialIDByName(s: string): integer;
var
  i: integer;
begin
  Result := -1;
  if FNumMaterials > 0 then
  begin
    for i := 0 to High(FMaterial) do
    begin
      if uppercase(FMaterial[i].FName) = uppercase(s) then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

//T3DSMaterial routines...
constructor T3dsMaterial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDifR := 0.0;
  FDifG := 0.0;
  FDifB := 0.0;
  FIsDiffuse := False;
  FAmbR := 0.0;
  FAmbG := 0.0;
  FAmbB := 0.0;
  FIsAmbient := False;
  FSpcR := 0.0;
  FSpcG := 0.0;
  FSpcB := 0.0;
  FIsSpecular := False;
  FHasTextureMap := False;
  FTransparency := 1.0;
end;

destructor T3dsMaterial.Destroy;
begin
  if FHasTexturemap = True then gldeletetextures(1, @FTexId); //lets clean up afterwards...
  inherited Destroy;
end;

procedure T3DSMaterial.Apply;
var
  ambient, diffuse, specular, emissive: TGLCOLOR;
  power: single;
begin
if IgnoreMaterials then
 exit;

  gldisable(GL_TEXTURE_2D); //just to make sure...

  diffuse.red := FDifR;
  diffuse.green := FDifG;
  diffuse.blue := FDifB;
  diffuse.alpha := FTransparency;

  //if no ambient color data then also set diffuse for ambient
  if FIsAmbient then
  begin
    ambient.red := FAmbR/2;
    ambient.green := FAmbG/2;
    ambient.blue := FAmbB/2;
    ambient.alpha := 1.0;
  end
  else
  begin
    ambient.red := FDifR/2;
    ambient.green := FDifG/2;
    ambient.blue := FDifB/2;
    ambient.alpha := 1.0;
  end;

  specular.red := FSpcR/2;
  specular.green := FSpcG/2;
  specular.blue := FSpcB/2;
  specular.alpha := 1.0;

  with emissive do
  begin
    red := 0.0;
    green := 0.0;
    blue := 0.0;
    alpha := 1.0;
  end;
  power := 1.0;

  glDisable(GL_BLEND);

//  glMaterialfv(GL_FRONT, gl_ambient, @ambient);
//  glMaterialfv(GL_FRONT, gl_diffuse, @diffuse);
//  glMaterialfv(GL_FRONT, gl_specular, @specular);
//  glMaterialfv(GL_FRONT, gl_shininess, @power);
//  glMaterialfv(GL_FRONT, gl_emission, @emissive);
  glColor4f(Ambient.red, Ambient.green, Ambient.blue, FTransparency);
  if FTransparency < 1 then
   begin
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   end;

  if FHastexturemap = True then
  begin
    glActiveTexture(GL_TEXTURE0);
    glenable(GL_TEXTURE_2D);
    glbindtexture(GL_TEXTURE_2D, ftexture);
//    raise Exception.Create(IntToStr(hastexture));
    {if FHasBumpMap then
    begin
      //TODO: only change blendfunc when needed?
      If FTransParency = 1.0 then
        glBlendFunc(GL_SRC_ALPHA, GL_ZERO) //only fake bumpmapping
      else
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); //fake bumpmapping with transparency

      // RGB
      glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_COMBINE);

      glTexEnvf(GL_TEXTURE_ENV,GL_COMBINE_RGB,GL_MODULATE);
      glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE0_RGB,GL_TEXTURE);
      glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND0_RGB,GL_SRC_COLOR);
      glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE1_RGB,GL_PREVIOUS);
      glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND1_RGB,GL_SRC_COLOR);

      // alpha
      glTexEnvf(GL_TEXTURE_ENV,GL_COMBINE_ALPHA,GL_REPLACE);
      glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE0_ALPHA,GL_TEXTURE);
      glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND0_ALPHA,GL_SRC_ALPHA);
    end; }
  end;

  {if FHasBumpmap = True then
  begin
    glActiveTexture(GL_TEXTURE1);
    glenable(GL_TEXTURE_2D);
    glbindtexture(GL_TEXTURE_2D, FTexId);

    // RGB
    glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_COMBINE);
    glTexEnvf(GL_TEXTURE_ENV,GL_COMBINE_RGB,GL_REPLACE);
    glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE0_RGB,GL_PREVIOUS);
    glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND0_RGB,GL_SRC_COLOR);

    // alpha
    glTexEnvf(GL_TEXTURE_ENV,GL_COMBINE_ALPHA,GL_ADD_SIGNED);
    glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE0_ALPHA,GL_TEXTURE);
    glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND0_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glTexEnvf(GL_TEXTURE_ENV,GL_SOURCE1_ALPHA,GL_PREVIOUS);
    glTexEnvf(GL_TEXTURE_ENV,GL_OPERAND1_ALPHA,GL_SRC_ALPHA);
  end;  }

  //Two Sided Materials
  if FTwoSided then
    glDisable(GL_CULL_FACE)
  else
    glEnable(GL_CULL_FACE);
end;

procedure EmptyFunc(var red, green, blue, alpha: Byte; Advanced: Pointer);
begin
  Red   := 255;
  Green := 255;
  Blue  := 255;
end;

procedure T3DSMaterial.updatetexture;
begin
if FHasTexturemap then
 begin
 FDetailTex := 0;
 {$ifdef release}
{  if Pos('detail', Name) > 0 then
   LoadFromResource(FDetailTex, 'DETAIL', 'JPG');
  LoadFromResource(FTexture, Copy(FFileName, 1, Length(FFileName)-4), Copy(FFileName, Length(FFileName)-2, 3));}
 {$else}
{  if not DontLoadTextures then
   LoadTexture(lowercase(TAll3DSMesh(self.owner).FTexturePath + FFileName), FTexture);
  if Pos('detail', Name) > 0 then
   LoadTexture(lowercase(TAll3DSMesh(self.owner).FTexturePath + 'detail.jpg'), FDetailTex);}
 {$endif}
//  Format := LastFormat;
 end;
end;

//T3DSMesh routines...
destructor T3DSMesh.Destroy;
begin
  FName := '';
  SetLength(FVertex, 0);
  SetLength(FMatName, 0);
  SetLength(FVnormal, 0);
  SetLength(FMapping, 0);
  SetLength(FMatId, 0);
  SetLength(FIndices, 0);
  Setlength(FNormalIndices, 0);
  SetLength(FBoneId, 0);
  FVertex := nil;
  FMatName := nil;
  FVnormal := nil;
  FMapping := nil;
  FMatId := nil;
  FIndices := nil;
  FNormalIndices := nil;
  FBoneId := nil;
  inherited Destroy;
end;

procedure T3dsMesh.RenderBoundBox;
begin
  glBegin(GL_LINE_LOOP);
    glVertex3f(minimum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, maximum.y, minimum.z);
    glVertex3f(minimum.x, maximum.y, minimum.z);
  glEnd;
  glBegin(GL_LINE_LOOP);
    glVertex3f(minimum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, maximum.y, maximum.z);
    glVertex3f(minimum.x, maximum.y, maximum.z);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(minimum.x, minimum.y, minimum.z);
    glVertex3f(minimum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, minimum.y, minimum.z);
    glVertex3f(maximum.x, minimum.y, maximum.z);
    glVertex3f(maximum.x, maximum.y, minimum.z);
    glVertex3f(maximum.x, maximum.y, maximum.z);
    glVertex3f(minimum.x, maximum.y, minimum.z);
    glVertex3f(minimum.x, maximum.y, maximum.z);
  glEnd;
end;

procedure T3DSMesh.Calculatesize;
var
  {m,} f: integer;
  x, y, z: single;
begin
  //am i allowed to assume the the first vertex is a minimum and/or maximum?
  FMinimum.x := FVertex[FIndices[0]].x;
  FMinimum.y := FVertex[FIndices[0]].y;
  FMinimum.z := FVertex[FIndices[0]].z;
  FMaximum.x := FVertex[FIndices[0]].x;
  FMaximum.y := FVertex[FIndices[0]].y;
  FMaximum.z := FVertex[FIndices[0]].z;
  for f := 0 to FNumIndices - 1 do
    begin
      x := FVertex[FIndices[f]].x;
      y := FVertex[FIndices[f]].y;
      z := FVertex[FIndices[f]].z;
      if x < FMinimum.x then FMinimum.x := x;
      if y < FMinimum.y then FMinimum.y := y;
      if z < FMinimum.z then FMinimum.z := z;
      if x > FMaximum.x then FMaximum.x := x;
      if y > FMaximum.y then FMaximum.y := y;
      if z > FMaximum.z then FMaximum.z := z;
    end;
end;

procedure T3DSMesh.AddFace(v1, v2, v3: T3DPoint; matname: string);
begin
  //first add vertices
  FNumVertex := FNumVertex + 3;
  SetLength(FVertex, FNumVertex);
  //increase the number of indices
  FNumIndices := FNumIndices + 3;
  SetLength(FIndices, FNumIndices);

  //add the data
  FIndices[FNumIndices - 3] := FNumIndices - 3;
  FIndices[FNumIndices - 2] := FNumIndices - 2;
  FIndices[FNumIndices - 1] := FNumIndices - 1;

  FVertex[FIndices[FNumIndices - 3]].x := v1.x;
  FVertex[FIndices[FNumIndices - 3]].y := v1.y;
  FVertex[FIndices[FNumIndices - 3]].z := v1.z;

  FVertex[FIndices[FNumIndices - 2]].x := v2.x;
  FVertex[FIndices[FNumIndices - 2]].y := v2.y;
  FVertex[FIndices[FNumIndices - 2]].z := v2.z;

  FVertex[FIndices[FNumIndices - 1]].x := v3.x;
  FVertex[FIndices[FNumIndices - 1]].y := v3.y;
  FVertex[FIndices[FNumIndices - 1]].z := v3.z;

  //add the material
  SetLength(FMatName, 1);
  FMatName[0] := matname;
  SetLength(FMatID, (FNumIndices div 3));
  FMatId[(FNumIndices div 3) - 1] := TAll3DSMesh(owner).GetMaterialIdByName(MatName);
  //add mapping...
  SetLength(FMapping, FNumIndices);
  FMapping[FIndices[FNumIndices - 3]].tu := v1.x;
  FMapping[FIndices[FNumIndices - 3]].tv := v1.y;
  FMapping[FIndices[FNumIndices - 2]].tu := v2.x;
  FMapping[FIndices[FNumIndices - 2]].tv := v2.y;
  FMapping[FIndices[FNumIndices - 1]].tu := v3.x;
  FMapping[FIndices[FNumIndices - 1]].tv := v3.y;
end;

function T3DSMesh.GetVertex(Index: integer): T3DPoint;
begin
  Result := FVertex[index];
end;

function T3DSMesh.GetNormal(Index: integer): T3DPoint;
begin
  Result := FVnormal[index];
end;

function T3DSMesh.GetMapping(Index: integer): TMap;
begin
  Result := FMapping[index];
end;

procedure T3DSMesh.SetVertex(Index: integer; Value: T3DPoint);
begin
  FVertex[index]:=Value;
end;

procedure T3DSMesh.SetMapping(Index: integer; Value: TMap);
begin
  FMapping[index]:=Value;
end;

function T3DSMesh.GetMatID(Index: integer): word;
begin
if (Index > High(FMatID)) or (Index < Low(FMatID)) then
 raise Exception.Create('T3DSMesh->GetMadID->"'+Name+'"->Index out of bounds!');
  Result := FMatid[index];
end;

function T3DSMesh.GetFace(Index: integer): word;
begin
  Result := FIndices[index];
end;

procedure T3DSMesh.BuildDisplayList;
begin
FDisplaylist := glGenLists(1);
glNewList(FDisplaylist, GL_COMPILE);
 Render;
glEndList;
end;

procedure T3DSMesh.render;
var
  f,v: integer;
  matid: integer;
begin
if fdisplaylist <> 0 then
 glcalllist(fdisplaylist)
else
 begin
 matid := 999; //hmm since material now starts with 0 this has to be higer...
 if FNumIndices > 0 then
  begin
  f := 0;
  while f < FNumIndices - 1 do
   begin
   //begin setting material
   if FMatId[f div 3] <> matid then
    begin
    matid := FMatId[f div 3];
    TAll3DSMesh(owner).material[matid].apply;
    end;
   //render the face
   glBegin(GL_TRIANGLES);
   glNormal3f(FaceNormal[f div 3].x, FaceNormal[f div 3].y, FaceNormal[f div 3].z);
    for v := 0 to 2 do
     begin
    // glNormal3fv(@FVnormal[FNormalIndices[f+v]]);
     glMultiTexCoord2f(GL_TEXTURE0,FMapping[FIndices[f+v]].tu, FMapping[FIndices[f+v]].tv);
     glVertex3fv(@FVertex[FIndices[f + v]].x);
     end;
    glEnd;
    f := f + 3;
   end;
  end;
 end;
end;

end.
