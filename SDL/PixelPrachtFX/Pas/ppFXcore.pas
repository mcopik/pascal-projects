{
***************************[ PixelPrachtFX - Core ]*****************************

Package:           PixelprachtFX

Unit:              ppFXcore

Version:           1.1 (05.06.2003)

Description:       This unit contains the core types, constants and methods of
                   the PixelPrachtFX System.

                   To implement an effect you'll inherit from the class FXSystem
                   and alter the methods Init() and Advance(). You'll add Groups
                   of Particles, set RenderInstructions and modify the Groups
                   (adding particles, moving them...) by using the methods you
                   find in the unit ppFXlib.

Change-Log:        v1.1 (03.03.2003)
                   - BoundingBox added to TFxSystem to use with Occlusion Culling
                   - Group-Textures support tiled Textures.
                   - Fading of Particles changed.
                   - Improved Z-Sorting (preallocating the mem)



  Happy Coding
        Lithander (lithander@gmx.de)



************************[Copyright (c) 2003 Thomas Jahn]************************
}

unit ppFXcore;

interface

uses gl, math;


//** TYPES **

type

TFxVector = record
  X     : single;
  Y     : single;
  Z     : single;
end;

TFxColor = record
  Red     : single;
  Green   : single;
  Blue    : single;
end;


TFxBoundingBox = record
  Min   : TFxVector;
  Max   : TFxVector;
end;

//Bottom-Left Edge = 0,0 Top-Right Edge = 1,1 (ogl-coordinate-system)
TFxTexTile = record
  MinU  : single;
  MinV  : single;
  MaxU  : single;
  MaxV  : single;
end;

//Size = 15*Single + 2 * Integer + boolean = 68 Byte
TFxParticle = record
  Position : TFxVector;
  Velocity : TFxVector;
  Density  : single; //1 standard
  Mass     : single;
  Size     : single;
  Spin     : single; //rotation per sec. no effect if FX_(Tex)Spark or FX_Points
  Rotation : single; //rotation in deg.
  Color    : TFxColor;
  Sat      : single;        //[0..1]
  LiveSpan : integer;
  Age      : integer;
  Junk     : boolean;
end;

pFxParticle = ^TFxParticle;

TFxGsMode = (FX_POINT, FX_QUAD, FX_SPARK, FX_TEXQUAD, FX_TEXSPARK, FX_SPRITE);//FX_SPRITE = Textured Quad with AlphaMask

TFxGsBlending = (FX_OPAQUE, FX_GLOW, FX_TRANSPARENT, FX_MASKED, FX_LIGHT);

TFxGsFading = (FX_NO_FADE, FX_FADE_IN, FX_FADE_OUT, FX_FADE_IN_AND_OUT);

TFxGroupSettings = record
  Mode        : TFxGsMode;
  Blending    : TFxGsBlending;
  Texture     : GluInt; //used only for TEXQUAD, TEXSPARK or SPRITE
  TexTile     : TFxTexTile;
  Elongation  : Single; //used only for SPARK or TEXSPARK.
  FadeInStop  : Single; //ranges from 0..1*
  FadeOutStart: Single; //ranges from 0..1*
  ZSort       : Boolean;
end;

{*) How to use FadeIn and FadeOut...
 The Particles Lifetime is scaled down to 1.
 From 0 to FadeInStop the Particle fades in.
 From FadeInStop to FadeOutStart the Particle stays constant.
 From FadeOutStart to 1 the Particle fades out.
 Thus both need to range between 0..1 while FadeOutStart needs to be bigger than
 FadeInStop.}

//** CONSTANTS **

const

FX_STD_TEMPLATE : TFxParticle = (Position : (X : 0; Y : 0; Z : 0;);
                                 Velocity : (X : 0; Y : 0; Z : 0;);
                                 Density  : 1;
                                 Mass     : 1;
                                 Size     : 0.1;
                                 Spin     : 0;
                                 Rotation : 0;
                                 Color    : (Red: 1; Green : 1; Blue : 1;);
                                 Sat      : 1;
                                 LiveSpan : 1000;
                                 Age      : 0;
                                 Junk     : false;);


FX_EMPTY_BOUNDINGBOX : TFxBoundingBox = (Min : (X : 0; Y : 0; Z : 0;);
                                         Max : (X : 0; Y : 0; Z : 0;));

                                         
//Particle-Attribute-Bitmask
FX_POSITION = $0001;
FX_VELOCITY = $0002;
FX_COLOR    = $0004;
FX_MASS     = $0008;
FX_SIZE     = $0010;
FX_DENSITY  = $0020;
FX_SPIN     = $0040;
FX_ROTATION = $0080;
FX_SAT      = $0100;
FX_LIVESPAN = $0200;
//Vector-Parts
FX_POS_X    = $0400;
FX_POS_Y    = $0800;
FX_POS_Z    = $1000;
FX_VEL_X    = $2000;
FX_VEL_Y    = $4000;
FX_VEL_Z    = $8000;

FX_ALL      = $03FF; //All Attrbutes


//** METHODS **

//Billboarding

procedure fxBillboardBegin;

procedure fxBillboardEnd;

procedure fxBillboard;

//Creation of Record-Types

function fxVector(const X, Y, Z : single) : TFxVector;

function fxColor(const Red, Green, Blue : single) : TFxColor;

function fxTexTile(MinU, MinV, MaxU, MaxV : single) : TFxTexTile;

function fxParticle(const Position, Velocity : TFxVector;
                    const Density, Mass, Size, Spin, Rotation, Sat : single;
                    const Color : TFxColor;
                    const LiveSpan, Age : integer) : TFxParticle;

//** CLASSES **

type

TFxGroup = class(TObject)
protected
  FJunkCount : word;
  JunkIndices : array of word;
  FTransparency : single; //can be used to blend whole groups in and out. 0 = invisible. 1 = fully visible
  FHighestActive : integer;
  FCalcBoundingBox :Boolean;
  FBoundingBox : TFxBoundingBox;
  function getSize : word; //how many Particle-Slots?
  function getActiveCount : word;
  procedure setTransparency(transp : single);
public
  Particles : array of TFXParticle; //don't add Particles directly... use Add() instead
  MinimizeSpread : boolean; //advance is slightly faster - deleteing is slower due to a loop through all particles
  CalcBoundingBox : boolean;
  Visible : boolean;
  Update : boolean;
  Settings  : TFxGroupSettings; //Rendering, additional common Particle attributes etc

  property BoundingBox : TFxBoundingBox read FBoundingBox;
  property Size : word read GetSize;
  property ActiveCount : word read GetActiveCount;
  property JunkCount   : word read FJunkCount;
  property HighestActive : integer read FHighestActive; //Update and Render only Particles up too HighestActive
  property Transparency : single read FTransparency write setTransparency;

  constructor create(aSize : word);
  function add(aParticle : TFxParticle) : integer; overload;//returns Index of created Particle
  procedure add(aParticle: TFxParticle; const count : integer); overload;
  procedure delete(aIndex : integer);
  procedure advance(aTime : integer); // Updating all Particles up to HeighestActive (time in ms)
  procedure clear;
end;


TFxDepthEntry = record
  GroupIdx : integer;
  Particle : pFxParticle;
  Z : single;
end;


TFxDepthList = class(TObject)
protected
  FCount : integer;
  FSize : word; //Slots
  Temp  : array of TFxDepthEntry;
  procedure setSize(aSize : word);
  procedure mergesort(const low, high : integer);
public
  Entrys : array of TFxDepthEntry;
  property Count : integer read FCount;
  property Size : word read FSize write setSize;

  constructor Create(aSize : word);
  procedure add(aGroupIdx : integer; aParticle : pFxParticle);
  procedure sort;
  procedure calcZ;
  procedure clear;
end;


TFxTemplateEntry = record
  Template : TFxParticle;
  Name  : string;
end;


TFxTemplateLib = class(TObject)
protected
  FTemplCount : integer;
  function getTemplateByName(aName : string) : pFxParticle;
public
  Templates : array of TFxTemplateEntry; //don't add Particles directly... use Add() instead
  property TemplatesByName[Name : string] : pFxParticle read GetTemplateByName; default;
  property TemplCount : integer read FTemplCount;

  constructor create;
  function add(aName : string) : integer; //returns Index
  procedure delete(aName : string); overload;
  procedure delete(index : integer); overload;
  procedure clear;
end;


TFxGroupEntry = record
  Group : TFxGroup;
  Name  : string;
end;


TFxGroupLib = class(TObject)
protected
  FGroups : array of TFxGroupEntry;
  FGroupCount : integer;
  FCalcBoundingBox :Boolean;
  function getBoundingBox : TFxBoundingBox;
  procedure setCalcBoundingBox(arg : Boolean);
  function getGroup(Index : Integer) : TFxGroup;
  function getGroupByName(aName : string) : TFxGroup;
public
  property Groups[Index: Integer] : TFxGroup read GetGroup;
  property GroupsByName[Name: String] : TFxGroup read GetGroupByName; default; //TEMP
  property GroupCount : integer read FGroupCount;
  property CalcBoundingBox : boolean read FCalcBoundingBox write setCalcBoundingBox;
  property BoundingBox : TFxBoundingBox read getBoundingBox;

  constructor create;
  destructor destroy; override;
  function add(aName : string; aSize : word) : integer; //returns Index
  procedure delete(aName : string); overload;
  procedure delete(index : integer); overload;
  procedure clear;
end;


TFxSystem = class(TObject)
private
  DepthList : TFxDepthList;
protected
  Groups : TFxGroupLib;
  Templates : TFxTemplateLib;
  FCalcBoundingBox :Boolean;
  FAge : LongInt;
  function getBoundingBox : TFxBoundingBox;
  procedure setCalcBoundingBox(arg : Boolean);
  procedure init; virtual; //Override when Implementing FX!
public
  property CalcBoundingBox : boolean read FCalcBoundingBox write setCalcBoundingBox;
  property BoundingBox : TFxBoundingBox read getBoundingBox;
  property Age : longInt read FAge;

  constructor create;
  destructor destroy; override;
  //Interface
  procedure render; virtual;
  procedure restart; virtual;
  procedure advance(aTime : integer); virtual; //Override when Implementing FX!
end;


//#############################################################################

implementation

//#############################################################################


uses ppFXrender;

//**************************** FXBillBoard *************************************

procedure fxBillboard;
var x,y : byte;
    Matrix : array[0..15] of single;
begin
 glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
 for x := 0 to 2 do
  for y := 0 to 2 do
   if x=y then Matrix[x*4+y] := 1 else Matrix[x*4+y] := 0;
 glLoadMatrixf(@Matrix);
end;


procedure fxBillboardBegin;
var x,y : byte;
    Matrix : array[0..15] of single;
begin
//save original Matrix
 glPushMatrix;
 glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
 for x := 0 to 2 do
  for y := 0 to 2 do
   if x=y then Matrix[x*4+y] := 1 else Matrix[x*4+y] := 0;
 glLoadMatrixf(@Matrix);
end;


procedure fxBillboardEnd;
begin
  //restore original Matrix
  glPopMatrix;
end;



//************************ Type-Data-Creation***********************************

function fxVector(const X, Y, Z : single) : TFxVector;
begin
  result.X := X;
  result.Y := Y;
  result.Z := Z;
end;

function fxColor(const Red, Green, Blue : single) : TFxColor;
begin
  result.Red := Red;
  result.Green := Green;
  result.Blue := Blue;
end;

function fxTexTile(MinU, MinV, MaxU, MaxV : single) : TFxTexTile;
begin
  result.MinU := MinU;
  result.MaxU := MaxU;
  result.MinV := MinV;
  result.MaxV := MaxV;
end;

function fxParticle(const Position, Velocity : TFxVector; const Density, Mass, Size, Spin, Rotation, Sat : single;
                    const Color : TFxColor; const LiveSpan, Age : integer) : TFxParticle;
begin
  result.Position := Position;
  result.Velocity := Velocity;
  result.Density  := Density;
  result.Mass     := Mass;
  result.Size     := Size;
  result.Spin     := Spin;
  result.Rotation := Rotation;
  result.Color    := Color;
  result.Sat      := Sat;
  result.LiveSpan := LiveSpan;
  result.Age      := Age;
  result.Junk     := false;
end;

//****************************** FXGroup ***************************************

function TFxGroup.getActiveCount;
begin
  result := length(Particles) - FJunkCount;
end;


function TFxGroup.getSize;
begin
  result := length(Particles);
end;


procedure TFxGroup.setTransparency(transp : single);
begin
  if (transp >= 0) and (transp <= 1) then FTransparency := transp;
end;

constructor TFxGroup.create(aSize : word);
var i : integer;
begin
  SetLength(Particles, aSize);
  SetLength(JunkIndices, aSize);
  for i := 0 to aSize-1 do begin
    JunkIndices[i]:= aSize - i - 1;
    Particles[i].Junk := true;
  end;
  FJunkCount := aSize;
  FHighestActive := -1;
  FTransparency := 1;
  Visible := true;
  Update := true;
  MinimizeSpread := false;
  //Set default RI
  with Settings do begin
    Mode         := FX_POINT;
    Blending     := FX_OPAQUE;
    Texture      := 0;
    TexTile      := fxTexTile(0,0,1,1);
    Elongation   := 2;
    FadeInStop   := 0;
    FadeOutStart := 1;
    ZSort        := false;
  end;
end;


function TFxGroup.add(aParticle : TFxParticle) : integer;
begin
  result := 0;
  if FJunkCount = 0 then exit;  //No Space for another Particle
  Particles[JunkIndices[FJunkCount-1]] := aParticle;
  Particles[JunkIndices[FJunkCount-1]].Junk := false;
  result := JunkIndices[FJunkCount-1];
  if FHighestActive < JunkIndices[FJunkCount-1] then FHighestActive := JunkIndices[FJunkCount-1];
  dec(FJunkCount);
end;


procedure TFxGroup.add(aParticle: TFxParticle; const count : integer);
var i : integer;
begin
  for i := 1 to count do begin
    if FJunkCount = 0 then exit; //No Space for further Particles!
    Particles[JunkIndices[FJunkCount-1]] := aParticle;
    Particles[JunkIndices[FJunkCount-1]].Junk := false;
    if FHighestActive < JunkIndices[FJunkCount-1] then FHighestActive := JunkIndices[FJunkCount-1];
    dec(FJunkCount);
  end;
end;


procedure TFxGroup.delete(aIndex : integer);
var index, temp : word;
begin
  if Particles[aIndex].Junk then exit; //Exit because Particle is allready deleted

  Particles[aIndex].Junk := True; //Kill Particle
  //Add to JunkIndices
  JunkIndices[FJunkCount] := aIndex;

  if MinimizeSpread then begin
    //OPTIMIZE - aIndex is sorted into JunkIndices. Smallest Indices are on top.
    for index := (FJunkCount) downto (FJunkCount - (FHighestActive - ActiveCount)) do
     if JunkIndices[index] > JunkIndices[index-1] then begin
       //swap
       temp := JunkIndices[index];
       JunkIndices[index] := JunkIndices[index-1];
       JunkIndices[index-1] := temp;
    end;
  end;

  inc(FJunkCount);
end;


procedure TFxGroup.advance(aTime : integer);
var i, hiactv : integer;
    maxSize : single;
    TimeBase : single;
begin
  TimeBase := 0.001 * aTime;
  if CalcBoundingBox then begin
    FBoundingBox := FX_EMPTY_BOUNDINGBOX;
    maxSize := 0;
  end;
  //if not Update then exit;
  hiactv := 0;
  for i := 0 to FHighestActive do
   with Particles[i] do if not Junk then begin
     //Update Position
     Position.X := Position.X + Velocity.X * TimeBase;
     Position.Y := Position.Y + Velocity.Y * TimeBase;
     Position.Z := Position.Z + Velocity.Z * TimeBase;
     //Rotate
     Rotation   := Rotation - Spin * TimeBase;
     if Rotation > 360 then Rotation := Rotation - 360;
     if Rotation < 0 then Rotation := Rotation + 360;
     //Update LiveSpan and Age
     LiveSpan   := LiveSpan - aTime;
     Age := Age + aTime;
     if LiveSpan > 0 then hiactv := i else Delete(i);

     //Calculate the BoundingBox
     if CalcBoundingBox then begin
       if maxSize < Size then maxSize := Size;
       if Position.X > FBoundingBox.Max.X then FBoundingBox.Max.X := Position.X;
       if Position.Y > FBoundingBox.Max.Y then FBoundingBox.Max.Y := Position.Y;
       if Position.Z > FBoundingBox.Max.Z then FBoundingBox.Max.Z := Position.Z;
       if Position.X < FBoundingBox.Min.X then FBoundingBox.Min.X := Position.X;
       if Position.Y < FBoundingBox.Min.Y then FBoundingBox.Min.Y := Position.Y;
       if Position.Z < FBoundingBox.Min.Z then FBoundingBox.Min.Z := Position.Z;
     end;
  end;

  if CalcBoundingBox then with FBoundingBox do begin
    //Enlarge BB so the Billboard-Expression of Particles are inside. (Won't work with FX_POINT and FX_SPARK)
    Max.X := Max.X + maxSize;
    Max.Y := Max.Y + maxSize;
    Max.Z := Max.Z + maxSize;
    Min.X := Min.X - maxSize;
    Min.Y := Min.Y - maxSize;
    Min.Z := Min.Z - maxSize;
  end;

  FHighestActive := hiactv;
end;


procedure TFxGroup.clear;
var i : word;
begin
  for i := 0 to Size-1 do begin
    Particles[i].Junk := true;
    JunkIndices[i]:= Size - i;
  end;
end;

//***************************** FXDepthList ************************************

constructor TFxDepthList.create(aSize : word);
begin
  FSize := aSize;
  FCount := 0;
  SetLength(Entrys,aSize);
  SetLength(Temp,aSize);
end;

Procedure TFxDepthList.setSize(aSize : word);
begin
  FSize := aSize;
  FCount := 0;
  SetLength(Entrys,FSize);
  SetLength(Temp,FSize);
end;


procedure TFxDepthList.add(aGroupIdx : integer; aParticle : pFxParticle);
begin
  if FCount >= FSize then exit;
  inc(FCount);
  Entrys[Count-1].GroupIdx := aGroupIdx;
  Entrys[Count-1].Particle := aParticle;
end;


procedure TFxDepthList.mergesort(const low, high : integer);
var pivot   : integer;
    length  : integer;
    i,pos1,pos2 : integer;
begin
  if low = high then exit;
  pivot := trunc((low+high) shr 1);
  length := high-low+1;
  mergesort(low,pivot);
  mergesort(pivot+1, high);
  for i := 0 to length-1 do temp[i] := Entrys[low+i];
  //Merge
  pos1 := 0;
  pos2 := pivot-low+1;
  for i := 0 to length-1 do if pos2 < length then begin
    if pos1 <= pivot-low then begin
      if temp[pos1].z > temp[pos2].z then begin
        Entrys[low+i] := temp[pos2];
        inc(pos2)
      end else begin
        Entrys[low+i] := temp[pos1];
        inc(pos1);
      end;
    end else begin //only upper Entrys left
      Entrys[i+low] := temp[pos2];
      inc(pos2);
    end;
  end else begin //only superior Entrys left
    Entrys[i+low] := temp[pos1];
    inc(pos1);
  end;
end;


procedure TFxDepthList.sort;
begin
  mergesort(0, Count-1);
end;


procedure TFxDepthList.calcZ;
var Matrix : array [0..15] of single;
    i      : integer;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
  for i := 0 to Count-1 do
  with Entrys[i].Particle^.Position do
  Entrys[i].z := x * Matrix[2] + y * Matrix[6] + z * Matrix[10] + Matrix[14];
end;


procedure TFxDepthList.clear;
begin
  FCount := 0;
end;

//*************************** FXTemplateList ***********************************

function TFXTemplateLib.getTemplateByName(aName : string) : pFxParticle;
var i : integer;
begin
  result := nil;
  for i := 0 to TemplCount -1 do if Templates[i].Name = aName then begin
    result := @Templates[i].Template;
    exit;
  end;
  //Exception - Template not found!
end;


constructor TFXTemplateLib.create;
begin
  FTemplCount := 0;
  setLength(Templates, TemplCount);
end;


function TFXTemplateLib.add(aName : string) : integer; //returns Index
begin
  inc(FTemplCount);
  SetLength(Templates, TemplCount);
  Templates[TemplCount - 1].Name := aName;
  Templates[TemplCount - 1].Template := FX_STD_TEMPLATE;
  result := TemplCount - 1;
end;


procedure TFXTemplateLib.delete(aName : string);
var i, j : integer;
begin
  for i := 0 to TemplCount -1 do
   if Templates[i].Name = aName then begin
     {Delete}
     for j := i+1 to TemplCount-1 do Templates[j-1] := Templates[j];
     dec(FTemplCount);
     setLength(Templates, TemplCount);
  end;
end;


procedure TFXTemplateLib.delete(index : integer);
var i : integer;
begin
  if index >= TemplCount then exit;
  for i := index+1 to TemplCount-1 do Templates[i-1] := Templates[i];
  dec(FTemplCount);
  setLength(Templates, TemplCount);
end;


procedure TFXTemplateLib.clear;
begin
  FTemplCount := 0;
  setLength(Templates, TemplCount);
end;


//***************************** FXGroupLib ************************************

function TFxGroupLib.getGroup(Index : Integer) : TFxGroup;
begin
  if Index >= FGroupCount then begin
    result := nil;
    //Exception - Group not found!
  end else result := FGroups[Index].Group;
end;


function TFxGroupLib.getGroupByName(aName : String) : TFxGroup;
var i : integer;
begin
  result := nil;
  for i := 0 to GroupCount -1 do if FGroups[i].Name = aName then begin
    result := FGroups[i].Group;
    exit;
  end;
  //Exception - Group not found!
end;


procedure TFxGroupLib.setCalcBoundingBox(arg : boolean);
var i : integer;
begin
  FCalcBoundingBox := arg;
  //update Groups
  for i := 0 to GroupCount - 1 do FGroups[i].Group.CalcBoundingBox := arg;
end;


function TFxGroupLib.getBoundingBox : TFxBoundingBox;
var i : integer;
    BB: TFxBoundingBox;
begin
  BB := FX_EMPTY_BOUNDINGBOX;
  //Is BoundingBox enabled?
  if not CalcBoundingBox then begin
    result := BB;
    exit;
  end;

  //Merge Groups BoundingBoxes into BB
  for i := 0 to GroupCount - 1 do with FGroups[i].Group.BoundingBox do begin
    if Max.X > BB.Max.X then BB.Max.X := Max.X;
    if Max.Y > BB.Max.Y then BB.Max.Y := Max.Y;
    if Max.Z > BB.Max.Z then BB.Max.Z := Max.Z;

    if Min.X < BB.Min.X then BB.Min.X := Min.X;
    if Min.Y < BB.Min.Y then BB.Min.Y := Min.Y;
    if Min.Z < BB.Min.Z then BB.Min.Z := Min.Z;
  end;
  result := BB;
end;


constructor TFxGroupLib.create;
begin
  FGroupCount := 0;
  FCalcBoundingBox := false;
  setLength(FGroups, GroupCount);
end;


destructor TFxGroupLib.destroy;
var i : integer;
begin
  for i := 0 to GroupCount - 1 do FGroups[i].Group.Free;
end;


function TFxGroupLib.add(aName : string; aSize : word) : integer; //returns Index
begin
  inc(FGroupCount);
  setLength(FGroups, GroupCount);
  FGroups[GroupCount - 1].Name := aName;
  FGroups[GroupCount - 1].Group := TFxGroup.Create(aSize);
  FGroups[GroupCount - 1].Group.CalcBoundingBox := FCalcBoundingBox;
  result := GroupCount -1;
end;


procedure TFxGroupLib.delete(aName : string);
var i, j : integer;
begin
  for i := 0 to GroupCount -1 do
   if FGroups[i].Name = aName then begin
     {Delete}
     FGroups[i].Group.Free;
     for j := i+1 to GroupCount-1 do FGroups[j-1] := FGroups[j];
     dec(FGroupCount);
     setLength(FGroups, GroupCount);
   end;
end;


procedure TFxGroupLib.delete(index : integer);
var j : integer;
begin
  if index >= FGroupCount then exit;
  FGroups[index].Group.Free;
  for j := index+1 to GroupCount-1 do FGroups[j-1] := FGroups[j];
  dec(FGroupCount);
  setLength(FGroups, GroupCount);
end;


procedure TFxGroupLib.clear;
var i : integer;
begin
   for i := 0 to GroupCount - 1 do FGroups[i].Group.Free;
   FGroupCount := 0;
   SetLength(FGroups, GroupCount);
end;

//****************************** FXSystem **************************************

constructor TFxSystem.create;
begin
  Groups := TFxGroupLib.create;
  Templates := TFxTemplateLib.create;
  DepthList := TFxDepthList.create(0);
  FCalcBoundingBox := false;
  Init;
end;


destructor TFxSystem.destroy;
begin
  Groups.free;
  Templates.free;
  DepthList.free;
end;


procedure TFxSystem.setCalcBoundingBox(arg : boolean);
begin
  FCalcBoundingBox := arg;
  Groups.CalcBoundingBox := arg;
end;


function TFxSystem.getBoundingBox : TFxBoundingBox;
begin
  result := Groups.BoundingBox;
end;


procedure TFxSystem.render;
var i,j       : word;
    GS        : TFxGroupSettings;
    GIdx      : integer;
    Transp    : single;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  //PREPARE DEPTHLIST
  DepthList.clear;
  j := 0;
  for i := 0 to Groups.GroupCount-1 do if Groups.Groups[i].Settings.ZSort then inc(j,Groups.Groups[i].ActiveCount);
  DepthList.Size := j;
  //RENDER
  for i := 0 to Groups.GroupCount-1 do
  with Groups.Groups[i] do if (Visible AND (ActiveCount > 0)) then begin
    //RENDER UNSORTED PARTICLES
    if not Settings.zSort then begin
      Transp := Groups.Groups[i].Transparency;
      applyRenderSettings(Settings);
      for j := 0 to HighestActive do if not Particles[j].Junk then begin
        case Settings.Mode of
          FX_TEXQUAD : FXRenderTexQuad(Particles[j], Settings, Transp);
          FX_SPRITE  : FxRenderSprite(Particles[j], Settings, Transp); //needs special blending modes
          FX_QUAD    : FXRenderQuad(Particles[j],  Settings, Transp);
          FX_POINT   : FXRenderPoint(Particles[j], Settings, Transp);
          FX_SPARK   : FXRenderSpark(Particles[j], Settings, Transp);
          FX_TEXSPARK: FXRenderTexSpark(Particles[j], Settings, Transp);
        end;
      end;
    //PUT INTO DEPTHLIST
    end else begin
      //Add to DepthList for z-ordered Rendering
      for j := 0 to HighestActive do if not Particles[j].Junk then DepthList.add(i, @Particles[j]);
    end;
  end;

  //RENDER PARTICLES FROM DEPTHLIST
  if DepthList.Count = 0 then begin
    glPopAttrib;
    exit;
  end;
  //Sort DepthList
  DepthList.calcZ;
  DepthList.sort;
  GIdx := -1;
  for i := 0 to Depthlist.Count-1 do begin
    //Set RI if not allready set
    if Depthlist.Entrys[i].GroupIdx <> GIdx then begin
      GIdx := Depthlist.Entrys[i].GroupIdx;
      GS := Groups.Groups[Depthlist.Entrys[i].GroupIdx].Settings;
      //Set Group-Transparency
      Transp := Groups.Groups[Depthlist.Entrys[i].GroupIdx].Transparency;
      applyRenderSettings(GS);
    end;
    //Render
    case GS.Mode of
      FX_TEXQUAD : fxRenderTexQuad(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_SPRITE  : fxRenderSprite(Depthlist.Entrys[i].Particle^, GS, Transp); //needs special blending modes
      FX_QUAD    : fxRenderQuad(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_POINT   : fxRenderPoint(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_SPARK   : fxRenderSpark(Depthlist.Entrys[i].Particle^, GS, Transp);
      FX_TEXSPARK: fxRenderTexSpark(Depthlist.Entrys[i].Particle^, GS, Transp);
    end;
  end;
  glPopAttrib;
end;


procedure TFxSystem.restart;
begin
  FAge := 0;
  Groups.clear;
  Templates.clear;

  init;
end;


procedure TFxSystem.init;
begin

end;


procedure TFxSystem.advance(aTime : integer);
var i : integer;
begin
  inc(FAge, aTime);

  for i := 0 to Groups.GroupCount -1 do with Groups.GetGroup(i) do
   if Update then advance(aTime);
end;


end.
