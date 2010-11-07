{
***************************[ PixelPrachtFX - Method Library ]*****************************

Package:           PixelprachtFX

Unit:              ppFXlib

Version:           1.1 (16.12.2002)

Description:       These methods are used when creating custom Effects.
                   Most methods allow you to change the properties of Particles.
                   For example there are methods for applying physical effects,
                   changing attributes directly but also for generating particles.


  Happy Coding
        Lithander (lithander@gmx.de)


  Change-Log:      v1.1 (02.07.2003)
                   dropped fxBullet
                   added fxMoveDest
                   added fxLineGrav


****************************[Copyright (c) 2003 Thomas Jahn]****************************
}

unit ppFXlib;

interface

uses ppFXcore, sysutils;

const

RND_NUANCES = 100;

GRAV_MIN_RADIUS = 0.2;

//************************
//*** Particle-Actions ***
//************************

//FUNCTIONS
function fxGenRandParticle(const aMinTmpl, aMaxTmpl : TFxParticle) : TFxParticle;
function fxGenInterpParticle(const aTmpl1, aTmpl2 : TFxParticle; const IFactor : single) : TFxParticle;  //IFactor: 0 -> result = aTmpl1; 1 -> result = aTmpl2

//PROCEDURES
procedure fxScale(const Attributes : word; var aParticle : TFxParticle; const scale1, scale2, scale3 : single); overload;
procedure fxScale(const Attributes : word; var aParticle : TFxParticle; const scale : single); overload;
procedure fxScale(const Attributes : word; aGroup : TFxGroup; const scale1, scale2, scale3 : single); overload;
procedure fxScale(const Attributes : word; aGroup : TFxGroup; const scale : single); overload;

procedure fxAddValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxAddValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxSubValues(Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxSubValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxReplaceValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle); overload;
procedure fxReplaceValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle); overload;

procedure fxInterpValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle; IFactor : single); overload;
procedure fxInterpValues(const Attributes : word; aGroup : TFxGroup; const aSrcParticle : TFxParticle; IFactor : single); overload;

procedure fxInvert(const Attributes : word; var aParticle : TFxParticle); overload;
procedure fxInvert(const Attributes : word; aGroup : TFxGroup); overload;

procedure fxSetIntoSphere(var aParticle : TFxParticle; const center : TFxVector; const radius : single); overload; //index of added particle
procedure fxSetIntoSphere(var aParticle : TFxParticle; const cx, cy, cz, radius : single); overload; //index of added particle

procedure fxSetIntoCube(var aParticle : TFxParticle; const center : TFxVector; const size : single); overload;
procedure fxSetIntoCube(var aParticle : TFxParticle; const cx, cy, cz, size : single); overload;

//procedure SetInto...();

procedure fxMoveDest(var aParticle : TFxParticle; const target : TFxVector); overload;
procedure fxMoveDest(var aParticle : TFxParticle; const tx, ty, tz : single); overload;
procedure fxMoveDest(aGroup : TFxGroup; const target : TFxVector); overload;
procedure fxMoveDest(aGroup : TFxGroup; const tx, ty, tz : single); overload;

procedure fxLinearGrav(var aParticle : TFxParticle; const direction : TFxVector; const gravity, time : single); overload;
procedure fxLinearGrav(var aParticle : TFxParticle; const dx, dy, dz : single; const gravity, time : single); overload;
procedure fxLinearGrav(aGroup : TFxGroup; const direction : TFxVector; const gravity, time : single); overload;
procedure fxLinearGrav(aGroup : TFxGroup; const dx, dy, dz : single; const gravity, time : single); overload;

procedure fxCircularGrav(var aParticle : TFxParticle; const point : TFxVector; const gravity, time : single); overload;
procedure fxCircularGrav(var aParticle : TFxParticle; const px, py, pz : single; const gravity, time : single); overload;
procedure fxCircularGrav(aGroup : TFxGroup; const point : TFxVector; const gravity, time : single); overload;
procedure fxCircularGrav(aGroup : TFxGroup; const px, py, pz : single; const gravity, time : single); overload;

procedure fxPointGrav(var aParticle : TFxParticle; const point : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(var aParticle : TFxParticle; const px, py, pz : single; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(aGroup : TFxGroup; const point : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxPointGrav(aGroup : TFxGroup; const px, py, pz : single; const minRadius, attrMass, time : single); overload;

procedure fxLineGrav(var aParticle : TFxParticle; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(var aParticle : TFxParticle; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(aGroup : TFxGroup; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
procedure fxLineGrav(aGroup : TFxGroup; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;

procedure fxParticleGrav(aGroup : TFxGroup; const time : single); overload;
procedure fxParticleGrav(aGroup : TFxGroup; const minradius, time : single); overload;

procedure fxFriction(var aParticle : TFxParticle; const viscosity, time : single); overload;
procedure fxFriction(aGroup : TFxGroup; const viscosity, time : single); overload;

procedure fxSimpleFlow(var aParticle : TFxParticle; const direction : TFxVector; const viscosity, time : single); overload;
procedure fxSimpleFlow(var aParticle : TFxParticle; const dx, dy, dz : single; const viscosity, time : single); overload;
procedure fxSimpleFlow(aGroup : TFxGroup; const direction : TFxVector; const viscosity, time : single); overload;
procedure fxSimpleFlow(aGroup : TFxGroup; const dx, dy, dz : single; const viscosity, time : single); overload;

implementation

//******************************************************************************
//*                             PARTICLE-ACTIONS                               *
//******************************************************************************


function fxGenRandParticle(const aMinTmpl, aMaxTmpl : TFxParticle) : TFxParticle;
begin
  with result do begin
  //Random-Value      Lower Bound                      Range                                 Factor (0..1)
    Position.x := aMinTmpl.Position.x + (aMaxTmpl.Position.x - aMinTmpl.Position.x) * Random(RND_NUANCES)/RND_NUANCES;
    Position.y := aMinTmpl.Position.y + (aMaxTmpl.Position.y - aMinTmpl.Position.y) * Random(RND_NUANCES)/RND_NUANCES;
    Position.z := aMinTmpl.Position.z + (aMaxTmpl.Position.z - aMinTmpl.Position.z) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.x := aMinTmpl.Velocity.x + (aMaxTmpl.Velocity.x - aMinTmpl.Velocity.x) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.y := aMinTmpl.Velocity.y + (aMaxTmpl.Velocity.y - aMinTmpl.Velocity.y) * Random(RND_NUANCES)/RND_NUANCES;
    Velocity.z := aMinTmpl.Velocity.z + (aMaxTmpl.Velocity.z - aMinTmpl.Velocity.z) * Random(RND_NUANCES)/RND_NUANCES;
    Color.Red  := aMinTmpl.Color.Red  + (aMaxTmpl.Color.Red - aMinTmpl.Color.Red)   * Random(RND_NUANCES)/RND_NUANCES;
    Color.Green:= aMinTmpl.Color.Green+ (aMaxTmpl.Color.Green - aMinTmpl.Color.Green)* Random(RND_NUANCES)/RND_NUANCES;
    Color.Blue := aMinTmpl.Color.Blue + (aMaxTmpl.Color.Blue - aMinTmpl.Color.Blue) * Random(RND_NUANCES)/RND_NUANCES;
    Density    := aMinTmpl.Density    + (aMaxTmpl.Density - aMinTmpl.Density)       * Random(RND_NUANCES)/RND_NUANCES;
    Mass       := aMinTmpl.Mass       + (aMaxTmpl.Mass - aMinTmpl.Mass)             * Random(RND_NUANCES)/RND_NUANCES;
    Size       := aMinTmpl.Size       + (aMaxTmpl.Size - aMinTmpl.Size)             * Random(RND_NUANCES)/RND_NUANCES;
    Spin       := aMinTmpl.Spin       + (aMaxTmpl.Spin - aMinTmpl.Spin)             * Random(RND_NUANCES)/RND_NUANCES;
    Rotation   := aMinTmpl.Rotation   + (aMaxTmpl.Rotation - aMinTmpl.Rotation)     * Random(RND_NUANCES)/RND_NUANCES;
    Sat        := aMinTmpl.Sat        + (aMaxTmpl.Sat - aMinTmpl.Sat)               * Random(RND_NUANCES)/RND_NUANCES;
    LiveSpan   := aMinTmpl.LiveSpan   + Round((aMaxTmpl.LiveSpan - aMinTmpl.LiveSpan)*Random(RND_NUANCES)/RND_NUANCES);
    Age        := 0;
    Junk       := false;
  end;
end;


function fxGenInterpParticle(const aTmpl1, aTmpl2 : TFxParticle;const IFactor : single) : TFxParticle;
var k1, k2 : single;
begin
  //Add a Particle and Set it's values...
  k2 := IFactor;
  if k2 > 1 then k2 := 1;
  if k2 < 0 then k2 := 0;
  k1 := 1 - k2;
  with result do begin
  //Random-Value
    Position.x := aTmpl1.Position.x*k1 + aTmpl2.Position.x*k2;
    Position.y := aTmpl1.Position.y*k1 + aTmpl2.Position.y*k2;
    Position.z := aTmpl1.Position.z*k1 + aTmpl2.Position.z*k2;
    Velocity.x := aTmpl1.Velocity.x*k1 + aTmpl2.Velocity.x*k2;
    Velocity.y := aTmpl1.Velocity.y*k1 + aTmpl2.Velocity.y*k2;
    Velocity.z := aTmpl1.Velocity.z*k1 + aTmpl2.Velocity.z*k2;
    Color.Red  := aTmpl1.Color.Red*k1  + aTmpl2.Color.Red*k2;
    Color.Green:= aTmpl1.Color.Green*k1+ aTmpl2.Color.Green*k2;
    Color.Blue := aTmpl1.Color.Blue*k1 + aTmpl2.Color.Blue*k2;
    Density    := aTmpl1.Density*k1    + aTmpl2.Density*k2;
    Mass       := aTmpl1.Mass*k1       + aTmpl2.Mass*k2;
    Size       := aTmpl1.Size*k1       + aTmpl2.Size*k2;
    Spin       := aTmpl1.Spin*k1       + aTmpl2.Spin*k2;
    Rotation   := aTmpl1.Rotation*k1   + aTmpl2.Rotation*k2;
    Sat        := aTmpl1.Sat*k1        + aTmpl2.Sat*k2;
    LiveSpan   := round(aTmpl1.LiveSpan*k1 + aTmpl2.LiveSpan*k2);
    Age        := 0;
    Junk       := false;
  end;
end;


//*** fxScale ***

procedure fxScale(const Attributes : word;var aParticle : TFxParticle;const scale1, scale2, scale3 : single);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.X := Position.X * scale1;
      Position.Y := Position.Y * scale2;
      Position.Z := Position.Z * scale3;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X * scale1;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y * scale2;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z * scale3;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.X := Velocity.X * scale1;
      Velocity.Y := Velocity.Y * scale2;
      Velocity.Z := Velocity.Z * scale3;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X * scale1;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y * scale2;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z * scale3;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * scale1;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * scale2;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * scale3;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * scale1;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * scale1;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * scale1;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * scale1;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density * scale1;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * scale1;
  end;
end;


procedure fxScale(const Attributes : word;var aParticle : TFxParticle;const scale : single);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.X := Position.X * scale;
      Position.Y := Position.Y * scale;
      Position.Z := Position.Z * scale;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X * scale;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y * scale;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z * scale;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.X := Velocity.X * scale;
      Velocity.Y := Velocity.Y * scale;
      Velocity.Z := Velocity.Z * scale;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X * scale;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y * scale;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z * scale;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * scale;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * scale;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * scale;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * scale;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * scale;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * scale;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * scale;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density * scale;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * scale;
  end;
end;


procedure fxScale(const Attributes : word; aGroup : TFxGroup;const scale1, scale2, scale3 : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxScale(Attributes, aGroup.Particles[i], scale1, scale2, scale3);
end;


procedure fxScale(const Attributes : word; aGroup : TFxGroup;const scale : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxScale(Attributes, aGroup.Particles[i], scale);
end;


//*** fxAddValues ***

procedure fxAddValues(const Attributes : word; var aDestParticle : TFxParticle;const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x + aSrcParticle.Position.x;
      Position.y := Position.y + aSrcParticle.Position.y;
      Position.z := Position.z + aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X + aSrcParticle.Position.x;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y + aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z + aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x + aSrcParticle.Velocity.x;
      Velocity.y := Velocity.y + aSrcParticle.Velocity.y;
      Velocity.z := Velocity.z + aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X + aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y + aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z + aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red + aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green + aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue + aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass + aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size + aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin + aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation + aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density + aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat + + aSrcParticle.Sat;
  end;
end;


procedure fxAddValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxAddValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxSubValues

procedure fxSubValues(Attributes : word; var aDestParticle : TFxParticle;const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x - aSrcParticle.Position.x;
      Position.y := Position.y - aSrcParticle.Position.y;
      Position.z := Position.z - aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := Position.X - aSrcParticle.Position.x;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := Position.Y - aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := Position.Z - aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x - aSrcParticle.Velocity.x;
      Velocity.y := Velocity.y - aSrcParticle.Velocity.y;
      Velocity.z := Velocity.z - aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.X - aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.Y - aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.Z - aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red - aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green - aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue - aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass - aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size - aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin - aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation - aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := Density - aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat - aSrcParticle.Sat;
  end;
end;


procedure fxSubValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxSubValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxReplaceValues

procedure fxReplaceValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle);
begin
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := aSrcParticle.Position.x;
      Position.y := aSrcParticle.Position.y;
      Position.z := aSrcParticle.Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := aSrcParticle.Position.X;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := aSrcParticle.Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := aSrcParticle.Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := aSrcParticle.Velocity.x;
      Velocity.y := aSrcParticle.Velocity.y;
      Velocity.z := aSrcParticle.Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := aSrcParticle.Velocity.x;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := aSrcParticle.Velocity.y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := aSrcParticle.Velocity.z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := aSrcParticle.Color.Red;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := aSrcParticle.Color.Green;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := aSrcParticle.Color.Blue;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := aSrcParticle.Mass;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := aSrcParticle.Size;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := aSrcParticle.Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := aSrcParticle.Rotation;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density := aSrcParticle.Density;
    if (Attributes and FX_SAT = FX_SAT) then Sat := aSrcParticle.Sat;
  end;
end;


procedure fxReplaceValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxReplaceValues(Attributes, aGroup.Particles[i], aSrcParticle);
end;


//*** fxInterpValues

procedure fxInterpValues(const Attributes : word; var aDestParticle : TFxParticle; const aSrcParticle : TFxParticle; IFactor : single);
var k1, k2 : single;
begin
  k2 := IFactor;
  if k2 > 1 then k2 := 1;
  if k2 < 0 then k2 := 0;
  k1 := 1 - k2;
  with aDestParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := Position.x * k1 + aSrcParticle.Position.x * k2;
      Position.y := Position.y * k1 + aSrcParticle.Position.y * k2;
      Position.z := Position.z * k1 + aSrcParticle.Position.z * k2;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.x := Position.x * k1 + aSrcParticle.Position.x * k2;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.y := Position.y * k1 + aSrcParticle.Position.y * k2;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.z := Position.z * k1 + aSrcParticle.Position.z * k2;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := Velocity.x * k1 + aSrcParticle.Velocity.x * k2;
      Velocity.y := Velocity.y * k1 + aSrcParticle.Velocity.y * k2;
      Velocity.z := Velocity.z * k1 + aSrcParticle.Velocity.z * k2;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := Velocity.x * k1 + aSrcParticle.Velocity.x * k2;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := Velocity.y * k1 + aSrcParticle.Velocity.y * k2;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := Velocity.z * k1 + aSrcParticle.Velocity.z * k2;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := Color.Red * k1 + aSrcParticle.Color.Red * k2;
      if Color.Red > 1 then Color.Red := 1; if Color.Red < 0 then Color.Red := 0;
      Color.Green := Color.Green * k1 + aSrcParticle.Color.Green * k2;
      if Color.Green > 1 then Color.Green := 1; if Color.Green < 0 then Color.Green := 0;
      Color.Blue := Color.Blue * k1 + aSrcParticle.Color.Blue * k2;
      if Color.Blue > 1 then Color.Blue := 1; if Color.Blue < 0 then Color.Blue := 0;
    end;
    if (Attributes and FX_MASS = FX_MASS) then Mass := Mass * k1 + aSrcParticle.Mass * k2;
    if (Attributes and FX_SIZE = FX_SIZE) then Size := Size * k1 + aSrcParticle.Size * k2;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := Spin * k1 + aSrcParticle.Spin * k2;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := Rotation * k1 + aSrcParticle.Rotation * k2;
    if (Attributes and FX_DENSITY = FX_DENSITY) then Density :=  Density * k1 + aSrcParticle.Density * k2;
    if (Attributes and FX_SAT = FX_SAT) then Sat := Sat * k1 + aSrcParticle.Sat * k2;
  end;
end;


procedure fxInterpValues(const Attributes : word; aGroup : TFxGroup;const aSrcParticle : TFxParticle; IFactor : single);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxInterpValues(Attributes, aGroup.Particles[i], aSrcParticle, IFactor);
end;


//*** fxInvert ***

procedure fxInvert(const Attributes : word; var aParticle : TFxParticle);
begin
  with aParticle do begin
    if (Attributes and FX_POSITION = FX_POSITION) then begin
      Position.x := -Position.x;
      Position.y := -Position.y;
      Position.z := -Position.z;
    end else begin
      if (Attributes and FX_POS_X = FX_POS_X) then Position.X := -Position.X;
      if (Attributes and FX_POS_Y = FX_POS_Y) then Position.Y := -Position.Y;
      if (Attributes and FX_POS_Z = FX_POS_Z) then Position.Z := -Position.Z;
    end;
    if (Attributes and FX_VELOCITY = FX_VELOCITY) then begin
      Velocity.x := -Velocity.x;
      Velocity.y := -Velocity.y;
      Velocity.z := -Velocity.z;
    end else begin
      if (Attributes and FX_VEL_X = FX_VEL_X) then Velocity.X := -Velocity.X;
      if (Attributes and FX_VEL_Y = FX_VEL_Y) then Velocity.Y := -Velocity.Y;
      if (Attributes and FX_VEL_Z = FX_VEL_Z) then Velocity.Z := -Velocity.Z;
    end;
    if (Attributes and FX_COLOR = FX_COLOR) then begin
      Color.Red := 1-Color.Red;
      Color.Green := 1-Color.Green;
      Color.Blue := 1-Color.Blue;
    end;
    if (Attributes and FX_SAT = FX_SAT) then Sat := 1-Sat;
    if (Attributes and FX_SPIN = FX_SPIN) then Spin := -Spin;
    if (Attributes and FX_ROTATION = FX_ROTATION) then Rotation := -Rotation;
  end;
end;


procedure fxInvert(const Attributes : word; aGroup : TFxGroup);
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
   fxInvert(Attributes, aGroup.Particles[i]);
end;


//*** fxSetIntoSphere ***

procedure fxSetIntoSphere(var aParticle : TFxParticle;const center : TFxVector;const radius : single);
begin
  fxSetIntoSphere(aParticle, center.x, center.y, center.z, radius);
end;


procedure fxSetIntoSphere(var aParticle : TFxParticle;const cx, cy, cz, radius : single);
begin
  with aParticle.Position do repeat
   x := cx + random*radius*2-radius;
   y := cy + random*radius*2-radius;
   z := cz + random*radius*2-radius;
  until ((cx-x)*(cx-x)+(cy-y)*(cy-y)+(cz-z)*(cz-z)) < radius; //Radius small enough (Pythagoras)
end;


//*** fxSetIntoCube

procedure fxSetIntoCube(var aParticle : TFxParticle;const center : TFxVector;const size : single);
begin
  fxSetIntoCube(aParticle, center.x, center.y, center.z, size);
end;


procedure fxSetIntoCube(var aParticle : TFxParticle;const cx, cy, cz, size : single);
begin
  aParticle := aParticle;
  with aParticle.Position do begin
   x := cx + random*size*2-size;
   y := cy + random*size*2-size;
   z := cz + random*size*2-size;
  end;
end;

//*** fxMoveDest

procedure fxMoveDest(var aParticle : TFxParticle;const target : TFxVector);   
begin
  fxMoveDest(aParticle, target.x, target.y, target.z);
end;


procedure fxMoveDest(var aParticle : TFxParticle;const tx, ty, tz : single); 
var f : single;
begin

  with aParticle.Velocity do begin
    //calculate f := Velocity / Distance
    f := sqrt(x*x + y*y + z*z) / sqrt(sqr(tx -aParticle.Position.x) +  sqr(ty -aParticle.Position.y) + sqr(tz -aParticle.Position.z));
    x := f * (tx -aParticle.Position.x);
    y := f * (ty -aParticle.Position.y);
    z := f * (tz -aParticle.Position.z);
  end;
end;


procedure fxMoveDest(aGroup : TFxGroup; const target : TFxVector); 
begin
  fxMoveDest(aGroup, target.x, target.y, target.z);
end;


procedure fxMoveDest(aGroup : TFxGroup; const tx, ty, tz : single);
var f : single;
    i   : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i] do begin
    //calculate f := Velocity / Distance
    f := sqrt(sqr(Velocity.x) + sqr(Velocity.y) + sqr(Velocity.z)) / sqrt(sqr(tx -Position.x) +  sqr(ty -Position.y) + sqr(tz -Position.z));
    Velocity.x := f * (tx -Position.x);
    Velocity.y := f * (ty -Position.y);
    Velocity.z := f * (tz -Position.z);
  end;
end;

//*** fxLinearGrav ***

procedure fxLinearGrav(var aParticle : TFxParticle; const direction : TFxVector; const gravity, time : single);
begin
  fxLinearGrav(aParticle, direction.x, direction.y, direction.z, gravity, time);
end;


procedure fxLinearGrav(var aParticle : TFxParticle; const dx, dy, dz : single; const gravity, time : single);
var dlength : single;
begin
  //normalize Vector
  dlength := sqrt(dx*dx + dy*dy + dz*dz);
  if dlength = 0 then dlength := 0.0001;
  //GlobalGravity pulls in the given direction (parallel)
  with aParticle.Velocity do begin
    //v = v0 + a * t
    x := x + (dx / dlength) * gravity * time / 1000;
    y := y + (dy / dlength) * gravity * time / 1000;
    z := z + (dz / dlength) * gravity * time / 1000;
  end;
end;


procedure fxLinearGrav(aGroup : TFxGroup;const direction : TFxVector;const gravity, time : single);
begin
  fxLinearGrav(aGroup, direction.x, direction.y, direction.z, gravity, time);
end;


procedure fxLinearGrav(aGroup : TFxGroup;const dx, dy, dz : single;const gravity, time : single);
var i : integer;
    dlength : single;
    dirx, diry, dirz : single;
begin
  //normalize Vector
  dlength := sqrt(dx*dx + dy*dy + dz*dz);
  if dlength = 0 then dlength := 0.0001;
  dirx := dx / dlength;
  diry := dy / dlength;
  dirz := dz / dlength;
  //GlobalGravity pulls in the given direction (parallel)
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i].Velocity do begin
    //v = v0 + a * t
    x := x + dirx * gravity * time / 1000;
    y := y + diry * gravity * time / 1000;
    z := z + dirz * gravity * time / 1000;
  end;
end;


//*** fxCircularGrav ***

procedure fxCircularGrav(var aParticle : TFxParticle; const point : TFxVector; const gravity, time : single);
begin
  fxCircularGrav(aParticle, point.x, point.y, point.z, gravity, time);
end;


procedure fxCircularGrav(var aParticle : TFxParticle; const px, py, pz : single; const gravity, time : single);
var dlength : single;
    dirx, diry, dirz : single;
begin
  //CircularGravity pulls constantly to the given Point
  with aParticle do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //get dir-length
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    if dlength = 0 then dlength := 0.0001;
    //v = v0 + a * t
    Velocity.x := Velocity.x + (dirx / dlength) * gravity * time / 1000;
    Velocity.y := Velocity.y + (diry / dlength) * gravity * time / 1000;
    Velocity.z := Velocity.z + (dirz / dlength) * gravity * time / 1000;
  end;
end;


procedure fxCircularGrav(aGroup : TFxGroup; const point : TFxVector; const gravity, time : single); 
begin
  fxCircularGrav(aGroup, point.x, point.y, point.z, gravity, time);
end;


procedure fxCircularGrav(aGroup : TFxGroup; const px, py, pz : single; const gravity, time : single); 
var i : integer;
    gf : single;
    dlength : single;
    dirx, diry, dirz : single;
begin
  //CircularGravity pulls constantly to the given Point
  gf := gravity * time / 1000;//precalc
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //get dir-length
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    if dlength = 0 then dlength := 0.0001;
    //v = v0 + a * t
    Velocity.x := Velocity.x + (dirx / dlength) * gf;
    Velocity.y := Velocity.y + (diry / dlength) * gf;
    Velocity.z := Velocity.z + (dirz / dlength) * gf;
  end;
end;


//*** fxPointGrav ***

procedure fxPointGrav(var aParticle : TFxParticle; const point : TFxVector; const minRadius, attrMass, time : single);
begin
  fxPointGrav(aParticle, point.x, point.y, point.z, minRadius, attrMass, time);
end;

procedure fxPointGrav(var aParticle : TFxParticle; const px, py, pz : single; const minRadius, attrMass, time : single);
var dlength, gf : single;
    dirx, diry, dirz : single;
    gravity : double;
begin
  gf := (time * attrMass) / 1000000;
  with aParticle do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //normalize vector
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    //calc Gravity (Newtons Law of Gravity)
    if dlength < minradius then exit;
    gravity := gf / (dlength*dlength*dlength);
    //v = v0 + a * t
    Velocity.x := Velocity.x + dirx * gravity;
    Velocity.y := Velocity.y + diry * gravity;
    Velocity.z := Velocity.z + dirz * gravity;
  end;
end;


procedure fxPointGrav(aGroup : TFxGroup;const point : TFxVector;const minradius, attrMass, time : single); 
begin
  fxPointGrav(aGroup, point.x, point.y, point.z, minradius, attrMass, time);
end;


procedure fxPointGrav(aGroup : TFxGroup;const px, py, pz : single;const minradius, attrMass, time : single);
var i : integer;
    dlength, f : single;
    dirx, diry, dirz : single;
    gravity : double;
begin
  //PointGravity pulls to the given point. (F proportional 1/R^2)
  f := time / 1000000;
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
  with aGroup.Particles[i] do begin
    //get direction
    dirx := px - Position.x;
    diry := py - Position.y;
    dirz := pz - Position.z;
    //normalize vector
    dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
    //calc Gravity (Newtons Law of Gravity)
    if dlength < minradius then dlength := minradius;
    gravity := f * attrMass / (dlength*dlength*dlength);
    //v = v0 + a * t
    Velocity.x := Velocity.x + dirx * gravity;
    Velocity.y := Velocity.y + diry * gravity;
    Velocity.z := Velocity.z + dirz * gravity;
  end;
end;

//*** fxLineGrav

procedure fxLineGrav(var aParticle : TFxParticle; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
var i,vLength : single;
    u,v : TfxVector;
begin
   //calculate the point on the line this particle is atracted to
   u.x := aParticle.Position.x - p1.x; //u = particle - p1;
   u.y := aParticle.Position.y - p1.y;
   u.z := aParticle.Position.z - p1.z;
   v.x := p2.x - p1.x; //v = p2 - p1;
   v.y := p2.y - p1.y;
   v.z := p2.z - p1.z;
   //length of v
   vLength := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
   //project u on a...
   //v dot u / |v|^2
   i := (v.x * u.x + v.y * u.y + v.z * u.z) / sqr(vLength);
   //pAtrr = p1 + i * v
   fxPointGrav(aParticle, p1.x + i*v.x, p1.y + i*v.y, p1.z + i*v.z, minRadius, attrMass, time);
end;

procedure fxLineGrav(var aParticle : TFxParticle; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
var i,vLength : single;
    u,v : TfxVector;
begin
   //calculate the point on the line this particle is atracted to
   u.x := aParticle.Position.x - p1x; //u = particle - p1;
   u.y := aParticle.Position.y - p1y;
   u.z := aParticle.Position.z - p1z;
   v.x := p2x - p1x; //v = p2 - p1;
   v.y := p2y - p1y;
   v.z := p2z - p1z;
   //length of v
   vLength := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
   //project u on a...
   //v dot u / |v|^2
   i := (v.x * u.x + v.y * u.y + v.z * u.z) / sqr(vLength);
   //pAtrr = p1 + i * v
   fxPointGrav(aParticle, p1x + i*v.x, p1y + i*v.y, p1z + i*v.z, minRadius, attrMass, time);
end;


procedure fxLineGrav(aGroup : TFxGroup; const p1, p2 : TFxVector; const minRadius, attrMass, time : single); overload;
var i : integer;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
    fxLineGrav(aGroup.Particles[i],p1,p2,minRadius, attrMass, time);
end;

procedure fxLineGrav(aGroup : TFxGroup; const p1x, p1y, p1z, p2x, p2y, p2z : single; const minRadius, attrMass, time : single); overload;
var i : integer;
begin
   for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then
    fxLineGrav(aGroup.Particles[i],p1x, p1y, p1z, p2x, p2y, p2z, minRadius, attrMass, time);
end;

//*** fxParticleGrav

procedure fxParticleGrav(aGroup : TFxGroup;const time : single);
begin
  fxParticleGrav(aGroup, GRAV_MIN_RADIUS, time);
end;


procedure fxParticleGrav(aGroup : TFxGroup;const minradius, time : single);
var i, j : integer;
    dlength : single;
    dirx, diry, dirz : single;
    gravity, f : double;
begin
  //OPTIMISED BUT UN_UNDERSTANDABLE ;)
  //GlobalGravity pulls constantly in the given direction (parallel)
  f := time / 100000;
  with aGroup do begin
    for i := 0 to HighestActive do if not Particles[i].Junk then
     for j := 0 to HighestActive do if not Particles[j].Junk then begin
      //get direction
      dirx := Particles[j].Position.x - Particles[i].Position.x;
      diry := Particles[j].Position.y - Particles[i].Position.y;
      dirz := Particles[j].Position.z - Particles[i].Position.z;
      //get length
      dlength := sqrt(dirx*dirx + diry*diry + dirz*dirz);
      //calc Gravity (Newtons Law of Gravity)
      if dlength < minradius then dlength := minradius;
      gravity := Particles[j].mass * f / (dlength*dlength*dlength);

      Particles[i].Velocity.x := Particles[i].Velocity.x + dirx * gravity;
      Particles[i].Velocity.y := Particles[i].Velocity.y + diry * gravity;
      Particles[i].Velocity.z := Particles[i].Velocity.z + dirz * gravity;
    end;
  end;
end;


//*** fxFriction

procedure fxFriction(var aParticle : TFxParticle; const viscosity, time : single);
var k : single;
    speed, difspeed : single;
begin
  {Derivation:
   F = k * Volume * viscosity * |v|
   a = k * viscosity * |v| / density
   -> |v'| = k * viscosity * |v| * aTime / density
   -> |v| := |v| - |v'| }
  with aParticle do begin
   speed := sqrt(Velocity.x*Velocity.x + Velocity.y*Velocity.y + Velocity.z*Velocity.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   Velocity.x := Velocity.x * k;
   Velocity.y := Velocity.y * k;
   Velocity.z := Velocity.z * k;
  end;
end;


procedure fxFriction(aGroup : TFxGroup; const viscosity, time : single);
var i               : integer;
    k               : single;
    speed, difspeed : single;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
   speed := sqrt(Velocity.x*Velocity.x + Velocity.y*Velocity.y + Velocity.z*Velocity.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   Velocity.x := Velocity.x * k;
   Velocity.y := Velocity.y * k;
   Velocity.z := Velocity.z * k;
  end;
end;


//*** fxSimpleFlow

procedure fxSimpleFlow(var aParticle : TFxParticle; const direction : TFxVector; const viscosity, time : single);
begin
  fxSimpleFlow(aParticle, direction.x, direction.y, direction.z, viscosity, time);
end;


procedure fxSimpleFlow(var aParticle : TFxParticle; const dx, dy, dz : single; const viscosity, time : single);
var k : double;
    relVel : TFxVector;
    speed, difspeed : single;
begin
  {Derivation: see friction
   but v is a relative Velocity according to the surrounding
   so what i do:
   - calc relVel. (vrel = v - dir)
   - apply the friction
   - calc v' = vrel + dir
  }
  with aParticle do begin
   relVel.x := Velocity.x - dx;
   relVel.y := Velocity.y - dy;
   relVel.z := Velocity.z - dz;

   speed := sqrt(relVel.x*relVel.x + relVel.y*relVel.y + relVel.z*relVel.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   relVel.x := relVel.x * k;
   relVel.y := relVel.y * k;
   relVel.z := relVel.z * k;

   Velocity.x := relVel.x + dx;
   Velocity.y := relVel.y + dy;
   Velocity.z := relVel.z + dz;
  end;
end;


procedure fxSimpleFlow(aGroup : TFxGroup; const direction : TFxVector; const viscosity, time : single);
begin
  fxSimpleFlow(aGroup, direction.x, direction.y, direction.z, viscosity, time);
end;


procedure fxSimpleFlow(aGroup : TFxGroup; const dx, dy, dz : single; const viscosity, time : single);
var i : integer;
    k : double;
    relVel : TFxVector;
    speed, difspeed : single;
begin
  for i := 0 to aGroup.HighestActive do if not aGroup.Particles[i].Junk then with aGroup.Particles[i] do begin
   relVel.x := Velocity.x - dx;
   relVel.y := Velocity.y - dy;
   relVel.z := Velocity.z - dz;

   speed := sqrt(relVel.x*relVel.x + relVel.y*relVel.y + relVel.z*relVel.z); // |v|
   if speed = 0 then exit;
   difspeed := 18 * viscosity * speed * time / 1000 / density; //k ought to be about 6 * pi  = 18
   if difspeed < speed then k := (speed - difspeed) / speed else k := 0;
   relVel.x := relVel.x * k;
   relVel.y := relVel.y * k;
   relVel.z := relVel.z * k;

   Velocity.x := relVel.x + dx;
   Velocity.y := relVel.y + dy;
   Velocity.z := relVel.z + dz;
  end;
end;

end.
