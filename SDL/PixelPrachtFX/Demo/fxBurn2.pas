unit fxBurn2;

interface

uses ppFXcore, ppFXlib, textures, gl;

type

  TFxBurn2 = class(TFxSystem)
  private
    LastShineEmission : integer;
    ShineRate : Integer;
    LastSparkEmission : integer;
    SparkRate : Integer;
    LastFlameEmission : integer;
    FlameRate : Integer;
  procedure init; override;
  public
    Done : Boolean;
    procedure advance(aTime : integer); override;
  end;

implementation

  procedure TFxBurn2.init;
  var tex : GlUint;
  begin

    Done := false;
    ShineRate := 400;
    LastShineEmission := 400;
    SparkRate := 50;
    LastSparkEmission := 50;
    FlameRate := 55;
    LastFlameEmission := 60;
    randomize;

    //***SHINE***
    Groups.Add('Shine',300);
    with Groups['Shine'].Settings do begin
      Mode := FX_TEXQUAD;
      Blending := FX_LIGHT;
      LoadTexture('gfx/flare.jpg', Tex);
      Texture := tex;
      FadeInStop := 0.3;
      FadeOutStart := 0.7;
      ZSort := true;
    end;
    Templates.add('MinShine');
    with Templates['MinShine']^ do begin
      LiveSpan := 800;
      Size := 0.5;
      Sat := 1;
      Spin := 60;
      Color := fxColor(0.6,0.6,0);
      //Position := fxVector(-0.00,-0.,-0.07);
    end;
    Templates.add('MaxShine');
    with Templates['MaxShine']^ do begin
      LiveSpan := 1500;
      Size := 0.7;
      Sat := 0.8;
      Spin := -120;
      Color := fxColor(0.9,0.3,0.2);
      //Position := fxVector(0.07,-0.05,0.07);
    end;

    //***SPARKS***
    Groups.Add('Sparks',500);
    with Groups['Sparks'].Settings do begin
      Mode := FX_TEXQUAD;
      Blending := FX_LIGHT;
      LoadTexture('gfx/spark.jpg',Tex);
      Texture := Tex;
      FadeInStop := 0.0;
      FadeOutStart := 0.7;
      ZSort := true;
    end;
    Templates.add('MinSpark');
    with Templates['MinSpark']^ do begin
      LiveSpan := 800;
      Size := 0.02;
      Sat := 0.7;
      Density := 2;
      Position := fxVector(-0.2,-0.2,-0.2);
      Velocity := fxVector(-1,-1,-1);
      Color := fxColor(0.9, 0.5, 0.3);
    end;
    Templates.add('MaxSpark');
    with Templates['MaxSpark']^ do begin
      LiveSpan := 1000;
      Size := 0.03;
      Sat := 1;
      Density := 10;
      Position := fxVector(0.2,0.0,0.2);
      Velocity := fxVector(1,1,1);
      Color := fxColor(1, 0.9, 0.5);
    end;         


    //***Flames***
    Groups.Add('Flames',300);
    with Groups['Flames'].Settings do begin
      Mode := FX_SPRITE;
      Blending := FX_GLOW;
      LoadTexture('gfx/flame.tga',Tex);
      Texture := tex;
      FadeInStop := 0.5;
      FadeOutStart := 0.7;
      ZSort := true;
    end;
    //ADD TEMPLATE 'MinFlame'
    Templates.add('MinFlame');
    with Templates['MinFlame']^ do begin
      LiveSpan := 600;
      Sat := 0.5;
      Size := 0.25;
      Spin := 200;
      Rotation := 0;
      Density := 2;
      Position := fxVector(-0.15,-0.15,-0.15);
    end;
    //ADD TEMPLATE 'MaxFlame'
    Templates.add('MaxFlame');
    with Templates['MaxFlame']^ do begin
      LiveSpan := 900;
      Size := 0.35;
      Sat := 1;
      Spin := -200;
      Rotation := 360;
      Density := 4;
      Position := fxVector(0.15,0.0,0.15);
    end;
  end;


  procedure TFxBurn2.advance(aTime : integer);
  var p : TFxParticle;
      i : integer;
  begin
    inherited;
    //SHINE
    inc(LastShineEmission,aTime);
    while LastShineEmission > ShineRate do begin
      p := fxGenRandParticle(Templates['MinShine']^,Templates['MaxShine']^);
      Groups['Shine'].add(p);
      dec(LastShineEmission, ShineRate);
    end;
    //Sparks
    inc(LastSparkEmission,aTime);
    while LastSparkEmission > SparkRate do begin
      p := fxGenRandParticle(Templates['MinSpark']^,Templates['MaxSpark']^);
      Groups['Sparks'].add(p);
      dec(LastSparkEmission, SparkRate);
    end;
    fxSimpleFlow(Groups['Sparks'],fxVector(0,2,0),1,aTime);
    fxLinearGrav(Groups['Sparks'],0,-1,0,0.6,aTime);
    //Flames
    inc(LastFlameEmission,aTime);
    while LastFlameEmission > FlameRate do begin
      p := fxGenRandParticle(Templates['MinFlame']^,Templates['MaxFlame']^);
      Groups['Flames'].add(p);
      dec(LastFlameEmission, FlameRate);
    end;
    fxSimpleFlow(Groups['Flames'],fxVector(0,1.2,0),0.8,aTime);
    fxScale(FX_SIZE,Groups['Flames'], 1 - aTime*0.0012);
    fxPointGrav(Groups['Flames'],fxVector(0,-0.5,0),0.01,-350,aTime);
    //fxCircularGrav(Groups['Flames'],fxVector(0,0,0),-8,aTime);
    fxLineGrav(Groups['Flames'],fxVector(0.0,0.0,0),fxVector(0.0,0.35,0),0.1,250,aTime);
  end;

end.
