unit fxBurn;

interface

uses ppFXcore, ppFXlib, textures, gl;

type

  TFxBurn = class(TFxSystem)
  private
    LastEmission : integer;
  procedure init; override;
  public
    Done : Boolean;
    EmissionRate : integer;
    procedure advance(aTime : integer); override;
  end;

implementation

  procedure TFxBurn.init;
  var tex : GlUint;
  begin

    Done := false;
    EmissionRate := 100;
    LastEmission := 100;
    randomize;
    //LoadTexture('burnfx.tga', tex);
    //ADD GROUP 'Smoke'
    Groups.Add('Smoke',1000);
    with Groups['Smoke'].Settings do begin
      Mode := FX_SPRITE;
      Blending := FX_TRANSPARENT;
      Texture := tex;
      LoadTexture('gfx/smokering.tga',Texture);
      //TexTile := fxTexTile(0,0,0.5,0.5);
      FadeInStop := 0;
      FadeOutStart := 0.3;
      ZSort := true;
      //ZSort := false;
    end;

    //ADD GROUP 'Shine'
    Groups.Add('Shine',300);
    with Groups['Shine'].Settings do begin
      Mode := FX_TEXQUAD;
      Blending := FX_LIGHT;
      Texture := tex;
      LoadTexture('gfx/star.jpg', Texture);
      //TexTile := fxTexTile(0.51,0.01,0.99,0.49);
      //Fading := FX_FADE_IN_AND_OUT;
      //FadeSegment := 0.3;
      FadeInStop := 0.3;
      FadeOutStart := 0.8;
      ZSort := true;
      ZSort := false;
    end;
    //ADD GROUP 'Sparks'
    Groups.Add('Sparks',2000);
    with Groups['Sparks'].Settings do begin
      Mode := FX_SPRITE;
      Blending := FX_LIGHT;
      Texture := tex;
      LoadTexture('gfx/sparkooo.tga',Texture);
      //TexTile := fxTexTile(0.5,0.5,1,1);
      //Fading := FX_FADE_OUT;
      //FadeSegment := 0.3;
      FadeInStop := 0.0;
      FadeOutStart := 0.7;
      //ZSort := true;
      ZSort := false;
    end;
    //ADD GROUP 'Burn'
    Groups.Add('Burn',300);
    with Groups['Burn'].Settings do begin
      Mode := FX_SPRITE;
      Blending := FX_GLOW;
      Texture := tex;
      LoadTexture('gfx/flametest.tga',Texture);
      //TexTile := fxTexTile(0,0.5,0.5,1);
      //Fading := FX_FADE_IN_AND_OUT;
      //FadeSegment := 0.3;
      FadeInStop := 0.3;
      FadeOutStart := 0.7;
      //ZSort := true;
      ZSort := false;
    end;

    //*** SMOKE ***


    Templates.add('MinSmoke');
    with Templates['MinSmoke']^ do begin
      LiveSpan := 3000;
      Size := 0.08;
      Sat := 0.2;
      Density := 3;
      Velocity := fxVector(-0.2,0.5,0.2);
      Color := fxColor(0.1,0.3,0.7);
      Spin := -90;
    end;
    Templates.add('MaxSmoke');
    with Templates['MaxSmoke']^ do begin
      LiveSpan := 1500;
      Size := 0.12;
      Sat := 0.5;
      Density := 1;
      Velocity := fxVector(0.2,0.9,0.2);
      Color := fxColor(0.3,0.5,1);
      Spin := 90;
    end;

    //*** Shine ***

    Templates.add('MinShine');
    with Templates['MinShine']^ do begin
      LiveSpan := 400;
      Size := 0.3;
      Sat := 0.2;
      Color := fxColor(0.7,0,0);
      Position := fxVector(-0.07,-0.15,-0.07);
    end;
    Templates.add('MaxShine');
    with Templates['MaxShine']^ do begin
      LiveSpan := 600;
      Size := 0.9;
      Sat := 0.4;
      Color := fxColor(1,0.2,0.8);
      Position := fxVector(0.07,-0.05,0.07);
    end;

    //ADD TEMPLATE 'MinBurn'
    Templates.add('MinBurn');
    with Templates['MinBurn']^ do begin
      LiveSpan := 500;
      Sat := 0.2;
      Size := 0.05;
      Spin := 360;
      Position := fxVector(0,-0.01,0);
    end;

    //ADD TEMPLATE 'MaxBurn'
    Templates.add('MaxBurn');
    with Templates['MaxBurn']^ do begin
      LiveSpan := 600;
      Size := 0.25;
      Sat := 0.6;
      Spin := -180;
      Position := fxVector(0,0.01,0);
    end;

    //*** Sparks ***
    Templates.add('MinSpark');
    with Templates['MinSpark']^ do begin
      LiveSpan := 800;
      Size := 0.03;
      Sat := 0.7;
      Density := 5;
      Velocity := fxVector(-1,0.1,-1);
      Color := fxColor(0.8, 0.8, 0.8);
    end;
    Templates.add('MaxSpark');
    with Templates['MaxSpark']^ do begin
      LiveSpan := 1000;
      Size := 0.1;
      Sat := 1;
      Density := 40;
      Velocity := fxVector(1,0,1);
      Color := fxColor(1, 1, 1);
    end;
  end;

  procedure TFxBurn.advance(aTime : integer);
  var p : TFxParticle;
      i : integer;
  begin
    inherited;

    inc(LastEmission,aTime);
    while LastEmission > EmissionRate do begin
      //SMOKE
      p := fxGenRandParticle(Templates['MinSmoke']^,Templates['MaxSmoke']^);
      Groups['Smoke'].add(p);
      //SPARK
      p := fxGenRandParticle(Templates['MinSpark']^,Templates['MaxSpark']^);
      Groups['Sparks'].add(p);
      //SHINE
      p := fxGenRandParticle(Templates['MinShine']^,Templates['MaxShine']^);
      Groups['Shine'].add(p);
      //Burn
      p := fxGenRandParticle(Templates['MinBurn']^,Templates['MaxBurn']^);
      Groups['Burn'].add(p);
      dec(LastEmission, EmissionRate);
    end;

    fxSimpleFlow(Groups['Smoke'],fxVector(0,0.3,0.1),0.3,aTime);
    fxLinearGrav(Groups['Smoke'],0,-1,0,0.2,aTime);
    fxScale(FX_SIZE,Groups['Smoke'],1 + aTime*0.0009);
    fxLinearGrav(Groups['Sparks'],0,-1,0,2.5,aTime);
  end;

end.
