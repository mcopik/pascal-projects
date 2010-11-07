unit fxExplosion;

interface

uses ppFXcore, ppFXlib, textures, gl;

type

TFxExplosion = class(TFxSystem)
private
  LastEmission : integer;
  EmissionRate : integer;
  procedure init; override;
public
  Done : Boolean;
  procedure advance(aTime : integer); override;
end;


implementation

procedure TFxExplosion.advance;
var i, j : integer;
    k : single;
    p, p2 : TFxParticle;
    HalfLive : integer;
begin
  inherited;
  inc(LastEmission,aTime);

  //EmitParticles
  if (LastEmission >= EmissionRate) then begin
    dec(LastEmission, EmissionRate);
    if (Age > 1900) and (Age < 2400) then begin
      p := fxGenRandParticle(Templates['SmokeMin']^,Templates['SmokeMax']^);
      fxSetIntoSphere(p,0,0,0,0.2);
      Groups['Smoke'].add(p);
    end;
    //Debris
    if (Age < 1000) and (Age > 500) then for i := 0 to 3 do begin
      p := fxGenRandParticle(Templates['DebrisMin']^,Templates['DebrisMax']^);
      fxSetIntoSphere(p,0,0,0,0.3);
      Groups['Debris'].add(p);
    end;
    if (Age < 1500) then for i := 0 to 1+round(Age / 1000) do begin
      if Age > 500 then for j := 0 to 1+round(Age / 1000) do begin
        p := fxGenRandParticle(Templates['FlameMin']^,Templates['FlameMax']^);
        fxSetIntoSphere(p,0,-0.0,0,Age / 3000+0.03);
        p.Size := p.Size * ((Age / 1200)+0.4);
        fxScale(FX_POS_Y,p,0.7);
        Groups['Flame'].add(p);
      end;
      if Age < 1000 then for j := 0 to 1+round(Age / 1000) do begin
        p2 := fxGenRandParticle(Templates['FireMin']^,Templates['FireMax']^);
        fxSetIntoSphere(p2,0,0,0,Age / 2500);
        fxScale(FX_SIZE,p2,Age / 700 + 0.2);
        fxScale(FX_POS_Y,p2,0.7);
        Groups['Fire'].add(p2);
      end;
    end;
  end;

  //Apply Actions
  //SMOKE
  k := 1-Age/(Age+3000);
  if k < 0 then k := 0;
  fxScale(FX_SIZE,Groups['Smoke'],1+(aTime*0.00025));
  fxCircularGrav(Groups['Smoke'],0,0,0,-3,aTime); //Repelling from the middle
  fxSimpleFlow(Groups['Smoke'],0,0.2,0,0.3,aTime);

  //DEBRIS
//  fxCircularGrav(Groups['Debris'],0,0,0,0.0,aTime); //Repelling from the middle
  fxPointGrav(Groups['Debris'],0,-0.1,0,0.4,-5000,aTime); //Repelling from the middle
  fxSimpleFlow(Groups['Debris'],0,0,0,0.3,aTime);
//  fxLinearGrav(Groups['Debris'],0,0,0,3,aTime);

  //FIRE
  fxCircularGrav(Groups['Fire'],0,0,0,-3,aTime);
  fxFriction(Groups['Fire'],0.3,aTime);

  //FLAME
  fxScale(FX_SIZE,Groups['Flame'],1+(aTime*0.0002));
  fxScale(FX_COLOR,Groups['Flame'],1-aTime*0.0004);
  fxPointGrav(Groups['Flame'],0,0,0,0.35,-1400,aTime);
  fxSimpleFlow(Groups['Flame'],0,0,0,0.3,aTime);

  if Age > 5000 then Done := true;
end;

procedure TFxExplosion.init;
var i : integer;
    p : TFxParticle;
begin
  Done := false;
  randomize;
  LastEmission := 0;
  EmissionRate := 100;
  //ADD GROUP 'Smoke'
  Groups.Add('Smoke',1000);
  with Groups['Smoke'].Settings do begin
    Mode := FX_TEXQUAD;
    Blending := FX_TRANSPARENT;
    LoadTexture('gfx/smoke.jpg',Texture);
    //Fading := FX_FADE_OUT;
    //FadeSegment := 0.3;
    ZSort := true;
  end;
  //ADD GROUP 'Debris'
  Groups.Add('Debris',500);
  with Groups['Debris'].Settings do begin
    Mode := FX_SPRITE;
    Blending := FX_TRANSPARENT;
    LoadTexture('gfx/debris.tga',Texture);
    ZSort := true;
  end;
  //ADD GROUP 'Fire'
  Groups.Add('Fire',1000);
  with Groups['Fire'].Settings do begin
    Mode := FX_TEXQUAD;
    Blending := FX_LIGHT;
    LoadTexture('gfx/YellowWideGlow.jpg',Texture);
    //Fading := FX_FADE_IN_AND_OUT;
    //FadeSegment := 1;
    ZSort := true;
  end;
  //ADD GROUP 'Flame'
  Groups.Add('Flame',1000);
  with Groups['Flame'].Settings do begin
    Mode := FX_SPRITE;
    Blending := FX_TRANSPARENT;
    LoadTexture('gfx/darksmoke.tga',Texture);
    //Fading := FX_FADE_IN_AND_OUT;
    //FadeSegment := 0.6;
    ZSort := true;
  end;
  //ADD SMOKE-TEMPLATES
  Templates.add('SmokeMin');
  with Templates['SmokeMin']^ do begin
    LiveSpan := 2000;
    Size := 0.5;
    Sat := 0.5;
    Density := 0.5;
    Color := fxColor(0.6, 0.6, 0.6);
  end;
  Templates.add('SmokeMax');
  with Templates['SmokeMax']^ do begin
    LiveSpan := 2500;
    Size := 0.7;
    Sat := 0.7;
    Density := 1;
    Color := fxColor(0.7, 0.6, 0.6);
  end;
  //ADD DEBRIS-TEMPLATES
  Templates.add('DebrisMin');
  with Templates['DebrisMin']^ do begin
    LiveSpan := 2000;
    Size := 0.02;
    Sat := 0.9;
    Density := 5;
    Color := fxColor(1, 0.7, 0.7);
  end;
  Templates.add('DebrisMax');
  with Templates['DebrisMax']^ do begin
    LiveSpan := 3000;
    Size := 0.05;
    Sat := 1;
    Density := 10;
    Color := fxColor(1, 0.8, 0.7);
  end;
  //ADD FIRE-TEMPLATES
  Templates.add('FireMin');
  with Templates['FireMin']^ do begin
    LiveSpan := 1000;
    Size     := 0.3;
    Sat      := 0.4;
    Density := 0.4;
    Color := fxColor(1, 1, 1);
  end;
  Templates.add('FireMax');
  with Templates['FireMax']^ do begin
    LiveSpan := 2000;
    Size     := 0.2;
    Sat      := 0.9;
    Density := 0.6;
    Color := fxColor(1, 1, 1);
  end;
  //ADD FIRE-TEMPLATES
  Templates.add('FlameMin');
  with Templates['FlameMin']^ do begin
    LiveSpan := 1000;
    Size     := 0.3;
    Sat      := 0.3;
    Spin     := -20;
    Density := 0.1;
    Color := fxColor(1, 1, 1);
  end;
  Templates.add('FlameMax');
  with Templates['FlameMax']^ do begin
    LiveSpan := 2500;
    Size     := 0.4;
    Sat      := 0.5;
    Spin     := 20;
    Density  := 0.4;
    Color := fxColor(1, 1, 1);
  end;
end;

end.
