{
***************************[ PixelPrachtFX - Method Library ]*****************************

Package:           PixelprachtFX

Unit:              ppFXrender

Version:           1.0 (16.12.2002)

Description:       These methods are called inside ppFXcore to render effects.
                   There are methods for setting RenderSettings, rendering
                   whole groups of particles and rendering single particles.


  Happy Coding
        Lithander (lithander@gmx.de)

Change-Log:
   

****************************[Copyright (c) 2003 Thomas Jahn]****************************
}

unit ppFXrender;

interface

uses ppFXcore;

procedure ApplyRenderSettings(var GS : TFxGroupSettings);

function fxFade(const LiveSpan, Age : Integer;const Settings : TFxGroupSettings) : single;

procedure fxRenderPoint(const Particle : TFxParticle;const Settings : TFxGroupSettings; const Transp : single);

procedure fxRenderQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);

procedure fxRenderTexQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);

procedure fxRenderSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);

procedure fxRenderTexSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);

procedure fxRenderSprite(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);

implementation

uses gl, math;

//************************ Particle Rendering ************************************

procedure ApplyRenderSettings(var GS : TFxGroupSettings);
var texactiv : boolean;
begin
  //Texturing
  if GS.Mode in [FX_TEXQUAD, FX_TEXSPARK, FX_SPRITE] then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,GS.Texture);
  end
  else
     glDisable(GL_TEXTURE_2D);
  //BlendMode
  glEnable(GL_BLEND);
  case GS.Mode of
    FX_SPRITE:
      begin
        case GS.Blending of
          FX_OPAQUE : begin
            glBlendFunc(GL_ONE, GL_ZERO);
          end;
          FX_GLOW, FX_LIGHT : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE);
          end;
          FX_TRANSPARENT : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          end;
        end;
      end;
  else
    case GS.Blending of
      FX_OPAQUE : begin
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      FX_GLOW : begin
        glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE);
      end;
      FX_TRANSPARENT : begin
        glBlendFunc(GL_SRC_COLOR, GL_ONE);
      end;
      FX_LIGHT : begin
        glBlendFunc(GL_ONE, GL_ONE);
      end;
    end;
  end;
end;


function fxFade(const LiveSpan, Age : Integer;const Settings : TFxGroupSettings) : single;
var now : single;
begin
  result := 1;
  //Check for the correct Range
  if (Settings.FadeInStop > 1) or
     (Settings.FadeOutStart > 1) or
     (Settings.FadeInStop < 0) or
     (Settings.FadeOutStart < 0) then exit;
  //Check that FadeIn and FadeOut doesn't cross
  if (Settings.FadeInStop > Settings.FadeOutStart) then exit;

  now := Age / (LiveSpan+Age);
  //Fade In
  if now < Settings.FadeInStop then result := now / (Settings.FadeInStop);
  //Fade Out
  if now > Settings.FadeOutStart then result := (1 - now) / (1 - Settings.FadeOutStart);
end;


procedure fxRenderPoint(const Particle : TFxParticle;const Settings : TFxGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    Trsp := 1;
    if Settings.Blending <> FX_OPAQUE then
       Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glPointSize(2);
    glBegin(GL_POINTS);
      glVertex3f(Position.x,Position.y,Position.z);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glVertex3f(size,-size,0);
      glVertex3f(-size,-size,0);
      glVertex3f(-size,size,0);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderTexQuad(const Particle : TFxParticle;const Settings : TFXGroupSettings; const Transp : single);
var Trsp : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size,size,0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
var v            : TFXVector;
    vLength      : single;
    Alpha        : single;
    Tail         : single;
    Matrix       : array[0..15] of single;
    Trsp         : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glTranslatef(Position.x,Position.y,Position.z);
    //**Calculate Tail
    //rotate velocity-vector
    glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
    v.x := velocity.x * Matrix[0] + velocity.y * Matrix[4] + velocity.z * Matrix[8];
    v.y := velocity.x * Matrix[1] + velocity.y * Matrix[5] + velocity.z * Matrix[9];
    vLength := sqrt(v.x*v.x+v.y*v.y);
    Tail := 1 + vLength*Settings.Elongation;
    //calc rotation (angle between v mapped to the viewplane and the y-axis)
    Alpha := arccos( v.y  / vlength) * 360 / (2*pi);//cos(alpha) = v * yaxis / vlength;
    if v.x < 0 then Alpha := - Alpha;
    //**Render
    fxBillboard;
    glRotatef(-Alpha,0,0,1);
    glBegin(GL_QUADS);
      glColor4f(0,0,0,0);
      glVertex3f(size, - size * Tail, 0);
      glVertex3f(-size, -size * Tail, 0);
      glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
      glVertex3f(-size, size, 0);
      glVertex3f(size, size, 0);
    glEnd;
    glPopMatrix;
  end;
end;


procedure fxRenderTexSpark(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
var v            : TFXVector;
    vLength      : single;
    Alpha        : single;
    Tail         : single;
    Matrix       : array[0..15] of single;
    Trsp         : single;
begin
  with Particle do begin
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then Trsp := 1 else Trsp := Sat * Transp * fxFade(LiveSpan, Age, Settings);
    glTranslatef(Position.x,Position.y,Position.z);
    //**Calculate Tail
    //rotate velocity-vector
    glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix);
    v.x := velocity.x * Matrix[0] + velocity.y * Matrix[4] + velocity.z * Matrix[8];
    v.y := velocity.x * Matrix[1] + velocity.y * Matrix[5] + velocity.z * Matrix[9];
    vLength := sqrt(v.x*v.x+v.y*v.y);
    Tail := 1 + vLength*Settings.Elongation;
    //calc rotation (angle between v mapped to the viewplane and the y-axis)
    Alpha := arccos( v.y  / vlength) * 360 / (2*pi);//cos(alpha) = v * yaxis / vlength;
    if v.x < 0 then Alpha := - Alpha;
    //**Render
    fxBillboard;
    glRotatef(-Alpha,0,0,1);
    glBegin(GL_QUADS);
      glColor4f(Color.Red*Trsp,Color.Green*Trsp,Color.Blue*Trsp,Transp);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size, - size * Tail, 0);
      glTexCoord2f(0,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size, -size * Tail, 0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size, size, 0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(size, size, 0);
    glEnd;
    glPopMatrix;
  end;
end;

procedure fxRenderSprite(const Particle : TFxParticle;const Settings : TFXGroupSettings; Transp : single);
begin
  with Particle do begin
    //NEEDS A TEXTURE WITH ALPHA CHANNEL!!!
    glPushMatrix;
    if Settings.Blending = FX_OPAQUE then glColor4f(Color.Red,Color.Green,Color.Blue, Transp)
    else glColor4f(Color.Red,Color.Green,Color.Blue,sat * Transp * fxFade(LiveSpan, Age, Settings));
    glTranslatef(Position.x,Position.y,Position.z);
    fxBillboard;
    if Rotation <> 0 then glRotatef(Rotation,0,0,1);
    glBegin(GL_QUADS);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MinV);
      glVertex3f(size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MinV);
      glVertex3f(-size,-size,0);
      glTexCoord2f(Settings.TexTile.MinU,Settings.TexTile.MaxV);
      glVertex3f(-size,size,0);
      glTexCoord2f(Settings.TexTile.MaxU,Settings.TexTile.MaxV);
      glVertex3f(+size,+size,0);
    glEnd;
    glPopMatrix;
  end;
end;


end.
