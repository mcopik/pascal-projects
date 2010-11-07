unit SDLButton;

{*********************************************************************
             SDLButton v1.1b -  04.03.2005.
             
    Copyright (C) 2004  Igor Stojkovic

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Author:      Igor Stojkovic
Contact:     stojkovic7@verat.net  (please report any bugs)
             stojkovic@galeb.etf.bg.ac.yu
Description: TSDLButton.

If you have any suggestions or you made some modifications
please inform me.

Changes:
 - Fixed a bug with pressing space on button
 - Modifed GroupIndex behavior
**********************************************************************}

interface

uses sdl, sdlgui, SDLDraw;

type
  TSDLButton = class(TSDLStdControl)
  private
    FHighlight : Boolean;
    FCaption: string;
    FGlyph: TSDLImage;
    FDown: Boolean;
    FGroupIndex: Integer;
    FSp: Byte;
    FGlyphIndex: Integer;
    procedure SetGlyph(const Value: TSDLImage);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetGlyphIndex(const Value: Integer);
    procedure SetDown(const Value: Boolean);
  protected
    procedure DoDraw; override;
    procedure DoAnimate(MoveCount: Integer); override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure Click(AX,AY: Integer); override;
    procedure KeyUp(var Key: Word; Modifier: TSDLMod); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
    function LoseFocus: Boolean; override;
    procedure StartLoading; override;
  public
    constructor Create(AParent: TSDLObject); override;
    function SetFocus: Boolean; override;
    property Glyph: TSDLImage read FGlyph write SetGlyph;
  published
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex default -1;
    property Spacing: Byte read FSp write FSp;
    property Down: Boolean read FDown write SetDown;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property Caption: string read FCaption write FCaption;
  end;

implementation

{ TSDLButton }

procedure TSDLButton.Click(AX, AY: Integer);
var i : Integer;
begin
  inherited;
  if GroupIndex=0 then Exit;

  if Assigned(ControlParent) and not ControlParent.HasFocus then Exit;

  FDown := not FDown;

  for i := 0 to ControlParent.ControlCount-1 do
    if (ControlParent.Controls[i]<>Self)and
       (ControlParent.Controls[i] is TSDLButton) then
      with ControlParent.Controls[i] as TSDLButton do
        if GroupIndex=Self.FGroupIndex then
        begin
          FDown := False;
          DrawNormal;
          Self.FDown := True;
        end;
end;

constructor TSDLButton.Create(AParent: TSDLObject);
begin
  inherited;
  FGlyphIndex := -1;
  Width := 128;
  Height := 30;
  Caption := 'Button';
end;

procedure TSDLButton.DoAnimate(MoveCount: Integer);
begin
  inherited;
  if not FHighlight then Exit;
  if (Gui.SDLMouse.OverControl<>Self)and not HasFocus then
  begin
    DrawNormal;
    FHighlight := False;
  end;
end;

procedure TSDLButton.DoDraw;
var txtRect,Temp: TSDL_Rect;
    tx,ty : Integer;
    c: Cardinal;
begin
  if Assigned(Image) then
  begin
    if Down then DrawDown;
    inherited;
  end
  else
  begin
    if FHighlight then c := FocusColor
    else if not Enabled then c := DisabledColor
    else c := Color;
    SDLScreen.Draw3DControl(SDLScreen.Surface,BoundsRect,c,BorderWidth,Pushed or FDown);
  end;

  txtRect := BoundsRect;
  if Assigned(FGlyph) then
  begin
    tx := 0;
    if not Enabled then tx := 1;
    if Pushed then tx := 2;
    if tx>=FGlyph.PatternCount then
      tx := 0;
    ty := txtRect.y+(txtRect.h-FGlyph.PatternHeight)div 2;
    if Pushed or Down then Inc(ty,BorderWidth div 2);
    FGlyph.Draw(SDLScreen.Surface,txtRect.x+BorderWidth+FSp,ty,tx);
  end;

  if FCaption='' then Exit;

  Temp := Font.TextExtent(FCaption);
  tx := (txtRect.w-Temp.w)div 2;
  ty := txtRect.y+1+(Height-Temp.h)div 2;
  if Assigned(FGlyph) and
     (tx<BorderWidth+FSp shl 1+FGlyph.PatternWidth) then
    tx := BorderWidth+FSp shl 1+FGlyph.PatternWidth;
  Inc(tx,txtRect.x);

  InflateRect(txtRect,-BorderWidth,-BorderWidth);
  if Pushed or Down then
  begin
    Inc(ty,BorderWidth div 2);
    Inc(tx,BorderWidth div 2);
  end;
  Font.TextRectA(SDLScreen.Surface,txtRect,tx,ty,FCaption);
end;

procedure TSDLButton.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  inherited;
  if Key = SDLK_RETURN then
    Click(WorldX,WorldY)
  else if Key = SDLK_SPACE then
  begin
    Pushed := True;
    DrawDown;
  end;
end;

procedure TSDLButton.KeyUp(var Key: Word; Modifier: TSDLMod);
begin
  inherited;
  if Pushed and(Key = SDLK_SPACE) then
  begin
    Pushed := False;
    DrawFocus;
    Click(WorldX,WorldY)
  end;
end;

function TSDLButton.LoseFocus: Boolean;
begin
  Result := inherited LoseFocus;
  FHighlight := False;
end;

procedure TSDLButton.MouseMove(Modifier: TSDLMod; AX, AY: Word);
var Temp: Boolean;
begin
  inherited;
  if FHighlight then Exit;

  Temp := False;
  if Parent=Gui then
    with Gui do
      Temp := not Assigned(ActiveControl)or
              (ActiveControl is TSDLButton)
  else if Assigned(ControlParent) then
    with ControlParent do
      Temp := HasFocus and((not Assigned(ActiveControl)or
              (ActiveControl is TSDLButton)));

  if Temp then Self.SetFocus
  else begin
    Self.DrawFocus;
    FHighlight := True;
  end;
end;

procedure TSDLButton.SetDown(const Value: Boolean);
var i: Integer;
begin
  if FDown = Value then Exit;
  if Value then
    for i := 0 to ControlParent.ControlCount-1 do
      if ControlParent.Controls[i] is TSDLButton then
        with ControlParent.Controls[i] as TSDLButton do
          if GroupIndex=Self.FGroupIndex then
          begin
            FDown := False;
            DrawNormal;
          end;
  FDown := Value;
end;

function TSDLButton.SetFocus: Boolean;
begin
  Result := inherited SetFocus;
  FHighlight := Result;
end;

procedure TSDLButton.SetGlyph(const Value: TSDLImage);
begin
  FGlyph := Value;
  if Width<FGlyph.Width+BorderWidth shl 1 then
    Width := FGlyph.Width+BorderWidth shl 1;
  if Height<FGlyph.Height+BorderWidth shl 1 then
    Height := FGlyph.Height+BorderWidth shl 1;
end;

procedure TSDLButton.SetGlyphIndex(const Value: Integer);
var ot: TSDLObject;
    pi: ^TSDLImages;
begin
  if Assigned(Glyph) and (Value = FGlyphIndex) then Exit;
  FGlyphIndex := Value;
  if FGlyphIndex<0 then Exit;
  ot := Self;
  pi := nil;
  while (pi=nil)and(ot is TSDLComponent) do
  begin
    pi := ot.FieldAddress('RImages');
    ot := ot.Parent;
  end;
  if Assigned(pi) and Assigned(pi^) then
    if FGlyphIndex<pi^.Count then
      Glyph := pi^.Items[FGlyphIndex];
end;

procedure TSDLButton.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  FDown := not FDown;
  SetDown(not FDown);
end;

procedure TSDLButton.StartLoading;
begin
  inherited;
  Caption := '';
end;

initialization
  SDLRegisterClasses([TSDLButton]);

end.
