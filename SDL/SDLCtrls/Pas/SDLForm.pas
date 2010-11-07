unit SDLForm;

{*********************************************************************
             SDLForm v1.0b -  03.10.2004.
             
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
Contact:     jimyiigor@ptt.yu  (please report any bugs)
             stojkovic@galeb.etf.bg.ac.yu
Description: TSDLForm, TSDLPanel - containers for other controls.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses SDL, SDLUtils, sdlgui, SDLDraw, SDLScroll, SysUtils;

type
  TSDLForm = class(TSDLStdControl)
  private
    FDraging : Boolean;
    FOldPos : TPoint;
    FCaption: string;
    FMovable: Boolean;
    FModal: Boolean;
    FTitleBarHeight: Word;
    Fifn: TFileName;
    FDestroyRImages: Boolean;
    procedure Setifn(const Value: TFileName);
  protected
    procedure SetZ(const Value: Integer); override;
    procedure DoDraw; override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY: Integer); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY: Integer); override;
    function LoseFocus: Boolean; override;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure Close;
    function SetFocus: Boolean; override;
  published
    RImages: TSDLImages;  //Reserved images for loading controls
    property Modal: Boolean read FModal write FModal default False;
    property TitleBarHeight: Word read FTitleBarHeight write FTitleBarHeight;
    property Movable: Boolean read FMovable write FMovable default True;
    property Caption: string read FCaption write FCaption;
    property DestroyRImages: Boolean read FDestroyRImages write FDestroyRImages default True;
    property ImagesFileName: TFileName read Fifn write Setifn;
    property Z stored False;
  end;

  TSDLPanel = class(TSDLScrollControl)
  private
    FOldSBPos: Integer;
    procedure SBChange(Sender: TSDLComponent);
  protected
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure RefreshScroll;
  end;

implementation

var FormCount : Byte;

{ TSDLForm }

procedure TSDLForm.Close;
var i,t : Integer;
    tc : TSDLImageObject;
begin
  i := Gui.IndexOfObject(Self);
  t := Gui.Count-1;
  tc := nil;
  while (i<t)and not(tc is TSDLForm) do
  begin
    Inc(i);
    tc := Gui.Objects[i];
  end;
  if Assigned(tc)and(tc is TSDLForm) then
    tc.Z := tc.Z-1;
  Dec(FormCount);
  Kill;
end;

constructor TSDLForm.Create(AParent: TSDLObject);
begin
  Inc(FormCount);
  inherited;

  TitleBarHeight := 20;
  Movable := True;
  AlwaysUp := True;
  Z := FormCount;
  FDestroyRImages := True;
end;

destructor TSDLForm.Destroy;
begin
  if FDestroyRImages and Assigned(RImages) then RImages.Free;
  inherited;
end;

procedure TSDLForm.DoDraw;
var r : TSDL_Rect;
    c,ct : Cardinal;
begin
  inherited;
  if (Caption='')or(TitleBarHeight<5) then Exit;

  r := BoundsRect;
  r.h := TitleBarHeight;
  Font.TextRect(SDLScreen.Surface,r,FCaption);

  if Assigned(Image) then Exit;

  if HasFocus then c := FocusColor
  else if not Enabled then c := DisabledColor
  else c := Color;

  ct := LightColor(c,-40);
  c := LightColor(c,40);
  SDLScreen.DrawOrthoLine(SDLScreen.Surface,True,X+BorderWidth,Y+TitleBarHeight,
                Width-BorderWidth shl 1,c);
  SDLScreen.DrawOrthoLine(SDLScreen.Surface,True,X+BorderWidth,Y+TitleBarHeight+1,
                Width-BorderWidth shl 1,ct);
end;

function TSDLForm.LoseFocus: Boolean;
begin
  if Modal and not Designing then
    Result := False
  else
    Result := inherited LoseFocus;
end;

procedure TSDLForm.MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY: Integer);
var r : TSDL_Rect;
begin
  inherited;
  if (Button<>1)or not Movable then Exit;
  r := BoundsRect;
  FOldPos := SDLPoint(AX,AY);
  r.h := TitleBarHeight;
  FDraging := PointInRect(FOldPos,r);
end;

procedure TSDLForm.MouseMove(Modifier: TSDLMod; AX, AY: Word);
begin
  inherited;
  if Designing then Exit;
  if FDraging then
  begin
    X := X+(AX-FOldPos.X);
    Y := Y+(AY-FOldPos.Y);
    FOldPos.X := AX;
    FOldPos.Y := AY;
    if X+Width<5 then
      X := 5-Width;
    if Y+Height<5 then
      Y := 5-Height;
  end;
end;

procedure TSDLForm.MouseUp(Button: Integer;Modifier: TSDLMod; AX,AY: Integer);
begin
  inherited;
  FDraging := False;
end;

function TSDLForm.SetFocus: Boolean;
begin
  Result := inherited SetFocus;
  if Result then
    Z := FormCount;
end;

procedure TSDLForm.Setifn(const Value: TFileName);
begin
  if (Fifn = Value)or not FileExists(Value) then Exit;
  Fifn := Value;
  if not Assigned(RImages) then
    RImages := TSDLImages.Create(SDLScreen.SDLImageClass);
  RImages.Clear;
  RImages.LoadFromSil(Fifn);
  ImageIndex := ImageIndex; //Refresh
end;

procedure TSDLForm.SetZ(const Value: Integer);
var i,t,v : Integer;
    tc : TSDLImageObject;
begin
  v := Value;
  if v>=FormCount then
    if HasFocus then
      v := FormCount
    else
      v := FormCount-1;

  i := Gui.IndexOfObject(Self);
  t := Gui.Count-1;
  tc := nil;
  while (i<t)and not(tc is TSDLForm) do
  begin
    Inc(i);
    tc := Gui.Objects[i];
  end;
  if Assigned(tc)and(tc is TSDLForm) then
    tc.Z := tc.Z-1;
  inherited SetZ(v);
end;

{ TSDLPanel }

constructor TSDLPanel.Create(AParent: TSDLObject);
begin
  inherited;
  Width := 100;
  Height := 100;
  AlwaysDown := True;
  ScrollBar.Max := 0;
  FOldSBPos := ScrollBar.Position;
  ScrollBar.OnChange := SBChange;
  RefreshScroll;
end;

procedure TSDLPanel.KeyDown(var Key: Word; Modifier: TSDLMod);
var tb1,tb2: Boolean;
begin
  tb1 := (Key = SDLK_TAB)and(KMOD_Shift and Modifier>0);
  tb1 := tb1 and Assigned(ActiveControl)and(IndexOfControl(ActiveControl)=0);
  tb2 := (Key = SDLK_TAB)and(KMOD_Shift and Modifier=0);
  tb2 := tb2 and Assigned(ActiveControl)and
        (IndexOfControl(ActiveControl)=ControlCount-1);
  if not(tb1 or tb2) then
    inherited;
end;

procedure TSDLPanel.RefreshScroll;
var i,m: Integer;
begin
  m := 0;
  for i := 1 to Count-1 do
    with Objects[i] do
      if Y+Height>m then
        m := Y+Height;
  m := (m-ScrollBar.Height)div 20+1;
  if m<=0 then
    ScrollBar.Max := 0
  else
    ScrollBar.Max := m;
end;

procedure TSDLPanel.SBChange(Sender: TSDLComponent);
var i: Integer;
begin
  for i := 1 to Count-1 do
    with Objects[i] do
      Y := Y+FOldSBPos*20-ScrollBar.Position*20;
  FOldSBPos := ScrollBar.Position;
end;

initialization
  SDLRegisterClasses([TSDLForm,TSDLPanel]);

end.
