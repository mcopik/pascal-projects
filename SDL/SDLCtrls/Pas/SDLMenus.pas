unit SDLMenus;

{*********************************************************************
             SDLMenus v1.0b -  03.10.2004.
             
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
Description: TSDLMenuItem, TSDLPopupMenu, TSDLMenu.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses Classes, sdl, sdlgui, SDLDraw, SDLFont, SDLForm;

type
  TSDLMenuItem = class(TSDLComponent)
  private
    FCaption: string;
    FChecked: Boolean;
    FRadioItem: Boolean;
    FGroupIndex: Byte;
    FMenuIndex: Integer;
    FMainMenuItem: Boolean;
    FSelected: Boolean;
    FTimeToPopup: Integer;
    function GetItem(Index: Integer): TSDLMenuItem;
    procedure SetChecked(const Value: Boolean);
    procedure SetGroupIndex(const Value: Byte);
    procedure SetRadioItem(const Value: Boolean);
  protected
    procedure DoDraw; override;
    procedure DoAnimate(MoveCount: Integer); override;
    function TestCollision: Boolean; override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure Click(AX,AY: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    function Add(const ACaption: string; AOnClick: TSDLMouseClickEvent): Integer;
    procedure Delete(Index: Integer);
    function Find(const ACaption: string): TSDLMenuItem;
    function Insert(Index: Integer;const ACaption: string): Integer;
    property Items[Index: Integer]: TSDLMenuItem read GetItem; default;
  published
    property Caption: string read FCaption write FCaption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property MenuIndex: Integer read FMenuIndex;
    property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
    property X stored False;
    property Y stored False;
    property Z stored False;
    property Height stored False;
    property Width stored False;
    property DragMode stored False;
  end;

  TSDLMenuBase = class(TSDLStdControl)
  private
    FMenuParent: TSDLComponent;
    function GetItem(Index: Integer): TSDLMenuItem; virtual;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure AddItems(AItems: TStrings);
    function Add(const ACaption: string; AOnClick: TSDLMouseClickEvent): Integer;
    procedure Delete(Index: Integer);
    function Find(const ACaption: string): TSDLMenuItem;
    function Insert(Index: Integer;const ACaption: string): Integer;
    property Items[Index: Integer]: TSDLMenuItem read GetItem; default;
  end;

  TSDLPopupMenu = class(TSDLMenuBase)
  protected
    procedure DoDraw; override;
    function TestCollision: Boolean; override;
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure HidePopupMenu; override;
    procedure Popup(AX,AY: Integer); override;
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property Width stored False;
    property Height stored False;
  end;

  TSDLMenu = class(TSDLMenuBase)
  private
    FActiveMenu: Boolean;
    procedure ShowMyMenu(Sender: TSDLMenuItem);
  protected
    procedure DoAnimate(MoveCount: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure HidePopupMenu; override;
    procedure UpdateItems;
    procedure Loaded; override;
  end;

implementation

uses SysUtils;

{ TSDLMenuItem }

function TSDLMenuItem.Add(const ACaption: string;
         AOnClick: TSDLMouseClickEvent): Integer;
begin
  if PopupMenu=nil then
    TSDLPopupMenu.Create(Self);
  Result := TSDLPopupMenu(PopupMenu).Add(ACaption,AOnClick);
end;

procedure TSDLMenuItem.Click(AX, AY: Integer);
var pmp: TSDLMenuBase;
begin
  if FMainMenuItem then
  begin
    if Assigned(PopupMenu) then
    begin
      FSelected := not FSelected;
      TSDLMenu(Parent).FActiveMenu := FSelected;
      Gui.PopupMenu := TSDLComponent(Parent);
      if FSelected then PopupMenu.Popup(WorldX,WorldY+Height+1)
      else PopupMenu.Visible := False;
      inherited;
    end;
  end
  else
  begin
    if FGroupIndex>0 then
      if not(FRadioItem and FChecked) then
        SetChecked(not FChecked);
    if Assigned(PopupMenu) then
    begin
      if PointInRect(SDLPoint(AX,AY),BoundsRect) then
        if Assigned(OnClick) then OnClick(Self,AX,AY);
      FTimeToPopup := 0;
      Exit;
    end;
    pmp := TSDLPopupMenu(Parent);
    pmp.Visible := False;
    if pmp.FMenuParent is TSDLControl then
      TSDLControl(pmp.FMenuParent).SetFocus
    else
    begin
      while pmp.FMenuParent is TSDLMenuItem do
        pmp := TSDLMenuBase(pmp.FMenuParent.Parent);
      if pmp.FMenuParent is TSDLControl then
      begin
        pmp.HidePopupMenu;
        TSDLControl(pmp.FMenuParent).SetFocus;
      end;
    end;
    if PointInRect(SDLPoint(AX,AY),BoundsRect) then
      if Assigned(OnClick) then OnClick(Self,AX,AY);
  end;
end;

constructor TSDLMenuItem.Create(AParent: TSDLObject);
begin
  inherited;
  FLockedCoords := True;
end;

procedure TSDLMenuItem.Delete(Index: Integer);
begin
  if Assigned(PopupMenu) then
    if PopupMenu.Count=1 then PopupMenu.Kill
    else PopupMenu.Objects[Index].Kill;
end;

procedure TSDLMenuItem.DoAnimate(MoveCount: Integer);
var i: Integer;
begin
  inherited;
  if FMainMenuItem then
  begin
    if Assigned(PopupMenu) and not PopupMenu.Visible then FSelected := False;
    Exit;
  end;

  if (Gui.SDLMouse.OverControl=Self)and not (FMainMenuItem or FSelected) then
  begin
    for i := 0 to Parent.Count-1 do
      with Parent.Objects[i] as TSDLMenuItem do
      begin
        FSelected := False;
        if Assigned(PopupMenu) then PopupMenu.Visible := False;
      end;
    FSelected := True;
    if Assigned(PopupMenu) then
      FTimeToPopup := 500;
  end;

  if not Assigned(PopupMenu)or PopupMenu.Visible then Exit;

  if FSelected then
  begin
    Dec(FTimeToPopup,MoveCount);
    if FTimeToPopup<0 then PopupMenu.Popup(WorldX+Width,WorldY);
  end;
end;

procedure TSDLMenuItem.DoDraw;
var txtRect: TSDL_Rect;
    FC : Cardinal;
begin
  if not TSDLMenuBase(Parent).Visible then Exit;

  inherited;

  txtRect := BoundsRect;
  FC := TSDLMenuBase(Parent).Color;

  if FMainMenuItem then
  begin
    if Gui.SDLMouse.OverControl = Self then
      SDLScreen.Draw3DControl(SDLScreen.Surface, txtrect, FC, 1, FSelected)
    else if FSelected then
      SDLScreen.Draw3DControl(SDLScreen.Surface, txtrect, FC, 1, True);
  end
  else if Gui.SDLMouse.OverControl = Self then
    SDLScreen.FillRect(SDLScreen.Surface, txtRect, $FF80);

  if (FGroupIndex>0) and FChecked then
    if FRadioItem then
      Font.TextRectA(SDLScreen.Surface,txtRect,txtRect.x+BorderWidth,
                     txtRect.y,'*')
    else
      Font.TextRectA(SDLScreen.Surface,txtRect,txtRect.x+BorderWidth+2,
                     txtRect.y,'x');

  if FCaption<>'' then
  begin
    Font.SetTempAlign(taCenter,tlCenter);
    Font.TextRect(SDLScreen.Surface,txtRect,FCaption);
    Font.RestoreAlignment;
  end;
  if not FMainMenuItem and Assigned(PopupMenu) then
  begin
   with txtRect do
    begin
      x := x+w-12;
      y := y+h div 2-3;
      h := 6;
      w := h;
    end;
   SDLScreen.DrawArrow(SDLScreen.Surface, adRight, txtRect, 0);
  end;
end;

function TSDLMenuItem.Find(const ACaption: string): TSDLMenuItem;
begin
  if Assigned(PopupMenu) then
    Result := TSDLPopupMenu(PopupMenu).Find(ACaption)
  else
    Result := nil;
end;

function TSDLMenuItem.GetItem(Index: Integer): TSDLMenuItem;
begin
  if Assigned(PopupMenu) then
    Result := PopupMenu.Objects[Index] as TSDLMenuItem
  else Result := nil;
end;

function TSDLMenuItem.Insert(Index: Integer;
  const ACaption: string): Integer;
begin
  if PopupMenu=nil then
    TSDLPopupMenu.Create(Self);
  Result := TSDLPopupMenu(PopupMenu).Insert(Index,ACaption);
end;

procedure TSDLMenuItem.MouseMove(Modifier: TSDLMod; AX, AY: Word);
begin
  inherited;
  if FMainMenuItem and Assigned(PopupMenu) and not PopupMenu.Visible then
    TSDLMenu(Parent).ShowMyMenu(Self);
end;

procedure TSDLMenuItem.SetChecked(const Value: Boolean);
var i: Integer;
begin
  FChecked := Value;
  if not FChecked then Exit;
  if FRadioItem then
    for i := 0 to Parent.Count-1 do
      if Parent.Objects[i]<>Self then
        with Parent.Objects[i] as TSDLMenuItem do
          if FRadioItem and (FGroupIndex=Self.FGroupIndex) then
            Checked := False
end;

procedure TSDLMenuItem.SetGroupIndex(const Value: Byte);
begin
  FGroupIndex := Value;
  SetChecked(FChecked);
end;

procedure TSDLMenuItem.SetRadioItem(const Value: Boolean);
begin
  FRadioItem := Value;
  SetChecked(FChecked);
end;

function TSDLMenuItem.TestCollision: Boolean;
begin
  Result := Visible and Enabled;
end;

{ TSDLMenuBase }

function TSDLMenuBase.Add(const ACaption: string;
         AOnClick: TSDLMouseClickEvent): Integer;
begin
  with TSDLMenuItem.Create(Self) do
  begin
    Z := Self.Count-1;
    Caption := ACaption;
    Result := Z;
    Font := Self.Font;
    OnClick := AOnClick;
  end;
end;

procedure TSDLMenuBase.AddItems(AItems: TStrings);
var i,n,l,level: Integer;
    CurItem, LastItem: TSDLMenuItem;
    tn: string;
begin
  level := 1;
  if AItems[0][1]=#9 then Exit;
  Clear;
  CurItem := nil;
  LastItem := nil;
  for i := 0 to AItems.Count-1 do
  begin
    if Trim(AItems[i]) = '' then Continue;
    l := 1;
    while AItems[i][l]=#9 do Inc(l);
    if l>level+1 then Exit;  //error
    if l>level then
    begin
      CurItem := LastItem;
      Inc(level);
    end
    else
    begin
       if l = 1 then CurItem := nil
       else
         for n := l to level-1 do
           CurItem := TSDLMenuBase(CurItem.Parent).FMenuParent as TSDLMenuItem;
       level := l;
    end;
    tn := Trim(AItems[i]);
    if l = 1 then
    begin
      l := Add(tn,nil);
      LastItem := Items[l];
    end
    else
    begin
      l := CurItem.Add(tn,nil);
      LastItem := CurItem.Items[l];
    end;
  end;
end;

constructor TSDLMenuBase.Create(AParent: TSDLObject);
begin
  inherited;
  Color := FocusColor;
  FLockedCoords := True;
end;

procedure TSDLMenuBase.Delete(Index: Integer);
begin
  Objects[Index].Kill;
end;

function TSDLMenuBase.Find(const ACaption: string): TSDLMenuItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    with Objects[i] as TSDLMenuItem do
      if Caption = ACaption then
      begin
        Result := Objects[i] as TSDLMenuItem;
        Exit;
      end;
end;

function TSDLMenuBase.GetItem(Index: Integer): TSDLMenuItem;
begin
  Result := Objects[Index] as TSDLMenuItem;
end;

function TSDLMenuBase.Insert(Index: Integer; const ACaption: string): Integer;
var i : Integer;
begin
  if Index >= Count then Index := Count-1;
  for i := Index to Count-1 do
    Objects[i].Z := Objects[i].Z+1;
  with TSDLMenuItem.Create(Self) do
  begin
    Z := Index;
    Caption := ACaption;
    Result := Z;
    Font := Self.Font;
  end;
end;

{ TSDLPopupMenu }

constructor TSDLPopupMenu.Create(AParent: TSDLObject);
begin
  inherited Create(AParent.GUI);
  if AParent is TSDLComponent then
    Designing := TSDLComponent(AParent).Designing;
  FMenuParent := AParent as TSDLComponent;
  FMenuParent.PopupMenu := Self;
  Font := FMenuParent.Font;
  Visible := False;
end;

destructor TSDLPopupMenu.Destroy;
begin
  FMenuParent.PopupMenu := nil;
  inherited;
end;

procedure TSDLPopupMenu.DoDraw;
begin
  if Visible then  //Eliminate FDesigning option for drawing
    inherited;
end;

procedure TSDLPopupMenu.HidePopupMenu;
begin
  inherited;
  if Gui.PopupMenu=nil then Visible := False;
end;

procedure TSDLPopupMenu.Popup(AX, AY: Integer);
var i,th,tw,w : Integer;
begin
  inherited;
  if Count=0 then Exit;
  Visible := True;
  X := AX;
  Y := AY;
  if GUI.PopupMenu = nil then
    GUI.PopupMenu := Self;
  th := Font.TextHeight('O');
  w := 0;
  for i := 0 to Count-1 do
  begin
    Objects[i].Height := th;
    Objects[i].X := BorderWidth;
    Objects[i].Y := th*i+BorderWidth;
    tw := Font.TextWidth(Items[i].FCaption);
    if tw>w then w := tw;
  end;
  w := w+th*3;
  for i := 0 to Count-1 do
    Objects[i].Width := w;
  Width := w+BorderWidth*2;
  Height := th*Count+BorderWidth*2;
end;

procedure TSDLPopupMenu.SetVisible(const Value: Boolean);
var i: Integer;
    tc: TSDLComponent;
begin
  inherited;
  for i := 0 to Count-1 do
  begin
    tc := TSDLComponent(Objects[i]).PopupMenu;
    if Assigned(tc) then tc.Visible := False;
    TSDLMenuItem(Objects[i]).FSelected := False;
  end;
  if not Visible then
    if Gui.PopupMenu=Self then Gui.PopupMenu := nil;
end;

function TSDLPopupMenu.TestCollision: Boolean;
begin
  Result := Visible and Enabled;
end;

{ TSDLMenu }

constructor TSDLMenu.Create(AParent: TSDLObject);
var FSDLForm: TSDLForm;
begin
  inherited;
  FSDLForm := AParent as TSDLForm;
  FMenuParent := FSDLForm;
  X := FSDLForm.BorderWidth;
  Y := FSDLForm.TitleBarHeight+FSDLForm.BorderWidth;
  Width := FSDLForm.Width-FSDLForm.BorderWidth * 2;
  Height := 28;
  AlwaysUp := True;
  BorderWidth := 1;
end;

procedure TSDLMenu.DoAnimate(MoveCount: Integer);
begin
  Width := FMenuParent.Width-FMenuParent.BorderWidth * 2;
  inherited;
end;

procedure TSDLMenu.HidePopupMenu;
var i : Integer;
begin
  with Gui.SDLMouse do
  if Assigned(DownControl) and (DownControl is TSDLMenuItem) then Exit;
  inherited;
  if Assigned(Gui.PopupMenu) then Exit;
  FActiveMenu := False;
  for i := 0 to Count-1 do
    with Objects[i] as TSDLMenuItem do
    begin
      if Assigned(PopupMenu) then PopupMenu.Visible := False;
      FSelected := False;
    end;
end;

procedure TSDLMenu.Loaded;
begin
  inherited;
  UpdateItems;
end;

procedure TSDLMenu.ShowMyMenu(Sender: TSDLMenuItem);
var i: Integer;
    tmipm: TSDLComponent;
begin
  if not FActiveMenu then Exit;
  for i := 0 to Count-1 do
  begin
   tmipm := TSDLMenuItem(Objects[i]).PopupMenu;
   if Assigned(tmipm) then
     tmipm.Visible := False;
  end;
  TSDLMenuItem(Sender).FSelected := True;
  with Sender do
    PopupMenu.Popup(WorldX,WorldY+Height+1);
end;

procedure TSDLMenu.UpdateItems;
var i : Integer;
begin
  if Count=0 then Exit;
  Height := Font.TextHeight('O')+4;
  Objects[0].X := 0;
  Objects[0].Y := 1;
  Objects[0].Width := Self.Font.TextWidth(Items[0].FCaption)+Height-2;
  Objects[0].Height := Height-2;
  Items[0].FMainMenuItem := True;
  for i := 1 to Count-1 do
  begin
    Objects[i].Y := 1;
    Objects[i].X := Objects[i-1].X+Objects[i-1].Width;
    Objects[i].Width := Font.TextWidth(Items[i].FCaption)+Height-2;
    Objects[i].Height := Height-2;
    TSDLMenuItem(Objects[i]).FMainMenuItem := True;
  end;
end;

initialization
  SDLRegisterClasses([TSDLMenuItem,TSDLPopupMenu,TSDLMenu]);

end.
