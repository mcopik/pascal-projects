unit SDLListBox;

{*********************************************************************
             SDLListBox v1.0b -  03.10.2004.
             
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
Description: TSDLListBox.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, sdlutils, SDLScroll, sdlgui, SDLDraw, Classes;

type
  TSDLListBox = class(TSDLScrollControl)
  private
    FItems : TStringList;
    FItemIndex : Integer;
    FMultiSelect : Boolean;
    FSelColor: Cardinal;
    FOnDrawItem: TSDLDrawItemEvent;
    procedure SetItems(const Value: TStringList);
    procedure SetTopIndex(const Value: Integer);
    function GetTopIndex: Integer;
    procedure DeselectAll;
    procedure ItemsChange(Sender: TObject);
    procedure Select(No: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetMultiSelect(const Value: Boolean);
  protected
    procedure SetHeight(const Value: Integer); override;
    procedure DoDraw; override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
  public
    Selected : array of Boolean;
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    function ItemHeight: Integer;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  published
    property Items: TStringList read FItems write SetItems;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property SelColor: Cardinal read FSelColor write FSelColor;
    property OnDrawItem: TSDLDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

implementation

{ TSDLListBox }

constructor TSDLListBox.Create(AParent: TSDLObject);
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  SelColor := $8000;
  FItemIndex := -1;
  AlwaysDown := True;
  Width := 128;
  Height := 128;
end;

procedure TSDLListBox.DeselectAll;
var i : Integer;
begin
  For i := 0 to High(Selected) do
    Selected[i] := False;
end;

destructor TSDLListBox.Destroy;
begin
  FItems.Free;
  Finalize(Selected);
  inherited;
end;

procedure TSDLListBox.DoDraw;
var till,i : Integer;
    tr,cr,ir,tc : TSDL_Rect;
    State: Integer;
begin
  inherited;
  if FItems.Count=0 then Exit;

  with ScrollBar do
  begin
    till := LargeChange+Position;
    if till>=Max+LargeChange then till := Max+LargeChange-1;
    if till>=FItems.Count then till := FItems.Count-1;
  end;
  tr := SDLRect(WorldX+BorderWidth,WorldY+BorderWidth,
               ScrollBar.X-BorderWidth,Height-BorderWidth shl 1);
  cr := tr;
  tc := SDLScreen.ClipRect;

  SDLScreen.SetClipRect(SDLScreen.Surface,@cr);
  tr.h := ItemHeight;
  for i := ScrollBar.Position to till do
  begin
    if Assigned(OnDrawItem) then
    begin
      State := 0;
      if Selected[1] then State := stSelected;
      if FItemIndex=1 then State := State or stFocused;
      OnDrawItem(Self,i,tr,State);
    end
    else
    begin
      if Selected[i] then
        SDLScreen.FillRect(SDLScreen.Surface,tr,SelColor);
      if FItemIndex=i then
        ir := tr;
      with cr do
        if tr.y+tr.h>y+h then tr.h := y+h-tr.y;
      Font.TextOut(SDLScreen.Surface,tr.x,tr.y,FItems[i]);
    end;
    Inc(tr.y,tr.h);
  end;
  Dec(ir.w);
  with ScrollBar do
    if (FItemIndex>=Position)and(FItemIndex<=till) then
      SDLScreen.DrawFrame(SDLScreen.Surface,ir,0);
  SDLScreen.SetClipRect(SDLScreen.Surface,@tc);
end;

function TSDLListBox.GetTopIndex: Integer;
begin
  Result := ScrollBar.Position;
end;

function TSDLListBox.ItemHeight: Integer;
begin
  Result := Font.TextHeight('O');
end;

procedure TSDLListBox.ItemsChange(Sender: TObject);
begin
  ScrollBar.Max := FItems.Count-ScrollBar.LargeChange;
  if FItemIndex>=FItems.Count then
    ItemIndex := FItems.Count-1;
  SetLength(Selected,FItems.Count);
end;

procedure TSDLListBox.KeyDown(var Key: Word; Modifier: TSDLMod);
var Temp : Boolean;
    t : Integer;
begin
  inherited;

  Temp := True;
  t := -1;
  case Key of
  SDLK_UP: if FItemIndex>-1 then t := FItemIndex-1;
  SDLK_DOWN: t := FItemIndex+1;
  SDLK_PAGEUP:
    begin
      t := FItemIndex-ScrollBar.LargeChange;
      if t<0 then t := 0;
      if FMultiSelect and(KMOD_SHIFT and Modifier>0) then
        Select(t);
    end;
  SDLK_PAGEDOWN:
    begin
      t := FItemIndex+ScrollBar.LargeChange;
      if t>=Length(Selected) then t := Length(Selected)-1;
      if FMultiSelect and(KMOD_SHIFT and Modifier>0) then
        Select(t);
    end;
  SDLK_HOME:
    begin
      t := 0;
      if FMultiSelect and(KMOD_SHIFT and Modifier>0) then
        Select(t);
    end;
  SDLK_END:
    begin
      t := Length(Selected)-1;
      if FMultiSelect and(KMOD_SHIFT and Modifier>0) then
        Select(t);
    end;
  else Temp := False;
  end;

  if Temp then
  begin
    Key := 0;
    if not FMultiSelect or((KMOD_CTRL or KMOD_SHIFT)and Modifier=0) then
      DeselectAll;
    ItemIndex := t;
    Click(WorldX,WorldY);
  end;
end;

procedure TSDLListBox.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
var tr : TSDL_Rect;
    s : Integer;
begin
  inherited;
  if not HasFocus then Exit;
  if (Button<>1)or(FItems.Count=0) then Exit;

  tr := BoundsRect;
  InflateRect(tr,-BorderWidth,-BorderWidth);
  if not PointInRect(SDLPoint(AX,AY),tr) then Exit;

  with ScrollBar do
  begin
    s := Position+(AY-tr.y)div ItemHeight;
    if s>=Max+LargeChange then Exit;
    if s>=FItems.Count then s := FItems.Count-1;
  end;

  if FMultiSelect then
  begin
    if KMOD_CTRL and Modifier>0 then
    begin
      Selected[s] := not Selected[s];
      if not Selected[s] and (FItemIndex=s) then
        ItemIndex := -1;
    end
    else if KMOD_SHIFT and Modifier>0 then
      Select(s)
    else
    begin
      DeselectAll;
      Selected[s] := True;
    end;
  end
  else
  begin
    if FItemIndex>-1 then
      Selected[FItemIndex] := False;
    Selected[s] := True;
  end;

  if not(FMultiSelect and(KMOD_CTRL and Modifier>0)) then
    ItemIndex := s
  else
    FItemIndex := s;
end;

procedure TSDLListBox.Select(No: Integer);
var sStart,sEnd,i : Integer;
begin
  sStart := -1;
  sEnd := -1;
  if FItemIndex=-1 then sStart := 0
  else
    for i := 0 to High(Selected) do
    begin
      if Selected[i] then
        if sStart=-1 then
          sStart := i
        else
          sEnd := i;
    end;
  if sEnd=-1 then sEnd := sStart;
  if No<sStart then
  begin
     sStart := No;
     No := sEnd;
  end;
  DeselectAll;
  for i := sstart to No do
    Selected[i] := True;
end;

procedure TSDLListBox.SetHeight(const Value: Integer);
var Temp: Integer;
begin
  Temp := (Value-BorderWidth shl 1) div ItemHeight;
  ScrollBar.LargeChange := Temp;
  ScrollBar.Max := FItems.Count-Temp;
  inherited SetHeight(Value);
end;

procedure TSDLListBox.SetItemIndex(const Value: Integer);
begin
  if FItemIndex=Value then Exit;
  if not FMultiSelect and (FItemIndex>-1) then
    Selected[FItemIndex] := False;
  if (Value>-1)and(Value<Length(Selected)) then
    Selected[Value] := True;

  if Value<Length(Selected) then
  begin
    FItemIndex := Value;
    with ScrollBar do
      if FItemIndex<Position then
        Position := FItemIndex
      else if FItemIndex>=Position+LargeChange then
        Position := FItemIndex-LargeChange+1;
  end
  else FItemIndex := -1;
end;

procedure TSDLListBox.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TSDLListBox.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect=Value then Exit;
  FMultiSelect := Value;
  if not Value then
  begin
    DeselectAll;
    if (FItemIndex>-1)and(Length(Selected)>FItemIndex) then
      Selected[FItemIndex] := True;
  end;
end;

procedure TSDLListBox.SetTopIndex(const Value: Integer);
begin
  ScrollBar.Position := Value;
end;

initialization
  SDLRegisterClasses([TSDLListBox]);

end.
