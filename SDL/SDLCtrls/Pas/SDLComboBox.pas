unit SDLComboBox;

{*********************************************************************
             SDLComboBox v1.1b -  05.03.2005.
             
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
Description: TSDLDropList and TSDLComboBox.

If you have any suggestions or you made some modifications
please inform me.

Changes:
 - Fixed a call to OnDrawItem event
**********************************************************************}

interface

uses sdl, sdlgui, SDLScroll, SDLDraw, SDLFont, Classes;

type
  TSDLComboBox = class;

  TSDLDropList = class(TSDLScrollComponent)
  private
    FSDLComboBox: TSDLComboBox;
    FItemIndex: Integer;
  protected
    procedure SetVisible(const Value: Boolean); override;
    procedure DoDraw; override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure Click(AX,AY: Integer); override;
  public
    SelColor : Cardinal;
    Color : Cardinal;
    procedure DropDown;
    constructor Create(AParent: TSDLObject); override;
  end;

  TSDLComboBox = class(TSDLStdControl)
  private
    FItemHeight : Word;
    FItems : TStringList;
    FItemIndex : Integer;
    FDropDownCount : Byte;
    FDroppedDown : Boolean;
    FOnDrawItem: TSDLDrawItemEvent;
    FOnChange: TSDLNotifyEvent;
    procedure ItemsChange(Sender: TObject);
    procedure SetItems(Value: TStringList);
    procedure SetItemIndex(Value: Integer);
    procedure SetDDCount(Value:Byte);
    procedure SetDroppedDown(Value: Boolean);
    procedure SetText(const Value: string);
    function GetText: string;
    function GetDDColor: Cardinal;
    procedure SetDDColor(const Value: Cardinal);
    function GetSelColor: Cardinal;
    procedure SetSelColor(const Value: Cardinal);
  protected
    procedure DoDraw; override;
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure DblClick(AX,AY: Integer); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
    function LoseFocus: Boolean; override;
    procedure SetFont(const Value: TSDLFont); override;
    procedure Change; virtual;
  public
    DropList : TSDLDropList;
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure HidePopupMenu; override;
    property Text: string read GetText write SetText;
    property DroppedDown: Boolean read FDroppedDown write SetDroppedDown;
  published
    property Items: TStringList read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property DropDownColor: Cardinal read GetDDColor write SetDDColor;
    property SelColor: Cardinal read GetSelColor write SetSelColor;
    property DropDownCount: Byte read FDropDownCount write SetDDCount;
    property OnChange: TSDLNotifyEvent read FOnChange write FOnChange;
    property OnDrawItem: TSDLDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

implementation

{ TSDLDropList }

procedure TSDLDropList.Click(AX, AY: Integer);
begin
  inherited;
  with FSDLComboBox do
  begin
    ItemIndex := Self.FItemIndex;
    DroppedDown := False;
  end;
end;

constructor TSDLDropList.Create(AParent: TSDLObject);
begin
  inherited Create(AParent.Gui);
  FSDLComboBox := AParent as TSDLComboBox;
  BorderWidth := 1;
  SelColor := $8000;
  Color := $B4C3DC;
end;

procedure TSDLDropList.DoDraw;
var T,i,State : Integer;
    tr: TSDL_Rect;
begin
  inherited;

  tr := BoundsRect;

  if Image=nil then
  begin
    SDLScreen.FillRect(SDLScreen.Surface, tr, Color);
    SDLScreen.DrawFrame(SDLScreen.Surface, tr, 0);
  end;
  if FSDLComboBox.FItems.Count=0 then Exit;

  T := ScrollBar.Position+FSDLComboBox.DropDownCount-1;
  with ScrollBar do
    if T>Max+LargeChange then
      T := Max+LargeChange;
  with FSDLComboBox.FItems do
    if T>=Count then T := Count-1;
  InflateRect(tr,-BorderWidth-1,-BorderWidth);
  tr.h := FSDLComboBox.FItemHeight;
  tr.w := tr.w - BorderWidth-ScrollBar.Width;

  for i := ScrollBar.Position to T do
  begin
    if Assigned(FSDLComboBox.OnDrawItem) then
    begin
      State := 0;
      if FItemIndex = i then State:=stSelected or stFocused;
      FSDLComboBox.OnDrawItem(FSDLComboBox,i,tr,State);
    end
    else
    begin
      if FItemIndex=i then
        SDLScreen.FillRect(SDLScreen.Surface, tr, SelColor);
      FSDLComboBox.Font.TextRectA(SDLScreen.Surface,tr,tr.x,tr.y,FSDLComboBox.FItems[i]);
    end;
    OffsetRect(tr,0,FSDLComboBox.FItemHeight);
  end;
end;

procedure TSDLDropList.DropDown;
begin
  Visible := True;
  Width := FSDLComboBox.Width;
  with FSDLComboBox do
  begin
    Self.Height := FItemHeight*DropDownCount+
                   Self.BorderWidth shl 1+1;
    Self.X := WorldX;
    Self.Y := WorldY+Height;
    Self.FItemIndex := FItemIndex;
  end;
  Gui.PopupMenu := FSDLComboBox;//Self;
end;

procedure TSDLDropList.MouseMove(Modifier: TSDLMod; AX, AY: Word);
var s : Integer;
begin
  inherited;
  s := ScrollBar.Position+(AY-Y-BorderWidth)div FSDLComboBox.FItemHeight;
  with ScrollBar do
    if s>Max+LargeChange then Exit;

  FItemIndex := s;
end;

procedure TSDLDropList.SetVisible(const Value: Boolean);
begin
  inherited;
  Gui.PopupMenu := nil;
end;

{ TSDLComboBox }

procedure TSDLComboBox.Change;
begin
  if Assigned(OnChange) then OnChange(Self);
end;

constructor TSDLComboBox.Create(AParent: TSDLObject);
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  DropList := TSDLDropList.Create(Self);
  DropList.Visible := False;
  DropDownCount := 1;
  FItemIndex := -1;
  AlwaysDown := True;
  Width := 128;
  Height := 25;
end;

procedure TSDLComboBox.DblClick(AX, AY: Integer);
var tr: TSDL_Rect;
begin
  tr := BoundsRect;
  InflateRect(tr,-BorderWidth,-BorderWidth);
  with tr do
  begin
    x := x+w-h;
    w := h;
  end;

  if not PointInRect(SDLPoint(AX,AY),tr) then
    inherited;
end;

destructor TSDLComboBox.Destroy;
begin
  inherited;
  FItems.Free;
end;

procedure TSDLComboBox.DoDraw;
var c: Cardinal;
    tr: TSDL_Rect;
begin
  inherited;

  if Image=nil then
  begin
    tr := BoundsRect;
    with tr do
    begin
      x := x+w-h;
      w := h;
    end;
    InflateRect(tr,-BorderWidth,-BorderWidth);
    if OverlapRect(SDLScreen.SurfaceRect,tr) then
    begin
      c := Color;
      SDLScreen.Draw3DControl(SDLScreen.Surface,tr,c,BorderWidth,Pushed);
      InflateRect(tr,-BorderWidth-3,-BorderWidth-3);
      SDLScreen.DrawArrow(SDLScreen.Surface,adDown,tr,0);
    end;
  end;

  if FItemIndex>-1 then
  begin
    tr := BoundsRect;
    InflateRect(tr,-BorderWidth,-BorderWidth);
    tr.w := tr.w-tr.h;
    if Assigned(OnDrawItem) then
      OnDrawItem(Self,FItemIndex,tr,stDefault)
    else
      Font.TextRect(SDLScreen.Surface, tr, FItems[FItemIndex]);
  end;
end;

function TSDLComboBox.GetDDColor: Cardinal;
begin
  Result := DropList.Color;
end;

function TSDLComboBox.GetSelColor: Cardinal;
begin
  Result := DropList.SelColor;
end;

function TSDLComboBox.GetText: string;
begin
  if FItemIndex>-1 then
    Result := FItems[FItemIndex]
  else
    Result := '';
end;

procedure TSDLComboBox.HidePopupMenu;
begin
  if Gui.SDLMouse.OverControl=Self then Exit;
  Gui.PopupMenu := DropList;
  inherited;
  if Gui.PopupMenu = nil then
    DroppedDown := False
  else
    Gui.PopupMenu := Self;
end;

procedure TSDLComboBox.ItemsChange(Sender: TObject);
begin
  with DropList.ScrollBar do
    Max := FItems.Count-LargeChange-1;
  if FItems.Count>5 then
    DropDownCount := 5
  else
    DropDownCount := FItems.Count;
  DropList.ScrollBar.Visible := FItems.Count>DropDownCount;
  if FItemIndex>=FItems.Count then
    ItemIndex := FItems.Count-1;
end;

procedure TSDLComboBox.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  inherited;
  if Key=SDLK_RETURN then
  begin
    if FDroppedDown then
      ItemIndex := DropList.FItemIndex;
    DroppedDown := not FDroppedDown;
    Key := 0;
  end
  else if (Key=SDLK_ESCAPE)and FDroppedDown then
  begin
    DroppedDown := False;
    Key := 0;
  end;

  if (Key<>SDLK_UP)and(Key<>SDLK_DOWN) then Exit;

  if FDroppedDown then
    with DropList do
    begin
      if Key=SDLK_UP then
      begin
        if FItemIndex>0 then Dec(FItemIndex);
        with ScrollBar do
          if Position>FItemIndex then
            Position := FItemIndex;
      end
      else
      begin
        if FItemIndex<FItems.Count-1 then Inc(FItemIndex);
        with ScrollBar do
          if Position+LargeChange<FItemIndex+1 then
            Position := FItemIndex+1-LargeChange;
      end;
    end
  else
    if Key = SDLK_UP then
    begin
      if FItemIndex>0 then SetItemIndex(FItemIndex-1);
    end
    else
      if FItemIndex<FItems.Count-1 then SetItemIndex(FItemIndex+1);
  Key := 0;
end;

function TSDLComboBox.LoseFocus: Boolean;
begin
  Result := inherited LoseFocus;
  DroppedDown := False;
end;

procedure TSDLComboBox.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
var tr: TSDL_Rect;
begin
  inherited;
  if not HasFocus or(Button<>1) then Exit;
  tr := BoundsRect;
  InflateRect(tr,-BorderWidth,-BorderWidth);
  with tr do
  begin
    x := x+w-h;
    w := h;
  end;

  if not PointInRect(SDLPoint(AX,AY),tr) then
    DrawFocus;
end;

procedure TSDLComboBox.MouseUp(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
var tr : TSDL_Rect;
begin
  inherited;
  if not HasFocus or(Button<>1) then Exit;
  tr := BoundsRect;
  InflateRect(tr,-BorderWidth,-BorderWidth);
  with tr do
  begin
    x := x+w-h;
    w := h;
  end;

  if PointInRect(SDLPoint(AX,AY),tr) then
    DroppedDown := not FDroppedDown
  else
    DroppedDown := False;
end;

procedure TSDLComboBox.MouseWheel(Dir: Integer; Modifier: TSDLMod; AX, AY: Integer);
begin
  inherited;
  if FDroppedDown then
    DropList.MouseWheel(Dir, Modifier, AX, AY);
end;

procedure TSDLComboBox.SetDDColor(const Value: Cardinal);
begin
  DropList.Color := Value;
end;

procedure TSDLComboBox.SetDDCount(Value: Byte);
begin
  if Value=0 then Value := 1;
  FDropDownCount := Value;
  DropList.ScrollBar.LargeChange := Value-1;
  with DropList.ScrollBar do
    Max := FItems.Count-LargeChange-1;
  DropList.ScrollBar.Visible := FItems.Count>DropDownCount;
end;

procedure TSDLComboBox.SetDroppedDown(Value: Boolean);
begin
  if FDroppedDown=Value then Exit;
  FDroppedDown := Value;
  if Value then
    DropList.DropDown
  else
    DropList.Visible := False;
end;

procedure TSDLComboBox.SetFont(const Value: TSDLFont);
begin
  inherited;
  FItemHeight := Font.TextHeight('O')-1;
end;

procedure TSDLComboBox.SetItemIndex(Value: Integer);
begin
  if FItemIndex=Value then Exit;

  if (Value<FItems.Count)or FLoading then
    FItemIndex := Value
  else
    FItemIndex := -1;
  Change;
end;

procedure TSDLComboBox.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TSDLComboBox.SetSelColor(const Value: Cardinal);
begin
  DropList.SelColor := Value;
end;

procedure TSDLComboBox.SetText(const Value: string);
begin
  FItemIndex := FItems.IndexOf(Value);
end;

initialization
  SDLRegisterClasses([TSDLComboBox,TSDLDropList]);

end.
