unit SDLCheck;

{*********************************************************************
             SDLCheck v1.0b -  03.10.2004.
             
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
Description: TSDLCheckBox, TSDLRadioButton, TSDLRadioGroup.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, sdlutils, sdlgui, SDLFont, SDLDraw, Classes;

type
  TSDLCheckBase = class(TSDLStdControl)
  private
    FChecked : Boolean;
    FSignPos : Integer;
    FCaption: String;
    FAlignment: TSDLAlignment;
    FOnChange: TSDLNotifyEvent;
    procedure SetChecked(const Value: Boolean); virtual;
  protected
    procedure SetImage(Value: TSDLImage); override;
    procedure DoDraw; override;
  public
    constructor Create(AParent: TSDLObject); override;
  published
    property Checked:Boolean read FChecked write SetChecked;
    property Caption : String read FCaption write FCaption;
    property Alignment: TSDLAlignment read FAlignment write FAlignment;
    property OnChange: TSDLNotifyEvent read FOnChange write FOnChange;
  end;

  TSDLCheckBox = class(TSDLCheckBase)
  protected
    procedure Click(AX,AY: Integer); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
  end;

  TSDLRadioButton = class(TSDLCheckBase)
  private
    procedure SetChecked(const Value: Boolean); override;
  public
    function SetFocus: Boolean; override;
  end;

  TSDLRadioGroup = class(TSDLControl)
  private
    FItems: TStringList;
    FRadioImage: TSDLImage;
    FCaption: string;
    FItemIndex: Integer;
    FRWidth: Integer;
    FRHeight: Integer;
    FColor: Cardinal;
    FLoadII: Integer;
    procedure RadioChange(Sender: TSDLComponent);
    procedure ItemsChange(Sender: TObject);
    procedure SetItems(const Value: TStringList);
    procedure SetRadioImage(const Value: TSDLImage);
    procedure SetItemIndex(const Value: Integer);
    procedure SetRHeight(const Value: Integer);
    procedure SetRWidth(const Value: Integer);
  protected
    procedure DoDraw; override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
    procedure Loaded; override;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    property RadioImage: TSDLImage read FRadioImage write SetRadioImage;
  published
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: TStringList read FItems write SetItems;
    property Caption: string read FCaption write FCaption;
    property RadioWidth: Integer read FRWidth write SetRWidth;
    property RadioHeight: Integer read FRHeight write SetRHeight;
    property Color: Cardinal read FColor write FColor;
  end;

implementation

{ TSDLCheckBase }

constructor TSDLCheckBase.Create(AParent: TSDLObject);
begin
  inherited;
  Alignment := taRight;
  AlwaysDown := True;
  Width := 20;
  Height := 20;
  Caption := Copy(ClassName,2,Length(ClassName));
end;

procedure TSDLCheckBase.DoDraw;
var tx,ty,TempAS : Integer;
    Size : TSDL_Rect;
begin
  inherited;

  Size := BoundsRect;
  if FChecked then
    if Assigned(Image) then
    begin
      TempAS := AnimStart;
      AnimStart := FSignPos;
      inherited;
      AnimStart := TempAS;
    end
    else
    begin
      InflateRect(Size,-BorderWidth,-BorderWidth);
      Font.TextRect(SDLScreen.Surface,Size,'X');
    end;

  if Caption='' then Exit;

  Size := Font.TextExtent(Caption);

  ty := WorldY+(Height-Size.h)div 2;
  if Alignment=taLeft then
    tx := WorldX-Size.w-3
  else if Alignment=taRight then
    tx := WorldX+Width+3
  else
  begin
    tx := WorldX+(Width-Size.w)div 2;
    ty := ty - Height+3;
  end;
  Font.TextOut(SDLScreen.Surface,tx,ty,Caption);
end;

procedure TSDLCheckBase.SetChecked(const Value: Boolean);
begin
  if FChecked=Value then Exit;
  FChecked := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TSDLCheckBase.SetImage(Value: TSDLImage);
var RowCount : Byte;
begin
  if not Assigned(Value) then
  begin
    inherited;
    Exit;
  end;
  RowCount := Value.Height div Value.PatternHeight;
  if RowCount=1 then Exit;
  inherited;
  if RowCount<5 then
  begin
    Dec(RowCount);
    StateImages := StateImages-[TStateImage(RowCount-1)];
  end;
  FSignPos := AnimCount*RowCount;
end;

{ TSDLCheckBox }

procedure TSDLCheckBox.Click(AX, AY: Integer);
begin
  if not HasFocus or Designing then Exit;
  Checked := not FChecked;
  inherited;
end;

procedure TSDLCheckBox.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_SPACE then
    Checked := not FChecked;
  inherited;
end;

{ TSDLRadioButton }

procedure TSDLRadioButton.SetChecked(const Value: Boolean);
var i : Integer;
    ts : TSDLControl;
begin
  if Value=FChecked then Exit;
  if Value then
    with Parent do
      for i := 0 to ControlCount-1 do
      begin
        ts := Controls[i];
        if (ts is TSDLRadioButton) then
          TSDLRadioButton(ts).FChecked := False;
      end;
  inherited;
end;

function TSDLRadioButton.SetFocus: Boolean;
begin
  Result := inherited SetFocus;
  if Result and not Designing then Checked := True;
end;

{ TSDLRadioGroup }

constructor TSDLRadioGroup.Create(AParent: TSDLObject);
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  FItemIndex := -1;
  Color := $B4C3DC;
  FRHeight := 20;
  FRWidth := 20;
  Width := 80;
  Height := 40;
  Caption := 'RadioGroup';
end;

destructor TSDLRadioGroup.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSDLRadioGroup.DoDraw;
var Size,Rect: TSDL_Rect;
    lc,dc: Cardinal;
begin
  if Assigned(Image) then
  begin
    inherited;
    Exit;
  end;

  Rect := BoundsRect;
  Dec(Rect.w); Dec(Rect.h);
  lc := LightColor(Color,40);
  dc := LightColor(Color,-40);
  SDLScreen.DrawFrame(SDLScreen.Surface,Rect,dc);
  OffsetRect(Rect,1,1);
  SDLScreen.DrawFrame(SDLScreen.Surface,Rect,lc);
  if FCaption='' then Exit;

  Size := Font.TextExtent(FCaption);
  Inc(Size.w);
  Size.x := Rect.x+10;
  Size.y := Rect.y+1-Size.h div 2;
  SDLScreen.FillRect(SDLScreen.Surface,Size,Color);
  Font.TextOut(SDLScreen.Surface,Size.x,Size.y,FCaption);
end;

procedure TSDLRadioGroup.ItemsChange(Sender: TObject);
var i: Integer;
begin
  Clear;
  for i := 0 to FItems.Count-1 do
    with TSDLRadioButton.Create(Self) do
    begin
      Tag := i;
      OnChange := RadioChange;
      Font := Self.Font;
      X := 10;
      Y := 15+(5+FRHeight)*i;
      Width := FRWidth;
      Height := FRHeight;
      Caption := FItems[i];
      if Assigned(FRadioImage) then
        Image := FRadioImage;
      Designing := False;
    end;
  if FItemIndex>=ControlCount then
    ItemIndex := -1;
end;

procedure TSDLRadioGroup.KeyDown(var Key: Word; Modifier: TSDLMod);
var tb1,tb2: Boolean;
begin
  tb1 := (Key = SDLK_LEFT)or(Key = SDLK_UP)or
        (Key = SDLK_TAB)and(KMOD_Shift and Modifier>0);
  tb1 := tb1 and (FItemIndex=0);
  tb2 := (Key = SDLK_RIGHT)or(Key = SDLK_DOWN)or
        (Key = SDLK_TAB)and(KMOD_Shift and Modifier=0);
  tb2 := tb2 and (FItemIndex=FItems.Count-1);
  if not(tb1 or tb2) then
    inherited;
end;

procedure TSDLRadioGroup.Loaded;
begin
  inherited;
  ItemIndex := FLoadII;
end;

procedure TSDLRadioGroup.RadioChange(Sender: TSDLComponent);
begin
  with Sender as TSDLRadioButton do
    FItemIndex := Tag;
  if Assigned(OnClick) then OnClick(Self,WorldX,WorldY);
end;

procedure TSDLRadioGroup.SetItemIndex(const Value: Integer);
var i: Integer;
begin
  if Value<0 then
  begin
    for i := 0 to ControlCount-1 do
      TSDLRadioButton(Controls[i]).SetChecked(False);
    FItemIndex := -1;
  end
  else if Value<FItems.Count then
    TSDLRadioButton(Controls[Value]).SetChecked(True)
  else if FLoading then
    FLoadII := Value;
end;

procedure TSDLRadioGroup.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;

procedure TSDLRadioGroup.SetRadioImage(const Value: TSDLImage);
var i : Integer;
begin
  if FRadioImage=Value then Exit;
  FRadioImage := Value;
  for i := 0 to ControlCount-1 do
    Controls[i].Image := FRadioImage;
end;

procedure TSDLRadioGroup.SetRHeight(const Value: Integer);
var i: Integer;
begin
  if FRHeight = Value then Exit;
  FRHeight := Value;
  for i := 0 to ControlCount-1 do
  begin
    Controls[i].Height := FRHeight;
    Controls[i].Y := 15+(5+FRHeight)*i;
  end;
end;

procedure TSDLRadioGroup.SetRWidth(const Value: Integer);
var i: Integer;
begin
  if FRWidth = Value then Exit;
  FRWidth := Value;
  for i := 0 to ControlCount-1 do
    Controls[i].Width := FRWidth;
end;

initialization
  SDLRegisterClasses([TSDLCheckBox,TSDLRadioButton,TSDLRadioGroup]);

end.
