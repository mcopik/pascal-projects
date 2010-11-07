unit SDLScroll;

{*********************************************************************
             SDLScroll v1.0b -  03.10.2004.
             
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
Description: TSDLScrollBar and base classes for scrolling components
             and controls.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, sdlgui, SDLDraw;

type
  TSDLBarState = (sbNone,sbLnUp,sbLnDown,sbPgUp,sbPgDown,sbTracking);

  TSDLScrollBar = class(TSDLComponent)
  private
    FTime: Word;
    FDoAuto: Boolean;
    FState : TSDLBarState;
    FSmallSide: PInteger;
    FBigSide: PInteger;
    FBigCoord: Word;
    FSliderPos: Word;
    FMin: Integer;
    FPosition: Integer;
    FMax: Integer;
    FKind: TSDLBarKind;
    FJumping: Boolean;
    FButtonColor: Cardinal;
    FBackColor: Cardinal;
    FOnChange: TSDLNotifyEvent;
    FLargeChange: Word;
    FSmallChange: Word;
    FWheelChange: Word;
    procedure CalcSliderPos;
    procedure CheckClick(C: Integer);
    procedure DrawSelf;
    procedure SetKind(const Value: TSDLBarKind);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
  protected
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetImage(Value: TSDLImage); override;
    procedure DoDraw; override;
    procedure DoAnimate(MoveCount: Integer); override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure SetParams(const Pos,Min,Max: Integer);
  published
    property Kind: TSDLBarKind read FKind write SetKind;
    property Jumping: Boolean read FJumping write FJumping default False;
    property OnChange : TSDLNotifyEvent read FOnChange write FOnChange;
    property SmallChange: Word read FSmallChange write FSmallChange;
    property LargeChange: Word read FLargeChange write FLargeChange;
    property WheelChange: Word read FWheelChange write FWheelChange;
    property BackColor: Cardinal read FBackColor write FBackColor;
    property ButtonColor: Cardinal read FButtonColor write FButtonColor;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition;
  end;

  TSDLScrollComponent = class(TSDLComponent)
  protected
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetBorderWidth(const Value: Byte); override;
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
  published
    ScrollBar: TSDLScrollBar;
  end;

  TSDLScrollControl = class(TSDLStdControl)
  protected
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetBorderWidth(const Value: Byte); override;
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
  published
    ScrollBar: TSDLScrollBar;
  end;

implementation

{ TSDLScrollBar }

procedure TSDLScrollBar.CalcSliderPos;
var Lc : Integer;
    Lp : Integer;
begin
  Lc := FBigSide^-FSmallSide^ * 3;
  if Lc<0 then Exit;
  Lp := FMax-FMin;
  if Lp>0 then
    FSliderPos := Round(Lc/Lp * (FPosition-FMin))
  else
    FSliderPos := 0;
  Inc(FSliderPos,FSmallSide^);
end;

procedure TSDLScrollBar.CheckClick(C: Integer);
begin
  FDoAuto := False;

  if C<=FSmallSide^ then
  begin
    FState := sbLnUp;
    Position := FPosition-SmallChange;
  end
  else if C>=FBigSide^-FSmallSide^ then
  begin
    FState := sbLnDown;
    Position := FPosition+SmallChange;
  end
  else if (C>=FSliderPos) then
    if (C<=FSliderPos+FSmallSide^) then
      FState := sbTracking
    else
    begin
      FState := sbPgDown;
      Position := FPosition+LargeChange;
    end
  else
  begin
    FState := sbPgUp;
    Position := FPosition-LargeChange;
  end;
  if (FState in [sbPgDown,sbPgUp])and Jumping then
  begin
    FState := sbTracking;
    with Gui.SDLMouse do
      Self.MouseMove(0,X,Y);
  end;
end;

constructor TSDLScrollBar.Create(AParent: TSDLObject);
begin
  inherited;
  WheelChange := 3;
  FMax := 100;
  SmallChange := 1;
  LargeChange := 10;
  Kind := sbVertical;
  Width := 20;
  Height := 128;
  BackColor := $DAE1ED;
  ButtonColor := $B4C3DC;
end;

procedure TSDLScrollBar.DoAnimate(MoveCount: Integer);
begin
  inherited;

  if FState in [sbNone,sbTracking] then Exit;

  if not FDoAuto then
  begin
    Inc(FTime,MoveCount);
    FDoAuto := (FTime>350);
    if FDoAuto then FTime := 0;
  end
  else
  begin
    Inc(FTime,MoveCount);
    if FTime<100 then Exit;
    FTime := 0;
    case FState of
    sbLnUp: Position := FPosition-SmallChange;
    sbLnDown: Position := FPosition+SmallChange;
    else
      CheckClick(FBigCoord);
      FDoAuto := True;
    end;
  end;
end;

procedure TSDLScrollBar.DoDraw;
var sr,dr : TSDL_Rect;
    t,f : Integer;
    BigCoord : PInteger;
    OffsetC : ^SmallInt;
begin
  if not Assigned(Image) then
  begin
    DrawSelf;
    Exit;
  end;
  if AnimCount>1 then
    if FMax=FMin then AnimPos := 0
    else AnimPos := AnimCount*(FPosition-FMin)/(FMax-FMin);
  inherited; //Draw Background;

  sr := Image.PatternRects[AnimCount];
  dr := BoundsRect;
  if FKind = sbHorizontal then
  begin
    f := Image.PatternHeight;
    sr.w := f;
    dr.w := FSmallSide^;
    OffsetC := @sr.x;
    BigCoord := @dr.x;
  end
  else
  begin
    f := Image.PatternWidth;
    sr.h := f;
    dr.h := FSmallSide^;
    OffsetC := @sr.y;
    BigCoord := @dr.y;
  end;
  t := BigCoord^;

  //Upper Arrow
  if FState=sbLnUp then OffsetC^ := f;
  Image.StretchDrawRect(SDLScreen.Surface,sr,dr);

  //Lower Arrow
  if FState=sbLnDown then OffsetC^ := 5 * f
  else OffsetC^ := 4 * f;
  BigCoord^ := BigCoord^+FBigSide^-FSmallSide^;
  Image.StretchDrawRect(SDLScreen.Surface,sr,dr);

  //Slider
  if FState=sbTracking then OffsetC^ := 3 * f
  else OffsetC^ := 2 * f;
  BigCoord^ := t+FSliderPos;
  Image.StretchDrawRect(SDLScreen.Surface,sr,dr);
end;

procedure TSDLScrollBar.DrawSelf;
var Button,Base: TSDL_Rect;
    BigCoord: PInteger;
begin
  if (Width<4) or (Height<4) then Exit;
  Base := BoundsRect;
  if not OverlapRect(Base,SDLScreen.ClipRect) then Exit;
  SDLScreen.FillRect(SDLScreen.Surface,Base,BackColor);
  if FKind = sbHorizontal then
  begin
    Base.w := FSmallSide^;
    BigCoord := @Button.x;
  end
  else
  begin
    Base.h := FSmallSide^;
    BigCoord := @Button.y;
  end;
  Button := Base;

  //Upper Arrow
  SDLScreen.Draw3DControl(SDLScreen.Surface,Button,ButtonColor,BorderWidth,FState=sbLnUp);
  InflateRect(Button,-BorderWidth-3,-BorderWidth-3);
  SDLScreen.DrawArrow(SDLScreen.Surface,TArrowDirection(Kind=sbHorizontal),Button,0);
  Button := Base;

  //Lower Arrow
  BigCoord^ := BigCoord^+FBigSide^-FSmallSide^;
  SDLScreen.Draw3DControl(SDLScreen.Surface,Button,ButtonColor,BorderWidth,FState=sbLnDown);
  InflateRect(Button,-BorderWidth-3,-BorderWidth-3);
  SDLScreen.DrawArrow(SDLScreen.Surface,TArrowDirection(Ord(Kind=sbHorizontal)+2),Button,0);
  Button := Base;

  //Slider
  BigCoord^ := BigCoord^+FSliderPos;
  SDLScreen.Draw3DControl(SDLScreen.Surface,Button,ButtonColor,BorderWidth,FState=sbTracking);
end;

procedure TSDLScrollBar.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  if Assigned(ControlParent) and not ControlParent.HasFocus then Exit;
  if (Button<>1)or(FState<>sbNone) then Exit;

  CheckClick(FBigCoord);
end;

procedure TSDLScrollBar.MouseMove(Modifier: TSDLMod; AX, AY: Word);
var Lc,Ld,Pos : Integer;
begin
  inherited;

  if FKind = sbHorizontal then Pos := AX-WorldX
  else Pos := AY-WorldY;
  if Pos<0 then FBigCoord := 0
  else FBigCoord := Pos;

  if FState<>sbTracking then Exit;

  Lc := FBigSide^-FSmallSide^ * 3;
  if Lc<=0 then
  begin
    Position := 0;
    Exit;
  end;
  Ld := FMax-FMin;
  Pos := FBigCoord-FSmallSide^-FSmallSide^ shr 1;
  Position := Round(Ld/Lc*Pos)+FMin;
end;

procedure TSDLScrollBar.MouseUp(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  FState := sbNone;
  FTime := 0;
end;

procedure TSDLScrollBar.MouseWheel(Dir: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  Position := FPosition+Dir*WheelChange;
end;

procedure TSDLScrollBar.SetHeight(const Value: Integer);
begin
  inherited;
  if Image=nil then
    Kind := TSDLBarKind(Width<Height);
  CalcSliderPos;
end;

procedure TSDLScrollBar.SetImage(Value: TSDLImage);
begin
  if Value=nil then
  begin
    inherited;
    Exit;
  end;
  if Value.Height>Value.Width then
    Kind := sbVertical
  else
    Kind := sbHorizontal;
  inherited;
  AnimSpeed := 0;
  AnimLooped := False;
  AnimCount := AnimCount-1;
  CalcSliderPos;
end;

procedure TSDLScrollBar.SetKind(const Value: TSDLBarKind);
begin
  FKind := Value;
  if FKind=sbHorizontal then
  begin
    FSmallSide := @Height;
    FBigSide := @Width;
  end
  else
  begin
    FSmallSide := @Width;
    FBigSide := @Height;
  end;
end;

procedure TSDLScrollBar.SetMax(const Value: Integer);
begin
  if Value<FMin then
    FMax := FMin
  else
    FMax := Value;
  if FPosition>FMax then Position := FMax;
  CalcSliderPos;
end;

procedure TSDLScrollBar.SetMin(const Value: Integer);
begin
  if Value>FMax then
    FMin := FMax
  else
    FMin := Value;
  if FPosition<FMin then Position := FMin;
  CalcSliderPos;
end;

procedure TSDLScrollBar.SetParams(const Pos, Min, Max: Integer);
begin
  if Min<Max then
  begin
    FMin := Min;
    FMax := Max;
  end
  else
  begin
    FMin := Max;
    FMax := Min;
  end;
  Position := Pos;
end;

procedure TSDLScrollBar.SetPosition(const Value: Integer);
begin
  if Value>FMax then FPosition := FMax
  else if Value<FMin then FPosition := FMin
  else FPosition := Value;
  CalcSliderPos;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TSDLScrollBar.SetWidth(const Value: Integer);
begin
  inherited;
  if Image=nil then
    Kind := TSDLBarKind(Width<Height);
  CalcSliderPos;
end;

{ TSDLScrollComponent }

constructor TSDLScrollComponent.Create(AParent: TSDLObject);
begin
  inherited;
  TSDLScrollBar.Create(Self).Name := 'ScrollBar';
  ScrollBar.Y := BorderWidth;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
  ScrollBar.FLockedCoords := True;
end;

procedure TSDLScrollComponent.MouseWheel(Dir: Integer; Modifier: TSDLMod;
  AX, AY: Integer);
begin
  inherited;
  ScrollBar.MouseWheel(Dir,Modifier, AX, AY);
end;

procedure TSDLScrollComponent.SetBorderWidth(const Value: Byte);
begin
  inherited;
  ScrollBar.Y := BorderWidth;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
end;

procedure TSDLScrollComponent.SetHeight(const Value: Integer);
begin
  inherited;
  ScrollBar.Height := Height-BorderWidth shl 1;
end;

procedure TSDLScrollComponent.SetWidth(const Value: Integer);
begin
  inherited;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
end;

{ TSDLScrollControl }

constructor TSDLScrollControl.Create(AParent: TSDLObject);
begin
  inherited;
  TSDLScrollBar.Create(Self).Name := 'ScrollBar';
  ScrollBar.Y := BorderWidth;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
  ScrollBar.FLockedCoords := True;
end;

procedure TSDLScrollControl.MouseWheel(Dir: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  ScrollBar.MouseWheel(Dir,Modifier,AX,AY);
end;

procedure TSDLScrollControl.SetBorderWidth(const Value: Byte);
begin
  inherited;
  ScrollBar.Y := BorderWidth;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
end;

procedure TSDLScrollControl.SetHeight(const Value: Integer);
begin
  inherited;
  ScrollBar.Height := Height-BorderWidth shl 1;
end;

procedure TSDLScrollControl.SetWidth(const Value: Integer);
begin
  inherited;
  ScrollBar.X := Width-BorderWidth-ScrollBar.Width;
end;

initialization
  SDLRegisterClasses([TSDLScrollBar]);

end.
