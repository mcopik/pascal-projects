unit SDLEdit;

{*********************************************************************
             SDLEdit v1.0b -  03.10.2004.
             
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
Description: TSDLEdit just like TEdit (except for right click menu).

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, sdlgui, SDLFont, SDLDraw;

type
  TSDLEdit = class(TSDLStdControl)
  private
    FTime : Word;
    FBlink : Boolean;
    FTextPos : TPoint;
    FMaxLength : Word;
    FText : String;
    FSelText : String;
    FSelStart : Word;
    FSelEnd : Word;
    FSelLength: Word;
    FReadOnly: Boolean;
    FAutoSelect: Boolean;
    FHideSelection: Boolean;
    FSelColor: Cardinal;
    FCursorColor: Cardinal;
    FOnChange: TSDLNotifyEvent;
    procedure CalcTextPos;
    function ConvCurPos(AX: Integer): Word;
    procedure SetMaxLength(const Value: Word);
    procedure SetSelLength(const Value: Word);
    procedure SetSelStart(const Value: Word);
    procedure SetSelText(const Value: String);
  protected
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;
    procedure SetText(const Value: String); virtual;
    procedure DoDraw; override;
    procedure DoAnimate(MoveCount: Integer); override;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY: Integer); override;
    procedure KeyDown(var Key: Word;Modifier: TSDLMod); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure SelectAll;
    function SetFocus: Boolean; override;
    property SelText: String read FSelText write SetSelText;
    property SelStart: Word read FSelStart write SetSelStart;
    property SelLength: Word read FSelLength write SetSelLength;
  published
    property Text: String read FText write SetText;
    property MaxLength:Word read FMaxLength write SetMaxLength default $FFFF;
    property AutoSelect : Boolean read FAutoSelect write FAutoSelect default False;
    property HideSelection : Boolean read FHideSelection write FHideSelection default True;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property SelColor: Cardinal read FSelColor write FSelColor;
    property CursorColor : Cardinal read FCursorColor write FCursorColor;
    property OnChange: TSDLNotifyEvent read FOnChange write FOnChange;
  end;

var Clipboard : string;

implementation

function Min(X,Y: Integer): Integer;
begin
  if X<Y then Result := X
  else Result := Y;
end;

{ TSDLEdit }

procedure TSDLEdit.CalcTextPos;
var CursorPos,cpb,cpe,tw : Integer;
begin
  FTime := 500;

  tw := Font.TextWidth(Copy(FText,1,FSelStart));

  CursorPos := FTextPos.X+tw;
  cpb := BorderWidth+1;
  cpe := Width-BorderWidth shl 1-1;

  if CursorPos > cpe then
    FTextPos.X := cpe-tw
  else if CursorPos<cpb then
    FTextPos.X := cpb-tw;
end;

function TSDLEdit.ConvCurPos(AX: Integer): Word;
var cpos,tw : Integer;
    tx,min,min1 : Integer;
    S : ShortInt;
begin
  if Length(FText)=0 then
  begin
    Result := 0;
    Exit;
  end;
  tw := Font.TextWidth(FText);

  Dec(AX,WorldX);
  if AX>FTextPos.X+tw then
    Result := Length(FText)
  else if AX<FTextPos.X then
    Result := 0
  else
  begin
    cpos := Length(FText)*(AX-FTextPos.X) div tw;
    tx := FTextPos.X + Font.TextWidth(Copy(FText,1,cpos));

    if tx<AX then S := 1
    else S := -1;

    min1 := abs(AX-tx);

    repeat
      min := min1;
      Inc(cpos,S);
      tx := FTextPos.X+Font.TextWidth(Copy(FText,1,cpos));
      min1 := abs(AX-tx);
    until (min1>=min)or(cpos>Length(FText));

    Result := cpos-S;
  end;
end;

constructor TSDLEdit.Create(AParent: TSDLObject);
begin
  inherited;
  FMaxLength := $FFFF;
  SelColor := $8000;
  CursorColor := 0;
  HideSelection := True;
  AlwaysDown := True;
  Width := 128;
  Height := 30;
end;

procedure TSDLEdit.DoAnimate(MoveCount: Integer);
begin
  inherited;
  FTime := (FTime + MoveCount) mod 1000;
  FBlink := (FTime>500);
end;

procedure TSDLEdit.DoDraw;
  procedure ClipRect(var RectC: TSDL_Rect; const CRect: TSDL_Rect);
  begin
    with RectC do
    begin
      if CRect.x>x then
      begin
        w := w-CRect.x+x;
        x := CRect.x;
      end;
      if CRect.x+CRect.w<x+w then
        w := CRect.x+CRect.w-x;
    end;
  end;

var txtRect, SelRect: TSDL_Rect;
    tx,ty,ss : Integer;
begin
  inherited;
  txtRect := BoundsRect;
  tx := txtRect.x+FTextPos.x;
  ty := txtRect.y+FTextPos.y;
  InflateRect(txtRect,-BorderWidth,-BorderWidth);

  if FText<>'' then
  begin
    if (FSelLength>0)and(HasFocus or not HideSelection) then
    begin
      ss := Min(FSelStart,FSelEnd);
      with SelRect do
      begin
        x := tx+Font.TextWidth(Copy(FText,1,ss));
        w := Font.TextWidth(FSelText);
        y := ty+2;
        h := (txtRect.y-y) shl 1+txtRect.h+1;
      end;
      ClipRect(SelRect,txtRect);
      SDLScreen.FillRect(SDLScreen.Surface,SelRect,SelColor);
    end;
    Font.TextRectA(SDLScreen.Surface,txtRect,tx,ty,FText);
  end;

  if HasFocus and FBlink then  //Draw Cursor
  begin
    with txtRect do
    begin
      x := tx-1+Font.TextWidth(Copy(FText,1,FSelStart));
      w := 2;
      h := (y-ty-1)shl 1+h;
      y := ty+1;
    end;
    SDLScreen.FillRect(SDLScreen.Surface,txtRect,CursorColor);
  end;
end;

procedure TSDLEdit.KeyDown(var Key: Word; Modifier: TSDLMod);
var Temp,IsShift : Boolean;
    ss,SelLen : Word;
begin
  inherited;

  Temp := True;
  IsShift := (KMOD_SHIFT and Modifier>0);

  if FSelLength=0 then
  begin
    SelLen := 1;
    ss := FSelStart;
  end
  else
  begin
    if Key=SDLK_BACKSPACE then
      Key := SDLK_DELETE;
    SelLen := FSelLength;
    ss := Min(FSelStart,FSelEnd);
  end;

  case Key of
  SDLK_LEFT:
    if FSelStart>0 then
    begin
      if IsShift and(FSelLength=0)then
        FSelEnd := FSelStart;
      Dec(FSelStart);
    end;
  SDLK_RIGHT:
    if FSelStart<Length(FText) then
    begin
      if IsShift and(FSelLength=0)then
        FSelEnd := FSelStart;
      Inc(FSelStart);
    end;
  SDLK_HOME:
    if FSelStart>0 then
    begin
      if IsShift and(FSelLength=0)then
        FSelEnd := FSelStart;
      FSelStart := 0;
    end;
  SDLK_END:
    if FSelStart<Length(FText) then
    begin
      if IsShift and(FSelLength=0)then
        FSelEnd := FSelStart;
      FSelStart := Length(FText);
    end;
  SDLK_BACKSPACE:
    if (FSelStart>0)and not ReadOnly then
    begin
      Delete(FText,ss,SelLen);
      FSelStart := ss-1;
    end;
  SDLK_DELETE:
    if not ReadOnly then
    begin
      Delete(FText,ss+1,SelLen);
      FSelStart := ss;
    end;
  else
    Temp := False;
  end;

  if (KMOD_Ctrl and Modifier>0)and not Temp then
  begin
    Temp := True;
    if Key=SDLK_A then
    begin
      FSelEnd := 0;
      FSelStart := Length(Text);
      IsShift := True;
    end
    else if not ReadOnly then
      case Key of
      SDLK_X: if FSelLength>0 then
           begin
             Clipboard := SelText;
             Delete(FText,ss+1,SelLen);
             FSelStart := ss;
           end;
      SDLK_C: if FSelLength>0 then
           begin
             Clipboard := SelText;
             Temp := False;
           end;
      SDLK_V:
           begin
             if FSelLength>0 then
             begin
               Delete(FText,ss+1,SelLen);
               SelLength := 0;
             end;
             Insert(ClipBoard,FText,ss+1);
             FSelStart := ss+Length(Clipboard);
           end;
      else
        Temp := False;
      end
    else
      Temp := False;
  end;

  if Temp then
  begin
    Text := FText;

    if isShift then
    begin
      FSelLength := Abs(FSelEnd-FSelStart);
      FSelText := Copy(FText,Min(FSelStart,FSelEnd)+1,FSelLength);
    end
    else
      SelLength := 0;

    Key := 0;
    FTime := 500;
  end;
end;

procedure TSDLEdit.KeyPress(var Key: Char);
const
  Characters='`1234567890-=\qwertyuiop[]asdfghjkl;''zxcvbnm,./~!@#$%^&*()_+|QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>? ';
begin
  inherited;
  if ReadOnly then Exit;

  if Pos(Key,Characters)>0 then
  begin
    if FSelLength>0 then
    begin
      if FSelStart>FSelEnd then
        FSelStart := FSelEnd;
      Delete(FText,FSelStart+1,FSelLength);
      SelLength := 0;
    end;
    if Length(FText)<FMaxLength then
    begin
      Inc(FSelStart);
      Insert(Key,FText,FSelStart);
      Text := FText;
    end;
    Key := #0;
  end;
end;

procedure TSDLEdit.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  if not HasFocus then Exit;
  
  if (Button=1) then
  begin
    SelLength := 0;
    FSelStart := ConvCurPos(AX);
    CalcTextPos;
  end;
end;

procedure TSDLEdit.MouseMove(Modifier: TSDLMod; AX, AY: Word);
var tx : Integer;
begin
  inherited;

  if KMOD_MBLEFT and Modifier>0 then
  begin
    if FSelLength=0 then
      FSelEnd := FSelStart;
    tx := WorldX;
    if AX<tx+BorderWidth then
    begin
      if FSelStart>0 then
      begin
        Dec(FSelStart);
        CalcTextPos;
      end;
    end
    else if AX>tx+Width-BorderWidth then
    begin
      if FSelStart<Length(FText) then
      begin
        Inc(FSelStart);
        CalcTextPos;
      end;
    end
    else
      FSelStart := ConvCurPos(AX);
    FSelLength := Abs(FSelEnd-FSelStart);
    FSelText := Copy(FText,Min(FSelStart,FSelEnd)+1,FSelLength);
  end;
end;

procedure TSDLEdit.SelectAll;
begin
  FSelStart := 0;
  SelLength := Length(FText);
  CalcTextPos;
end;

function TSDLEdit.SetFocus: Boolean;
begin
  Result := inherited SetFocus;
  if Result and AutoSelect then
    SelectAll;
end;

procedure TSDLEdit.SetHeight(const Value: Integer);
var r: TSDL_Rect;
    th : Integer;
begin
  inherited;

  r := BoundsRect;
  th := Font.TextHeight('M');
  FTextPos.y := (r.h-th)div 2;
end;

procedure TSDLEdit.SetMaxLength(const Value: Word);
begin
  FMaxLength := Value;
  if Length(Text)>FMaxLength then
    Text := Copy(FText,1,FMaxLength);
end;

procedure TSDLEdit.SetSelLength(const Value: Word);
begin
  if FSelLength=Value then Exit;

  FSelEnd := FSelStart;
  Inc(FSelStart,Value);
  if FSelStart>Length(FText) then
    FSelStart := Length(Text);

  FSelLength := Value;
  if FSelLength=0 then FSelText := ''
  else FSelText := Copy(FText,FSelEnd+1,FSelLength);
end;

procedure TSDLEdit.SetSelStart(const Value: Word);
begin
  if SelStart=Value then Exit;
  SelLength := 0;
  FSelStart := Value;
end;

procedure TSDLEdit.SetSelText(const Value: String);
var ss : Integer;
begin
  if (FSelText='')or(FSelText=Value) then Exit;
  ss := Min(FSelStart,FSelEnd)+1;
  Delete(FText,ss,FSelLength);
  Insert(Value,FText,ss);
  Text := FText;
  FSelStart := ss-1;
  SelLength := Length(Value);
end;

procedure TSDLEdit.SetText(const Value: String);
begin
  if FText<>Value then
  begin
    SelLength := 0;
    FText := Value;
    if FSelStart>Length(FText) then
      FSelStart := Length(FText);
  end;
  CalcTextPos;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TSDLEdit.SetWidth(const Value: Integer);
begin
  inherited;
  CalcTextPos;
end;

initialization
  SDLRegisterClasses([TSDLEdit]);

end.
