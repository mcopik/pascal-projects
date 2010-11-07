unit SDLGauge;

{*********************************************************************
             SDLGauge v1.0b -  03.10.2004.
             
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
Description: TSDLGauge.

If you have any suggestions or you made some modifications
please inform me.
**********************************************************************}

interface

uses sdl, sdlgui, SDLDraw;

type
  TSDLGauge = class(TSDLComponent)
  private
    FProgress: Integer;
    FMaxValue: Integer;
    FMinValue: Integer;
    FKind: TSDLBarKind;
    FShowText: Boolean;
    FBFSpace: Byte;
    FBackColor: Cardinal;
    FForeColor: Cardinal;
    FBorderColor: Cardinal;
    function SolveForY(AX,AZ: Integer): Integer;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetProgress(Value: Integer);
    procedure SetKind(const Value: TSDLBarKind);
  protected
    procedure SetImage(Value: TSDLImage); override;
    procedure DoDraw; override;
  public
    constructor Create(AParent: TSDLObject); override;
  published
    property Progress: Integer read FProgress write SetProgress;
    property MinValue: Integer read FMinValue write SetMin default 0;
    property MaxValue: Integer read FMaxValue write SetMax default 100;
    property Kind: TSDLBarKind read FKind write SetKind;
    property ShowText: Boolean read FShowText write FShowText default True;
    property BorderColor: Cardinal read FBorderColor write FBorderColor;
    property BackColor: Cardinal read FBackColor write FBackColor;
    property ForeColor: Cardinal read FForeColor write FForeColor;
    property BorderForeSpace: Byte read FBFSpace write FBFSpace;
  end;

implementation

uses SysUtils;

{ TSDLGauge }

constructor TSDLGauge.Create(AParent: TSDLObject);
begin
  inherited;
  BorderWidth := 1;
  BorderForeSpace := 2;
  MaxValue := 100;
  ShowText := True;
  BorderColor := $FFFFFF;
  BackColor := $505050;
  ForeColor := $B4C3DC;
  Width := 128;
  Height := 20;
end;

procedure TSDLGauge.DoDraw;
var P : Integer;
  procedure Resize(var Rect: TSDL_Rect);
  var w : Integer;
  begin
    if FKind=sbHorizontal then
    begin
      w := P*Rect.w div 100;
      Rect.w := w;
    end
    else
    begin
      w := P*Rect.h div 100;
      Rect.y := Rect.y+Rect.h-w;
      Rect.h := w;
    end;
  end;

var dest,source: TSDL_Rect;
    S: string;
begin
  inherited;

  P := SolveForY(FProgress-FMinValue,FMaxValue-FMinValue);
  dest := BoundsRect;

  if Assigned(Image) then
  begin
    InflateRect(dest,-BorderWidth,-BorderWidth);
    source := Image.PatternRects[AnimCount+Trunc(AnimPos)];
    InflateRect(source,-BorderWidth,-BorderWidth);
    Resize(source);
    Resize(Dest);
    Image.StretchDrawRect(SDLScreen.Surface,source,dest);
  end
  else
    with SDLScreen do
    begin
      FillRect(Surface,dest,BorderColor);
      InflateRect(Dest,-BorderWidth,-BorderWidth);
      FillRect(Surface,dest,BackColor);
      InflateRect(dest,-BorderForeSpace,-BorderForeSpace);
      Resize(dest);
      FillRect(Surface,dest,ForeColor);
    end;

  if ShowText then
  begin
    dest := BoundsRect;
    S := Format('%d%%',[P]);
    Font.TextRect(SDLScreen.Surface,dest,S);
  end;
end;

procedure TSDLGauge.SetImage(Value: TSDLImage);
begin
  inherited;
  if Width>Height then FKind := sbHorizontal
  else FKind := sbVertical;
  AnimCount := AnimCount div 2;
end;

procedure TSDLGauge.SetKind(const Value: TSDLBarKind);
begin
  if Value=FKind then Exit;
  FKind := Value;
end;

procedure TSDLGauge.SetMax(Value: Integer);
begin
  if Value=FMaxValue then Exit;
  if Value < FMinValue then
    Value := FMinValue+1;
  FMaxValue := Value;
  if FProgress > Value then FProgress := Value;
end;

procedure TSDLGauge.SetMin(Value: Integer);
begin
  if Value > FMaxValue then
    Value := FMaxValue-1;
  FMinValue := Value;
  if FProgress < Value then FProgress := Value;
end;

procedure TSDLGauge.SetProgress(Value: Integer);
begin
  if Value < FMinValue then
    Value := FMinValue
  else if Value > FMaxValue then
    Value := FMaxValue;
  FProgress := Value;
end;

function TSDLGauge.SolveForY(AX, AZ: Integer): Integer;
begin
  if AZ = 0 then Result := 0
  else Result := AX * 100 div AZ;
end;

initialization
  SDLRegisterClasses([TSDLGauge]);

end.
