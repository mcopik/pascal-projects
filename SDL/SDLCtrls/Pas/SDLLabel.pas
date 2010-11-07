unit SDLLabel;

{*********************************************************************
             SDLLabel v1.0b -  03.10.2004.
             
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
Description: TSDLLabel.

If you have any suggestions or you made some modifications
please inform me.
**********************************************************************}

interface

uses sdl, sdlgui, SDLDraw, SDLFont;

type
  TSDLLabel = class(TSDLComponent)
  private
    FWrapManager: TWrapManager;
    FCaption: String;
    FWordWrap: Boolean;
    FAlignment: TSDLAlignment;
    FLayout: TSDLTextLayout;
    FAutoSize: Boolean;
    procedure SetCaption(const Value: String);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
  protected
    procedure SetWidth(const Value: Integer); override;
    procedure SetBorderWidth(const Value: Byte); override;
    procedure DoDraw; override;
    procedure StartLoading; override;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
  published
    property WordWrap:Boolean read FWordWrap write SetWordWrap;
    property Caption: String read FCaption write SetCaption;
    property Alignment: TSDLAlignment read FAlignment write FAlignment;
    property Layout: TSDLTextLayout read FLayout write FLayout;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
  end;

implementation

{ TSDLLabel }

constructor TSDLLabel.Create(AParent: TSDLObject);
begin
  inherited;
  BorderWidth := 0;
  Alignment := taCenter;
  Layout := tlCenter;
  Caption := 'Label';
  Width := Font.TextWidth(Caption);
  Height := Font.TextHeight(Caption);
end;

destructor TSDLLabel.Destroy;
begin
  inherited;
  FWrapManager.Free;
end;

procedure TSDLLabel.DoDraw;
var txtRect: TSDL_Rect;
begin
  inherited;

  if FCaption='' then Exit;

  txtRect := BoundsRect;
  InflateRect(txtRect,-BorderWidth,-BorderWidth);

  Font.SetTempAlign(FAlignment, FLayout);
  if FWordWrap then
    FWrapManager.TextRect(SDLScreen.Surface,txtRect)
  else
    Font.TextRect(SDLScreen.Surface,txtRect,FCaption);
  Font.RestoreAlignment;
end;

procedure TSDLLabel.SetAutoSize(const Value: Boolean);
var tr: TSDL_Rect;
begin
  if FAutoSize=Value then Exit;
  FAutoSize := Value;
  if not FAutoSize then Exit;
  tr := Font.TextExtent(FCaption);
  Height := tr.h;
  Width := tr.w;
end;

procedure TSDLLabel.SetBorderWidth(const Value: Byte);
begin
  inherited;
  if FWordWrap then
    FWrapManager.WrapWidth := Width-BorderWidth;
end;

procedure TSDLLabel.SetCaption(const Value: String);
var tr: TSDL_Rect;
begin
  if Value=FCaption then Exit;
  FCaption := Value;
  if FWordWrap then
    FWrapManager.WrapingText := FCaption
  else if FAutoSize then
  begin
    tr := Font.TextExtent(FCaption);
    Height := tr.h;
    Width := tr.w;
  end;
end;

procedure TSDLLabel.SetWidth(const Value: Integer);
begin
  inherited;
  if FWordWrap then
    FWrapManager.WrapWidth := Width-BorderWidth;
end;

procedure TSDLLabel.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap=Value then Exit;

  FWordWrap := Value;
  if FWordWrap then
  begin
    FWrapManager := TWrapManager.Create(Font);
    FWrapManager.WrapWidth := Width-BorderWidth;
    FWrapManager.WrapingText := FCaption;
  end
  else
  begin
    FWrapManager.Free;
    FWrapManager := nil;
  end;
end;

procedure TSDLLabel.StartLoading;
begin
  inherited;
  Caption := '';
end;

initialization
  SDLRegisterClasses([TSDLLabel]);

end.
