unit SDLDialogs;

{**********************************************************************
                    SDLDialogs v1.0b -  03.10.2004.
             
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
Description: TSDLMessageDlg and SDLMessageDlg function

***********************************************************************}

interface

uses sdlgui, SDLForm, SDLLabel, SDLButton;

type
  TSDLDlgButton = (dbOK,dbYes,dbNo,dbCancel);
  TSDLDlgButtons = set of TSDLDlgButton;
  TSDLDlgCallBack = procedure(Sender: TSDLForm; Button: TSDLDlgButton)of object;

  TSDLMessageDlg = class(TSDLForm)
  private
    FSelfDestroy: Boolean;
    FCallBack: TSDLDlgCallBack;
    procedure ButtonClick(Sender: TSDLComponent;AX,AY: Integer);
  public
    FMessage: TSDLLabel;
    FbtOK: TSDLButton;
    FbtYes: TSDLButton;
    FbtNo: TSDLButton;
    FbtCancel: TSDLButton;
    procedure Show(CallBack: TSDLDlgCallBack; Buttons: TSDLDlgButtons; const Cpt,Msg: string);
    constructor Create(AParent: TSDLObject); override;
  end;

procedure SDLMessageDlg(CallBack: TSDLDlgCallBack; AGui: TSDLObject;
          Buttons: TSDLDlgButtons; const Cpt,Msg: string);

implementation

uses SDLDraw;

{ TSDLMessageDlg }

procedure TSDLMessageDlg.ButtonClick(Sender: TSDLComponent; AX, AY: Integer);
var cb : TSDLDlgCallBack;
begin
  Modal := False;
  LoseFocus;
  Visible := False;
  cb := FCallBack;
  FCallBack := nil;
  if Assigned(cb) then cb(Self,TSDLDlgButton(Sender.Tag));
  if FSelfDestroy then Kill;
end;

constructor TSDLMessageDlg.Create(AParent: TSDLObject);
var i,n: Integer;
begin
  inherited;
  Visible := False;
  FMessage := TSDLLabel.Create(Self);
  FMessage.WordWrap := True;
  FMessage.X := 5;
  FMessage.Y := TitleBarHeight+5;
  FbtOk := TSDLButton.Create(Self);
  FbtOk.Caption := 'OK';
  FbtYes := TSDLButton.Create(Self);
  FbtYes.Caption := 'Yes';
  FbtNo := TSDLButton.Create(Self);
  FbtNo.Caption := 'No';
  FbtCancel := TSDLButton.Create(Self);
  FbtCancel.Caption := 'Cancel';
  n := 0;
  for i := 0 to ControlCount-1 do
    if Controls[i] is TSDLButton then
      with Controls[i] as TSDLButton do
      begin
        Width := 100;
        Height := 28;
        Tag := n;
        Inc(n);
        OnClick := ButtonClick;
      end;
end;

procedure TSDLMessageDlg.Show(CallBack: TSDLDlgCallBack; Buttons: TSDLDlgButtons;
          const Cpt,Msg: string);
var i,n,p: Integer;
    t: TSDLDlgButton;
begin
  FCallBack := CallBack;
  Visible := True;
  Modal := True;
  SetFocus;
  Caption := Cpt;
  Width := SDLScreen.SurfaceRect.w div 2;
  Height := SDLScreen.SurfaceRect.h div 4;
  X := (SDLScreen.SurfaceRect.w-Width)div 2;
  Y := (SDLScreen.SurfaceRect.h-Height)div 2;
  FMessage.Width := Width-10;
  FMessage.Height := Height-50;
  FMessage.Caption := Msg;
  n := 0;
  for t := dbOK to dbCancel do
    if t in Buttons then Inc(n);
  if n = 0 then
  begin
    Buttons := [dbOK];
    n := 1;
  end;
  p := 0;
  for i := 0 to ControlCount-1 do
    if Controls[i] is TSDLButton then
      with Controls[i] as TSDLButton do
      begin
        Visible := TSDLDlgButton(Tag) in Buttons;
        if not Visible then Continue;
        X := (Self.Width-20)*p div n+15;
        Y := Self.Height - 35;
        Width := (Self.Width-20)div n - 15;
        Inc(p);
      end;
end;

procedure SDLMessageDlg(CallBack: TSDLDlgCallBack; AGui: TSDLObject;
          Buttons: TSDLDlgButtons; const Cpt,Msg: string);
begin
  with TSDLMessageDlg.Create(AGui) do
  begin
    FSelfDestroy := True;
    Show(CallBack,Buttons,Cpt,Msg);
  end;
end;

end.
