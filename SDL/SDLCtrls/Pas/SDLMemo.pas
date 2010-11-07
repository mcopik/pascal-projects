unit SDLMemo;

{*********************************************************************
             SDLMemo v1.0b -  03.10.2004.
             
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
Description: TSDLMemo.

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, sdlutils, sdlgui, SDLFont, SDLScroll, SDLDraw;

type
  TSDLMemo = class(TSDLScrollControl)
  private
    FTime : Word;
    FBlink : Boolean;
    FCursorPos : TPoint;
    FWrapManager: TWrapManager;
    FReadOnly: Boolean;
    FCursorColor: Cardinal;
    procedure WrapChange(Sender: TObject);
    function PosToText: Integer;
    procedure TextToPos(TxtPos: Integer);
    function GetLines(Index: Integer): string;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetFont(const Value: TSDLFont); override;
    procedure DoDraw; override;
    procedure DoAnimate(MoveCount: Integer); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property Lines[Index: Integer]: string read GetLines;
  published
    property Text: string read GetText write SetText;
    property CursorColor : Cardinal read FCursorColor write FCursorColor;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;

implementation

uses SysUtils, Classes;

{ TSDLMemo }

constructor TSDLMemo.Create(AParent: TSDLObject);
begin
  inherited;
  AlwaysDown := True;
  Width := 128;
  Height := 128;
end;

destructor TSDLMemo.Destroy;
begin
  FWrapManager.Free;
  inherited;
end;

procedure TSDLMemo.DoAnimate(MoveCount: Integer);
begin
  inherited;
  FTime := (FTime + MoveCount) mod 1000;
  FBlink := (FTime>500);
end;

procedure TSDLMemo.DoDraw;
var Rect,curRect : TSDL_Rect;
    t,th : Integer;
begin
  inherited;
  Rect := BoundsRect;
  InflateRect(Rect,-BorderWidth,-BorderWidth);
  th := Font.TextHeight('O');
  if FWrapManager.WrapingText='' then
  begin
    Rect.w := 2;
    Rect.h := th;
    if HasFocus and FBlink then
      SDLScreen.FillRect(SDLScreen.Surface,Rect,CursorColor);
    Exit;
  end;
  Rect.w := Rect.w - ScrollBar.Width-2;
  curRect := Rect;
  Inc(Rect.x,2);
  t := Rect.y-ScrollBar.Position*th;
  FWrapManager.TextRectA(SDLScreen.Surface, Rect, Rect.x, t);
  
  if HasFocus and FBlink then
  begin
    with Rect,FWrapManager do
    begin
      x := x-1+Font.TextWidth(Copy(WrapedLines[FCursorPos.Y],1,FCursorPos.X));
      w := 2;
      y := t+th*FCursorPos.Y;
      h := th;
    end;
    if RectInRect(Rect,curRect) then
      SDLScreen.FillRect(SDLScreen.Surface,Rect,CursorColor);
  end;
end;

function TSDLMemo.GetLines(Index: Integer): string;
begin
  Result := FWrapManager.WrapedLines[Index];
end;

function TSDLMemo.GetText: string;
begin
  Result := FWrapManager.WrapingText;
end;

procedure TSDLMemo.KeyDown(var Key: Word; Modifier: TSDLMod);
var T: Boolean;
    txt: string;
    txtpos: Integer;
begin
  if ReadOnly and (Key in [SDLK_BACKSPACE,SDLK_DELETE,SDLK_TAB]) then
    Exit;
  txt := FWrapManager.WrapingText;
  if (txt='')and(Key<>SDLK_RETURN) then Exit;
  T := True;
  case Key of
  SDLK_LEFT:
    if FCursorPos.x>0 then
      Dec(FCursorPos.x)
    else if FCursorPos.y>0 then
    begin
      Dec(FCursorPos.y);
      with FWrapManager do
        FCursorPos.x := Length(WrapedLines[FCursorPos.y]);
    end;
  SDLK_RIGHT:
    with FWrapManager do
      if FCursorPos.x<Length(WrapedLines[FCursorPos.y])then
        Inc(FCursorPos.x)
      else if FCursorPos.Y<LinesCount-1 then
      begin
        FCursorPos.x := 0;
        Inc(FCursorPos.y);
      end;
  SDLK_UP:
    if FCursorPos.y>0 then
      Dec(FCursorPos.y);
  SDLK_DOWN:
    with FWrapManager do
      if FCursorPos.Y<LinesCount-1 then
        Inc(FCursorPos.y);
  SDLK_HOME: FCursorPos.x := 0;
  SDLK_END: FCursorPos.x := Length(FWrapManager.WrapedLines[FCursorPos.y]);
  SDLK_PAGEUP:
    begin
      Dec(FCursorPos.y,ScrollBar.LargeChange);
      if FCursorPos.y<0 then FCursorPos.y := 0;
    end;
  SDLK_PAGEDOWN:
    begin
      Inc(FCursorPos.y,ScrollBar.LargeChange);
      with FWrapManager do
        if FCursorPos.y>=LinesCount then FCursorPos.y := LinesCount-1;
    end;
  SDLK_BACKSPACE:
    begin
      if FCursorPos.x>0 then
        Dec(FCursorPos.x)
      else if FCursorPos.y>0 then
      begin
        Dec(FCursorPos.y);
        with FWrapManager do
          FCursorPos.x := Length(WrapedLines[FCursorPos.y]);
      end
      else
        T := False;
      if T then
      begin
        txtpos := PosToText;
        if txt[txtpos]=#13 then
          Delete(txt,txtpos,2)
        else
          Delete(txt,txtpos,1);
        FWrapManager.WrapingText := txt;
        TextToPos(txtpos-1);
      end
      else T := True;
    end;
  SDLK_DELETE:
    with FWrapManager do
      if (FCursorPos.x<Length(WrapedLines[FCursorPos.y]))or
         (FCursorPos.y<LinesCount-1) then
      begin
        txtpos := PosToText;
        if txt[txtpos]=#13 then
          Delete(txt,txtpos,2)
        else
          Delete(txt,txtpos,1);
        FWrapManager.WrapingText := txt;
        TextToPos(txtpos-1);
      end;
  SDLK_RETURN:
    begin
      txtpos := PosToText;
      Insert(#13#10,txt,txtpos);
      FWrapManager.WrapingText := txt;
      TextToPos(txtpos+1);
      FCursorPos.x := 0;
    end;
  SDLK_TAB:
    begin
      txtpos := PosToText;
      if Font is TSDLRasterFont then
        Insert(#9,txt,txtpos)
      else
      begin
        Insert('     ',txt,txtpos);
        Inc(txtpos,4);
      end;
      FWrapManager.WrapingText := txt;
      TextToPos(txtpos);
    end;
  else
    T := False;
  end;

  if T then
  begin
    if FWrapManager.WrapedLines.Count>0 then
      txtpos := Length(FWrapManager.WrapedLines[FCursorPos.y])
    else txtpos := 0;
    if FCursorPos.x>txtpos then
      FCursorPos.x := txtpos;
    with ScrollBar do
      if FCursorPos.y<Position then
        Position := FCursorPos.y
      else if FCursorPos.y>Position+LargeChange-1 then
        Position := FCursorPos.y-LargeChange+1;
    FTime := 501;
    Key := 0
  end;

  inherited;
end;

procedure TSDLMemo.KeyPress(var Key: Char);
const
  Characters='`1234567890-=\qwertyuiop[]asdfghjkl;''zxcvbnm,./~!@#$%^&*()_+|QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>? ';
var txtpos: Integer;
    txt : string;
begin
  inherited;
  if ReadOnly or (AnsiPos(Key,Characters)=0) then Exit;

  txt := FWrapManager.WrapingText;
  if txt='' then txtpos := 1
  else txtpos := PosToText;
  Insert(Key,txt,txtpos);
  FWrapManager.WrapingText := txt;
  TextToPos(txtpos);
  with ScrollBar do
    if FCursorPos.y<Position then
      Position := FCursorPos.y
    else if FCursorPos.y>Position+LargeChange-1 then
      Position := FCursorPos.y-LargeChange+1;
end;

procedure TSDLMemo.LoadFromFile(const FileName: string);
begin
  FWrapManager.WrapedLines.LoadFromFile(FileName);
  FWrapManager.WrapingText := FWrapManager.WrapedLines.Text;
end;

procedure TSDLMemo.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
var tx,cpos,min1,min,tw,xpos,S : Integer;
    txt : String;
begin
  inherited;
  if not HasFocus or(Button<>1) then Exit;
  if FWrapManager.WrapingText='' then Exit;

  FCursorPos.Y := (AY-WorldY)div Font.TextHeight('O');
  Inc(FCursorPos.Y,ScrollBar.Position);
  if FCursorPos.Y>=FWrapManager.LinesCount then
    FCursorPos.Y := FWrapManager.LinesCount-1;
  xpos := WorldX+BorderWidth+2;
  txt := FWrapManager.WrapedLines[FCursorPos.Y];
  tw := Font.TextWidth(txt);

  if AX>=xpos+tw then
    FCursorPos.X := Length(txt)
  else if AX<=xpos then
    FCursorPos.X := 0
  else
  begin
    cpos := Length(txt)*(AX-xpos) div tw;
    tx := xpos + Font.TextWidth(Copy(txt,1,cpos));

    if tx<AX then S := 1
    else S := -1;

    min1 := abs(AX-tx);
    repeat
      min := min1;
      Inc(cpos,S);
      tx := xpos+Font.TextWidth(Copy(txt,1,cpos));
      min1 := abs(AX-tx);
    until (min1>min)or(cpos>Length(txt))or(cpos<=0);

    FCursorPos.X := cpos-S;
    FTime := 501;
  end;
end;

function TSDLMemo.PosToText: Integer;
var i: Integer;
    txt: string;
begin
  Result := 0;
  txt := FWrapManager.WrapingText;
  with FWrapManager do
    for i := 0 to FCursorPos.y-1 do
    begin
      Result := Result+Length(WrapedLines[i]);
      if txt[Result+1]=#13 then Inc(Result,2);
    end;
  Result := Result+FCursorPos.x+1;
  if Result=0 then Result := 1;
end;

procedure TSDLMemo.SaveToFile(const FileName: string);
var cet: TStringList;
begin
  cet := TStringList.Create;
  cet.Text := Text;
  cet.SaveToFile(FileName);
  cet.Free;
end;

procedure TSDLMemo.SetFont(const Value: TSDLFont);
var txt: String;
begin
  inherited;
  if Assigned(FWrapManager) then
    txt := FWrapManager.WrapingText
  else
    txt := '';
  FWrapManager.Free;
  FWrapManager := TWrapManager.Create(Value);
  FWrapManager.TrimLines := False;
  if Width > 0 then
    FWrapManager.WrapWidth := Width-BorderWidth shl 1-ScrollBar.Width;
  FWrapManager.OnChange := WrapChange;
  if Height > 0 then
    ScrollBar.LargeChange := (Height-BorderWidth shl 1)div Font.TextHeight('O');
  FWrapManager.WrapingText := txt;
end;

procedure TSDLMemo.SetHeight(const Value: Integer);
var t: Integer;
begin
  inherited;
  if Assigned(Font) then
    t := Font.TextHeight('O')
  else t := 10;
  ScrollBar.LargeChange := (Height-BorderWidth shl 1)div t;
end;

procedure TSDLMemo.SetText(const Value: string);
begin
  FWrapManager.WrapingText := Value;
end;

procedure TSDLMemo.SetWidth(const Value: Integer);
begin
  inherited;
  if Assigned(FWrapManager) then
    FWrapManager.WrapWidth := Width-BorderWidth shl 1-ScrollBar.Width;
end;

procedure TSDLMemo.TextToPos(TxtPos: Integer);
var txt: string;
    p,i: Integer;
begin
  txt := FWrapManager.WrapingText;
  if txt = '' then
  begin
    FCursorPos.x := 0;
    FCursorPos.y := 0;
    Exit;
  end;
  p := AnsiPos(#13,txt);
  while (p>0)and(p<txtpos-1) do
  begin
    Delete(txt,p,2);
    Dec(TxtPos,2);
    p := AnsiPos(#13,txt);
  end;
  p := Length(FWrapManager.WrapedLines[0]);
  i := 0;
  while (p<TxtPos)and(i<FWrapManager.WrapedLines.Count-1) do
  begin
    Inc(i);
    p := p+Length(FWrapManager.WrapedLines[i]);
  end;
  FCursorPos.y := i;
  if txtpos>2 then
    if txt[txtpos-1]=#13 then Dec(txtpos,2);
  p := p-TxtPos;
  FCursorPos.x := Length(FWrapManager.WrapedLines[i])-p;
end;

procedure TSDLMemo.WrapChange(Sender: TObject);
begin
  if Assigned(ScrollBar) then
    With ScrollBar do
      Max := FWrapManager.LinesCount-LargeChange;
end;

initialization
  SDLRegisterClasses([TSDLMemo]);

end.
