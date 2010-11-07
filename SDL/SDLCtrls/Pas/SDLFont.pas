unit SDLFont;

{*********************************************************************
             SDLFont v1.0b -  03.10.2004.
             
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
Description: SDL Raster and TTF Font classes + WrapManager
Based on:    Todd Lang's SDLFont

If you have any suggestions or you made some modifications
please inform me.

**********************************************************************}

interface

uses sdl, SDLDraw, sdl_ttf, Classes;

type
  TSDLTextLayout = (tlTop, tlCenter, tlBottom);
  TSDLAlignment = (taLeft, taCenter, taRight);
  TSDLFontStyles = (fsBold,fsItalic,fsUnderline);
  TSDLFontStyle = set of TSDLFontStyles;

  TFontData = record
    Version: Word;
    Height: Integer;
    BackColor: Cardinal;
    FontColor: Cardinal;
    CharacterNo: Word;
  end;

  TSDLFont = class(TCollectionItem)
  private
    FName : string;
    FAlignment: TSDLAlignment;
    FLayout: TSDLTextLayout;
    FTempAlign: TSDLAlignment;
    FTempLayout: TSDLTextLayout;
    FColor: Cardinal;
    FFileName: string;
  protected
    procedure SetColor(const Value: Cardinal); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure TextOut(Target: PSDL_Surface;X,Y: Integer;const Text: string); virtual; abstract;
    procedure TextRect(Target : PSDL_Surface;const Rect: TSDL_Rect;const Text: string);
    procedure TextRectA(Target : PSDL_Surface;Rect: TSDL_Rect;X,Y: Integer;const Text: string);
    function TextExtent(const Text: string): TSDL_Rect; virtual; abstract;
    function TextWidth(const Text: string): Integer; virtual; abstract;
    function TextHeight(const Text: string): Integer; virtual; abstract;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure RestoreAlignment;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SetTempAlign(AAlignment: TSDLAlignment; ALayout: TSDLTextLayout);
    property Name : string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Alignment: TSDLAlignment read FAlignment write FAlignment;
    property Layout: TSDLTextLayout read FLayout write FLayout;
    property Color: Cardinal read FColor write SetColor;
  end;

  TWrapManager = class
  private
    FFont: TSDLFont;
    FWrapingText: string;
    FWrapedLines : TStringList;
    FTrimLines: Boolean;
    FWrapWidth: Word;
    FWrapHeight : Integer;      //Height of a letter
    FMaxWidth : Integer;        //The longest row width
    FWrapWidths : array of Integer;
    FOnChange: TNotifyEvent;
    FWrapToWidth: Boolean;
    procedure CalcWidths;
    procedure WrapUsingWidth;
    procedure SetWrapingText(const Value: string);
    function GetLinesCount: Integer;
    procedure SetWrapWidth(const Value: Word);
    procedure SetWrapToWidth(const Value: Boolean);
  public
    constructor Create(AFont: TSDLFont);
    destructor Destroy; override;
    procedure TextOut(Target : PSDL_Surface;X,Y: Integer);
    procedure TextRect(Target : PSDL_Surface;const Rect: TSDL_Rect);
    procedure TextRectA(Target : PSDL_Surface;const Rect: TSDL_Rect;X,Y: Integer);
    property TrimLines: Boolean read FTrimLines write FTrimLines;
    property WrapToWidth: Boolean read FWrapToWidth write SetWrapToWidth;
    property WrapWidth: Word read FWrapWidth write SetWrapWidth;
    property WrapingText: string read FWrapingText write SetWrapingText;
    property WrapedLines: TStringList read FWrapedLines;
    property LinesCount: Integer read GetLinesCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSDLRasterFont = class(TSDLFont)
  private
    FImage : TSDLImage;
    FCharCount: Byte;
    FCharPos : array of Word;
    FHeight : Byte;
    FCharset: string;
    FReplaceColor: Boolean;
    FTabSpaces: Byte;
  protected
    procedure SetColor(const Value: Cardinal); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Load(const FontName, ImageFile: string);
    function TextExtent(const Text: string): TSDL_Rect; override;
    function TextWidth(const Text: string): Integer; override;
    function TextHeight(const Text: string): Integer; override;
    procedure TextOut(Target : PSDL_Surface;X, Y : Integer;const Text: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property CharSet: string read FCharset write FCharset;
    property ReplaceColor: Boolean read FReplaceColor write FReplaceColor;
    property TabSpaces: Byte read FTabSpaces write FTabSpaces;
  end;

  TSDLTTFFont = class(TSDLFont)
  private
    FFont : PTTF_Font;
    FSize: Byte;
    FStyle: TSDLFontStyle;
    procedure SetSize(const Value: Byte);
    procedure SetStyle(const Value: TSDLFontStyle);
  public
    Shaded: Boolean;
    BackColor: Cardinal;
    destructor Destroy; override;
    procedure Load(const FontName, AFileName: string;PointSize: Integer);
    function TextExtent(const Text: string): TSDL_Rect; override;
    function TextWidth(const Text: string) : Integer; override;
    function TextHeight(const Text: string) : Integer; override;
    procedure TextOut(Target: PSDL_Surface;X,Y: Integer;const Text: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Size: Byte read FSize write SetSize;
    property Style: TSDLFontStyle read FStyle write SetStyle;
  end;

  TSDLFonts = class( TCollection )
  private
    function GetItem(Index :Integer ): TSDLFont;
    procedure SetItem(Index :Integer;const Value: TSDLFont);
  public
    function AddTTFFont : TSDLTTFFont;
    function AddRasterFont : TSDLRasterFont;
    function Insert(Index : Integer) : TSDLFont;
    function ByName(const Name: string) : TSDLFont;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(Stream: TStream);
    property Items[Index : Integer] : TSDLFont read GetItem write SetItem; default;
  end;

const
  RASTERFONTVER = $0100;
  TTFFONTVER = $0100;

{ Most controls demands that you assign them font right after their creation.
  In order to avoid public variables like this one in every unit where you
  create controls I declared one here, so every unit that uses font can assign
  this font to controls it creates. All you need to do is create a default
  font and assign it to this variable before creating any controls.
}
var GlobalFont: TSDLFont;

implementation

uses SysUtils, sdlutils;

var TTFLoaded: Boolean = False;

{ TSDLFont }

constructor TSDLFont.Create(Collection: TCollection);
begin
  inherited;
  FAlignment := taCenter;
  FLayout := tlCenter;
  if GlobalFont=nil then
    GlobalFont := Self;
end;

procedure TSDLFont.LoadFromFile(const AFileName: string);
var Dat: TFileStream;
begin
  Dat := TFileStream.Create(AFileName,fmOpenRead);
  LoadFromStream(Dat);
  Dat.Free;
end;

procedure TSDLFont.RestoreAlignment;
begin
  Alignment := FTempAlign;
  Layout := FTempLayout;
end;

procedure TSDLFont.SaveToFile(const FileName: string);
var Save: TFileStream;
begin
  Save := TFileStream.Create(FFileName,fmOpenRead);
  SaveToStream(Save);
  Save.Free;
end;

procedure TSDLFont.SetColor(const Value: Cardinal);
begin
  FColor := Value;
end;

procedure TSDLFont.SetTempAlign(AAlignment: TSDLAlignment; ALayout: TSDLTextLayout);
begin
  FTempAlign := Alignment;
  FTempLayout := Layout;
  Alignment := AAlignment;
  Layout := ALayout;
end;

procedure TSDLFont.TextRect(Target: PSDL_Surface; const Rect: TSDL_Rect;
  const Text: string);
var Size: TSDL_Rect;
    x,y : Integer;
begin
  Size := TextExtent(Text);
  case FAlignment of
  taLeft: x := 0;
  taRight: x := Rect.w-Size.w;
  else x := (Rect.w-Size.w) div 2;
  end;
  case FLayout of
  tlTop: y := 0;
  tlBottom: y := Rect.h-Size.h;
  else y := (Rect.h-Size.h) div 2;
  end;
  TextRectA(Target,Rect,x+Rect.x,y+Rect.y,Text);
end;

procedure TSDLFont.TextRectA(Target : PSDL_Surface;Rect: TSDL_Rect; X, Y: Integer;
  const Text: string);
begin
  SDLScreen.SetClipRect(Target,@Rect);
  TextOut(Target,X,Y,Text);
  SDLScreen.SetClipRect(Target,nil);
end;

{ TWrapManager }

procedure TWrapManager.CalcWidths;
var i : Integer;
begin
  FMaxWidth := 0;
  FWrapHeight := FFont.TextHeight('O');
  for i := 0 to FWrapedLines.Count-1 do
  begin
    if FTrimLines then
      FWrapedLines[i] := Trim(FWrapedLines[i]);
    FWrapWidths[i] := FFont.TextWidth(FWrapedLines[i]);
    if FWrapWidths[i]>FMaxWidth then
      FMaxWidth := FWrapWidths[i];
  end;
end;

constructor TWrapManager.Create(AFont: TSDLFont);
begin
  inherited Create;
  FFont := AFont;
  FWrapedLines := TStringList.Create;
  FWrapWidth := 100;
  FTrimLines := True;
  FWrapToWidth := True;
end;

destructor TWrapManager.Destroy;
begin
  FWrapedLines.Free;
  //SetLength(FWrapWidths,0);
  Finalize(FWrapWidths);
  inherited;
end;

function TWrapManager.GetLinesCount: Integer;
begin
  Result := FWrapedLines.Count;
end;

procedure TWrapManager.SetWrapingText(const Value: string);
begin
  FWrapingText := Value;
  WrapUsingWidth;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TWrapManager.SetWrapToWidth(const Value: Boolean);
begin
  FWrapToWidth := Value;
  WrapUsingWidth;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TWrapManager.SetWrapWidth(const Value: Word);
begin
  if Value>FWrapHeight then
  begin
    FWrapWidth := Value;
    WrapUsingWidth;
  end;
end;

procedure TWrapManager.TextOut(Target: PSDL_Surface; X, Y: Integer);
var ax,i: Integer;
begin
  ax := X;

  for i := 0 to FWrapedLines.Count-1 do
  begin
    if FFont.Alignment=taCenter then
      ax := X + (FMaxWidth-FWrapWidths[i])div 2
    else if FFont.Alignment=taRight then
      ax := X+FMaxWidth-FWrapWidths[i];
    FFont.TextOut(Target,ax,Y,FWrapedLines[i]);
    Inc(Y,FWrapHeight);
  end;
end;

procedure TWrapManager.TextRect(Target: PSDL_Surface; const Rect: TSDL_Rect);
var X,Y,L,i : Integer;
begin
  L := FWrapedLines.Count;
  X := Rect.x; Y := Rect.y;
  if FFont.Layout = tlCenter then
    Y := Y + (Rect.h - FWrapHeight*L)div 2
  else if FFont.Layout = tlBottom then
    Y := Y + Rect.h - FWrapHeight*L;

  for i := 0 to L-1 do
  begin
    if FFont.Alignment = taCenter then
      X := Rect.x + (Rect.w-FWrapWidths[i])div 2
    else if FFont.Alignment = taRight then
      X := Rect.x + Rect.w - FWrapWidths[i];
    FFont.TextRectA(Target,Rect,X,Y,FWrapedLines[i]);
    Inc(Y,FWrapHeight);
  end;
end;

procedure TWrapManager.TextRectA(Target: PSDL_Surface;
  const Rect: TSDL_Rect; X, Y: Integer);
var i: Integer;
begin
  for i := 0 to FWrapedLines.Count-1 do
  begin
    FFont.TextRectA(Target,Rect,X,Y,FWrapedLines[i]);
    Inc(Y,FWrapHeight);
  end;
end;

procedure TWrapManager.WrapUsingWidth;
const BreakOn = #9+' ,.;:?!';
var Text,CurLine : String;
    MaxC,P,FP,C,i,t : Word;
begin
  if FWrapingText='' then
  begin
    FWrapedLines.Clear;
    Exit;
  end;
  Text := FWrapingText;
{$IFDEF MSWINDOWS}
  AdjustLineBreaks(Text);
{$ELSE}
  AdjustLineBreaks(Text,tlbsCRLF);
{$ENDIF}
  FWrapedLines.Text := Text;
  if Text[Length(Text)] = #10 then FWrapedLines.Append('');
  if (FFont.TextWidth(Text)<FWrapWidth)or not FWrapToWidth then
  begin
    SetLength(FWrapWidths,FWrapedLines.Count);
    CalcWidths;
    Exit;
  end;
  MaxC := FWrapWidth div FFont.TextWidth('i');
  C := 0;
  t := FWrapedLines.Count-1;

  for i := 0 to t do
  begin
    while FFont.TextWidth(FWrapedLines[C])>=FWrapWidth do
    begin
      CurLine := Copy(FWrapedLines[C],1,MaxC);

      P := Length(CurLine);
      FP := 0;
      while (FFont.TextWidth(CurLine)>=FWrapWidth)or
             (AnsiPos(CurLine[P],BreakOn)=0)do
      begin
        Delete(CurLine,P,1);
        Dec(P);
        if (FP=0)and(FFont.TextWidth(CurLine)<FWrapWidth) then
          FP := P;
        if P=0 then Break;
      end;

      if P=0 then P := FP;
      FWrapedLines.Insert(C+1,Copy(FWrapedLines[C],P+1,Length(FWrapedLines[C])));
      FWrapedLines[C] := Copy(FWrapedLines[C],1,P);
      Inc(C);
    end;
    Inc(C);
  end;
  SetLength(FWrapWidths,FWrapedLines.Count);
  CalcWidths;
end;

{ TSDLRasterFont }

constructor TSDLRasterFont.Create(Collection: TCollection);
begin
   inherited;
   FReplaceColor := True;
   FImage := SDLScreen.SDLImageClass.Create(nil);
   FImage.Transparent := True;
   FTabSpaces := 4;
end;

destructor TSDLRasterFont.Destroy;
begin
   FImage.Free;
   Finalize(FCharPos);
   inherited;
end;

procedure TSDLRasterFont.Load(const FontName, ImageFile: string);
var Dat : TFileStream;
    n : Integer;
begin
  Dat := TFileStream.Create(ImageFile,fmOpenRead);
  Dat.Seek(-SizeOf(Integer),soFromEnd);
  Dat.Read(n,SizeOf(Integer));
  Dat.Seek(n,soFromEnd);
  LoadFromStream(Dat);
  Dat.Free;
  FFileName := ImageFile;
  FImage.Load(FFileName);
end;

procedure TSDLRasterFont.LoadFromStream(Stream: TStream);
var FontData: TFontData;
    t: Integer;
begin
  Stream.Read(t,SizeOf(Integer));
  Stream.Read(FontData,SizeOf(TFontData));
  if FontData.Version<>RASTERFONTVER then
    raise Exception.Create('Unsupported version of TSDLFont!');
  FHeight := FontData.Height;
  FColor := FontData.FontColor;
  FImage.TransparentColor := FontData.BackColor;
  FCharCount := FontData.CharacterNo;
  SetLength(FCharPos,FCharCount+1);
  FCharpos[0] := 0;
  Stream.Read(FReplaceColor,SizeOf(Boolean));
  Stream.Read(FCharPos[1],FCharCount*SizeOf(Word));
  ReadString(Stream,FCharset);
  ReadString(Stream,FFileName);
  ReadString(Stream,FName);
  Stream.Read(t,SizeOf(Integer));
  if Length(FFileName)=0 then Exit;
  FImage.Load(FFileName);
end;

{
  Size Of Complete Font Data
  FontData record
  ReplaceColor
  Char Positions
  Charset
  FileName
  Name
  Minus Size of Complete Font Data
}

procedure TSDLRasterFont.SaveToStream(Stream: TStream);
var FontData: TFontData;
    ModifPos,t: Integer;
begin
  if FImage.Surface=nil then Exit;
  Modifpos := Stream.Position;
  t := ModifPos;
  Stream.Write(t,SizeOf(Integer)); //Is overwritten letter
  FontData.Version := RASTERFONTVER;
  FontData.Height := FHeight;
  FontData.BackColor := FImage.TransparentColor;
  FontData.FontColor := FColor;
  FontData.CharacterNo := FCharCount;
  Stream.Write(FontData,SizeOf(TFontData));
  Stream.Write(FReplaceColor,SizeOf(Boolean));
  Stream.Write(FCharPos[1],FCharCount*SizeOf(Word));
  WriteString(Stream,FCharset);
  WriteString(Stream,FFileName);
  WriteString(Stream,FName);
  ModifPos := ModifPos-Stream.Position-SizeOf(Integer);
  Stream.Write(ModifPos,SizeOf(Integer));
  Stream.Position := t; //In case user needs to skip font
  ModifPos := -ModifPos-SizeOf(Integer);
  Stream.Write(ModifPos,SizeOf(Integer));
end;

procedure TSDLRasterFont.SetColor(const Value: Cardinal);
var x,y: Integer;
    c,v,h1,h2 : Cardinal;
begin
  if (FColor=Value)or(FImage.Surface=nil) then Exit;
  if FReplaceColor then
  begin
    v := SDLScreen.MapCardinal(FImage.Surface.format,Value);
    if SDL_MustLock(FImage.Surface) then
      SDL_LockSurface(FImage.Surface);
    for x := 0 to FImage.Surface.w-1 do
      for y := 0 to FImage.Surface.h-1 do
      begin
        c := SDLScreen.GetRGBAPixel(FImage.Surface,x,y);
        v := v and $FFFFFF;
        v := v or c and $FF000000;
        if (c and $FFFFFF) = FColor then
          SDL_PutPixel(FImage.Surface,x,y,v);
      end;
    if SDL_MustLock(FImage.Surface) then
      SDL_UnLockSurface(FImage.Surface);
  end
  else
  begin
    h1 := RGBtoHSL(FColor);
    if (h1 and $FF00)=0 then
      SDLScreen.Colorize(FImage.Surface,Value)
    else
    begin
      h1 := (h1 shr 16)and $FF;
      h2 := (RGBtoHSL(Value) shr 16)and $FF;
      SDLScreen.ModifyHueFor(FImage.Surface,h2-h1);
    end;
  end;
  FImage.DisplayFormat;
  inherited;
end;

function TSDLRasterFont.TextExtent(const Text: string): TSDL_Rect;
begin
  FillChar(Result,SizeOf(Result),0);
  Result.h := FHeight;
  Result.w := TextWidth(Text);
end;

function TSDLRasterFont.TextHeight(const Text: string): Integer;
begin
   Result := FHeight;
end;

procedure TSDLRasterFont.TextOut(Target: PSDL_Surface;X,Y: INTEGER;const Text: string);
var src: TSDL_Rect;
    i,t : Integer;
begin
  src.y := 1;
  src.h := FHeight;
  for i := 1 to Length(Text) do
  begin
    if Text[i]=' ' then
    begin
      Inc(X,FCharPos[1]);
      Continue;
    end
    else if Text[i]=#9 then
    begin
      Inc(X,FCharPos[1]*FTabSpaces);
      Continue;
    end;
    t := AnsiPos(Text[i],FCharset);
    if t=0 then Continue;
    src.x := FCharPos[t-1];
    src.w :=  FCharPos[t]-src.x;
    FImage.DrawRect(Target,X,Y,src);
    Inc(X,src.w);
  end;
end;

function TSDLRasterFont.TextWidth(const Text: string): Integer;
var i,p : Integer;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  begin
    if Text[i]=' ' then p := 1
    else
      p :=AnsiPos(Text[i],FCharset);
    if p > 0 then
      Result := Result+FCharPos[p]-FCharPos[p-1]
    else if Text[i]=#9 then
      Result := Result+FCharPos[1]*FTabSpaces;
  end;
end;

{ TSDLTTFFont}

destructor TSDLTTFFont.Destroy;
begin
  if Assigned(FFont) then
    TTF_CloseFont(FFont);
  inherited;
end;

procedure TSDLTTFFont.Load(const FontName, AFileName: string;PointSize: Integer);
begin
  if SDLScreen.OpenGLScreen then Exit;
  FSize := PointSize;
  if AFileName='' then Exit;
  if not TTFLoaded then
    TTFLoaded := Boolean(TTF_Init+1);
  Name := FontName;
  FFileName := AFileName;
  if Assigned(FFont) then
    TTF_CloseFont(FFont);
  FFont := TTF_OpenFont(PChar(FFileName),PointSize );
  SetStyle(FStyle);
end;

procedure TSDLTTFFont.LoadFromStream(Stream: TStream);
var t: Integer;
    ver: Word;
begin
  Stream.Read(t,SizeOf(Integer));
  Stream.Write(ver,SizeOf(Word));
  if ver<>TTFFONTVER then
    raise Exception.Create('Unsupported version of TTFFont Data!');
  Stream.Read(FSize, SizeOf(Byte));
  Stream.Read(FStyle, SizeOf(TSDLFontStyle));
  Stream.Read(Shaded, SizeOf(Boolean));
  Stream.Read(BackColor,SizeOf(Cardinal));
  Stream.Read(FColor,SizeOf(Cardinal));
  ReadString(Stream,FName);
  ReadString(Stream,FFileName);
  Stream.Read(t,SizeOf(Integer));
  Load(FName,FFileName,FSize);
end;

procedure TSDLTTFFont.SaveToStream(Stream: TStream);
var ModifPos,t: Integer;
    ver: Word;
begin
  Modifpos := Stream.Position;
  t := ModifPos;
  ver := TTFFONTVER;
  Stream.Write(t,SizeOf(Integer)); //Is overwritten latter
  Stream.Write(ver,SizeOf(Word));
  Stream.Write(FSize, SizeOf(Byte));
  Stream.Write(FStyle, SizeOf(TSDLFontStyle));
  Stream.Write(Shaded, SizeOf(Boolean));
  Stream.Write(BackColor,SizeOf(Cardinal));
  Stream.Write(FColor,SizeOf(Cardinal));
  WriteString(Stream,FName);
  WriteString(Stream,FFileName);
  ModifPos := ModifPos-Stream.Position-SizeOf(Integer);
  Stream.Write(ModifPos,SizeOf(Integer));
  Stream.Position := t; //In case user needs to skip font
  ModifPos := -ModifPos-SizeOf(Integer);
  Stream.Write(ModifPos,SizeOf(Integer));
end;

procedure TSDLTTFFont.SetSize(const Value: Byte);
begin
  if FSize=Value then Exit;
  Load(Name,FileName,Value);
end;

procedure TSDLTTFFont.SetStyle(const Value: TSDLFontStyle);
var st: Integer;
begin
  FStyle := Value;
  st := 0;
  if fsBold in FStyle then st := TTF_STYLE_BOLD;
  if fsItalic in FStyle then st := st or TTF_STYLE_ITALIC;
  if fsUnderline in FStyle then st := st or TTF_STYLE_UNDERLINE;
  if Assigned(FFont) then
    TTF_SetFontStyle(FFont,st);
end;

function TSDLTTFFont.TextExtent(const Text: string): TSDL_Rect;
var W, H : Integer;
begin
  FillChar(Result,SizeOf(Result),0);
  if FFont<>nil then
  begin
    TTF_SizeText(FFont,PChar(Text),W,H);
    Result.w := W;
    Result.h := H;
  end;
end;

function TSDLTTFFont.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).h;
end;

procedure TSDLTTFFont.TextOut(Target: PSDL_Surface;X, Y: Integer;
  const Text: string);
var FImage : PSDL_Surface;
    dst: TSDL_Rect;
    sc,dc: TSDL_Color;
begin
  if FFont<>nil then
  begin
    FillChar(dst,SizeOf(dst),0);
    sc.r := PByte(Integer(@FColor)+2)^;
    sc.g := PByte(Integer(@FColor)+1)^;
    sc.b := PByte(@FColor)^;
    if Shaded then
    begin
      dc.r := PByte(Integer(@BackColor)+2)^;
      dc.g := PByte(Integer(@BackColor)+1)^;
      dc.b := PByte(@BackColor)^;
      FImage := TTF_RenderText_Shaded(FFont,PChar(Text),sc,dc);
    end
    else
      FImage := TTF_RenderText_Solid(FFont,PChar(Text),sc);
    dst.x := x;
    dst.y := y;
    SDL_BlitSurface(FImage,nil,Target,@dst);
    SDL_FreeSurface(FImage);
  end;
end;

function TSDLTTFFont.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).w;
end;

{ TSDLFonts }

function TSDLFonts.AddRasterFont: TSDLRasterFont;
begin
  Result := TSDLRasterFont.Create(Self);
end;

function TSDLFonts.AddTTFFont: TSDLTTFFont;
begin
  if SDLScreen.OpenGLScreen then Result := nil
  else Result := TSDLTTFFont.Create(Self);
end;

function TSDLFonts.ByName(const Name: string): TSDLFont;
var Counter : Integer;
begin
  for Counter := 0 to Count-1 DO
    if CompareText(Items[Counter].Name, Name)=0 then
    begin
      Result := Items[Counter];
      Exit;
    end;
  Result := nil;
end;

function TSDLFonts.GetItem(Index: INTEGER): TSDLFont;
begin
   Result := TSDLFont(inherited GetItem(Index));
end;

function TSDLFonts.Insert(Index: Integer): TSDLFont;
begin
  Result := TSDLFont(inherited Insert(Index));
end;

procedure TSDLFonts.LoadFromFile(const AFileName: string);
var Dat: TFileStream;
begin
  Dat := TFileStream.Create(AFileName,fmCreate);
  LoadFromStream(Dat);
  Dat.Free;
end;

procedure TSDLFonts.LoadFromStream(Stream: TStream);
var ID: array[0..2]of Char;
    cnt,i: Integer;
    IsRaster: Boolean;
    CurFont: TSDLFont;
begin
  Stream.Read(ID,3);
  if ID<>'SFN' then
    raise Exception.Create('Unknown file format!');
  Stream.Read(cnt,SizeOf(Integer));
  for i := 0 to cnt-1 do
  begin
    Stream.Read(IsRaster,SizeOf(Boolean));
    if IsRaster then CurFont := AddRasterFont
    else CurFont := AddTTFFont;
    CurFont.LoadFromStream(Stream);
  end;
end;

procedure TSDLFonts.SaveToFile(const AFileName: string);
var Save: TFileStream;
begin
  Save := TFileStream.Create(AFileName,fmCreate);
  SaveToStream(Save);
  Save.Free;
end;

procedure TSDLFonts.SaveToStream(Stream: TStream);
var i: Integer;
    IsRaster: Boolean;
begin
  i := Count;
  if i=0 then Exit;
  Stream.Write('SFN',3);
  Stream.Write(i,SizeOf(Integer));
  for i := 0 to Count-1 do
  begin
    IsRaster := Items[i] is TSDLRasterFont;
    Stream.Write(IsRaster,SizeOf(Boolean));
    Items[i].SaveToStream(Stream);
  end;
end;

procedure TSDLFonts.SetItem(Index: INTEGER; const Value: TSDLFont);
begin
  inherited SetItem(Index, Value);
end;

end.
