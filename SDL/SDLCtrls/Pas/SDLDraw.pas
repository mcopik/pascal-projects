unit SDLDraw;

{*********************************************************************
             SDLDraw v1.1b -  06.03.2005.
             
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
Description: TSDLScreen,TSDLGLScreen,TSDLImage and TSDLGLImage classes,
             drawing functions and some utility functions
Thanks to:   Todd Lang's for his SDLClasses.pas

If you have any suggestions or you made some modifications
please inform me.

Changes:
  - Modified to use paszlib instead of ZLibEx for more compatibility
  - Fixed a few bugs with DrawOrthoLine and Draw3DControl
  - Added depth buffer initialization to DefaultInitGL
**********************************************************************}

interface

uses
  sdl, Classes, gl;

type
  //Used in DrawArrow method
  TArrowDirection = (adUp,adLeft,adDown,adRight);

  // Record for holding pixel data for a 32bpp OpenGL image
  PGLRGBQUAD = ^TGLRGBQUAD;
  tagGLRGBQUAD = packed record
    red : Byte;
    green : Byte;
    blue : Byte;
    alpha : Byte;
  end;
  TGLRGBQUAD = tagGLRGBQUAD;

  //Used for saving data with the image
  TSDLImageData = record
    UseAlphaCh: Boolean;
    Alpha: Byte;
    PatternHeight: Integer;
    PatternWidth: Integer;
    Transparent: Boolean;
    TransparentColor: Cardinal;
  end;

  //Forward declaration
  TSDLImage = class;
  TSDLImageClass = class of TSDLImage;

  TSDLScreen = class
  private
    FFPS: Integer;
    FFPSCnt: Integer;
    FTime: Cardinal;
    FSurface: PSDL_Surface;
    FOpenGLScreen: Boolean;
    FSDLImageClass: TSDLImageClass;
    FClipRect: TSDL_Rect;
    FSurfaceRect: TSDL_Rect;
    procedure CountFPS;
  public
    constructor Create(const Caption: string;W,H,Bpp: Integer; FS: Boolean); virtual;
    procedure Flip; virtual;
    procedure Clear(ASurface: PSDL_Surface;Color: Cardinal); virtual;
    procedure SetClipRect(ASurface: PSDL_Surface; rect: PSDL_Rect); virtual;
    function MapCardinal(Format : PSDL_PixelFormat;Color: Cardinal): Cardinal; virtual;
    function GetRGBAPixel(ASurface: Pointer;X,Y: Integer): Cardinal; virtual;
    procedure Colorize(ASurface: PSDL_Surface; Value: Cardinal);
    procedure ModifyHueFor(ASurface: PSDL_Surface; Value: Integer);
    procedure ModifyHueTo(ASurface: PSDL_Surface; ToColor: Cardinal);
    procedure DrawOrthoLine(S: PSDL_Surface;Horiz: Boolean;
                            x,y: Integer;Length,C: Cardinal); virtual;
    procedure FillRect(S: PSDL_Surface;const Rect: TSDL_Rect;C: Cardinal); virtual;
    procedure DrawFrame(S: PSDL_Surface;const Rect: TSDL_Rect;C: Cardinal); virtual;
    procedure Draw3DControl(S:PSDL_Surface;const Rect: TSDL_Rect;C: Cardinal;
              BorderWidth: Integer;Down: Boolean=False); virtual;
    procedure DrawArrow(S: PSDL_Surface;Dir: TArrowDirection;
                           const Rect: TSDL_Rect;C: Cardinal); virtual;
    property ClipRect: TSDL_Rect read FClipRect;
    property FPS: Integer read FFPS;
    property OpenGLScreen: Boolean read FOpenGLScreen;
    property SDLImageClass: TSDLImageClass read FSDLImageClass;
    property Surface: PSDL_Surface read FSurface;
    property SurfaceRect: TSDL_Rect read FSurfaceRect;
  end;

  TSDLGLScreen = class(TSDLScreen)
  public
    constructor Create(const Caption: string;W,H,Bpp: Integer; FS: Boolean); override;
    procedure Flip; override;
    procedure Clear(S: PSDL_Surface;Color: Cardinal); override;
    procedure SetClipRect(S: PSDL_Surface; rect: PSDL_Rect); override;
    function MapCardinal(Format : PSDL_PixelFormat;Color: Cardinal): Cardinal; override;
    function GetRGBAPixel(S: Pointer;X,Y: Integer): Cardinal; override;
    procedure DrawOrthoLine(S: PSDL_Surface;Horiz: Boolean;
                            x,y: Integer;Length,C: Cardinal); override;
    procedure FillRect(S: PSDL_Surface;const Rect: TSDL_Rect;C: Cardinal); override;
  end;

  TSDLImage = class(TCollectionItem)
  private
    FTransparent: Boolean;
    FAlpha: Byte;
    FTransparentColor: Cardinal;
    FPatternWidth: Integer;
    FPatternHeight: Integer;
    FName: string;
    FSurface: PSDL_Surface;
    FUseAlphaCh: Boolean;
    function GetHeight: Integer;
    function GetPatternCount: Integer;
    function GetPatternHeight: Integer;
    function GetPatternRect(Index: Integer): TSDL_Rect;
    function GetPatternWidth: Integer;
    function GetWidth: Integer;
    procedure SetTransparent(const Value: Boolean);
    procedure SetTransparentColor(const Value: Cardinal);
    procedure SetSurface(const Value: PSDL_Surface);
    procedure SetUseAlphaCh(const Value: Boolean);
  protected
    procedure SetAlpha(const Value: Byte); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Alloc(W,H: Integer; HWSurface: Boolean=True): Boolean; virtual;
    function Load(const FileName: string): Boolean;
    procedure SaveToFile(const FileName: string);
    procedure DisplayFormat; virtual;
    procedure Draw(Dest: PSDL_Surface; X, Y: Integer; PatternIndex: Integer);
    procedure DrawRect(Dest: PSDL_Surface; X, Y: Integer;const Src: TSDL_Rect); virtual;
    procedure StretchDraw(Dest: PSDL_Surface;const dr: TSDL_Rect; PatternIndex: Integer);
    procedure StretchDrawRect(Dest: PSDL_Surface;const Src,dr: TSDL_Rect); virtual;
    property Alpha: Byte read FAlpha write SetAlpha;
    property Height: Integer read GetHeight;
    property Name: string read FName write FName;
    property PatternCount: Integer read GetPatternCount;
    property PatternHeight: Integer read GetPatternHeight write FPatternHeight;
    property PatternRects[Index: Integer]: TSDL_Rect read GetPatternRect;
    property PatternWidth: Integer read GetPatternWidth write FPatternWidth;
    property Surface: PSDL_Surface read FSurface write SetSurface;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property TransparentColor: Cardinal read FTransparentColor write SetTransparentColor;
    property UseAlphaCh: Boolean read FUseAlphaCh write SetUseAlphaCh;
    property Width: Integer read GetWidth;
  end;

  TSDLGLImage = class(TSDLImage)
  private
    FMinFilter : GLint;        // Texture minification filter
    FMagFilter : GLint;        // Texture magnification filter
    FTexWrapS : GLint;         // Texture wrapping in s direction
    FTexWrapT : GLint;         // Texture wrapping in t direction
    procedure SetMagFilter(const Value: GLint);
    procedure SetMinFilter(const Value: GLint);
    procedure SetTexWrapS(const Value: GLint);
    procedure SetTexWrapT(const Value: GLint);
  protected
    procedure SetAlpha(const Value: Byte); override;
  public
    TextureID : GLuint;         // OpenGL texture id
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DisplayFormat; override;
    procedure DrawRect(Dest: PSDL_Surface; X, Y: Integer;const Src: TSDL_Rect); override;
    procedure StretchDrawRect(Dest: PSDL_Surface;const Src,dr: TSDL_Rect); override;
    property MinFilter : GLint read FMinFilter write SetMinFilter;
    property MagFilter : GLint read FMagFilter write SetMagFilter;
    property TexWrapS : GLint read FTexWrapS write SetTexWrapS;
    property TexWrapT : GLint read FTexWrapT write SetTexWrapT;
  end;

  TSDLImages = class(TCollection)
  private
    procedure LoadFrom(Stream: TStream);
    function GetItem( Index: Integer): TSDLImage;
    procedure SetItem(Index: Integer;const Value: TSDLImage);
  public
    destructor Destroy; override;
    function ByName(const AName: string): TSDLImage;
    function Add(const AName: string): TSDLImage; //Adds image type of main surface
    function Insert(Index: Integer): TSDLImage;
    function IndexOfItem(const AName: string): Integer;
    function LoadFromSil(const FileName: string): Integer;
    property Items[Index: Integer]: TSDLImage read GetItem write SetItem; default;
  end;

  function LightColor(Color: Cardinal;Value: Integer): Cardinal;
  function HueColorTo(Color,ToColor: Cardinal): Cardinal;
  function RGBtoHSL(Color: Cardinal): Cardinal;
  function HSLtoRGB(Color: Cardinal): Cardinal;

  procedure DefaultInitGL;
  function SDLPoint(AX,AY: Integer): TPoint;
  procedure InflateRect(var Rect: TSDL_Rect;dx,dy: Integer);
  procedure OffsetRect(var Rect: TSDL_Rect;dx,dy: Integer);
  function PointInRect(const Point: TPoint;const Rect: TSDL_Rect): Boolean;
  function RectInRect(const Rect1, Rect2: TSDL_Rect): Boolean;
  function OverlapRect(const Rect1, Rect2: TSDL_Rect): Boolean;
  procedure WriteString(Stream: TStream;const Value: String); //needed everywhere
  procedure ReadString(Stream: TStream;var Value: String); //so I put it here

const
  SILVER = $0002;
  ESILERROR = -1;
  ESILUNKNOWNFORMAT = -2;
  ESILUNSUPPORTEDVER = -3;
  ESILCOMPRESSED = 1;

var SDLScreen: TSDLScreen;

implementation

uses sdlutils, sdl_image, sdlstreams, SysUtils, glu, zlib;

type
  TSDLGLRect = record
    x,y,w,h: Single;
  end;

var
  SDLRLEACCEL: Word;

function GetGLRect(const SDLRect: TSDL_Rect;aw,ah : Integer):TSDLGLRect;
begin
  with Result do
  begin
    x := SDLRect.x / aw;
    y := SDLRect.y / ah;
    w := SDLRect.w / aw;
    h := SDLRect.h / ah;
  end;
end;

{ TSDLScreen }

procedure TSDLScreen.Clear(ASurface: PSDL_Surface; Color: Cardinal);
begin
  if ASurface=nil then ASurface := FSurface;
  FillRect(ASurface,ASurface.clip_rect,Color);
end;

procedure TSDLScreen.Colorize(ASurface: PSDL_Surface; Value: Cardinal);
var hsl,hd,sd: Cardinal;
    x,y: Integer;
begin
  if (ASurface=nil) then
    if not OpenGLScreen then ASurface := FSurface
    else Exit;
  hsl := RGBtoHSL(Value);
  hd := hsl and $FF0000;
  sd := hsl and $FF00;

  if SDL_MustLock(ASurface) then SDL_LockSurface(ASurface);
  for x := 0 to ASurface.w-1 do
    for y := 0 to ASurface.h-1 do
    begin
      hsl := SDLScreen.GetRGBAPixel(ASurface,x,y);
      hsl := RGBtoHSL(hsl);
      hsl := (hsl and $FF0000FF) or hd or sd;
      hsl := HSLtoRGB(hsl);
      hsl := MapCardinal(ASurface.format,hsl);
      SDL_PutPixel(ASurface,x,y,hsl);
    end;
  if SDL_MustLock(ASurface) then SDL_UnLockSurface(ASurface);
end;

procedure TSDLScreen.CountFPS;
begin
  Inc(FFPSCnt);
  if SDL_GetTicks-FTime>=1000 then
  begin
    FFPS := FFPSCnt;
    FFPSCnt := 0;
    FTime := SDL_GetTicks;
  end;
end;

constructor TSDLScreen.Create(const Caption: string; W, H,
            Bpp: Integer; FS: Boolean);
var Flags : Cardinal;
begin
   if SDL_Init(SDL_INIT_VIDEO)>=0 then
   begin
     Flags := SDL_HWSURFACE;
     if FS then Flags := Flags or SDL_FULLSCREEN;
     Flags := Flags or SDL_DOUBLEBUF;
     FSurface := SDL_SetVideoMode( W, H, BPP, Flags );
     SDL_WM_SetCaption(PChar(Caption),nil);
     FSDLImageClass := TSDLImage;
     FClipRect := SDLRect(0,0,W,H);
     FSurfaceRect := FClipRect;
   end;
end;

procedure TSDLScreen.Draw3DControl(S: PSDL_Surface; const Rect: TSDL_Rect;
          C: Cardinal; BorderWidth: Integer; Down: Boolean);

  procedure DrawUpLeft(Color: Cardinal);
  var i : Integer;
  begin
    with Rect do
      for i := 0 to BorderWidth-1 do
      begin
        DrawOrthoLine(S,True,x+i,y+i,w-2*i-1,Color);
        DrawOrthoLine(S,False,x+i,y+i,h-2*i-1,Color);
      end;
  end;

  procedure DrawDownRight(Color: Cardinal);
  var i : Integer;
  begin
    with Rect do
      for i := 0 to BorderWidth-1 do
      begin
        DrawOrthoLine(S,False,x+w-i-1,y+i+1,h-2*i-1,Color);
        DrawOrthoLine(S,True,x+i+1,y+h-i-1,w-2*i-1,Color);
      end;
  end;

var cl,cd : Cardinal;
begin
  if (Rect.w<=BorderWidth)or(Rect.h<=BorderWidth) then Exit;
  if S=nil then S := FSurface;
  if not OverlapRect(Rect, SDLScreen.ClipRect) then Exit;
  FillRect(S,Rect,C);
  cl := LightColor(C,50);
  cd := LightColor(C,-50);
  if Down then
  begin
    DrawUpLeft(cd);
    DrawDownRight(cl);
  end
  else
  begin
    DrawUpLeft(cl);
    DrawDownRight(cd);
  end;
end;

procedure TSDLScreen.DrawArrow(S: PSDL_Surface; Dir: TArrowDirection;
  const Rect: TSDL_Rect; C: Cardinal);
var i,p: Integer;
begin
  if S=nil then S := FSurface;
  case Dir of
  adUp:
    for i := 0 to Rect.h do
    begin
      p := i div 2;
      with Rect do
        if w>=p shl 1 then
          DrawOrthoLine(S,True,x+p,y+h-i,w-p shl 1,C);
    end;
  adDown:
    for i := 0 to Rect.h do
    begin
      p := i div 2;
      with Rect do
        if w>=p shl 1 then
           DrawOrthoLine(S,True,x+p,y+i,w-p shl 1,C);
    end;
  adLeft:
    for i := 0 to Rect.w do
    begin
      p := i div 2;
      with Rect do
        if h>=p shl 1 then
          DrawOrthoLine(S,False,x+w-i,y+p,h-p shl 1,C);
    end;
  adRight:
    for i := 0 to Rect.w do
    begin
      p := i div 2;
      with Rect do
        if h>=p shl 1 then
          DrawOrthoLine(S,False,x+i,y+p,h-p shl 1,C);
    end;
  end;
end;

procedure TSDLScreen.DrawFrame(S: PSDL_Surface; const Rect: TSDL_Rect;
  C: Cardinal);
begin
  if S=nil then S := FSurface;
  with Rect do
  begin
    DrawOrthoLine(S,True,x,y,w,C);
    DrawOrthoLine(S,False,x+w,y,h,C);
    DrawOrthoLine(S,True,x,y+h,w,C);
    DrawOrthoLine(S,False,x,y,h,C);
  end;
end;

procedure TSDLScreen.DrawOrthoLine(S: PSDL_Surface; Horiz: Boolean; x,
          y: Integer; Length, C: Cardinal);
var L: Integer;
begin
  if Length=0 then Exit;
  if S=nil then S := FSurface;
  C := MapCardinal(S.format,C);
  L := Length and $FFFF; //This avoids Combined signed and unsigned types Warning
  Dec(L);
  if Horiz then
  begin
    if (y<0)or(y>=S.clip_rect.y+S.clip_rect.h) then Exit;
    if x>=S.clip_rect.x+S.clip_rect.w then Exit;
    if x<0 then
    begin
      L := L+x;
      if L<=0 then Exit;
      x := 0;
    end;
    if x+L>=S.clip_rect.x+S.clip_rect.w then
      L := S.clip_rect.x+S.clip_rect.w-x-1;
    if SDL_MustLock(S) then
      SDL_LockSurface(S);
    SDL_DrawLine(S,x,y,x+L,y,C);
    if SDL_MustLock(S) then
      SDL_UnLockSurface(S);
  end
  else
  begin
    if (x<0)or(x>=S.clip_rect.x+S.clip_rect.w) then Exit;
    if y>=S.clip_rect.y+S.clip_rect.h then Exit;
    if y<0 then
    begin
      L := L+y;
      y := 0;
    end;
    if y+L>=S.clip_rect.y+S.clip_rect.h then
      L := S.clip_rect.y+S.clip_rect.h-y-1;
    if SDL_MustLock(S) then
      SDL_LockSurface(S);
    SDL_DrawLine(S,x,y,x,y+L,C);
    if SDL_MustLock(S) then
      SDL_UnLockSurface(S);
  end;
end;

procedure TSDLScreen.FillRect(S: PSDL_Surface; const Rect: TSDL_Rect; C: Cardinal);
var tr: TSDL_Rect;
begin
  if S=nil then S := FSurface;
  tr := Rect;
  SDL_FillRect(S,@tr,MapCardinal(S.Format,C))
end;

procedure TSDLScreen.Flip;
begin
  CountFPS;
  SDL_Flip(FSurface);
end;

function TSDLScreen.GetRGBAPixel(ASurface: Pointer; X,
  Y: Integer): Cardinal;
var r,g,b,a: Uint8;
begin
  if ASurface=nil then ASurface := FSurface;
  Result := SDL_GetPixel(ASurface,X,Y);
  SDL_GetRGBA(Result,PSDL_Surface(ASurface)^.format,@r,@g,@b,@a);
  Result := (a shl 24 or r shl 16 or g shl 8 or b);
end;

function TSDLScreen.MapCardinal(Format: PSDL_PixelFormat;
  Color: Cardinal): Cardinal;
var r,g,b,a: Byte;
begin
  Result := 0;
  if Format=nil then Exit;
  a := (Color shr 24)and $FF;
  r := (Color shr 16)and $FF;
  g := (Color shr 8) and $FF;
  b := Color and $FF;
  Result := SDL_MapRGBA(Format,r,g,b,a);
end;

procedure TSDLScreen.ModifyHueFor(ASurface: PSDL_Surface; Value: Integer);
var hd: Byte;
    hsl,x,y: Cardinal;
    h: Integer;
begin
  if (ASurface=nil) then
    if not OpenGLScreen then ASurface := FSurface
    else Exit;

  if SDL_MustLock(ASurface) then SDL_LockSurface(ASurface);
  for x := 0 to ASurface.w-1 do
    for y := 0 to ASurface.h-1 do
    begin
      hsl := SDLScreen.GetRGBAPixel(ASurface,x,y);
      if (hsl and $FF000000)=0 then Continue;
      hsl := RGBtoHSL(hsl);
      h := (hsl shr 16)and $FF;
      Inc(h,Value);
      if h<0 then h := h+255
      else if h>255 then h := h-255;
      hd := h;
      hsl := (hsl and $FF00FFFF)or(hd shl 16);
      hsl := HSLtoRGB(hsl);
      hsl := MapCardinal(ASurface.format,hsl);
      SDL_PutPixel(ASurface,x,y,hsl);
    end;
  if SDL_MustLock(ASurface) then SDL_UnLockSurface(ASurface);
end;

procedure TSDLScreen.ModifyHueTo(ASurface: PSDL_Surface; ToColor: Cardinal);
var hsl,x,y,hd: Cardinal;
begin
  if (ASurface=nil) then
    if not OpenGLScreen then ASurface := FSurface
    else Exit;
  hsl := RGBtoHSL(ToColor);
  hd := hsl and $FF0000;

  if SDL_MustLock(ASurface) then SDL_LockSurface(ASurface);
  for x := 0 to ASurface.w-1 do
    for y := 0 to ASurface.h-1 do
    begin
      hsl := SDLScreen.GetRGBAPixel(ASurface,x,y);
      hsl := RGBtoHSL(hsl);
      hsl := (hsl and $FF00FFFF) or hd;
      hsl := HSLtoRGB(hsl);
      hsl := MapCardinal(ASurface.format,hsl);
      SDL_PutPixel(ASurface,x,y,hsl);
    end;
  if SDL_MustLock(ASurface) then SDL_UnLockSurface(ASurface);
end;

procedure TSDLScreen.SetClipRect(ASurface: PSDL_Surface; rect: PSDL_Rect);
begin
  if ASurface = nil then ASurface := FSurface;
  if Assigned(rect) and (ASurface.w=rect.w)and(ASurface.h=rect.h)then
    rect := nil;
  SDL_SetClipRect(ASurface,rect);
  if ASurface=FSurface then
    if Assigned(rect) then
      FClipRect := rect^
    else
      FClipRect := FSurfaceRect;
end;

{ TSDLGLScreen }

procedure TSDLGLScreen.Clear(S: PSDL_Surface; Color: Cardinal);
var cl: TGLRGBQuad;
begin
  if Assigned(S) and (S<>FSurface) then
  begin
    inherited;
    Exit;
  end;
  Color := MapCardinal(nil,Color);
  Move(Color,cl,4);
  glClearColor(cl.red/255,cl.green/255,cl.blue/255,cl.alpha/255);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

constructor TSDLGLScreen.Create(const Caption: string; W, H, Bpp: Integer;
  FS: Boolean);
var Flags : Cardinal;
begin
   if SDL_Init(SDL_INIT_VIDEO)>=0 then
   begin
     Flags := SDL_HWSURFACE;
     if FS then Flags := Flags or SDL_FULLSCREEN;
     Flags := Flags or SDL_OPENGL;
     SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
     FSurface := SDL_SetVideoMode( W, H, BPP, Flags );
     SDL_WM_SetCaption(PChar(Caption),nil);
     FClipRect := SDLRect(0,0,W,H);
     FSurfaceRect := FClipRect;
     FOpenGLScreen := True;
     FSDLImageClass := TSDLGLImage;
   end;
end;

procedure TSDLGLScreen.DrawOrthoLine(S: PSDL_Surface; Horiz: Boolean; x,
  y: Integer; Length, C: Cardinal);
var L: Integer;
    cl: TGLRGBQuad;
begin
  if Assigned(S) and (S<>FSurface) then
  begin
    inherited;
    Exit;
  end;
  Inc(y);
  C := MapCardinal(nil,C);
  Move(C,cl,4);
  L := Length and $FFFF; //This avoids Combined signed and unsigned types Warning
  glLoadIdentity;
  with cl do
    glColor3ub(red,green,blue);
  glBegin(GL_LINES);
    glVertex2i(x,y);
    if Horiz then
      glVertex2i(x+L,y)
    else
      glVertex2i(x,y+l);
  glEnd;
end;

procedure TSDLGLScreen.FillRect(S: PSDL_Surface; const Rect: TSDL_Rect;
  C: Cardinal);
var cl: TGLRGBQuad;
begin
  if Rect.w*Rect.h=0 then Exit;
  if Assigned(S) and (S<>FSurface) then
  begin
    inherited;
    Exit;
  end;
  glLoadIdentity;
  C := MapCardinal(nil,C);
  Move(C,cl,4);
  glColor3ub(cl.red,cl.green,cl.blue);
  glRecti(rect.x,rect.y,rect.x+rect.w,rect.y+rect.h);
end;

procedure TSDLGLScreen.Flip;
begin
  CountFPS;
  glFlush;
  SDL_GL_SwapBuffers;
end;

function TSDLGLScreen.GetRGBAPixel(S: Pointer; X, Y: Integer): Cardinal;
var cl: Cardinal;
begin
  if Assigned(S) and (S<>FSurface) then
  begin
    Result := inherited GetRGBAPixel(S,X,Y);
    Exit;
  end;
  glReadPixels(X,Y,1,1,GL_RGBA,GL_UNSIGNED_BYTE,@cl);
  Result := MapCardinal(nil,cl);
end;

function TSDLGLScreen.MapCardinal(Format: PSDL_PixelFormat;
  Color: Cardinal): Cardinal;
begin
  if Assigned(Format)and(Format<>FSurface.format) then
    Result := inherited MapCardinal(Format,Color)
  else
    Result := (color and $FF000000)or((color and $FF0000)shr 16)or
              (color and $FF00)or((color and $FF)shl 16);
end;

procedure TSDLGLScreen.SetClipRect(S: PSDL_Surface; rect: PSDL_Rect);
begin
  if Assigned(S) and (S<>FSurface) then
  begin
    inherited;
    Exit;
  end;
  if (rect=nil)or(FSurfaceRect.w=rect.w)and(FSurfaceRect.h=rect.h)then
  begin
    glDisable(GL_SCISSOR_TEST);
    FClipRect := FSurfaceRect;
  end
  else
  begin
    with rect^ do
      glScissor(x,FSurfaceRect.h-y-h,w,h);
    glEnable(GL_SCISSOR_TEST);
    FClipRect := rect^;
  end;
end;

{ TSDLImage }

function TSDLImage.Alloc(W, H: Integer; HWSurface: Boolean): Boolean;
var flags : Cardinal;
begin
  if FSurface<>nil then SDL_FreeSurface(FSurface);
  Flags := SDL_SRCCOLORKEY;
  if HWSurface then Flags := Flags or SDL_HWSURFACE;
  if W<=0 then W := 1;
  FSurface := SDL_CreateRGBSurface(flags,W,H,32,0,0,0,0);
  Result := FSurface<>nil;
  if not Result then Exit;
  if FTransparent then
    SetTransparentColor(FTransparentColor)
  else DisplayFormat;
  Result := FSurface<>nil;
end;

constructor TSDLImage.Create(Collection: TCollection);
begin
  inherited;
  FAlpha := 255;
  if SDLScreen.OpenGLScreen then
    SDLRLEACCEL := 0
  else
    SDLRLEACCEL := SDL_RLEACCEL;
end;

destructor TSDLImage.Destroy;
begin
  if Assigned(FSurface) then
    SDL_FreeSurface(FSurface);
  inherited;
end;

procedure TSDLImage.DisplayFormat;
var TempS: PSDL_Surface;
begin
  if FUseAlphaCh or SDLScreen.OpenGLScreen then
    TempS := SDL_DisplayFormatAlpha(FSurface)
  else
  begin
    if FAlpha=255 then
      SDL_SetAlpha(FSurface,SDLRLEACCEL, FAlpha)
    else
      SDL_SetAlpha(FSurface,SDL_SRCALPHA or SDLRLEACCEL, FAlpha);
    TempS := SDL_DisplayFormat(FSurface);
  end;
  SDL_FreeSurface(FSurface);
  FSurface := TempS;
end;

procedure TSDLImage.Draw(Dest: PSDL_Surface; X, Y, PatternIndex: Integer);
var sr: TSDL_Rect;
begin
  if FSurface=nil then Exit;

  sr := PatternRects[PatternIndex];
  DrawRect(Dest,X,Y,sr);
end;

procedure TSDLImage.DrawRect(Dest: PSDL_Surface; X, Y: Integer;
  const Src: TSDL_Rect);
var dr: TSDL_Rect;
begin
  if FSurface=nil then exit;

  dr := SDLRect(X,Y,0,0);
  SDL_BlitSurface(FSurface,@Src,Dest,@dr);
end;

function TSDLImage.GetHeight: Integer;
begin
  Result := FSurface^.h;
  if FPatternHeight>0 then
    Result := Result div FPatternHeight * FPatternHeight;
end;

function TSDLImage.GetPatternCount: Integer;
begin
  if FPatternWidth=0 then Result := 1
  else Result := FSurface^.w div PatternWidth;
  if FPatternHeight<>0 then
    Result := Result*FSurface^.h div PatternHeight;
end;

function TSDLImage.GetPatternHeight: Integer;
begin
  if FPatternHeight=0 then
    Result := FSurface.h
  else
    Result := FPatternHeight;
end;

function TSDLImage.GetPatternRect(Index: Integer): TSDL_Rect;
var x,y: Integer;
begin
  if (Index<0)or(Index>=PatternCount) then
  begin
    Result := SDLRect(0,0,0,0);
    Exit;
  end;
  y := 0;
  x := FSurface^.w div PatternWidth;
  if Index>x-1 then
  begin
    y := Index div x * PatternHeight;
    x := Index mod x * PatternWidth;
  end
  else x := Index * PatternWidth;
  Result := SDLRect(x,y,PatternWidth,PatternHeight);
end;

function TSDLImage.GetPatternWidth: Integer;
begin
  if FPatternWidth = 0 then
    Result := FSurface.w
  else
    Result := FPatternWidth;
end;

function TSDLImage.GetWidth: Integer;
begin
  Result := FSurface^.w;
  if FPatternWidth>0 then
    Result := Result div FPatternWidth * FPatternWidth;
end;

function TSDLImage.Load(const FileName: string): Boolean;
begin
  if Assigned(FSurface) then SDL_FreeSurface(FSurface);
  FSurface := IMG_Load(PChar(FileName));
  Result := FSurface<>nil;
  if not Result then Exit;
  if FTransparent then
    SetTransparentColor(FTransparentColor)
  else DisplayFormat;
end;

procedure TSDLImage.SaveToFile(const FileName: string);
begin
  if not Assigned(FSurface) then Exit;
  SDL_SaveBMP(FSurface,PChar(FileName));
end;

procedure TSDLImage.SetAlpha(const Value: Byte);
begin
  if FAlpha=Value then Exit;
  FAlpha := Value;
  if Assigned(FSurface) then DisplayFormat;
end;

procedure TSDLImage.SetSurface(const Value: PSDL_Surface);
begin
  if Assigned(FSurface) then SDL_FreeSurface(FSurface);
  FSurface := Value;
  if FSurface=nil then Exit;
  if FTransparent then
    SetTransparentColor(FTransparentColor)
  else DisplayFormat;
end;

procedure TSDLImage.SetTransparent(const Value: Boolean);
begin
  if (Value=FTransparent) or FUseAlphaCh then Exit;
  FTransparent := Value;
  if FSurface = nil then Exit;
  if Value then
    SetTransparentColor(FTransparentColor)
  else
  begin
    SDL_SetColorKey(FSurface,0,0);
    DisplayFormat;
  end;
end;

procedure TSDLImage.SetTransparentColor(const Value: Cardinal);
var tc: Cardinal;
begin
  FTransparentColor := Value;
  if (FSurface=nil)or FUseAlphaCh or not FTransparent then Exit;

  tc := SDLScreen.MapCardinal(FSurface^.format,Value);
  SDL_SetColorKey(FSurface,SDL_SRCCOLORKEY or SDLRLEACCEL,tc);
  DisplayFormat;
end;

procedure TSDLImage.SetUseAlphaCh(const Value: Boolean);
begin
  Transparent := False;
  FUseAlphaCh := Value;
  //Assign this before you load an image
end;

procedure TSDLImage.StretchDraw(Dest: PSDL_Surface;
  const dr: TSDL_Rect; PatternIndex: Integer);
var sr: TSDL_Rect;
begin
  if FSurface=nil then Exit;
  sr := PatternRects[PatternIndex];
  StretchDrawRect(Dest,sr,dr);
end;

procedure TSDLImage.StretchDrawRect(Dest: PSDL_Surface; const Src,
  dr: TSDL_Rect);
var ts: TSDLImage;
    tr: TSDL_Rect;
begin
  if (FSurface=nil)or(Src.w*Src.h=0)or(dr.w*dr.h=0) then Exit;
  if (Src.w=dr.w)and(Src.h=dr.h) then
  begin
    DrawRect(Dest,dr.x,dr.y,Src);
    Exit;
  end;
  tr := dr;
  tr.x := 0; tr.y := 0;
  ts := TSDLImage.Create(nil);
  ts.TransparentColor := TransparentColor;
  ts.Alpha := Alpha;
  ts.UseAlphaCh := UseAlphaCh;
  ts.Alloc(dr.w,dr.h);
  SDL_ZoomSurface(FSurface,@Src,ts.Surface,@tr);
  ts.Transparent := Transparent;
  SDL_BlitSurface(ts.Surface,nil,Dest,@dr);
  ts.Free;
end;

{ TSDLGLImage }

constructor TSDLGLImage.Create(Collection: TCollection);
begin
  inherited;
  FMinFilter := GL_LINEAR;
  FMagFilter := GL_LINEAR;
  FTexWrapS := GL_REPEAT;
  FTexWrapT := GL_REPEAT;
end;

destructor TSDLGLImage.Destroy;
begin
  if TextureID>0 then
    glDeleteTextures(1, @TextureID);
  inherited;
end;

procedure TSDLGLImage.DisplayFormat;
var c: Word;
begin
  if FSurface = nil then Exit;

  inherited;

  // If a texture object has already been created, delete it
  if TextureID>0 then
    glDeleteTextures(1, @TextureID);

  // Create a new OpenGL texture object
  glGenTextures(1, @TextureID);
  // Set up parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, FTexWrapS);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, FTexWrapT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, FMagFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, FMinFilter);

  glBindTexture(GL_TEXTURE_2D, TextureID);
  c := 3+Ord(FUseAlphaCh or FTransparent or (FAlpha<255));

  if SDL_MustLock(FSurface) then
    SDL_LockSurface(FSurface);
  gluBuild2DMipmaps(GL_TEXTURE_2D, c, FSurface.w, FSurface.h,
    $80E1, GL_UNSIGNED_BYTE, PByte(FSurface.pixels)); //GL_BGRA = $80E1
  if SDL_MustLock(FSurface) then
    SDL_UnLockSurface(FSurface);
end;

procedure TSDLGLImage.DrawRect(Dest: PSDL_Surface; X, Y: Integer;
  const Src: TSDL_Rect);
var dr: TSDL_Rect;
begin
  if FSurface=nil then Exit;
  dr := SDLRect(X,Y,src.w,src.h);
  StretchDrawRect(Dest,Src,dr);
end;

procedure TSDLGLImage.SetAlpha(const Value: Byte);
var x,y: Integer;
    p,v: Cardinal;
begin
  if Value=FAlpha then Exit;
  if (FSurface=nil)or FUseAlphaCh then
  begin
    inherited;
    Exit;
  end;
  v := Value shl 24;
  if SDL_MustLock(FSurface) then
    SDL_LockSurface(FSurface);
  for x := 0 to FSurface.w-1 do
    for y := 0 to FSurface.h-1 do
    begin
      p := SDLScreen.GetRGBAPixel(FSurface,x,y);
      p := (p and $FFFFFF)or v;
      SDLScreen.MapCardinal(FSurface.format,p);
      SDL_PutPixel(FSurface,x,y,p);
    end;
  if SDL_MustLock(FSurface) then
    SDL_UnLockSurface(FSurface);
  inherited;
end;

procedure TSDLGLImage.SetMagFilter(const Value: GLint);
begin
  case Value of
    GL_NEAREST : FMagFilter := GL_NEAREST;
    GL_LINEAR : FMagFilter := GL_LINEAR;
  end;
end;

procedure TSDLGLImage.SetMinFilter(const Value: GLint);
begin
  case Value of
    GL_NEAREST : FMinFilter := GL_NEAREST;
    GL_LINEAR : FMinFilter := GL_LINEAR;
    GL_NEAREST_MIPMAP_NEAREST : FMinFilter := GL_NEAREST_MIPMAP_NEAREST;
    GL_LINEAR_MIPMAP_NEAREST : FMinFilter := GL_LINEAR_MIPMAP_NEAREST;
    GL_NEAREST_MIPMAP_LINEAR : FMinFilter := GL_NEAREST_MIPMAP_LINEAR;
    GL_LINEAR_MIPMAP_LINEAR : FMinFilter := GL_LINEAR_MIPMAP_LINEAR;
  end;
end;

procedure TSDLGLImage.SetTexWrapS(const Value: GLint);
begin
  case Value of
    GL_CLAMP : FTexWrapS := GL_CLAMP;
    GL_REPEAT : FTexWrapS := GL_REPEAT;
  end;
end;

procedure TSDLGLImage.SetTexWrapT(const Value: GLint);
begin
  case Value of
    GL_CLAMP : FTexWrapT := GL_CLAMP;
    GL_REPEAT : FTexWrapT := GL_REPEAT;
  end;;
end;

procedure TSDLGLImage.StretchDrawRect(Dest: PSDL_Surface; const Src,
  dr: TSDL_Rect);
var sr: TSDLGLRect;
begin
  if FSurface=nil then Exit;
  sr := GetGLRect(src,FSurface.w,FSurface.h);

  glLoadIdentity;
  glEnable(GL_TEXTURE_2D);
  if TextureID>0 then
    glBindTexture(GL_TEXTURE_2D, TextureID);
  glColor3ub(255,255,255);
  glBegin(GL_QUADS);
    glTexCoord2f(sr.x,sr.y);           glVertex2i(dr.x,dr.y);
    glTexCoord2f(sr.x+sr.w,sr.y);      glVertex2i(dr.x+dr.w,dr.y);
    glTexCoord2f(sr.x+sr.w,sr.y+sr.h); glVertex2i(dr.x+dr.w,dr.y+dr.h);
    glTexCoord2f(sr.x,sr.y+sr.h);      glVertex2i(dr.x,dr.y+dr.h);
  glEnd;
  glDisable(GL_TEXTURE_2D);
end;

{ TSDLImages }

function TSDLImages.Add(const AName: string): TSDLImage;
begin
  Result := TSDLImage(inherited Add);
  Result.Name := AName;
end;

function TSDLImages.ByName(const AName: string): TSDLImage;
var i: Integer;
begin
  i := IndexOfItem(AName);
  if i>=0 then
    Result := Items[i]
  else
    Result := nil;
end;

destructor TSDLImages.Destroy;
begin
  Clear;
  inherited;
end;

function TSDLImages.GetItem(Index: Integer): TSDLImage;
begin
  Result := TSDLImage(inherited GetItem(Index));
end;

function TSDLImages.IndexOfItem(const AName: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
end;

function TSDLImages.Insert(Index: Integer): TSDLImage;
begin
  Result := TSDLImage(inherited Insert(Index));
end;

procedure TSDLImages.LoadFrom(Stream: TStream);
var i,Size,till : Integer;
    t : TSDLImageData;
    RWops: PSDL_RWops;
    CurImg: TSDLImage;
    txt: string;
    tmp: TMemoryStream;
begin
  tmp := nil;
  if Stream is TDeCompressionStream then
  begin
    tmp := TMemoryStream.Create;
    RWops := SDLStreamSetup(tmp);
  end
  else RWops := SDLStreamSetup(Stream);
  Stream.Read(till,4);
  for i := 0 to till-1 do
  begin
    Stream.Read(t,SizeOf(TSDLImageData));
    ReadString(Stream,txt);
    CurImg := Add(txt);
    with CurImg do
    begin
      UseAlphaCh := t.UseAlphaCh;
      Alpha := t.Alpha;
      PatternHeight := t.PatternHeight;
      PatternWidth := t.PatternWidth;
      Transparent := t.Transparent;
      TransparentColor := t.TransparentColor;
      Stream.Read(Size,4);
      if Assigned(tmp) then
      begin
        tmp.CopyFrom(Stream,Size);
        tmp.Position := 0;
      end;
      FSurface := IMG_Load_RW(RWops,0);
      if Assigned(tmp) then tmp.Clear;
      if FTransparent then
        SetTransparentColor(FTransparentColor)
      else
        DisplayFormat;
    end;
  end;
  SDLStreamCloseRWops(RWops);
  tmp.Free;
end;

function TSDLImages.LoadFromSil(const FileName: string): Integer;
var Load: TFileStream;
    cmpS: TDeCompressionStream;
    Header: array[0..2]of Char;
    t: Integer;
    comp: Boolean;
begin
  Result := ESILERROR;
  if not FileExists(FileName)then Exit;
  Load := TFileStream.Create(FileName,fmOpenRead);
  cmpS := nil;
  t := 0;

  try
    Load.Read(Header,3);
    if Header<>'SIL' then
    begin
      Result := ESILUNKNOWNFORMAT;
      Exit;
    end;
    Load.Read(t,2);
    if t>SILVER then
    begin
      Result := ESILUNSUPPORTEDVER;
      Exit;
    end;
    Clear;
    Load.Read(comp,1);
    if comp then
    begin
      cmpS := TDeCompressionStream.Create(Load);
      LoadFrom(cmpS);
      Result := ESILCOMPRESSED;
    end
    else
    begin
      LoadFrom(Load);
      Result := 0;
    end;
  finally
    cmpS.Free;
    Load.Free;
  end;
end;

procedure TSDLImages.SetItem(Index: Integer; const Value: TSDLImage);
begin
  inherited SetItem(Index,Value);
end;

{ Functions }

function LightColor(Color: Cardinal;Value: Integer): Cardinal;
var hsl: Cardinal;
    l: Integer;
    l1: Byte;
begin
  hsl := RGBtoHSL(Color);
  l := hsl and $FF;
  Inc(l,Value);
  if l>255 then l := 255
  else if l<0 then l := 0;
  l1 := Byte(l);
  hsl := (hsl and $FFFFFF00)or l1;
  Result := HSLtoRGB(hsl);
end;

function HueColorTo(Color,ToColor: Cardinal): Cardinal;
var hsl1,hsl2: Cardinal;
begin
  hsl1 := RGBtoHSL(Color);
  hsl2 := RGBtoHSL(ToColor);
  hsl1 := (hsl1 and $FF00FFFF)or(hsl2 and $FF0000);
  Result := HSLtoRGB(hsl1);
end;

function Minimum(x,y,z: Single):Single;
begin
  if x<y then Result := x
  else Result := y;
  if z<Result then Result := z;
end;

function Maximum(x,y,z: Single):Single;
begin
  if x>y then Result := x
  else Result := y;
  if z>Result then Result := z;
end;

function RGBtoHSL(Color: Cardinal): Cardinal;
var r,g,b,h,s,l,min,max,d: Single;
    hb,sb,lb: Byte;
begin
  r := ((Color shr 16)and $FF)/255;
  g := ((Color shr 8)and $FF)/255;
  b := (Color and $FF)/255;

  min := Minimum(r,g,b);
  max := Maximum(r,g,b);
  d := max-min;
  l := (min+max)/2;
  lb := Round(l*255);
  if min=max then
  begin
    Result := (Color and $FF000000)or lb;
    Exit;
  end;
  if l<0.5 then s := d/(max+min)
  else s := d/(2-max-min);
  if r = max then h := (g-b)/d
  else if g = max then h := 2+(b-r)/d
  else h := 4+(r-g)/d;
  h := h*40;
  if h<0 then h := h+255;
  hb := Round(h);
  sb := Round(s*255);
  Result := (Color and $FF000000)or(hb shl 16)or(sb shl 8)or lb;
end;

function HSLtoRGB(Color: Cardinal): Cardinal;
var temp1,temp2,h,s,l: Single;
    t3,clr : array[0..2] of Single;
    i: Integer;
    r,g,b: Byte;
begin
  h := ((Color shr 16)and $FF)/255;
  s := ((Color shr 8)and $FF)/255;
  l := (Color and $FF)/255;
  if l = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if s=0 then
  begin
    r := Round(l*255);
    Result := (Color and $FF000000)or(r shl 16)or(r shl 8)or r;
    Exit;
  end;
  if l<0.5 then temp2 := l*(1+s)
  else temp2 := l+s-l*s;
  temp1 := 2*l-temp2;

  t3[0] := h+1/3; t3[1] := h; t3[2] := h-1/3;

  for i := 0 to 2 do
  begin
    if t3[i]<0 then t3[i] := t3[i]+1;
    if t3[i]>1 then t3[i] := t3[i]-1;

    if 6*t3[i]<1 then
      clr[i]:=temp1+(temp2-temp1)*t3[i]*6
    else if 2*t3[i]<1 then
      clr[i] := temp2
    else if 3*t3[i]<2 then
      clr[i]:=(temp1+(temp2-temp1)*((2.0/3.0)-t3[i])*6.0)
    else
      clr[i] := temp1;
  end;
  r := Round(clr[0]*255);
  g := Round(clr[1]*255);
  b := Round(clr[2]*255);
  Result := (Color and $FF000000)or(r shl 16)or(g shl 8)or b;
end;

procedure DefaultInitGL;
begin
  if not SDLScreen.OpenGLScreen then Exit;
  glViewport(0, 0, SDLScreen.FSurfaceRect.w, SDLScreen.FSurfaceRect.h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0,SDLScreen.FSurfaceRect.w,SDLScreen.FSurfaceRect.h,0,-1,1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  // Set the background black
  glClearColor( 0, 0, 0, 0 );

  // Depth buffer setup
  glClearDepth( 1.0 );
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

	glEnable(GL_BLEND);  //Needed for transparency
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

function SDLPoint(AX,AY: Integer): TPoint;
begin
  Result.x := AX;
  Result.y := AY;
end;

procedure InflateRect(var Rect: TSDL_Rect;dx,dy: Integer);
begin
  if (dx<0)and(Rect.w<-dx shl 1)or
     (dy<0)and(Rect.h<-dy shl 1) then
    with Rect do
    begin
      w := 0;
      h := 0;
    end
  else
    with Rect do
    begin
      Dec(x,dx);
      Dec(y,dy);
      w := w + dx shl 1;
      h := h + dy shl 1;
    end;
end;

procedure OffsetRect(var Rect: TSDL_Rect;dx,dy: Integer);
begin
  with Rect do
  begin
    Inc(x,dx);
    Inc(y,dy);
  end;
end;

function PointInRect(const Point: TPoint;const Rect: TSDL_Rect): Boolean;
begin
   with Rect do
    Result := (Point.X >= x) and
      (Point.X < x+w) and
      (Point.Y >= y) and
      (Point.Y < y+h);
end;

function RectInRect(const Rect1, Rect2: TSDL_Rect): Boolean;
begin
  Result := (Rect1.x >= Rect2.x) and
            (Rect1.x+Rect1.w <= Rect2.x+Rect2.w) and
            (Rect1.y >= Rect2.y) and
            (Rect1.y+Rect1.h <= Rect2.y+Rect2.h);
end;

function OverlapRect(const Rect1, Rect2: TSDL_Rect): Boolean;
begin
  Result := (Rect1.x < Rect2.x+Rect2.w-2) and
            (Rect1.x+Rect1.w > Rect2.x+2) and
            (Rect1.y < Rect2.y+Rect2.h-2) and
            (Rect1.y+Rect1.h > Rect2.y+2);
end;

procedure WriteString(Stream: TStream;const Value: String);
var l: Word;
begin
  l := Length(Value);
  Stream.Write(l,SizeOf(Word));
  if l>0 then
    Stream.Write(PChar(Value)^,l);
end;

procedure ReadString(Stream: TStream;var Value: String);
var l: Word;
begin
  Stream.Read(l,SizeOf(Word));
  if l = 0 then
  begin
    Value := '';
    Exit;
  end;
  SetLength(Value,l);
  Stream.Read(PChar(Value)^,l);
end;

end.
