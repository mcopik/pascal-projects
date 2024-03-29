program JEDISDLUtilsTest;
{******************************************************************************}
{                                                                              }
{    April 26   2003 - SS : Added ClipLine Demo and also ZoomSurface Demo      }
{                                                                              }
{******************************************************************************}
uses
  SysUtils,
  sdl,
  sfont,
  sdlutils;

const
  Screen_Width = 640;
  Screen_Height = 480;
  Screen_BPP = 32;

var
  Font, Screen : PSDL_Surface;
  Quit, CloseWindow : boolean;
  video_flags : cardinal;

procedure DrawBackground;
var
  x, y : integer;
begin
  SDL_FillRect( Screen, nil, 0 );
  SFont_Write( Screen, 0, 0, 'Working...' );
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  SDL_LockSurface( Screen );
  for y := 0 to Screen.h - 1 do
  begin
    for x := 0 to Screen.w - 1 do
      SDL_PutPixel( Screen, x, y, SDL_MapRGB( Screen.format, trunc( x * 256 /
        Screen.w ), trunc( y * 256 / Screen.h ), 255 - trunc( y * 256 / Screen.h
          )
        ) );
  end;
  SDL_UnlockSurface( Screen );
end;

function TestESC : boolean;
var
  Event : TSDL_Event;
begin
  Result := false;
  while SDL_PollEvent( @Event ) > 0 do
  begin
    case Event.key.type_ of
      SDL_QUITEV :
      begin
        CloseWindow := true;
        Result := True;
      end;

      SDL_KeyDown :
      begin
        case Event.key.keysym.sym of
          SDLK_ESCAPE, SDLK_SPACE :
            Result := true;
        end;
      end;
    end;
  end;
end;

procedure PutPixel_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $FF + 1;
    15 : MaxColor := $7FFF + 1;
    16 : MaxColor := $FFFF + 1;
    24 : MaxColor := $FFFFFF + 1;
    32 : MaxColor := $FFFFFF + 1;
  end;
  
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 99999 do
      begin
        color := random( MaxColor );
        SDL_PutPixel( Screen, random( Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '100000 PutPixels in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure NewPutPixel_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $FF + 1;
    15 : MaxColor := $7FFF + 1;
    16 : MaxColor := $FFFF + 1;
    24 : MaxColor := $FFFFFF + 1;
    32 : MaxColor := $FFFFFF + 1;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 99999 do
      begin
        color := random( MaxColor );
        SDL_PutPixel( Screen, random( Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '100000 NewPutPixels in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure AddPixel_test;
var
  Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  DrawBackground;
  Average := 0;
  Counts := 0;
  Color := 0;
  case Screen.format.BitsPerPixel of
    8 : Color := $65;
    15 : Color := $0842;
    16 : Color := $1082;
    24 : Color := $080808;
    32 : Color := $080808;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 99999 do
      begin
        SDL_AddPixel( Screen, random( Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '100000 AddPixels in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure SubPixel_test;
var
  Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  DrawBackground;
  Average := 0;
  Counts := 0;
  Color := 0;
  case Screen.format.BitsPerPixel of
    8 : Color := $65;
    15 : Color := $0842;
    16 : Color := $1082;
    24 : Color := $080808;
    32 : Color := $080808;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 99999 do
      begin
        SDL_SubPixel( Screen, random( Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '100000 SubPixels in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure DrawLine_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $FF + 1;
    15 : MaxColor := $7FFF + 1;
    16 : MaxColor := $FFFF + 1;
    24 : MaxColor := $FFFFFF + 1;
    32 : MaxColor := $FFFFFF + 1;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 9999 do
      begin
        Color := random( MaxColor );
        SDL_DrawLine( Screen, random( Screen.w ), random( Screen.h ), random(
          Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '10000 DrawLines in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure DrawDashedLine_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  x0, x1, y0, y1: word;
  Msg : string;
begin
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $FF + 1;
    15 : MaxColor := $7FFF + 1;
    16 : MaxColor := $FFFF + 1;
    24 : MaxColor := $FFFFFF + 1;
    32 : MaxColor := $FFFFFF + 1;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 9999 do
      begin
        Color := random( MaxColor );
        x0 := random (Screen.w -1);
        x1 := random (Screen.w -1);
        y0 := random (Screen.h -1);
        y1 := random (Screen.h -1);
        SDL_DrawLine( Screen, x0, y0, x1, y1, Color, 10, 10 );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '10000 Dashed DrawLines in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

//Draw some lines on the left half of the screen
//then on the right, show what it would look like if clipped
procedure ClipLine_Test;
var
  MaxColor, Color, i : cardinal;
  x0, x1, y0, y1: integer;
  ClipRect : SDL_Rect;
  Msg : string;
begin
  SDL_FillRect( Screen, nil, 0 );
  MaxColor := 0;
  ClipRect.x := (screen.w div 4) div 2;
  ClipRect.y := screen.h div 4;
  ClipRect.w := (Screen.w div 2) div 2;
  ClipRect.h := Screen.h div 2;

  case Screen.format.BitsPerPixel of
    8 : MaxColor := $FF + 1;
    15 : MaxColor := $7FFF + 1;
    16 : MaxColor := $FFFF + 1;
    24 : MaxColor := $FFFFFF + 1;
    32 : MaxColor := $FFFFFF + 1;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      for i := 1 to 10 do
      begin
        Color := random( MaxColor );
        x0 := random ((Screen.w -1) div 2);
        x1 := random ((Screen.w -1) div 2);
        y0 := random (Screen.h -1);
        y1 := random (Screen.h -1);
        SDL_DrawLine( Screen, x0, y0, x1, y1, Color);

        //clip the line and draw the result
        if SDL_ClipLine( x0, y0, x1, y1, @ClipRect) then
        SDL_DrawLine( Screen, x0 + (screen.w div 2), y0, x1 + (screen.w div 2), y1, Color);

      end;

        //Draw box to show clipping area
        SDL_DrawLine( Screen, ClipRect.x, Cliprect.y, ClipRect.x + ClipRect.w, ClipRect.y, 0);
        SDL_DrawLine( Screen, ClipRect.x, Cliprect.y, ClipRect.x, ClipRect.y + ClipRect.h, 0);
        SDL_DrawLine( Screen, ClipRect.x, Cliprect.y+ ClipRect.h, ClipRect.x + ClipRect.w, ClipRect.y + ClipRect.h, 0);
        SDL_DrawLine( Screen, ClipRect.x + ClipRect.w, Cliprect.y, ClipRect.x + ClipRect.w, ClipRect.y + ClipRect.h, 0);

      SDL_UnlockSurface( Screen );
      Msg := 'Testing line clipping';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure AddLine_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  DrawBackground;
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $65;
    15 : MaxColor := $0842;
    16 : MaxColor := $1082;
    24 : MaxColor := $080808;
    32 : MaxColor := $080808;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 999 do
      begin
        Color := random( MaxColor );
        SDL_AddLine( Screen, random( Screen.w ), random( Screen.h ), random(
          Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '1000 AddLines in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure SubLine_Test;
var
  MaxColor, Color, i, TickStart, Average, Counts : cardinal;
  Msg : string;
begin
  DrawBackground;
  Average := 0;
  Counts := 0;
  MaxColor := 0;
  case Screen.format.BitsPerPixel of
    8 : MaxColor := $65;
    15 : MaxColor := $0842;
    16 : MaxColor := $1082;
    24 : MaxColor := $080808;
    32 : MaxColor := $080808;
  end;
  if not CloseWindow then
    repeat
      SDL_LockSurface( Screen );
      TickStart := SDL_GetTicks;
      for i := 0 to 999 do
      begin
        Color := random( MaxColor );
        SDL_SubLine( Screen, random( Screen.w ), random( Screen.h ), random(
          Screen.w ), random( Screen.h ), Color );
      end;
      TickStart := SDL_GetTicks - TickStart;
      SDL_UnlockSurface( Screen );
      Msg := '1000 SubLines in ' + inttostr( TickStart ) + ' ticks';
      SFont_Write( Screen, 0, 0, pchar( msg ) );
      inc( Counts );
      inc( Average, TickStart );
      Msg := 'Average: ' + IntToStr( Average div Counts );
      SFont_Write( Screen, 0, 25, pchar( msg ) );
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    until TestESC;
end;

procedure AddSurface_Test;
var
  Blob, Buffer : PSDL_Surface;
  i : integer;
  Blobs : array[0..99] of record
    Pos : TSDL_Rect;
    xi, yi : integer;
  end;
begin
  Buffer := SDL_CreateRGBSURFACE( SDL_SWSURFACE, Screen.w, Screen.h,
    Screen.format.bitsperpixel, Screen.format.rmask, Screen.format.gmask,
    Screen.format.bmask, Screen.format.amask );
  DrawBackground;
  SFont_Write( Buffer, 0, 0, 'Testing SDL_AddSurface' );
  SDL_BlitSurface( Screen, nil, Buffer, nil );
  Quit := false;
  Blob := SDL_LoadBMP( 'images/Blob.bmp' );
  // Make sure that source image's format is the same as display format
  Blob := SDL_DisplayFormat( Blob );
  for i := 0 to 99 do
  begin
    Blobs[i].Pos := SDLUtils.SDLRect( random( 600 ), random( 440 ), 0, 0 );
    Blobs[i].xi := 1 + random( 6 );
    if Blobs[i].xi > 3 then
      Blobs[i].xi := Blobs[i].xi - 7;
    Blobs[i].yi := 1 + random( 6 );
    if Blobs[i].yi > 3 then
      Blobs[i].yi := Blobs[i].yi - 7;
  end;
  if not CloseWindow then
    repeat
      SDL_BlitSurface( Buffer, nil, Screen, nil );
      for i := 0 to 99 do
      begin
        with Blobs[i] do
        begin
          Pos.x := Pos.x + xi;
          Pos.y := Pos.y + yi;
          if ( Pos.x < -32 ) or ( Pos.x > Screen.w ) then
            xi := -xi;
          if ( Pos.y < -32 ) or ( Pos.y > Screen.h ) then
            yi := -yi;
        end;
        SDL_AddSurface( Blob, nil, Screen, @Blobs[i].Pos );
      end;
      SFont_Write( Buffer, 0, 0, 'Testing SDL_AddSurface' );
      SDL_Flip( Screen );
      SDL_Delay( 20 );
    until TestESC;
  SDL_FreeSurface( Blob );
  SDL_FreeSurface( Buffer );
end;

procedure SubSurface_Test;
var
  Blob, Buffer : PSDL_Surface;
  i : integer;
  Blobs : array[0..99] of record
    Pos : TSDL_Rect;
    xi, yi : integer;
  end;
begin
  Buffer := SDL_CreateRGBSURFACE( SDL_SWSURFACE, Screen.w, Screen.h,
    Screen.format.bitsperpixel, Screen.format.rmask, Screen.format.gmask,
    Screen.format.bmask, Screen.format.amask );
  DrawBackground;
  SFont_Write( Buffer, 0, 0, 'Testing SDL_SubSurface' );
  SDL_BlitSurface( Screen, nil, Buffer, nil );
  Quit := false;
  Blob := SDL_LoadBMP( 'images/Blob.bmp' );
  // Make sure that source image's format is the same as display format
  Blob := SDL_DisplayFormat( Blob );
  for i := 0 to 99 do
  begin
    Blobs[i].Pos := SDLUtils.SDLRect( random( 600 ), random( 440 ), 0, 0 );
    Blobs[i].xi := 1 + random( 6 );
    if Blobs[i].xi > 3 then
      Blobs[i].xi := Blobs[i].xi - 7;
    Blobs[i].yi := 1 + random( 6 );
    if Blobs[i].yi > 3 then
      Blobs[i].yi := Blobs[i].yi - 7;
  end;
  if not CloseWindow then
    repeat
      SDL_BlitSurface( Buffer, nil, Screen, nil );
      for i := 0 to 99 do
      begin
        with Blobs[i] do
        begin
          Pos.x := Pos.x + xi;
          Pos.y := Pos.y + yi;
          if ( Pos.x < -32 ) or ( Pos.x > Screen.w ) then
            xi := -xi;
          if ( Pos.y < -32 ) or ( Pos.y > Screen.h ) then
            yi := -yi;
        end;
        SDL_SubSurface( Blob, nil, Screen, @Blobs[i].Pos );
      end;
      SFont_Write( Buffer, 0, 0, 'Testing SDL_SubSurface' );
      SDL_Flip( Screen );
      SDL_Delay( 20 );
    until TestEsc;
  SDL_FreeSurface( Blob );
  SDL_FreeSurface( Buffer );
end;

procedure ZoomSurface_Test;
var
  Texture, Buffer : PSDL_Surface;
  overlap : SmallInt;
  ZoomSrc, ZoomDst : SDL_Rect;
begin
  Buffer := SDL_CreateRGBSURFACE( SDL_SWSURFACE, Screen.w, Screen.h,
    Screen.format.bitsperpixel, Screen.format.rmask, Screen.format.gmask,
    Screen.format.bmask, Screen.format.amask );
  DrawBackground;
  SFont_Write( Buffer, 0, 0, 'Testing SDL_ZoomSurface' );
  SDL_BlitSurface( Screen, nil, Buffer, nil );
  Quit := false;
  Texture := SDL_LoadBMP( 'images/Texture.bmp' );
  // Make sure that source image's format is the same as display format
  Texture := SDL_DisplayFormat( Texture );

  ZoomSrc.x := 0;
  ZoomSrc.y := 0;
  ZoomSrc.w := Texture.w;
  ZoomSrc.h := Texture.h;

  if not CloseWindow then
    repeat

      ZoomDst.x := random(Screen_Width - 10) + 1;
      ZoomDst.y := random(Screen_Height - 10) + 1;
      ZoomDst.w := random((Screen_Width div 2) - 10) + 10;
      ZoomDst.h := random((Screen_Height div 2)- 10) + 10;

      overlap := (ZoomDst.x + ZoomDst.w) - Screen_Width;
      If overlap > 0 then
        ZoomDst.w := ZoomDst.w - overlap;
      overlap := (ZoomDst.y + ZoomDst.h) - Screen_Height;
      If overlap > 0 then
        ZoomDst.y := ZoomDst.h - overlap;

      SDL_ZoomSurface(Texture, @ZoomSrc, Buffer, @ZoomDst );
      SDL_BlitSurface( Buffer, nil, Screen, nil );
      SFont_Write( Buffer, 0, 0, 'Testing SDL_ZoomSurface' );
      SDL_Flip( Screen );
    until TestEsc;
  SDL_FreeSurface( Texture );
  SDL_FreeSurface( Buffer );
end;

procedure MonoSurface_Test;
var
  Blob, Buffer : PSDL_Surface;
  i : integer;
  Blobs : array[0..99] of record
    Pos : TSDL_Rect;
    xi, yi : integer;
    Color : cardinal;
  end;
begin
  Buffer := SDL_CreateRGBSURFACE( SDL_SWSURFACE, Screen.w, Screen.h,
    Screen.format.bitsperpixel, Screen.format.rmask, Screen.format.gmask,
    Screen.format.bmask, Screen.format.amask );
  DrawBackground;
  SFont_Write( Buffer, 0, 0, 'Testing SDL_MonoSurface' );
  SDL_BlitSurface( Screen, nil, Buffer, nil );
  Quit := false;
  Blob := SDL_LoadBMP( 'images/Blob.bmp' );
  // Make sure that source image's format is the same as display format
  Blob := SDL_DisplayFormat( Blob );
  // We really don't need to set the colorkey to 0, because the default is 0
  SDL_SetColorKey( Blob, SDL_SRCCOLORKEY, 0 );
  for i := 0 to 99 do
  begin
    Blobs[i].Pos := SDLUtils.SDLRect( random( 600 ), random( 440 ), 0, 0 );
    Blobs[i].xi := 1 + random( 6 );
    if Blobs[i].xi > 3 then
      Blobs[i].xi := Blobs[i].xi - 7;
    Blobs[i].yi := 1 + random( 6 );
    if Blobs[i].yi > 3 then
      Blobs[i].yi := Blobs[i].yi - 7;
    Blobs[i].Color := SDL_MapRGB( Screen.format, random( 256 ), random( 256 ),
      random( 256 ) );
  end;
  if not CloseWindow then
    repeat
      SDL_BlitSurface( Buffer, nil, Screen, nil );
      for i := 0 to 99 do
      begin
        with Blobs[i] do
        begin
          Pos.x := Pos.x + xi;
          Pos.y := Pos.y + yi;
          if ( Pos.x < -32 ) or ( Pos.x > Screen.w ) then
            xi := -xi;
          if ( Pos.y < -32 ) or ( Pos.y > Screen.h ) then
            yi := -yi;
        end;
        SDL_MonoSurface( Blob, nil, Screen, @Blobs[i].Pos, Blobs[i].Color );
      end;
      SFont_Write( Screen, 0, 0, 'Testing SDL_MonoSurface' );
      SFont_Write( Screen, 0, 25, 'Useful for drawing texts and' );
      SFont_Write( Screen, 0, 50, 'non translucent shadows' );
      SDL_Flip( Screen );
      SDL_Delay( 20 );
    until TestEsc;
  SDL_FreeSurface( Blob );
  SDL_FreeSurface( Buffer );
end;

procedure TexturedSurface_Test;
var
  Blob, Texture, Buffer : PSDL_Surface;
  i : integer;
  Blobs : array[0..99] of record
    Pos, TextureRect : TSDL_Rect;
    TextureDir : cardinal;
    xi, yi : integer;
  end;
begin
  Buffer := SDL_CreateRGBSURFACE( SDL_SWSURFACE, Screen.w, Screen.h,
    Screen.format.bitsperpixel, Screen.format.rmask, Screen.format.gmask,
    Screen.format.bmask, Screen.format.amask );
  DrawBackground;
  SFont_Write( Buffer, 0, 0, 'Testing SDL_AddSurface' );
  SDL_BlitSurface( Screen, nil, Buffer, nil );
  Quit := false;
  Blob := SDL_LoadBMP( 'images/Blob.bmp' );
  // Make sure that source image's format is the same as display format
  Blob := SDL_DisplayFormat( Blob );
  Texture := SDL_LoadBMP( 'images/Texture.bmp' );
  // Make sure that texture's format is the same as display format
  Texture := SDL_DisplayFormat( Texture );
  // We really don't need to set the colorkey to 0, because the default is 0
  SDL_SetColorKey( Blob, SDL_SRCCOLORKEY, 0 );
  for i := 0 to 99 do
  begin
    Blobs[i].Pos := SDLUtils.SDLRect( random( 600 ), random( 440 ), 0, 0 );
    Blobs[i].xi := 1 + random( 6 );
    if Blobs[i].xi > 3 then
      Blobs[i].xi := Blobs[i].xi - 7;
    Blobs[i].yi := 1 + random( 6 );
    if Blobs[i].yi > 3 then
      Blobs[i].yi := Blobs[i].yi - 7;
    Blobs[i].TextureRect.x := 0;
    Blobs[i].TextureRect.y := 0;
    Blobs[i].TextureDir := random( 8 );
  end;
  if not CloseWindow then
    repeat
      SDL_BlitSurface( Buffer, nil, Screen, nil );
      for i := 0 to 99 do
      begin
        with Blobs[i] do
        begin
          Pos.x := Pos.x + xi;
          Pos.y := Pos.y + yi;
          if ( Pos.x < -32 ) or ( Pos.x > Screen.w ) then
            xi := -xi;
          if ( Pos.y < -32 ) or ( Pos.y > Screen.h ) then
            yi := -yi;
          if TextureDir in [0, 1, 7] then
            if TextureRect.y > 0 then
              dec( TextureRect.y )
            else
              TextureDir := random( 8 );
          if TextureDir in [1, 2, 3] then
            if TextureRect.x < Texture.w - Blob.w then
              inc( TextureRect.x )
            else
              TextureDir := random( 8 );
          if TextureDir in [3, 4, 5] then
            if TextureRect.y < Texture.h - Blob.h then
              inc( TextureRect.y )
            else
              TextureDir := random( 8 );
          if TextureDir in [5, 6, 7] then
            if TextureRect.x > 0 then
              dec( TextureRect.x )
            else
              TextureDir := random( 8 );
        end;
        SDL_TexturedSurface( Blob, nil, Screen, @Blobs[i].Pos, Texture,
          @Blobs[i].TextureRect );
      end;
      SFont_Write( Screen, 0, 0, 'Testing SDL_TexturedSurface' );
      SFont_Write( Screen, 0, 25, 'Useful for drawing animated' );
      SFont_Write( Screen, 0, 50, 'surfaces like water or for' );
      SFont_Write( Screen, 0, 75, 'example to simulate burning' );
      SFont_Write( Screen, 0, 100, 'surfaces, for drawing windows' );
      SFont_Write( Screen, 0, 125, 'or portals, text effects.' );
      SDL_Flip( Screen );
      SDL_Delay( 20 );
    until TestEsc;
  SDL_FreeSurface( Blob );
  SDL_FreeSurface( Texture );
  SDL_FreeSurface( Buffer );
end;

begin
  CloseWindow := false;
  randomize;
  SDL_Init( SDL_INIT_VIDEO );

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption( 'JEDI-SDL Utility functions Test', nil );

  // fire and forget...
  if ( ParamStr( 1 ) = '-fullscreen' ) or ( ParamStr( 1 ) = '-fs' ) then
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
  else
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE;

  SDL_ShowCursor( SDL_DISABLE );
  Screen := SDL_SetVideoMode( Screen_Width, Screen_Height, Screen_BPP, video_flags );
  Font := SDL_LoadBMP( 'images/24P_Copperplate_Gradient.bmp' );
  Font := SDL_DisplayFormat( Font );
  SFont_InitFont( Font );
  PutPixel_Test;
  NewPutPixel_Test;
  AddPixel_Test;
  SubPixel_Test;
  DrawLine_Test;
  DrawDashedLine_Test;
  ClipLine_Test;
  AddLine_Test;
  SubLine_Test;
  ZoomSurface_Test;
  AddSurface_Test;
  SubSurface_Test;
  MonoSurface_Test;
  TexturedSurface_Test;
  SDL_FreeSurface( Font );
  SDL_ShowCursor( SDL_ENABLE );
  SDL_Quit;
end.

