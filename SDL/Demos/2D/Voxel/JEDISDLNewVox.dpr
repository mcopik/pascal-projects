program JEDISDLNewVox;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Voxel Example                           }
{       Conversion of the SDL Voxel Demo                           }
{                                                                  }
{ Portions created by Andrea "6502" Griffini <agriff@ix.netcom.com>}
{ are  Copyright (C) Andrea "6502" Griffini.                  }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : newvox.c                                }
{                                                                  }
{ The original Pascal code is : JEDISDLNewVox.dpr                  }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2000 - 2001 Dominique Louis.                       }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ <Contributer Name> ( contributer@sp.sw )                         }
{ Uberto Barbini ( uberto@ubiland.net )                            }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{   Shows how to do voxel landscape rendering.                     }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   March   07 2001 - DL : Initial translation.                    }
{   June    26 2001 - DL : Combined Delphi and Kylix Demo          }
{   June    29 2003 - UB : Some improvement and bug fixes          }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  logger,
  sdl;

const
  SCREEN_WIDTH = 800; //320;
  SCREEN_HEIGHT = 600; //200;
  SCREEN_HORIZ = 180; //100;
  MAX_FAR = 100; //100
  ALTITUDE = 30; //30;

var
  HMap: array[0..(256*256)-1] of byte;      // Height field */
  CMap: array[0..(256*256)-1] of byte;      // Color map */
  Video: array[0..(SCREEN_WIDTH*SCREEN_HEIGHT)-1] of byte;     // Off-screen buffer */
  FOV:  Extended = 3.141592654/4;   // half of the xy field of view */
  lasty: array[0..SCREEN_WIDTH - 1] of integer; // Last pixel drawn on a given column */
  lastc: array[0..SCREEN_WIDTH - 1] of integer; // Color of last pixel on a column */

type
  TColorArray = array[0..64] of TSDL_Color;

// Reduces a value to 0..255 (used in height field computation) */
function Clamp(x : integer): integer;
begin
  if x < 0 then
    result := 0
  else
    if x > 255 then
      result := 255
    else
      result := x;
end;

// Heightfield and colormap computation */
procedure ComputeMap;
var
  h, p, i, j, k, k2, p2 : integer;
  a,b,c,d : integer;
begin
  // Start from a plasma clouds fractal */
  HMap[0]:=128;
  p := 256;

  while p > 1 do
  begin
    p2:=p shr 1;
    k := p * 8 + 20;
    k2:=k shr 1;
    i := 0;
    while i < 256 do
    begin
      j := 0;
      while j < 256 do
      begin
        a := HMap[(i shl 8)+j];
        b := HMap[(((i+p) and 255) shl 8)+j];
        c := HMap[(i shl 8)+((j+p) and 255)];
        d := HMap[(((i+p) and 255) shl 8)+((j+p) and 255)];

        HMap[(i shl 8)+((j+p2) and 255)]:= Clamp(((a+c) shr 1)+((random(1000) mod k-k2)));
        HMap[(((i+p2) and 255) shl 8)+((j+p2) and 255)]:= Clamp(((a+b+c+d) shr 2)+( random(1) mod k-k2));
        HMap[(((i+p2) and 255) shl 8)+j]:= Clamp(((a+b) shr 1)+((random(5000) mod k-k2)));
        j := j + p;
      end;
      i := i + p;
    end;
    p := p2;
  end;

  // Smoothing (3-passes)
  for k := 0 to 2 do
  begin
    i := 0;
    while i<(256*256)  do
    begin
      for j := 0 to 255 do
      begin
	      h := (HMap[((i+256) and $FF00)+j]+HMap[i+((j+1) and $FF)]+
		        HMap[((i-256)  and $FF00)+j]+HMap[i+((j-1) and $FF)]) shr 2;
        HMap[i+j] := h;
      end;
      inc(i,256);
    end;
  end;


  if random(2) = 0 then //50% of times reverse the map
  begin
    i := 0;
    while i<(256*256)  do
    begin
      for j := 0 to 255 do
      begin
        HMap[i+j] := 255 - HMap[i+j];
      end;
      inc(i,256);
    end;
  end;

//colors
  i := 0;
  while i<(256*256)  do
  begin
    for j:=0 to 255 do
    begin
      k:=128+(HMap[((i+256) and $FF00)+((j+1) and 255)]-HMap[i+j])*4;
      if ( k<0 ) then
        k:=0;
      if (k>255) then
        k:=255;
      CMap[i+j]:= k;
    end;
    inc(i,256);
  end;

end;

{  Draw a "section" of the landscape; x0,y0 and x1,y1 and the xy coordinates
   on the height field, hy is the viewpoint height, s is the scaling factor
   for the distance. x0,y0,x1,y1 are 16.16 fixed point numbers and the
   scaling factor is a 16.8 fixed point value. }

procedure Line(x0, y0, x1, y1, hy, s: integer );
type
  PByte = ^Byte;
var
  i,sx,sy : integer;
  c,y,h,u0,v0,u1,v1,a,b,h0,h1,h2,h3 : integer;
  b1 : PByte;
  sc, cc : integer;
begin
  // Compute xy speed */
  sx := (x1-x0) div SCREEN_WIDTH;
  sy := (y1-y0) div SCREEN_WIDTH;
  for i := 0 to SCREEN_WIDTH - 1 do
  begin
    { Compute the xy coordinates; a and b will be the position inside the
       single map cell (0..255). }
    u0 := (x0 shr 16) and $FF;
    v0 := ((y0 shr 8) and $FF00);
    u1 := (u0+1 ) and $FF;
    v1 := (v0+256) and $FF00;
    a:=(x0 shr 8) and 255;
    b:=(y0 shr 8) and 255;

    // Fetch the height at the four corners of the square the point is in */
    h0:=HMap[u0+v0];
    h2:=HMap[u0+v1];
    h1:=HMap[u1+v0];
    h3:=HMap[u1+v1];

    // Compute the height using bilinear interpolation */
    h0:=(h0 shl 8)+a*(h1-h0);
    h2:=(h2 shl 8)+a*(h3-h2);
    h:=((h0 shl 8)+b*(h2-h0)) shr 16;

    // Fetch the color at the four corners of the square the point is in */
    h0:=CMap[u0+v0];
    h1:=CMap[u1+v0];
    h2:=CMap[u0+v1];
    h3:=CMap[u1+v1];

    // Compute the color using bilinear interpolation (in 16.16) */
    h0:=(h0 shl 8)+a*(h1-h0);
    h2:=(h2 shl 8)+a*(h3-h2);
    c:=((h0 shl 8)+b*(h2-h0));

    // Compute screen height using the scaling factor
    //if hy is major than h it's higher in world
    y := (((h - hy) * s) div 2048) + SCREEN_HORIZ; //shr fails with negatives

    if y < 0 then
      y := 0;

    a := lasty[i];
    if a >= SCREEN_HEIGHT then //cut pits too low
      a := SCREEN_HEIGHT - 1;

    // Draw the column  from a (last height) to y (current height)
    if ( y < a ) then
    begin
      b1 := PByte(Integer(@Video[0]) + a * SCREEN_WIDTH + i);

      if lastc[i] = -1 then
      	lastc[i] := c;

      cc := lastc[i];
      sc := ( c - cc ) div ( a - y );

      while ( y < a ) do
      begin
      	b1^ := cc shr 18; //actually it draws the color
        inc( cc, sc );
      	dec( b1, SCREEN_WIDTH );
        dec( a );
      end;
      lasty[ i ] := y;
    end;
    lastc[ i ] := c;

    // Advance to next xy position */
    inc( x0, sx );
    inc( y0, sy );
  end;
end;


// Draw the view from the point x0,y0 (16.16) looking at angle a
procedure View(x0 : integer; y0 : integer; aa : single; screen_ : PSDL_Surface );
var
  hy, d, a,b,h,u0,v0,u1,v1,h0,h1,h2,h3 : integer;
  row : integer;
  src, dst : PUint8;
begin
  // Clear offscreen buffer */
  FillChar( Video[0], SCREEN_WIDTH * SCREEN_HEIGHT, 64);

  // Initialize last-y and last-color arrays */
  for d := 0 to SCREEN_WIDTH - 1 do
  begin
    lasty[ d ] := SCREEN_HEIGHT;
    lastc[ d ] := -1;
  end;

  // Compute viewpoint height value */

  { Compute the xy coordinates; a and b will be the position inside the
     single map cell (0..255).
   }
  u0 := ( x0 shr 16 ) and $FF;
  a := ( x0 shr 8 ) and 255;
  v0 := ( ( y0 shr 8 ) and $FF00);
  b := ( y0 shr 8 ) and 255;
  u1 := ( u0+1 ) and $FF;
  v1 := ( v0+256 ) and $FF00;

  // Fetch the height at the four corners of the square the point is in */
  h0 := HMap[u0+v0];
  h1 := HMap[u1+v0];
  h2 := HMap[u0+v1];
  h3 := HMap[u1+v1];

  // Compute the height using bilinear interpolation */
  h0 := (h0 shl 8) + a * (h1-h0);
  h2 := (h2 shl 8) + a * (h3-h2);
  h := ((h0 shl 8) + b * (h2-h0)) shr 16;

  //compute height of view point
  hy := h - ALTITUDE;

  // Draw the landscape from near to far without overdraw */
  d := 0;
  while d < MAX_FAR do
  begin
    Line( Round( x0 + d * 65536 * cos( aa - FOV ) ),
          Round( y0 + d * 65536 * sin( aa - FOV ) ),
          Round( x0 + d * 65536 * cos( aa + FOV ) ),
          Round( y0 + d * 65536 * sin( aa + FOV ) ),
          hy,
          Round( MAX_FAR * 256 / ( d + 1 ) ) );
    inc( d , 1 + ( d shr 6) );
  end;

  // Blit the final image to the screen */
  if ( SDL_LockSurface(screen_)  = 0 ) then
  begin
    src  :=  @Video[0];
    dst  :=  PUint8(screen_.pixels);

    for row := screen_.h downto 1 do
    begin
      Move(src^, dst^, SCREEN_WIDTH);
      inc( src, SCREEN_WIDTH );
      inc( dst, screen_.pitch );
    end;
    SDL_UnlockSurface(screen_);
  end;
  SDL_UpdateRect(screen_, 0, 0, 0, 0);
end;

//create the gradient for color palette
procedure SetUpColors(var colors: TColorArray);
var
  i: integer;
begin
  // Set up the first 64 colors to a grayscale */
{
  for i := 0 to 63 do
  begin
    colors[ i ].r := i * 4;
    colors[ i ].g := i * 4;
    colors[ i ].b := i * 4;
  end;
}
  for i := 0 to 63 do
  begin
    colors[ i ].r := 0;
    colors[ i ].g := i * 4;
    colors[ i ].b := 0;
  end;
  colors[64].r := 128;
  colors[64].g := 128;
  colors[64].b := 255;
end;

var
  screen_ : PSDL_Surface;
  done : Boolean;
  ss,sa,a,s : Single;
  x0,y0 : integer;
  colors : TColorArray;
  event : TSDL_Event;
  keystate : PKeyStateArr;
  video_flags : Cardinal;
begin
  Randomize;
  // Initialize SDL */
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) then
  begin
    Log.LogError(Format('Could not initialize SDL: %s', [SDL_GetError]), 'Main');
    SDL_Quit;
    exit;
  end;

  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
  else
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE;

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption('JEDI-SDL Voxel Demo', nil);

  // Enter 320x200x256 mode */
  screen_ := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, 8, video_flags );
  if ( screen_ = nil ) then
  begin
    Log.LogError(Format('ould not init video mode: %s', [SDL_GetError]), 'Main');
    SDL_Quit;
    exit;
  end;

  SetUpColors(colors);

  SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, 65);

  // Compute the height map */
  ComputeMap;


  {* Main loop

       a     := angle
       x0,y0 := current position
       s     := speed constant
       ss    := current forward/backward speed
       sa    := angular speed
   *}
  done:= false;
  a := 0;
  x0 := 0;
  y0 := 0;
  s := 100; //4696; //s:=4096;*/
  ss := 0;
  sa := 0;

  while( not done ) do
  begin
    // Draw the frame */
    View(x0,y0,a,screen_);

    // Update position/angle */
    x0 := Round( x0 + ss * cos(a) );
    y0 := Round( y0 + ss * sin(a) );
    a := a + sa;

    // Slowly reset the angle to 0 */
    if ( sa <> 0 ) then
    begin
      if ( sa < 0 ) then
        sa := sa + 0.001
      else
        sa := sa - 0.001;
    end;

    // User input */
    while ( SDL_PollEvent( @event ) = 1 ) do
    begin
      if ( event.type_ = SDL_QUITEV ) then
      begin
        done := true;
      end;
    end;
    keystate := PKeyStateArr(SDL_GetKeyState(nil));
    if ( keystate[SDLK_ESCAPE] <> 0 ) then
    begin
      done := true;
    end;
    if ( keystate[SDLK_UP] <> 0 ) then
    begin
      ss := ss + s ;
    end;
    if ( keystate[SDLK_DOWN] <> 0 ) then
    begin
      ss := ss - s;
    end;
    if ( keystate[SDLK_RIGHT] <> 0 ) then
    begin
      sa := sa + 0.002;
    end;
    if ( keystate[SDLK_LEFT] <> 0 ) then
    begin
      sa := sa - 0.002;
    end;
  end;

end.
