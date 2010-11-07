unit sdl_flic;
{******************************************************************}
{                                                                  }
{       Borland Delphi sdl_flic                                    }
{       Conversion of the Linux Games- sdl_flic Library for SDL    }
{                                                                  }
{ Original work created by Andre de Leiradella                     }
{ Copyright (C) 2003 Andre de Leiradella.                                  }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : sdl_flic.c                              }
{                                                                  }
{ The original Pascal code is : SDL_flic.pas                       }
{ The initial developer of the Pascal code is :                    }
{ Dean Ellis <dean@delphigamer.com>                                }
{                                                                  }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
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
{                                                                  }
{    SDL_Flic - SDL FLIC Library by Andre de Leiradella    	   }
{                                                                  }
{    Conversion of the SDL_flic library to Pascal.		   } 
{                                                                  }
{                                                                  }
{                                                                  }
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
{   Jan    01 2006 - DRE : Initial translation.                    }
{******************************************************************}

interface

uses sdl, Classes;

const

  FLI_FLI  = $AF11;
  FLI_FLC  = $AF12;


(*
The animation structure, all members are read-only
*)

type

  TFLI_Frame = record
    size, type_, numchunks:Uint32 ;
  end;
  PFLI_frame = ^TFLI_Frame;

  TFLI_Chunk = record
    size, type_, index: Uint32;
  end;
  PFLI_Chunk = ^TFLI_Chunk;


  FlI_animation = record
        format, numframes, width, height, depth, delay, offframe1, nextframe, offnextframe: UInt32;
        //(* rwops is where the animation is read from. *)
        //rwops: PSDL_RWops;
        Stream: TStream;
        (* surface is where the frames is rendered to. *)
        surface: PSDL_Surface;
        (* Are we drawing a frame already*)
        Locked: Boolean;
  end;
  PFLI_animation = ^FLI_animation;



function FLI_Version: Integer;
(*
Opens a FLIC animation and return a pointer to it. rwops is left at the same
point it was before the the call.
*)
function FLI_Open(AStream: TStream): PFLI_Animation;
(*
Closes the animation, closes the stream and frees all used memory.
*)
procedure FLI_Close(flic: PFLI_animation);
(*
Renders the next frame of the animation returning a bool to indicate if it was
successfull or not.
*)
function FLI_NextFrame(flic: PFLI_animation): Boolean;
(*
Rewinds the animation to the first frame.
*)
function FLI_Rewind(flic: PFLI_animation): Boolean;
(*
Skips the current frame of the animation without rendering it.
*)
function FLI_Skip(flic: PFLI_animation): Boolean;


implementation

uses SysUtils;

const
(* Chunk types. *)
  FLI_COLOR256 = 4;
  FLI_SS2     = 7;
  FLI_COLOR   = 11;
  FLI_LC      = 12;
  FLI_BLACK   = 13;
  FLI_BRUN    = 15;
  FLI_COPY    = 16;
  FLI_PSTAMP  = 18;

procedure readbuffer(flic: PFLI_animation; buffer: Pointer; Size: Integer);
begin
  flic.Stream.ReadBuffer(Buffer^, Size);
end;

function readu8(flic: PFLI_animation): UInt8;
begin
  readbuffer(flic, @Result, Sizeof(Uint8));
end;
function readu16(flic: PFLI_animation): UInt16;
var hi, lo: Uint16;
begin
  if SDL_BYTEORDER = SDL_BIG_ENDIAN then
  begin
    readbuffer(flic, @lo, Sizeof(UInt8));
    readbuffer(flic, @hi, Sizeof(UInt8));
    Result := (hi shr 8) or lo;
  end
  else
  begin
    readbuffer(flic, @Result, Sizeof(Uint16));
  end;
end;
function readu32(flic: PFLI_animation): UInt32;
var hi: Uint32;
begin
  if SDL_BYTEORDER = SDL_BIG_ENDIAN then
  begin
    hi := readu16(flic);
    Result := (hi shr 16) or readu16(flic);
  end
  else
  begin
    readbuffer(flic, @Result, Sizeof(Uint32));
  end;
end;

procedure readheader(flic: PFLI_animation);
begin
  flic.Stream.Seek(4,soFromCurrent);
  flic.format := readu16(flic);
  if (flic.format <> FLI_FLI) and (flic.format <> FLI_FLC) then
  begin
    raise Exception.Create('Invalid file format');
    Exit;
  end;
  flic.numframes := readu16(flic);
  if flic.numframes > 4000 then
  begin
    raise Exception.Create('Num frames exceed 4000');
    Exit;
  end;
  flic.width := readu16(flic);
  flic.height := readu16(flic);
  if (flic.format = FLI_FLI) and ((flic.width <> 320) or (flic.height <> 200)) then
  begin
    raise Exception.Create('FLi File must be 320x200');
    Exit;
  end;
  flic.depth := readu16(flic);
  if (flic.depth <> 8) then
  begin
    raise Exception.Create('FLi File must be 8 bit');
    Exit;
  end;
  readu16(flic);
  if (flic.format = FLI_FLI) then
  begin
    flic.delay := readu16(flic);
    flic.Stream.Seek(110,soFromCurrent);
  end
  else
  begin
    flic.delay := readu32(flic);
    flic.Stream.Seek(108,soFromCurrent);
  end;
end;

procedure readframe(flic: PFLI_animation;frame: PFLI_Frame);
begin
  frame.size := readu32(flic);
  if (flic.format = FLI_FLI) and (frame.size > 65536) then
  begin
     raise Exception.Create('FLi File is corrupt');
     Exit;
  end;
  frame.type_ := readu16(flic);
  if (frame.type_ <> $F1FA) then 
  begin
    if ((flic.format = FLI_FLC) and ((frame.type_ <> $F100) or (frame.type_ <> $F1FA))) then
    begin
      raise Exception.Create('FLi File is corrupt');
      Exit;
    end;
  end;
  frame.numchunks := readu16(flic);
  flic.Stream.Seek(8,soFromCurrent);
end;
procedure readchunk(flic: PFLI_animation;chunk: PFLI_Chunk);
begin
  chunk.size := readu32(flic);
  chunk.type_ := readu16(flic);
end;

procedure handlecolor(flic: PFLI_animation;chunk: PFLI_Chunk);
var color: TSDL_Color;
    numpackets, index, count: Uint16;
begin
  numpackets := readu16(flic);
  index := 0;
  while (numpackets > 0) do
  begin
    index := index+ readu8(flic);
    count := readu8(flic);
    if (count = 0) then count := 256;
    while (count > 0) do
    begin
      color.r := (Uint32(readu8(flic))) * 255 div 63;
      color.g := (Uint32(readu8(flic))) * 255 div 63;
      color.b := (Uint32(readu8(flic))) * 255 div 63;
      SDL_SetColors(flic.surface, @color, index, 1);
      inc(index);
      dec(count);
    end;
    dec(numpackets)
  end;
end;

procedure handlelc(flic:PFLI_animation;chunk: PFLI_Chunk);
var line , p: PUInt8;
    numlines, numpackets, size: Integer;
begin
  line :=PUint8( Integer(flic.Surface.pixels) + readu16(flic) * flic.Surface.pitch);
  numlines := readu16(flic);
  while (numlines > 0) do
  begin
    p := line;
    line := PUint8(Integer(line) + flic.Surface.pitch);
    numpackets := readu8(flic);
    while numpackets > 0 do
    begin
      p := PUint8(Integer(p)+ readu8(flic));
      size := Sint8(readu8(flic));
      if size >= 0 then
        readbuffer(flic, p, size)
      else
      begin
        size := -size;
        FillChar(p^, size, readu8(flic));
      end;
      p := PUint8(Integer(p) + Size);
      dec(numpackets);
    end;
    dec(numlines);
  end;
end;

procedure handleblack(flic:PFLI_animation;chunk: PFLI_Chunk);
begin
  SDL_FillRect(flic.surface, nil, 0);
end;

procedure handlebrun(flic:PFLI_animation;chunk: PFLI_Chunk);
var next , p: PUInt8;
    numlines, numpackets, size: Integer;
begin
  p := flic.Surface.pixels;
  numlines := flic.height;
  while (numlines > 0) do
  begin
          //* The number of packages is ignored, packets run until the next line is reached. */
    readu8(flic);
    next := PUint8(Integer(p) + flic.Surface.pitch);
    while (Integer(p) < Integer(next)) do
    begin
       // size pixels will change. */
       size := SInt8(readu8(flic));
       if (size < 0) then
       begin
          size := -size;
          //* Pixels follow. */
          readbuffer(flic, p, size);
       end
       else
       begin
          //* One pixel to be repeated follow. */
          FillChar(p^, size, readu8(flic));
       end;
       p := PUint8(Integer(p) + size);
     end;
     dec(numlines);
  end;
end;

procedure handlecopy(flic:PFLI_animation;chunk: PFLI_Chunk);
begin
  readbuffer(flic, flic.surface.pixels, flic.width * flic.height);
end;

procedure handlecolor256(flic:PFLI_animation;chunk: PFLI_Chunk);
var color: TSDL_Color;
    numpackets, index, count: Uint16;
begin
  if (flic.Format = FLI_FLI) then raise Exception.Create('Corrupt Chunk');
  numpackets := readu16(flic);
  index := 0;
  while (numpackets > 0) do
  begin
    index := index + readu8(flic);
    count := readu8(flic);
    if (count = 0) then count := 256;
    while (count > 0) do
    begin
      color.r := readu8(flic);
      color.g := readu8(flic);
      color.b := readu8(flic);
      SDL_SetColors(flic.Surface, @color, index, 1);
      inc(index);
      dec(count);
    end;
    dec(numpackets)
  end;
end;

procedure handless2(flic:PFLI_animation;chunk: PFLI_Chunk);
var numlines, y, code, size: Uint32;
    p: PUint8;
    c: Uint8;
begin
  if (flic.Format = FLI_FLI) then raise Exception.Create('Invalid Chunk');
  numlines := readu16(flic);
  y := 0;
  while (numlines > 0) do
  begin
    code := readu16(flic);
    case ((code shr 14) and $03) of
      $00:
        begin
          p := PUint8(Uint32(flic.Surface.pixels) + flic.Surface.pitch * y);
          while (code > 0) do
          begin
            // Skip some pixels.
            p := PUint8(Integer(p) + readu8(flic));
            size := SInt8(readu8(flic)) * 2;
            if (size >= 0) then
            begin
               // Pixels follow.
               readbuffer(flic, Pointer(p), size);
            end
            else
            begin
              size := -size;
              readu8(flic);
              FillChar(p, size, readu8(flic));
            end;
            p := PUint8(Integer(p)+size);
            dec(code);
          end;
          y := y + 1;
          dec(numlines);
        end;
      $01: raise Exception.Create('Invalid Chunk');
      $02:
        begin
          // Last pixel of the line. */
          p := Pointer(UInt32(flic.Surface.pixels) + flic.Surface.pitch * UInt32(y + 1));
          //p[-1] = code & 0xFF;
          PUint8(p^-1)^ := code and $FF;
        end;
      $03: inc(y,(code xor $FFFF) + 1);
    end;
  end;
end;

// Library version.
const  FLI_MAJOR = 1;
const  FLI_MINOR = 2;

function FLI_Version: Integer;
begin
  Result :=  (FLI_MAJOR shl 16) or FLI_MINOR;
end;

function FLI_Open(AStream: TStream): PFLI_Animation;
var Frame: TFLI_frame;
begin
   GetMem(Result, Sizeof(FLI_Animation));
   Result.Stream := AStream;
   Result.surface := nil;
   readheader(Result);
   Result.surface := SDL_CreateRGBSurface(SDL_SWSURFACE, result.width, result.height, 8, 0, 0, 0, 0);
   Result.offframe1 := AStream.Position;
   readframe(Result, @Frame);
   if (Frame.type_ = $F100) then
   begin
     AStream.Seek(Frame.size-16, soFromCurrent);
     Result.offframe1 := AStream.Position;
     dec(Result.numframes);
   end;
   Result.offnextframe := Result.offframe1;
   Result.nextframe := 1;
end;
procedure FLI_Close(flic: PFLI_animation);
begin
  if flic.Stream <> nil then
    flic.Stream.Free;
  if flic.surface <> nil then
    SDL_FreeSurface(flic.surface);
  FreeMem(flic);
end;

function FLI_NextFrame(flic: PFLI_animation): Boolean;
var Frame: TFLI_Frame;
    Chunk: TFLI_Chunk;
    i: Integer;
begin
  Result := True;
  if flic.Locked then Exit;
  Result := SDL_LockSurface(flic.Surface) = 0;
  if not Result then Exit;
  try
    flic.Locked := True;
    flic.Stream.Seek(flic.offnextframe, soFromBeginning);
    ReadFrame(flic, @Frame);
    for i := Frame.numchunks downto 1 do
    begin
      ReadChunk(flic,@Chunk);
      case Chunk.type_ of
        FLI_COLOR: handlecolor(flic,@Chunk);
        FLI_LC: handlelc(flic,@Chunk);
        FLI_BLACK: handleblack(flic,@Chunk);
        FLI_BRUN: handlebrun(flic, @Chunk);
        FLI_COPY: handlecopy(flic, @Chunk);
        FLI_COLOR256: handlecolor256(flic,@Chunk);
        FLI_SS2: handless2(flic,@Chunk);
        FLI_PSTAMP:;
      else
        raise Exception.Create('Invalid Chunk Type');
      end;
    end;
    inc(flic.nextframe);
    inc(flic.offnextframe, Frame.size);
    if flic.nextframe > flic.numframes then
    begin
      flic.offnextframe := flic.offframe1;
      flic.nextframe := 1;
    end;
  finally
    SDL_UnlockSurface(flic.Surface);
    flic.Locked := False;
  end;
end;

function FLI_Rewind(flic: PFLI_animation): Boolean;
begin
  flic.offnextframe := flic.offframe1;
  flic.nextframe := 1;
  Result := True;
end;

function FLI_Skip(flic: PFLI_animation): Boolean;
var Frame: TFLI_Frame;
begin
  flic.Stream.Seek(flic.offnextframe, soFromCurrent);
  readframe(flic, @Frame);
  if flic.nextframe+1 > flic.numframes then
  begin
    flic.offnextframe := flic.offframe1;
    flic.nextframe := 1;
  end
  else
  begin
    inc(flic.offnextframe, frame.size);
  end;
  Result := True;
end;

end.

