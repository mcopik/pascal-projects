program custcursor1;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                   Custom Cursor demo                                         }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ August Logan Bear, Jr <augustjr@columbus.rr.com>.                            }
{                                                                              }
{ Portions created by August Logan Bear, Jr. are                               }
{ Copyright (C) 2002 - 2100 August Logan Bear, Jr.                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ August Logan Bear, Jr <augustjr@columbus.rr.com>                             }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Shows how to use the SDLUtils function SDL_2xBlit for zooming              }
{   And to perform and Alpha Mask.                                             }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   Demonstrates Loading a Cursor from an XPM File                             }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{  January 16           2002 - LB : Initial creation                           }
{                                                                              }
{                                                                              }
{******************************************************************************}

uses
  SysUtils,
  logger,
  sdl;

const
  TITLE = 'Load Cursor From XPM File Demo';
  FRAMES_PER_SEC = 60;
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 32;

type
  cursorInfo = array of UInt8;

var
  screen: PSDL_Surface;
  flags: UInt32;
  next_tick: UInt32;
  redCursor, blueCursor, greenCursor, stdCursor, holdCursor: PSDL_Cursor;
  dstRect: SDL_Rect;
  Event: TSDL_Event;
  Quit: Boolean;

procedure WaitFrame;
var
  this_tick: Uint32;
begin
  // Wait for the next frame */
  this_tick := SDL_GetTicks();
  if (this_tick < next_tick) then
  begin
    SDL_Delay(next_tick - this_tick);
  end;
  next_tick := this_tick + (1000 div FRAMES_PER_SEC);
end; // WaitFrame

function ScanForChar(str: string; ch: Char; startPos: Integer; lookFor:
  Boolean): Integer;
begin
  Result := -1;
  while (((str[startPos] = ch) <> lookFor) and (startPos < Length(str))) do
    inc(startPos);
  if startPos <> Length(str) then
    Result := startPos;
end;

function LoadCursorFromXPMFile(Filename: string; hot_x, hot_y: Integer):
  PSDL_Cursor;
var
  fileString: string;
  step: Integer;
  holdPos: Integer;
  counter: Integer;
  dimensions: array[1..3] of Integer;
  clr, clrNone, clrBlack, clrWhite: Char;
  data, mask: cursorInfo;
  i, col: Integer;
  xpmFile: Textfile;
begin
  AssignFile(xpmFile, FileName);
  Reset(xpmFile);
  step := 0;
  i := -1;
  clrBlack := 'X';
  clrWhite := ',';
  clrNone := ' ';
  counter := 0;
  while not (eof(xpmFile)) do
  begin
    Readln(xpmFile, fileString);
    // scan for strings
    if fileString[1] = '"' then
    begin
      case step of
        0: // Get dimensions  (should be width height number-of-colors ???)
          begin
            counter := ScanForChar(fileString, ' ', 2, True);
            dimensions[1] := StrToInt(Copy(fileString, 2, counter - 2));
            counter := ScanForChar(fileString, ' ', counter, False);
            holdPos := counter;
            counter := ScanForChar(fileString, ' ', counter, True);
            dimensions[2] := StrToInt(Copy(fileString, holdPos, counter -
              HoldPos));
            counter := ScanForChar(fileString, ' ', counter, False);
            holdPos := counter;
            counter := ScanForChar(fileString, ' ', counter, True);
            dimensions[3] := StrToInt(Copy(fileString, holdPos, counter -
              HoldPos));
            step := 1;
            SetLength(data, (dimensions[1] * dimensions[2]) div 8);
            SetLength(mask, (dimensions[1] * dimensions[2]) div 8);
            Log.LogStatus('Length = ' + IntToStr((dimensions[1] * dimensions[2])
              div 8), 'LoadCursorFromFile');
          end;
        1: // get the symbols for transparent, black and white
          begin
            // get the symbol for the color
            clr := fileString[2];
            // look for the 'c' symbol
            counter := ScanForChar(fileString, 'c', 3, True);
            inc(counter);
            counter := ScanForChar(fileString, ' ', counter, False);
            if Copy(fileString, counter, 4) = 'None' then
            begin
              clrNone := clr;
            end;
            if LowerCase(Copy(fileString, counter, 7)) = '#ffffff' then
            begin
              clrBlack := clr;
            end;
            if LowerCase(Copy(fileString, counter, 7)) = '#000000' then
            begin
              clrWhite := clr;
            end;
            dec(dimensions[3]);
            if dimensions[3] = 0 then
            begin
              step := 2;
              counter := 0;
            end;
          end;
        2: // get cursor information -- modified from the SDL
          // documentation of SDL_CreateCursor.
          begin
            for col := 1 to dimensions[1] do
            begin
              if ((col mod 8) <> 1) then
              begin
                data[i] := data[i] shl 1;
                mask[i] := mask[i] shl 1;
              end
              else
              begin
                inc(i);
                data[i] := 0;
                mask[i] := 0;
              end;
              if fileString[col] = clrWhite then
              begin
                data[i] := data[i] or $01;
                mask[i] := mask[i] or $01;
              end
              else if fileString[col] = clrBlack then
              begin
                mask[i] := mask[i] or $01;
              end
              else if fileString[col + 1] = clrNone then
              begin

              end;
            end;
            inc(counter);
            if counter = dimensions[2] then
              step := 4;
          end;
      end;
    end;
  end;
  CloseFile(xpmFile);
  result := SDL_CreateCursor(PUInt8(data), PUInt8(mask), dimensions[1],
    dimensions[2], hot_x, hot_y);
end;

begin
  // Initialize the SDL library
  if (SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) < 0) then
  begin
    Log.LogError('Couldn''t initialize SDL', 'Init');
    exit;
  end;
  // Set the title bar in environments that support it
  SDL_WM_SetCaption(TITLE, nil);
  flags := SDL_SWSURFACE; // or SDL_FULLSCREEN;

  screen := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, flags);
  if (screen = nil) then
  begin
    Log.LogError('Could not set video mode', 'Init');
    SDL_Quit;
    exit;
  end;

  dstRect.x := 0;
  dstRect.y := 0;
  dstRect.w := screen.w div 2;
  dstRect.h := screen.h div 2;
  SDL_FillRect(screen, @dstRect, $FF0000);
  dstRect.x := dstRect.w;
  SDL_FillRect(screen, @dstRect, $00FF00);
  dstRect.x := 0;
  dstRect.y := dstRect.h;
  SDL_FillRect(screen, @dstRect, $0000FF);
  dstRect.x := dstRect.w;
  SDL_FillRect(screen, @dstRect, $FFFFFF);
  SDL_UpdateRect(screen, 0, 0, 0, 0);
  stdCursor := SDL_GetCursor;
  redCursor := LoadCursorFromXPMFile('cursors/red.xpm', 64, 16);
  blueCursor := LoadCursorFromXPMFile('cursors/blue.xpm', 64, 16);
  greenCursor := LoadCursorFromXPMFile('cursors/green.xpm', 64, 16);
  Quit := False;
  repeat
    while SDL_PollEvent(@Event) > 0 do
    begin
      WaitFrame;
      case Event.type_ of
        SDL_QUITEV:
        begin
          Quit := True;
        end;
        SDL_KEYDOWN:
          begin
            case Event.key.keysym.sym of
              SDLK_ESCAPE:
                begin
                  Quit := True;
                end; // SDLK_ESCAPE
              SDLK_P:
                begin
                  SDL_SaveBMP(screen, 'images/cc1board.bmp');
                end; // SDLK_P
            end; // case Event.key.keysym.sym
          end; // SDL_KEYDOWN
        SDL_MOUSEMOTION:
          begin
            // Check location for appropriate cursor
            if (Event.motion.x < (screen.w div 2)) then
              if (Event.motion.y < (screen.h div 2)) then
                holdCursor := redCursor
              else
                holdCursor := blueCursor
            else if (Event.motion.y < (screen.H div 2)) then
              holdCursor := greenCursor
            else
              holdCursor := stdCursor;
            if holdCursor <> SDL_GetCursor then
            begin
              SDL_SetCursor(holdCursor);
            end;
          end;
      end;
    end;
  until Quit = true;
  SDL_FreeCursor(greenCursor);
  SDL_FreeCursor(redCursor);
  SDL_FreeCursor(blueCursor);
  SDL_Quit;
end.
