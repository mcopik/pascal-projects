program custcursor2;
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
{   Demonstrates Loading a Cursor from a CSR File                              }
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
  TITLE = 'Load Cursor From CSR File Demo';
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
  redCursor, blueCursor, greenCursor, StdCursor, holdCursor: PSDL_Cursor;
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

function LoadCursorFromCSRFile(Filename: string; hot_x, hot_y: Integer):
  PSDL_Cursor;
var
  dimensions: array[1..3] of Integer;
  data, mask: cursorInfo;
  i: Integer;
  cursorFile: file;
begin
  AssignFile(cursorFile, Filename);
  Reset(cursorFile, 1);
  BlockRead(cursorFile, dimensions[1], SizeOf(dimensions[1]));
  BlockRead(cursorFile, dimensions[2], SizeOf(dimensions[2]));
  SetLength(data, (dimensions[1] * dimensions[2]) div 8);
  SetLength(mask, (dimensions[1] * dimensions[2]) div 8);
  for i := 0 to Length(data) - 1 do
  begin
    BlockRead(cursorFile, data[i], SizeOf(data[i]));
  end;
  for i := 0 to Length(mask) - 1 do
  begin
    BlockRead(cursorFile, mask[i], SizeOf(mask[i]));
  end;
  CloseFile(cursorFile);
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
  redCursor := LoadCursorFromCSRFile('cursors/red.csr', 64, 16);
  blueCursor := LoadCursorFromCSRFile('cursors/blue.csr', 64, 16);
  greenCursor := LoadCursorFromCSRFile('cursors/green.csr', 64, 16);
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
                  SDL_SaveBMP(screen, 'images/cc2board.bmp');
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
              { Since Windows does not erase the old cursor when it's changed,
                disable the cursor first.  }
{              SDL_ShowCursor(SDL_DISABLE);}
              SDL_SetCursor(holdCursor);
{             SDL_ShowCursor(SDL_ENABLE);   }
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
