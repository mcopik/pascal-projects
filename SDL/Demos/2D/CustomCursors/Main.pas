unit Main;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                   XPM To CSR convertor Demo                                  }
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
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{  January 16           2002 - LB : Initial creation                           }
{                                                                              }
{                                                                              }
{******************************************************************************}

interface

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
{$IFDEF VCL}
  Windows,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
{$ENDIF}
{$IFDEF CLX}
  Types,
  Variants,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
{$ENDIF}
  SysUtils,
  Classes;

type
  cursorInfo = array of Byte;

  TMainForm = class(TForm)
    OpenDialog1: TOpenDialog;
    SelectButton: TButton;
    Image1: TImage;
    procedure SelectButtonClick(Sender: TObject);
  private
    { Private declarations }
    xpmFile: TextFile;
    function ScanForChar(str: string; ch: Char; startPos: Integer; lookFor:
      Boolean): Integer;
  public
    { Public declarations }
  end;
  
var
  MainForm: TMainForm;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF}
{$IFDEF CLX}
{$R *.xfm}
{$ENDIF}

procedure TMainForm.SelectButtonClick(Sender: TObject);
var
  fileString: string;
  step: Integer;
  holdPos: Integer;
  counter: Integer;
  dimensions: array[1..3] of Integer;
  clr, clrNone, clrBlack, clrWhite: Char;
  data, mask: cursorInfo;
  i, col: Integer;
  cursorFile: file;
  screenRect: TRect;
begin
  if OpenDialog1.Execute then
  begin
    AssignFile(xpmFile, OpenDialog1.FileName);
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
              Image1.Width := dimensions[1];
              if MainForm.Width < Image1.Width then
                MainForm.Width := Image1.Width + 4;
              Image1.Left := (MainForm.Width - Image1.Width) div 2;
              Image1.Height := dimensions[2];
              if MainForm.Height < Image1.Height + Image1.Top then
                MainForm.Height := Image1.Height + Image1.Top + 10;
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
                Image1.Canvas.Brush.Color := clFuchsia;
                if fileString[col] = clrWhite then
                begin
                  data[i] := data[i] or $01;
                  mask[i] := mask[i] or $01;
                  Image1.Canvas.Brush.Color := clWhite;
                end
                else if fileString[col] = clrBlack then
                begin
                  mask[i] := mask[i] or $01;
                  Image1.Canvas.Brush.Color := clBlack;
                end
                else if fileString[col + 1] = clrNone then
                begin
                  Image1.Canvas.Brush.Color := clFuchsia;
                end;
                screenRect.Left := col - 1;
                screenRect.Top := counter;
                screenRect.Right := screenRect.Left + 4;
                screenRect.Bottom := screenRect.Top + 4;
                Image1.Canvas.FillRect(screenRect);
              end;
              inc(counter);
              if counter = dimensions[2] then
                step := 4;
            end;
        end;
      end;
    end;
    AssignFile(cursorFile, ChangeFileExt( OpenDialog1.Filename, '.csr') );
    Rewrite(cursorFile, 1);
    BlockWrite(cursorFile, dimensions[1], SizeOf(dimensions[1]));
    BlockWrite(cursorFile, dimensions[2], SizeOf(dimensions[2]));
    for i := 0 to Length(data) - 1 do
    begin
      BlockWrite(cursorFile, data[i], SizeOf(data[i]));
    end;
    for i := 0 to Length(mask) - 1 do
    begin
      BlockWrite(cursorFile, mask[i], SizeOf(mask[i]));
    end;
    CloseFile(xpmFile);
    CloseFile(cursorFile);
  end;
end;

function TMainForm.ScanForChar(str: string; ch: Char; startPos: Integer;
  lookFor: Boolean): Integer;
begin
  Result := -1;
  while (((str[startPos] = ch) <> lookFor) and (startPos < Length(str))) do
    inc(startPos);
  if startPos <> Length(str) then
    Result := startPos;
end;

end.

