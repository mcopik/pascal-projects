program custcursor;
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
{   Demonstrates Loading a Cursor from an XPM File and changes them depending  }
{   which regions they appear in.                                              }
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
  sdl,
  sdl_image,
  sdlsprites;

const
  TITLE = 'Changing Cursors depending on Mouse position';
  FRAMES_PER_SEC = 60;
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 32;
  TILE_HEIGHT = 40;
  TILE_WIDTH = 40;
  BOARD_BOTTOM = 422;
  BOARD_LEFT = 184;
  BOARD_OFFSET = 8;
  MAX_ROWS = 10;
  MAX_COLS = 10;
  TILE_MOVEMENT = 5;

type
  TCard = class( TSprite )
  private
  public
    constructor Create( Row, Col, Cardtype : Integer );
  end;

  cursorInfo = array of UInt8;
  
var
  screen, background : PSDL_Surface;
  flags : UInt32;
  next_tick : UInt32;
  SpriteEngine : TSpriteEngine;
  // TCard

constructor TCard.Create( Row, Col, CardType : Integer );
begin
  inherited Create( 'images/symbols.bmp', TILE_WIDTH, TILE_HEIGHT );
  AnimPhase := CardType;
  x := BOARD_LEFT + ( Col * TILE_WIDTH );
  y := BOARD_BOTTOM - ( Row * TILE_HEIGHT );
  if ( Row = MAX_ROWS ) then
    y := y - BOARD_OFFSET;
  if ( Col = MAX_COLS ) then
    x := x + BOARD_OFFSET;
end;

procedure RS_Blit( source, destination : PSDL_SURFACE; x, y : LongInt );
var
  dest : SDL_RECT;
begin
  dest.x := x;
  dest.y := y;
  dest.w := source.w;
  dest.h := source.h;
  SDL_BlitSurface( source, nil, destination, @dest );
end; //RS_Blit

procedure WaitFrame;
var
  this_tick : Uint32;
begin
  // Wait for the next frame */
  this_tick := SDL_GetTicks( );
  if ( this_tick < next_tick ) then
  begin
    SDL_Delay( next_tick - this_tick );
  end;
  next_tick := this_tick + ( 1000 div FRAMES_PER_SEC );
end; // WaitFrame

function ScanForChar( str : string; ch : Char; startPos : Integer; lookFor : Boolean ) : Integer;
begin
  Result := -1;
  while ( ( ( str[ startPos ] = ch ) <> lookFor ) and ( startPos < Length( str ) ) ) do
    inc( startPos );
  if startPos <> Length( str ) then
    Result := startPos;
end;

function LoadCursorFromFile( Filename : string; hot_x, hot_y : Integer ) : PSDL_Cursor;
var
  fileString : string;
  step : Integer;
  holdPos : Integer;
  counter : Integer;
  dimensions : array[ 1..3 ] of Integer;
  clr, clrNone, clrBlack, clrWhite : Char;
  data, mask : cursorInfo;
  i, col : Integer;
  xpmFile : Textfile;
begin
  AssignFile( xpmFile, FileName );
  Reset( xpmFile );
  step := 0;
  i := -1;
  clrBlack := 'X';
  clrWhite := ',';
  clrNone := ' ';
  counter := 0;
  while not ( eof( xpmFile ) ) do
  begin
    Readln( xpmFile, fileString );
    // scan for strings
    if fileString[ 1 ] = '"' then
    begin
      case step of
        0 : // Get dimensions  (should be width height number-of-colors ???)
          begin
            counter := ScanForChar( fileString, ' ', 2, True );
            dimensions[ 1 ] := StrToInt( Copy( fileString, 2, counter - 2 ) );
            counter := ScanForChar( fileString, ' ', counter, False );
            holdPos := counter;
            counter := ScanForChar( fileString, ' ', counter, True );
            dimensions[ 2 ] := StrToInt( Copy( fileString, holdPos, counter - HoldPos ) );
            counter := ScanForChar( fileString, ' ', counter, False );
            holdPos := counter;
            counter := ScanForChar( fileString, ' ', counter, True );
            dimensions[ 3 ] := StrToInt( Copy( fileString, holdPos, counter - HoldPos ) );
            step := 1;
            SetLength( data, ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 );
            SetLength( mask, ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 );
            Log.LogStatus( 'Length = ' + IntToStr( ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 ), 'LoadCursorFromFile' );
          end;
        1 : // get the symbols for transparent, black and white
          begin
            // get the symbol for the color
            clr := fileString[ 2 ];
            // look for the 'c' symbol
            counter := ScanForChar( fileString, 'c', 3, True );
            inc( counter );
            counter := ScanForChar( fileString, ' ', counter, False );
            if Copy( fileString, counter, 4 ) = 'None' then
            begin
              clrNone := clr;
            end;
            if LowerCase( Copy( fileString, counter, 7 ) ) = '#ffffff' then
            begin
              clrBlack := clr;
            end;
            if LowerCase( Copy( fileString, counter, 7 ) ) = '#000000' then
            begin
              clrWhite := clr;
            end;
            dec( dimensions[ 3 ] );
            if dimensions[ 3 ] = 0 then
            begin
              step := 2;
              counter := 0;
            end;
          end;
        2 : // get cursor information -- modified from the SDL
          // documentation of SDL_CreateCursor.
          begin
            for col := 1 to dimensions[ 1 ] do
            begin
              if ( ( col mod 8 ) <> 1 ) then
              begin
                data[ i ] := data[ i ] shl 1;
                mask[ i ] := mask[ i ] shl 1;
              end
              else
              begin
                inc( i );
                data[ i ] := 0;
                mask[ i ] := 0;
              end;
              if fileString[ col ] = clrWhite then
              begin
                data[ i ] := data[ i ] or $01;
                mask[ i ] := mask[ i ] or $01;
              end
              else if fileString[ col ] = clrBlack then
              begin
                mask[ i ] := mask[ i ] or $01;
              end
              else if fileString[ col + 1 ] = clrNone then
              begin
                //
              end;
            end;
            inc( counter );
            if counter = dimensions[ 2 ] then
              step := 4;
          end;
      end;
    end;
  end;
  CloseFile( xpmFile );
  result := SDL_CreateCursor( PUInt8( data ), PUInt8( mask ), dimensions[ 1 ], dimensions[ 2 ], hot_x, hot_y );
end;

procedure Play;
var
  counter1, counter2 : Integer;
  back1, back2 : PSDL_Surface;
  Quit : Boolean;
  Event : TSDL_Event;
  row, col, modRow, modCol : Integer;
  dnCursor, upCursor, lfCursor, rgCursor, noCursor : PSDL_Cursor;
begin
  SpriteEngine := TSpriteEngine.Create( screen );
  background := SDL_CreateRGBSurface( screen.flags, SCREEN_WIDTH, SCREEN_HEIGHT,
    screen.format.BitsPerPixel, 0, 0, 0, 0 );
  back1 := IMG_Load( PChar( 'images/back1.bmp' ) );
  back2 := IMG_Load( PChar( 'images/back2.bmp' ) );
  dnCursor := LoadCursorFromFile( 'cursors/down.xpm', 16, 30 );
  upCursor := LoadCursorFromFile( 'cursors/up.xpm', 16, 2 );
  lfCursor := LoadCursorFromFile( 'cursors/left.xpm', 2, 16 );
  rgCursor := LoadCursorFromFile( 'cursors/right.xpm', 30, 16 );
  noCursor := LoadCursorFromFile( 'cursors/no.xpm', 16, 16 );
  SDL_SetCursor( noCursor );
  for counter1 := 0 to 9 do
  begin
    for counter2 := 0 to 9 do
    begin
      if ( ( counter1 + counter2 ) mod 2 ) = 0 then
        RS_Blit( back1, background, BOARD_LEFT + ( counter1 * TILE_WIDTH ), BOARD_BOTTOM - ( counter2 * TILE_HEIGHT ) )
      else
        RS_BLIT( back2, background, BOARD_LEFT + ( counter1 * TILE_WIDTH ), BOARD_BOTTOM - ( counter2 * TILE_HEIGHT ) );
    end;
    if ( counter1 mod 2 ) = 0 then
    begin
      RS_BLIT( back1, background, BOARD_LEFT + ( counter1 * TILE_WIDTH ), BOARD_BOTTOM - ( MAX_ROWS * TILE_HEIGHT ) - BOARD_OFFSET );
      RS_BLIT( back1, background, BOARD_LEFT + ( MAX_COLS * TILE_WIDTH ) + BOARD_OFFSET, BOARD_BOTTOM - ( counter1 * TILE_HEIGHT ) );
    end
    else
    begin
      RS_BLIT( back2, background, BOARD_LEFT + ( counter1 * TILE_WIDTH ), BOARD_BOTTOM - ( MAX_ROWS * TILE_HEIGHT ) - BOARD_OFFSET );
      RS_BLIT( back2, background, BOARD_LEFT + ( MAX_COLS * TILE_WIDTH ) + BOARD_OFFSET, BOARD_BOTTOM - ( counter1 * TILE_HEIGHT ) );
    end;
  end;
  RS_Blit( background, screen, 0, 0 );
  SpriteEngine.BackgroundSurface := background;
  SDL_UpdateRect( screen, 0, 0, 0, 0 );
  for counter1 := 0 to 3 do
  begin
    for counter2 := 0 to 3 do
    begin
      SpriteEngine.AddSprite( TCard.Create( counter1, counter2, Random( 4 ) ) );
    end;
  end;
  SpriteEngine.AddSprite( TCard.Create( 10, 10, Random( 4 ) ) );
  SpriteEngine.Draw;
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  Quit := false;
  repeat
    while SDL_PollEvent( @Event ) > 0 do
    begin
      WaitFrame;
      case Event.type_ of
        SDL_QUITEV :
        begin
          Quit := True;
        end;
        SDL_KEYDOWN :
          begin
            case Event.key.keysym.sym of
              SDLK_ESCAPE :
                begin
                  Quit := True;
                end; // SDLK_ESCAPE
              SDLK_P :
                begin
                  SDL_SaveBMP( screen, 'images/board.bmp' );
                end; // SDLK_P
            end; // case Event.key.keysym.sym
          end; // SDL_KEYDOWN
        SDL_MOUSEMOTION :
          begin
            // Check location for appropriate cursor
            Col := ( Event.motion.x - BOARD_LEFT ) div TILE_WIDTH;
            modCol := ( Event.motion.x - BOARD_LEFT ) mod TILE_WIDTH;
            Row := ( BOARD_BOTTOM - ( Event.motion.y - TILE_HEIGHT ) ) div TILE_HEIGHT;
            modRow := ( BOARD_BOTTOM - ( Event.motion.y - TILE_HEIGHT ) ) mod TILE_HEIGHT;
            if ( Col > 3 ) or ( Row > 3 ) then
              SDL_SetCursor( NoCursor )
            else
            begin
              if modCol < modRow then
              begin
                if modRow + modCol < TILE_HEIGHT then
                begin
                  SDL_SetCursor( lfCursor );
                end
                else
                begin
                  SDL_SetCursor( upCursor );
                end;
              end
              else
              begin
                if modRow + modCol < TILE_HEIGHT then
                begin
                  SDL_SetCursor( dnCursor );
                end
                else
                begin
                  SDL_SetCursor( rgCursor );
                end;
              end;
            end;
          end;
      end;
    end;
  until Quit = true;
  SDL_FreeCursor( dnCursor );
  SDL_FreeCursor( upCursor );
  SDL_FreeCursor( lfCursor );
  SDL_FreeCursor( rgCursor );
  SDL_FreeCursor( noCursor );
end;

begin
  // Initialize the SDL library
  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_AUDIO ) < 0 ) then
  begin
    Log.LogError( 'Couldn''t initialize SDL', 'Init' );
    exit;
  end;
  // Set the title bar in environments that support it
  SDL_WM_SetCaption( TITLE, nil );
  flags := SDL_SWSURFACE {or SDL_FULLSCREEN};

  screen := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, flags );
  if ( screen = nil ) then
  begin
    Log.LogError( 'Could not set video mode', 'Init' );
    SDL_Quit;
    exit;
  end;

  Play;
  SDL_Quit;
end.
