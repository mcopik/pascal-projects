program JEDISDLOpenGL21Class;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Ti Leggett <leggett@eecs.tulane.edu>,  are   }
{ Copyright (C) 2001 Ti Leggett.                                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson21.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL21.dpr                }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
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
{  Shows how to use OpenGL with the SDL libraries                  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Also Makes uses of Mike Lischke's Cross-Platform OpenGL header.}
{   You can pick it up from...                                     }
{   http://www.lischke-online.de/Graphics.html#OpenGL12            }
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
{   June   06 2002 - DL : Initial translation.                     }
{                                                                  }
{   May     23 2003 - DL : Updated to use new GL Headers.          }
{                                                                  }
{******************************************************************}
uses
  Classes,
  SysUtils,
  gl,
  glext,
  sdl,
  sdlwindow,
  sdlgameinterface,
  sdlaudiomixer;

const
  MAX_TEXTURES = 2;

  steps : array[ 0..5 ] of byte = ( 1, 2, 4, 5, 10, 20 );

type
  TGameObject = record
    fx, fy : integer;
    x, y : integer;
    spin : single;
  end;

  TBitmapFont = class( TObject )
  private
    FBase : GLuint;
    FTextureID : GLuint;
    procedure BuildFont;
  public
    constructor Create( const aFileName : string );
    destructor Destroy; override;
    procedure SetColour( aRs, aGs, aBs : Single ); overload;
    procedure SetColour( aRb, aGb, aBb : Byte ); overload;
    procedure DrawText( aX, aY : GLint; aText : string; aFontSet : integer = 0 );
  end;

  TMenuItem = class( TObject )
  private
    FParentMenuItem : TMenuItem;
    FFont : TBitmapFont;
    FText: string;
    FItemList : TList;
    function GetItems(aIndex: Integer): TMenuItem;
    procedure SetItems(aIndex: Integer; const aValue: TMenuItem);
  public
    constructor Create( aParent : TMenuItem );
    procedure Add( aMenuItem : TMenuItem );
    property Items[Index: Integer]: TMenuItem read GetItems write SetItems; default;
    property Text : string read FText write FText;
  end;

  TMainMenu = class( TObject )
  private
    function GetItems(aIndex: Integer): TMenuItem;
    procedure SetItems(aIndex: Integer; const aValue: TMenuItem);
  public
    property Items[Index: Integer]: TMenuItem read GetItems write SetItems; default;
  end;

  TGameMenu = class( TGameInterface )
  protected
    procedure Render; override;
  public
    MainMenu : TMainMenu;
    procedure LoadSurfaces; override;
  end;

  TGamePlay = class( TGameInterface )
  private
    FFont : TBitmapFont;
    FPlayer : TGameObject;
    FEnemies : array[ 0..8 ] of TGameObject;
    FHourglass : TGameObject;
    FVline : array[ 0..10, 0..9 ] of boolean;
    FHline : array[ 0..9, 0..10 ] of boolean;
    FTextureID : GLuint;
    procedure ResetGameObjects;
  protected
    procedure Render; override;
    procedure Update( aElapsedTime : single ); override;
  public
    procedure LoadSurfaces; override;
  end;

{ TMenuItem }
procedure TMenuItem.Add(aMenuItem : TMenuItem);
begin
  FItemList.Add( aMenuItem );
end;

constructor TMenuItem.Create(aParent: TMenuItem);
begin
  inherited Create;

  FParentMenuItem := aParent;
end;

function TMenuItem.GetItems(aIndex: Integer): TMenuItem;
begin
  result := FItemList[aIndex];
end;

procedure TMenuItem.SetItems(aIndex: Integer; const aValue: TMenuItem);
begin
  FItemList[aIndex] := aValue;
end;

{ TBitmapFont }
procedure TBitmapFont.BuildFont;
var
  loop : GLuint; // Loop variable               
  cx : single; // Holds Our X Character Coord 
  cy : single; // Holds Our Y Character Coord 
begin
  // Creating 256 Display List 
  FBase := glGenLists( 256 );

  /// Select Our Font Texture 
  glBindTexture( GL_TEXTURE_2D, FTextureID );

  // Loop Through All 256 Lists 
  for loop := 0 to 255 do
  begin
    {* NOTE:
     *  BMPs are stored with the top-leftmost pixel being the
     * last byte and the bottom-rightmost pixel being the first
     * byte. So an image that is displayed as
     *    1 0
     *    0 0
     * is represented data-wise like
     *    0 0
     *    0 1
     * And because SDL_LoadBMP loads the raw data without
     * translating to how it is thought of when viewed we need
     * to start at the bottom-right corner of the data and work
     * backwards to get everything properly. So the below code has
     * been modified to reflect this. Examine how this is done and
     * how the original tutorial is done to grasp the differences.
     *
     * As a side note BMPs are also stored as BGR instead of RGB
     * and that is why we load the texture using GL_BGR. It's
     * bass-ackwards I know but whattaya gonna do?
     *}

    // X Position Of Current Character
    cx := 1 - ( loop mod 16 ) / 16.0;
    // Y Position Of Current Character 
    cy := 1 - ( loop div 16 ) / 16.0;

    // Start Building A List
    glNewList( FBase + ( 255 - loop ), GL_COMPILE );
    // Use A Quad For Each Character
    glBegin( GL_QUADS );
    // Texture Coord (Bottom Left)
    glTexCoord2f( cx - 0.0625, cy );
    // Vertex Coord (Bottom Left) 
    glVertex2i( 0, 16 );

    // Texture Coord (Bottom Right)
    glTexCoord2f( cx, cy );
    // Vertex Coord (Bottom Right)
    glVertex2i( 16, 16 );

    // Texture Coord (Top Right)
    glTexCoord2f( cx, cy - 0.0625 );
    // Vertex Coord (Top Right)
    glVertex2i( 16, 0 );

    // Texture Coord (Top Left)
    glTexCoord2f( cx - 0.0625, cy - 0.0625 );
    // Vertex Coord (Top Left)
    glVertex2i( 0, 0 );
    glEnd;

    // Move To The Left Of The Character
    glTranslated( 15, 0, 0 );
    glEndList;
  end;
end;

constructor TBitmapFont.Create(const aFileName: string);
var
  TextureImage : PSDL_Surface;
begin
  inherited Create;

  TextureImage := SDL_LoadBMP( PChar( aFileName ) );
  if TextureImage <> nil then
  begin
    // Create Texture
    glGenTextures( 1, @FTextureID );

    glBindTexture( GL_TEXTURE_2D, FTextureID );

    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
        TextureImage.h, 0, GL_BGR,
        GL_UNSIGNED_BYTE,
        TextureImage.pixels );

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );

    SDL_FreeSurface( TextureImage );
  end;

  BuildFont;
end;

destructor TBitmapFont.Destroy;
begin
  // Clean up our font list
  glDeleteLists( FBase, 256 );

  // Clean up our textures
  glDeleteTextures( 1, @FTextureID );
  
  inherited;
end;

procedure TBitmapFont.DrawText( aX, aY: GLint; aText: string;  aFontSet: integer = 0 );
begin
  // Did User Choose An Invalid Character Set?
  if ( aFontSet > 1 ) then
    aFontSet := 1;

  // Enable Texture Mapping
  glEnable( GL_TEXTURE_2D );

  // Select our texture
  glBindTexture( GL_TEXTURE_2D, FTextureID );

  // Disable depth testing
  glDisable( GL_DEPTH_TEST );

  // Reset The Modelview Matrix
  glLoadIdentity;

  // Position The Text (0,0 - Bottom Left)
  glTranslated( aX, aY, 0 );

  // Choose The Font Set (0 or 1)
  glListBase( FBase - 32 + ( 128 * aFontSet ) );

  // If Set 0 Is Being Used Enlarge Font
  if ( aFontSet = 0 ) then
    // Enlarge Font Width And Height
    glScalef( 1.5, 2.0, 1.0 );

  // Write The Text To The Screen
  glCallLists( Length( aText ), GL_BYTE, PChar( aText ) );

  // Disable Texture Mapping
  glDisable( GL_TEXTURE_2D );

  // Re-enable Depth Testing
  glEnable( GL_DEPTH_TEST );
end;

procedure TBitmapFont.SetColour(aRs, aGs, aBs: Single);
begin
  glColor3f( aRs, aGs, aBs );
end;

procedure TBitmapFont.SetColour(aRb, aGb, aBb: Byte);
begin
  glColor3f( aRb, aGb, aBb );
end;

{ TMainMenu }
function TMainMenu.GetItems(aIndex: Integer): TMenuItem;
begin
  result := Items[aIndex];
end;

procedure TMainMenu.SetItems(aIndex: Integer; const aValue: TMenuItem);
begin
  Items[aIndex] := aValue;
end;

var
  Application : TSDL3DWindow;
  CurrentGameInterface : TGameInterfaceClass = TGameMenu;
  GameWindow :  TGameInterface;
  GameAudio : TSDLAudioManager;
  GameFont : TBitmapFont;
  anti : boolean = true;
  filled : boolean;
  gameover : boolean;

  adjust : integer = 3;
  lives : integer = 5;
  level : integer = 1;
  level2 : integer = 1;
  stage : integer = 1;



{ TGameMenu }
procedure TGameMenu.LoadSurfaces;
begin
  inherited;

  // Enable smooth shading
  glShadeModel( GL_SMOOTH );

  // Set the background black
  glClearColor( 0.0, 0.0, 0.0, 0.5 );

  // Depth buffer setup
  glClearDepth( 1.0 );

  // Set Line Antialiasing
  glHint( GL_LINE_SMOOTH_HINT, GL_NICEST );

  // Enable Blending
  glEnable( GL_BLEND );

  // Type Of Blending To Use
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  // Setup our viewport.
  glViewport( 0, 0, MainWindow.Width, MainWindow.Height );

  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our ortho perspective/view
  glOrtho( 0.0, MainWindow.Width, MainWindow.Height, 0.0, -1.0, 1.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );

  // Reset The View
  glLoadIdentity;
end;

procedure TGameMenu.Render;
begin
  inherited;
  
end;

{ TGamePlay }

procedure TGamePlay.LoadSurfaces;
var
  TextureImage : PSDL_Surface;
begin
  inherited;

  TextureImage := SDL_LoadBMP( 'images/image.bmp' );
  if TextureImage <> nil then
  begin
    // Create Texture
    glGenTextures( 1, @FTextureID );

    glBindTexture( GL_TEXTURE_2D, FTextureID );

    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
        TextureImage.h, 0, GL_BGR,
        GL_UNSIGNED_BYTE,
        TextureImage.pixels );

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );

    SDL_FreeSurface( TextureImage );
  end;

  // Enable smooth shading
  glShadeModel( GL_SMOOTH );

  // Set the background black
  glClearColor( 0.0, 0.0, 0.0, 0.5 );

  // Depth buffer setup
  glClearDepth( 1.0 );

  // Set Line Antialiasing
  glHint( GL_LINE_SMOOTH_HINT, GL_NICEST );

  // Enable Blending
  glEnable( GL_BLEND );

  // Type Of Blending To Use
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  // Setup our viewport.
  glViewport( 0, 0, MainWindow.Width, MainWindow.Height );

  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our ortho perspective/view
  glOrtho( 0.0, MainWindow.Width, MainWindow.Height, 0.0, -1.0, 1.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );

  // Reset The View
  glLoadIdentity;

  // Initialise Game Objects
  ResetGameObjects
end;

procedure TGamePlay.Render;
var
  loop1, loop2 : integer;
begin
  inherited;
  // Clear The Screen And The Depth Buffer
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  // Reset the view
  glLoadIdentity;

  // Set Font Colour To Purple
  GameFont.SetColour( 1.0, 0.5, 1.0 );

  // Write GRID CRAZY On The Screen
  GameFont.DrawText( 207, 24, 'GRID CRAZY', 0 );

  // Set Color To Yellow
  GameFont.SetColour( 1.0, 1.0, 0.0 );

  // Write Actual Level Stats
  GameFont.DrawText( 20, 20, 'Level:' + IntToStr( level2 ), 1 );

  // Write Stage Stats
  GameFont.DrawText( 20, 40, 'Stage:' + IntToStr( stage ), 1 );

  // Is The Game Over?
  if ( gameover ) then
  begin
    // Pick A Random Color
    GameFont.SetColour( random( 255 ), random( 255 ), random( 255 ) );

    // Write GAME OVER To The Screen
    GameFont.DrawText( 472, 20, 'GAME OVER', 1 );

    // Write PRESS SPACE To The Screen
    GameFont.DrawText( 456, 40, 'PRESS SPACE', 1 );
  end;

  // Loop Through Lives Minus Current Life
  for loop1 := 0 to lives - 2 do
  begin
    // Reset The View
    glLoadIdentity( );
    // Move To The Right Of Our Title Text
    glTranslatef( 490 + ( loop1 * 40.0 ), 40.0, 0.0 );
    // Rotate Counter Clockwise
    glRotatef( -FPlayer.spin, 0.0, 0.0, 1.0 );
    // Set Player Color To Light Green
    glColor3f( 0.0, 1.0, 0.0 );

    // Start Drawing Our Player Using Lines
    glBegin( GL_LINES );
    glVertex2d( -5, -5 ); // Top Left Of Player
    glVertex2d( 5, 5 ); // Bottom Right Of Player
    glVertex2d( 5, -5 ); // Top Right Of Player
    glVertex2d( -5, 5 ); // Bottom Left Of Player
    glEnd( );

    // Rotate Counter Clockwise
    glRotatef( -FPlayer.spin * 0.5, 0.0, 0.0, 1.0 );
    // Set Player Color To Dark Green
    glColor3f( 0.0, 0.75, 0.0 );

    // Start Drawing Our Player Using Lines
    glBegin( GL_LINES );
    glVertex2d( -7, 0 ); // Left Center Of Player
    glVertex2d( 7, 0 ); // Right Center Of Player
    glVertex2d( 0, -7 ); // Top Center Of Player
    glVertex2d( 0, 7 ); // Bottom Center Of Player
    glEnd( );
  end;

  // Set Filled To True Before Testing
  filled := TRUE;
  // Set Line Width For Cells To 2.0
  glLineWidth( 2.0 );
  // Disable Antialiasing
  glDisable( GL_LINE_SMOOTH );
  // Reset The Current Modelview Matrix
  glLoadIdentity( );

  // Loop From Left To Right
  for loop1 := 0 to 10 do
  begin
    // Loop From Top To Bottom
    for loop2 := 0 to 10 do
    begin
      // Set Line Color To Blue
      glColor3f( 0.0, 0.5, 1.0 );

      // Has The Horizontal Line Been Traced
      if ( FHline[ loop1 ][ loop2 ] ) then
        glColor3f( 1.0, 1.0, 1.0 );

      // Dont Draw To Far Right
      if ( loop1 < 10 ) then
      begin
        // If A Horizontal Line Isn't Filled
        if ( not FHline[ loop1 ][ loop2 ] ) then
          filled := FALSE;

        // Start Drawing Horizontal Cell Borders
        glBegin( GL_LINES );
        // Left Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        // Right Side Of Horizontal Line
        glVertex2d( 80 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        glEnd( );
      end;

      // Set Line Color To Blue
      glColor3f( 0.0, 0.5, 1.0 );

      // Has The Horizontal Line Been Traced
      if ( FVline[ loop1 ][ loop2 ] ) then
        // If So, Set Line Color To White
        glColor3f( 1.0, 1.0, 1.0 );

      // Dont Draw To Far Down
      if ( loop2 < 10 ) then
      begin
        // If A Verticle Line Isn't Filled
        if ( not FVline[ loop1 ][ loop2 ] ) then
          filled := FALSE;

        // Start Drawing Verticle Cell Borders
        glBegin( GL_LINES );
        // Left Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        // Right Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          110 + ( loop2 * 40 ) );
        glEnd( );
      end;

      // Enable Texture Mapping
      glEnable( GL_TEXTURE_2D );
      // Bright White Color
      glColor3f( 1.0, 1.0, 1.0 );
      // Select The Tile Image
      glBindTexture( GL_TEXTURE_2D, FTextureID );

      // If In Bounds, Fill In Traced Boxes
      if ( ( loop1 < 10 ) and ( loop2 < 10 ) ) then
      begin
        // Are All Sides Of The Box Traced?
        if ( FHline[ loop1 ][ loop2 ] and
          FHline[ loop1 ][ loop2 + 1 ] and
          FVline[ loop1 ][ loop2 ] and
          FVline[ loop1 + 1 ][ loop2 ] ) then
        begin
          // Draw A Textured Quad
          glBegin( GL_QUADS );
          // Top Right
          glTexCoord2f( ( loop1 / 10.0 ) + 0.1, 1.0 - ( ( loop2 / 10.0 ) ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 59,
            70 + loop2 * 40 + 1 );
          // Top Left
          glTexCoord2f( ( loop1 / 10.0 ), 1.0 - ( ( loop2 / 10.0 ) ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 1,
            70 + loop2 * 40 + 1 );
          // Bottom Left
          glTexCoord2f( ( loop1 / 10.0 ), 1.0 - ( ( loop2 / 10.0 ) + 0.1 ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 1,
            ( 70 + loop2 * 40 ) + 39 );
          // Bottom Right
          glTexCoord2f( ( loop1 / 10.0 ) + 0.1, 1.0 - ( ( loop2 / 10.0 ) + 0.1 ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 59,
            ( 70 + loop2 * 40 ) + 39 );
          glEnd( );
        end;
      end;

      // Disable Texture Mapping
      glDisable( GL_TEXTURE_2D );
    end;
  end;

  // Set The Line Width To 1.0
  glLineWidth( 1.0 );

  // Is Anti TRUE?
  if ( anti ) then
    glEnable( GL_LINE_SMOOTH );

  // If fx=1 Draw The Hourglass
  if ( FHourglass.fx = 1 ) then
  begin
    // Reset The Modelview Matrix
    glLoadIdentity( );
    // Move To The Fine Hourglass Position
    glTranslatef( 20.0 + ( FHourglass.x * 60 ),
      70.0 + ( FHourglass.y * 40 ), 0.0 );
    // Rotate Clockwise
    glRotatef( FHourglass.spin, 0.0, 0.0, 1.0 );
    // Set Hourglass Color To Random Color
    glColor3ub( random( 255 ), random( 255 ), random( 255 ) );

    // Start Drawing Our Hourglass Using Lines
    glBegin( GL_LINES );
    // Top Left Of Hourglass
    glVertex2d( -5, -5 );
    // Bottom Right Of Hourglass
    glVertex2d( 5, 5 );
    // Top Right Of Hourglass
    glVertex2d( 5, -5 );
    // Bottom Left Of Hourglass
    glVertex2d( -5, 5 );
    // Bottom Left Of Hourglass
    glVertex2d( -5, 5 );
    // Bottom Right Of Hourglass
    glVertex2d( 5, 5 );
    // Top Left Of Hourglass
    glVertex2d( -5, -5 );
    // Top Right Of Hourglass
    glVertex2d( 5, -5 );
    glEnd( );
  end;

  // Reset The Modelview Matrix
  glLoadIdentity( );
  // Move To The Fine Player Position
  glTranslatef( FPlayer.fx + 20.0, FPlayer.fy + 70.0, 0.0 );
  // Rotate Clockwise
  glRotatef( FPlayer.spin, 0.0, 0.0, 1.0 );
  // Set Player Color To Light Green
  glColor3f( 0.0, 1.0, 0.0 );

  // Start Drawing Our Player Using Lines
  glBegin( GL_LINES );
  // Top Left Of Player
  glVertex2d( -5, -5 );
  // Bottom Right Of Player
  glVertex2d( 5, 5 );
  // Top Right Of Player
  glVertex2d( 5, -5 );
  // Bottom Left Of Player
  glVertex2d( -5, 5 );
  glEnd( );

  // Rotate Clockwise
  glRotatef( FPlayer.spin * 0.5, 0.0, 0.0, 1.0 );
  // Set Player Color To Dark Green
  glColor3f( 0.0, 0.75, 0.0 );
  // Start Drawing Our Player Using Lines
  glBegin( GL_LINES );
  // Left Center Of Player
  glVertex2d( -7, 0 );
  // Right Center Of Player
  glVertex2d( 7, 0 );
  // Top Center Of Player
  glVertex2d( 0, -7 );
  // Bottom Center Of Player
  glVertex2d( 0, 7 );
  glEnd( );

  // Loop To Draw Enemies
  for loop1 := 0 to ( stage * level ) - 1 do
  begin
    // Reset The Modelview Matrix
    glLoadIdentity( );
    glTranslatef( FEnemies[ loop1 ].fx + 20.0,
      FEnemies[ loop1 ].fy + 70.0, 0.0 );
    // Make Enemy Body Pink
    glColor3f( 1.0, 0.5, 0.5 );

    // Start Drawing Enemy
    glBegin( GL_LINES );
    // Top Point Of Body
    glVertex2d( 0, -7 );
    // Left Point Of Body
    glVertex2d( -7, 0 );
    // Left Point Of Body
    glVertex2d( -7, 0 );
    // Bottom Point Of Body
    glVertex2d( 0, 7 );
    // Bottom Point Of Body
    glVertex2d( 0, 7 );
    // Right Point Of Body
    glVertex2d( 7, 0 );
    // Right Point Of Body
    glVertex2d( 7, 0 );
    // Top Point Of Body
    glVertex2d( 0, -7 );
    glEnd( );

    // Rotate The Enemy Blade
    glRotatef( FEnemies[ loop1 ].spin, 0.0, 0.0, 1.0 );
    // Make Enemy Blade Red
    glColor3f( 1.0, 0.0, 0.0 );

    // Start Drawing Enemy Blade
    glBegin( GL_LINES );
    // Top Left Of Enemy
    glVertex2d( -7, -7 );
    // Bottom Right Of Enemy
    glVertex2d( 7, 7 );
    // Bottom Left Of Enemy
    glVertex2d( -7, 7 );
    // Top Right Of Enemy
    glVertex2d( 7, -7 );
    glEnd( );
  end;
end;

procedure TGamePlay.ResetGameObjects;
var
  loop1 : integer;
begin
  FPlayer.x := 0;
  FPlayer.y := 0;
  FPlayer.fx := 0;
  FPlayer.fy := 0;

  for loop1 := 0 to ( stage * level ) - 1 do
  begin
    FEnemies[ loop1 ].x := 5 + random( 6 );
    FEnemies[ loop1 ].y := random( 11 );
    FEnemies[ loop1 ].fx := FEnemies[ loop1 ].x * 60;
    FEnemies[ loop1 ].fy := FEnemies[ loop1 ].y * 40;
  end;
end;

procedure TGamePlay.Update( aElapsedTime : single );
begin
  
end;

begin
  // Load in the music
  //music := Mix_LoadMUS( 'sound/lktheme.mod' );

  // Play The Death Sound
  //PlaySound( 'sound/die.wav', 0 );


  // Play The Level Complete Sound
  // PlaySound( 'sound/complete.wav', 0 );

  // Play Freeze Enemy Sound
  //PlaySound( 'sound/freeze.wav', -1 );

  // Set The hourglass fx Variable To Two

  // Play The Hourglass Appears Sound
  // PlaySound( 'sound/hourglass.wav', 0 );


  Application := TSDL3DWindow.Create( 640, 480, 32 );

  try
    Application.SetIcon( nil, 0 );
    Application.ActivateVideoMode;
    Application.SetCaption( 'Grid Crazy : Class Edition', '' );
    GameFont := TBitmapFont.Create( 'images/font.bmp' );

    CurrentGameInterface := TGamePlay;

    while CurrentGameInterface <> nil do
    begin
      GameWindow := CurrentGameInterface.Create( Application );
      GameWindow.LoadSurfaces;

      Application.Show;
      CurrentGameInterface := GameWindow.NextGameInterface;

      if ( GameWindow <> nil ) then
        FreeAndNil( GameWindow );
      end;

  finally
    GameFont.Free;
    Application.Free;
  end;
end.


