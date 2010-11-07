program JEDISDLTruckOff;
{******************************************************************}
{                                                                  }
{  $Id: JEDISDLTruckOff.dpr,v 1.6 2004/04/13 17:46:34 savage Exp $                               }
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the Ed Jones' ODE TruckOff Demo              }
{                                                                  }
{ Portions created by Ed Jones <ed.jones@oracle.com>,  are         }
{ Copyright(C) 2002-2004 Ed Jones.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : JEDISDLTruckOff.dpr                }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright(C) 2002-2004 Dominique Louis.                          }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators( Project JEDI )             }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1(the "License"); you may   }
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
{  Shows how to interact with ODE and OpenGL.                      }
{  It shows an articulated ( semi-trailer ) truck simulation,      }
{  using ODE for physics and collisions.                           }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Also Makes uses of Tom Nuyden's Cross-Platform OpenGL header.  }
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
{   April   11 2002 - DL : Initial translation.                    }
{                                                                  }
{   May     23 2003 - DL : Updated to use new GL Headers.          }
{                                                                  }
{   March  08 2004 - DL : Updated to use new ODE API.              }
{                                                                  }
{                                                                  }
{  $Log: JEDISDLTruckOff.dpr,v $
{  Revision 1.6  2004/04/13 17:46:34  savage
{  Slight Change required for correct operation under Linux.
{
{  Revision 1.5  2004/04/07 22:07:29  savage
{  Changes required to accommodate odeimport.pas changes.
{
{  Revision 1.4  2004/03/31 21:39:28  savage
{  Made Reverse activate on Right Shift and turned it off on Left Shift
{
{  Revision 1.3  2004/03/31 09:57:23  savage
{  Change wheel colour and upward force when F is pressed.
{
{  Revision 1.2  2004/03/10 11:15:00  savage
{  Added more force to be able to flip the car, tried to fix the DrawText, but to no avail.
{
{  Revision 1.1  2004/03/08 21:29:15  savage
{  ODE TruckOff Demo
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  gl,
  glu,
  glext,
  logger,
  sdl,
  odeimport,
  vehicle,
  gfxutils;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH  = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP    = 16;
  MAX_TEXTURES  = 4;

  CLENGTH       = 0.7; // chassis length
  CWIDTH        = 0.5; // chassis width
  CHEIGHT       = 0.2; // chassis height
  WRADIUS       = 0.18; // wheel radius
  WWIDTH        = 0.1; // wheel width
  CSTARTZ       = 0.5; // starting height of chassis
  CMASS         = 1.0; // chassis mass
  WMASS         = 0.2; // wheel mass
  MAX_CONTACTS  = 20;  // The most contacts we can have
  OUTLINE_WIDTH = 1.0; // Outline width for cell-shading

var
  // This is our SDL surface
  surface : PSDL_Surface;

  // Dynamics and collision objects
  world : PdxWorld;
  space : PdxSpace;
  contactgroup : TdJointGroupID;
  ground : PdxGeom;
  ground_box : PdxGeom;

  GlobalFriction : GLFloat = 0.25;

  texture : array[ 0..MAX_TEXTURES - 1 ] of GLuint;
  // Storage For MAX_TEXTURES Textures

  //Status indicator
  Status : Boolean = false;

  pause : Boolean = false;
  Throttle : Boolean = false;
  Brake : Boolean = false;
  HandBrake : Boolean = false;
  Left : Boolean = false;
  Right : Boolean = false;
  Reverse : Boolean = false;

  tractor, trailer : TVehicle;

  // For drawing round things
  qobj : PGLUquadricObj;

  // Lighting
  LightAmb : array[ 0..3 ] of GLFloat = ( 0.4, 0.4, 0.4, 1.0 ); // Ambient Light
  LightDif : array[ 0..3 ] of GLFloat = ( 0.6, 0.6, 0.6, 1.0 ); // Diffuse Light
  LightPos : array[ 0..3 ] of GLFloat = ( 4.0, 4.0, 6.0, 1.0 );
    // Light Position

  base : GLuint;

  // this is called by dSpaceCollide when two objects in space are
  // potentially colliding.

procedure nearCallback( data : pointer; o1, o2 : PdxGeom ); cdecl;
{const
  cN = 10; }

var
  i, n : integer;
  c : TdJointID;
  contact : array[ 0..MAX_CONTACTS - 1 ] of TdContact;
begin
  n := dCollide( o1, o2, MAX_CONTACTS, contact[ 0 ].geom, sizeof( TdContact ) );
  if ( n > 0 ) then
  begin
    for i := 0 to n - 1 do
    begin
      contact[ i ].surface.mode := dContactSlip1 or dContactSlip2 or
        dContactSoftERP or dContactSoftCFM;
      contact[ i ].surface.mu := GlobalFriction;
      contact[ i ].surface.slip1 := 0.1;
      contact[ i ].surface.slip2 := 0.1;
      contact[ i ].surface.soft_erp := 0.5;
      contact[ i ].surface.soft_cfm := 0.3;

      c := dJointCreateContact( world, contactgroup, @contact[ i ] );
      dJointAttach( c, dGeomGetBody( contact[ i ].geom.g1 ), dGeomGetBody(
        contact[ i ].geom.g2 ) );
    end;
  end;
end;

procedure setCamera( posID : PdxBody; locID : PdxBody );
var
  pos, loc : PdVector3;
begin
  pos := dBodyGetPosition( posID );
  loc := dBodyGetPosition( locID );

  gluLookAt( 4, 3, 5, loc[ 0 ], loc[ 1 ] + 0.7, loc[ 2 ], 0, 1, 0 );
end;

procedure drawEverything( fill : boolean );
var
  ss : TdVector3;
begin
  // Draw the objects
  tractor.Render( qobj, fill );
  trailer.Render( qobj, fill );

  // draw the box embedded in the ground
  if ( fill ) then
    guSetColor( 1.0, 1.0, 0.0 );

  dGeomBoxGetLengths( ground_box, ss );

  guSetTransform( dGeomGetPosition( ground_box ), dGeomGetRotation( ground_box ) );
  guDrawBox( ss );
  guUnsetTransform;
end;

procedure KillFont;
begin
  // Clean up our font list
  glDeleteLists( base, 256 );

  // Clean up our textures
  glDeleteTextures( MAX_TEXTURES, @texture[ 0 ] );
end;

procedure TerminateApplication;
begin
  tractor.Free;
  trailer.Free;

  KillFont;

  gluDeleteQuadric( qobj );
  dJointGroupDestroy( contactgroup );
  dSpaceDestroy( space );
  dWorldDestroy( world );
  SDL_QUIT;
  Halt( 0 );
end;

procedure LoadImage( FileName : string; var ID : GLuint );
var
  TextureImage : PSDL_Surface;
begin
  TextureImage := SDL_LoadBMP( PChar( Filename ) );
  if ( TextureImage <> nil ) then
  begin
    try
      glGenTextures( 1, @ID );

      // Generate The Texture
      glBindTexture( GL_TEXTURE_2D, ID );

      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
        TextureImage.h, 0, GL_BGR,
        GL_UNSIGNED_BYTE,
        TextureImage.pixels );

      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    finally
      // Free up any memory we may have used
      SDL_FreeSurface( TextureImage );
    end;
  end
  else
    log.LogError( 'Texture ' + filename + ' not found...', 'Loadtexture' );
end;

function LoadGLTextures : Boolean;
var
  // Create storage space for the texture
  TextureImage : PSDL_Surface;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage := SDL_LoadBMP( 'images/font.bmp' );

  if ( TextureImage <> nil ) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures( MAX_TEXTURES, @texture[ 0 ] );
    glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
      TextureImage.h, 0, GL_BGR,
      GL_UNSIGNED_BYTE,
      TextureImage.pixels );

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
  end
  else
  begin
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    TerminateApplication;
  end;

  // Free up any memory we may have used
  if TextureImage <> nil then
    SDL_FreeSurface( TextureImage );
  
  Result := Status;
end;

// function to reset our viewport after a window resize

function ResizeWindow( width : integer; height : integer ) : Boolean;
begin
  // Protect against a divide by zero
  if ( height = 0 ) then
    height := 1;

  // Setup our viewport.
  glViewport( 0, 0, width, height );

  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our perspective
  gluPerspective( 45.0, width / height, 0.1, 100.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );

  // Reset The View
  glLoadIdentity;

  result := true;
end;

// function to handle key press events

procedure HandleKeyPress( keysym : PSDL_keysym );
begin
  case keysym.sym of
    SDLK_ESCAPE :
      // ESC key was pressed
      TerminateApplication;
    SDLK_TAB :
      begin
        if ( tractor.IsTowing ) then
          tractor.UnHook
        else
          tractor.HookUp( trailer );
      end;

    SDLK_UP :
      begin
        Throttle := true;
      end;

    SDLK_DOWN :
      begin
        Brake := true;
      end;

    SDLK_LEFT :
      begin
        Left := true;
      end;

    SDLK_RIGHT :
      begin
        Right := true;
      end;

    SDLK_SPACE :
      begin
        HandBrake := true;
      end;

    SDLK_LSHIFT :
      begin
        Reverse := false;
      end;

    SDLK_RSHIFT :
      begin
        Reverse := true;
      end;

    SDLK_PAUSE :
      begin
        pause := not pause;
      end;

    SDLK_RETURN :
      begin
        if ( keysym.Modifier and KMOD_ALT <> 0 ) then
        begin
          {* Alt+Enter key was pressed
           * this toggles fullscreen mode
           *}
          SDL_WM_ToggleFullScreen( surface );
        end;
      end;

    SDLK_F :
      begin
        // Flip Body
        dBodyAddForce( tractor.GetBodyID, 0.45, 5.45, 0 );
      end;
  end;
end;

// function to handle key press events

procedure HandleKeyRelease( keysym : PSDL_keysym );
begin
  case keysym.sym of
    SDLK_UP :
      begin
        Throttle := false;
      end;

    SDLK_DOWN :
      begin
        Brake := false;
      end;

    SDLK_LEFT :
      begin
        Left := false;
      end;

    SDLK_RIGHT :
      begin
        Right := false;
      end;

    SDLK_SPACE :
      begin
        HandBrake := false;
      end;
  end;
end;

procedure BuildFont;
var
  loop : GLuint; // Loop variable               */
  cx : single; // Holds Our X Character Coord */
  cy : single; // Holds Our Y Character Coord */
begin
  // Creating 256 Display List */
  base := glGenLists( 256 );
  /// Select Our Font Texture */
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

  // Loop Through All 256 Lists */
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
    // Y Position Of Current Character */
    cy := 1 - ( loop div 16 ) / 16.0;

    // Start Building A List
    glNewList( base + ( 255 - loop ), GL_COMPILE );
    // Use A Quad For Each Character
    glBegin( GL_QUADS );
    // Texture Coord (Bottom Left)
    glTexCoord2f( cx - 0.0625, cy );
    // Vertex Coord (Bottom Left) */
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

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL : Boolean;
var
  R : TdMatrix3;
begin
  glEnable( GL_TEXTURE_2D ); // Enable Texture Mapping

  LoadGLTextures; // Load that font Texture

  glShadeModel( GL_FLAT ); // Set Shading mode FLAT|SMOOTH

  glEnable( GL_DEPTH_TEST ); // Enables Depth Testing

  glDepthFunc( GL_LEQUAL ); // The Type Of Depth Testing To Do

  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
    // Really Nice Perspective Calculations

  glEnable( GL_BLEND ); // Use alpha blending (for shadows)

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    // Normal translucent blend mode

  glEnable( GL_CULL_FACE ); // Enable culling

  glCullFace( GL_BACK ); // Cull back faces

  glLineWidth( OUTLINE_WIDTH ); // Set the width of lines

  glEnable( GL_LINE_SMOOTH );
    // Antialiasing lines forces width to 1 (on my card)

  glPolygonMode( GL_BACK, GL_LINE ); // Draw Backfacing Polygons As Wireframes

  glPolygonMode( GL_FRONT, GL_FILL ); // Draw Backfacing Polygons As Wireframes

  glClearColor( 0.7, 0.7, 1.0, 0.0 ); // Light blue Background

  glClearDepth( 1.0 ); // Depth Buffer Setup

  glEnable( GL_COLOR_MATERIAL );

  glColorMaterial( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );

  glLightfv( GL_LIGHT0, GL_AMBIENT, @LightAmb[ 0 ] );
    // Set The Ambient Lighting For Light0
  glLightfv( GL_LIGHT0, GL_DIFFUSE, @LightDif[ 0 ] );
    // Set The Diffuse Lighting For Light0
  glLightfv( GL_LIGHT0, GL_POSITION, @LightPos[ 0 ] );
    // Set The Position For Light0

  glEnable( GL_LIGHT0 ); // Enable Light 0
  glEnable( GL_LIGHTING ); // Enable Lighting

  world := dWorldCreate;
  space := dHashSpaceCreate( nil );
  contactgroup := dJointGroupCreate( 0 );
  dWorldSetGravity( world, 0, -0.5, 0   );
  ground := dCreatePlane( space, 0, 1, 0, 0 );

  // chassis body
  tractor := TVehicle.Create( world, space, CWIDTH, CHEIGHT, CLENGTH, CMASS );
  tractor.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    1.0,
    WMASS,
    0.0,
    0.4,
    0.8,
    false
    ), -CLENGTH * 0.5, -CHEIGHT, -CWIDTH * 0.5 );

  tractor.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    1.0,
    WMASS,
    0.0,
    0.4,
    0.8,
    false
    ), -CLENGTH * 0.5, -CHEIGHT, CWIDTH * 0.5 );

  tractor.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    0.0,
    WMASS,
    1.0,
    0.4,
    0.8,
    false
    ), CLENGTH * 0.5, -CHEIGHT, -CWIDTH * 0.5 );

  tractor.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    0.0,
    WMASS,
    1.0,
    0.4,
    0.8,
    false
    ), CLENGTH * 0.5, -CHEIGHT, CWIDTH * 0.5 );
  tractor.SetColor( 0, 1, 1 );
  tractor.SetPosition( 0, CSTARTZ, 0 );
  tractor.CreateFifthWheel( -0.5 * CLENGTH, 0, 0 );

  // trailer body
  trailer := TVehicle.Create( world, space, CWIDTH, CHEIGHT, CLENGTH * 2.0, CMASS
    * 2.0 );
  trailer.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    0.0,
    WMASS,
    0.0,
    0.4,
    0.8,
    false
    ), -CLENGTH, -CHEIGHT, -CWIDTH * 0.5 );
  trailer.AddWheel( TWheel.Create(
    world,
    WWIDTH,
    WRADIUS,
    0.0,
    0.0,
    WMASS,
    0.0,
    0.4,
    0.8,
    false
    ), -CLENGTH, -CHEIGHT, CWIDTH * 0.5 );
  trailer.SetColor( 1, 0, 1 );
  trailer.SetPosition( -CLENGTH * 2.0, CSTARTZ, 0 );
  trailer.SetHookPoint( CLENGTH * 1.5, 0, 0 );

  // environment
  ground_box := dCreateBox( space, 2, 1, 1.5 );
  dRFromAxisAndAngle( R, 0, 0, 1, 0.15 );
  dGeomSetPosition( ground_box, 2, -0.34, 0 );
  dGeomSetRotation( ground_box, R );

  qobj := gluNewQuadric;

  BuildFont;

  result := true;
end;

// Function to draw the string on the screen
procedure glDrawText( x, y : GLint; Text : PChar; fontset : integer );
begin
   // Did User Choose An Invalid Character Set? */
  if ( fontset > 1 ) then
    fontset := 1;
  // Enable Texture Mapping */
  glEnable( GL_TEXTURE_2D );
  // Select our texture */
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );
  // Disable depth testing */
  glDisable( GL_DEPTH_TEST );
  // Reset The Modelview Matrix */
  glLoadIdentity;
  // Position The Text (0,0 - Bottom Left) */
  glTranslated( x, y, 0 );
  // Choose The Font Set (0 or 1) */
  glListBase( base - 32 + ( 128 * fontset ) );
  // If Set 0 Is Being Used Enlarge Font */
  if ( fontset = 0 ) then
    // Enlarge Font Width And Height */
    glScalef( 1.5, 2.0, 1.0 );
  // Write The Text To The Screen */
  glCallLists( Length( Text ), GL_BYTE, Text );
  // Disable Texture Mapping */
  glDisable( GL_TEXTURE_2D );
  // Re-enable Depth Testing */
  glEnable( GL_DEPTH_TEST );
end;

// The main drawing function.

procedure DrawGLScene;
var
  h, r : GLFloat;
begin
  if not pause then
  begin
    tractor.HandleBinaryControls( Throttle, Brake, HandBrake, Left, Right,
      Reverse );
  end;

  // SIMULATE STUFF
  if ( not pause ) then
  begin
    // work out the collisions and step the simulation
    dSpaceCollide( space, nil, @nearCallback );
    dWorldStep( world, 0.05 );

    // remove all contact joints
    dJointGroupEmpty( contactgroup );
  end;

  // DRAW STUFF
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  // Clear The Screen And The Depth Buffer
  glLoadIdentity; // Reset The Modelview Matrix
  
  setCamera( trailer.GetBodyID, tractor.GetBodyID );

  // Draw the ground
  glCullFace( GL_BACK );
  glBegin( GL_QUADS );
    guSetColor( 0.5, 0.5, 0.5 );
    glVertex3f( -100, 0, -100 );
    glVertex3f( -100, 0, 100 );
    glVertex3f( 100, 0, 100 );
    glVertex3f( 100, 0, -100 );
  glEnd;

  // Draw everything filled
  drawEverything( true );

  // Draw everything outlined in black to get a cell-shaded look
  glCullFace( GL_FRONT );
  guSetColor( 0, 0, 0 );
  drawEverything( false );
  
  // Draw the bottom of the rev counter
  glBegin( GL_LINES );
    guSetColor( 1, 0, 0 );
    glVertex3f( 0, 0, 0 );
    glVertex3f( 1, 0, 0 );

    guSetColor( 0, 1, 0 );
    glVertex3f( 0, 0, 0 );
    glVertex3f( 0, 1, 0 );

    guSetColor( 0, 0, 1 );
    glVertex3f( 0, 0, 0 );
    glVertex3f( 0, 0, 1 );
  glEnd;

  glCullFace( GL_BACK );
  glShadeModel( GL_SMOOTH );
  glDisable( GL_LIGHTING );

  // Draw the "rev counter"
  glMatrixMode( GL_MODELVIEW );
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
  glLoadIdentity;

  glBegin( GL_QUADS );
    h := 0.5 * tractor.GetRevsFraction;
    r := tractor.GetRedLineFraction;
    if 1.0 < r then
      r := 1.0;


    glColor4f( 0, 1, 0, 0.5 );
    glVertex3f( -0.9, -0.9, -0.9 );

    glColor4f( 0, 1, 0, 0.5 );
    glVertex3f( -0.8, -0.9, -0.9 );

    glColor4f( r, 1.0 - r, 0, 0.5 );
    glVertex3f( -0.8, -0.9 + h, -0.9 );

    glColor4f( r, 1.0 - r, 0, 0.5 );
    glVertex3f( -0.9, -0.9 + h, -0.9 );
  glEnd;

  glBegin( GL_LINES );
    glColor3f( 0, 0, 0 );
    glVertex3f( -0.9, -0.9, -0.91 );
    glVertex3f( -0.8, -0.9, -0.91 );
  glEnd;

  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix;

  glShadeModel( GL_FLAT );
  glEnable( GL_LIGHTING );

    // Set Color To Red
  glColor3f( 1.0, 0.0, 0.0 );
  // Show Draw Help
  glDrawText( 0, 10, 'Cursor Keys - Drive', 0 );
  glDrawText( 0, 40, 'TAB - Hook/UnHook Trailer', 0 );
  glDrawText( 0, 70, 'F - Flip Car', 0 );
  // Show what gear we're in
  glDrawText( 24, 207, PChar( tractor.GetGearChar ), 0 );

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
begin
  // Initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Could not initialize SDL : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;

  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;

  if ( videoInfo = nil ) then
  begin
    Log.LogError( Format( 'Video query failed : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;

  // the flags to pass to SDL_SetVideoMode
  videoFlags := SDL_OPENGL; // Enable OpenGL in SDL
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware

  // This checks to see if surfaces can be stored in memory
  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;

  // This checks if hardware blits can be done * /
  if ( videoInfo.blit_hw <> 0 ) then
    videoFlags := videoFlags or SDL_HWACCEL;

  // Make keys repeat;
  SDL_EnableKeyRepeat( SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL );

  // Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it
  SDL_WM_SetCaption( 'ED Jones'' TruckOff demo using JEDI-SDL', nil );

  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ),
      'Main' );
    TerminateApplication;
  end;

  // Loop, drawing and checking events
  InitGL;
  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

  Done := False;
  while ( not Done ) do
  begin
    // This could go in a separate function
    while ( SDL_PollEvent( @event ) = 1 ) do
    begin
      case event.type_ of
        SDL_QUITEV :
          begin
            Done := true;
          end;

        SDL_KEYDOWN :
          begin
            // handle key presses
            HandleKeyPress( @event.key.keysym );
          end;

        SDL_KEYUP :
          begin
            // handle key presses
            HandleKeyRelease( @event.key.keysym );
          end;

        SDL_VIDEORESIZE :
          begin
            surface := SDL_SetVideoMode( event.resize.w, event.resize.h,
              SCREEN_BPP, videoflags );
            if ( surface = nil ) then
            begin
              Log.LogError( Format( 'Could not get a surface after resize : %s',
                [ SDL_GetError ] ),
                'Main' );
              TerminateApplication;
            end;
            InitGL;
            ResizeWindow( event.resize.w, event.resize.h );
          end;
      end;
    end;
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.

