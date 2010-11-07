unit menu;

interface

uses
  gl;

type
  TMenu = class
  private
    m_menuX : integer;
    m_menuY : integer;
    m_lodX : integer;
    m_lodY : integer;
    m_textureId : integer;
    m_lodTextureId : integer;
    m_bMotionMovement : boolean;
    m_bLodMovement : boolean;
    m_bSkeleton : boolean;
    m_bWireframe : boolean;
    m_bLight : boolean;
    m_actionTimespan : array[ 0..1 ] of single;
    m_nextTimespan : single;
    m_bBoundingBox: boolean;
  protected
    function getMenuItem( x, y : integer ) : integer;
    procedure calculateMotionBlend( x, y : integer );
    procedure calculateLodLevel( x, y : integer );
    function isInside( x, y : integer ) : boolean;
  public
    constructor Create;
    function onInit( width, height : integer ) : boolean;
    procedure OnUpdate( elapsedSeconds : single );
    function onMouseButtonDown( button, x, y : integer ) : boolean;
    function onMouseButtonUp( button, x, y : integer ) : boolean;
    function onMouseMove( x, y : integer ) : boolean;
    procedure onResize( width, height : integer );
    procedure OnRender;
    function onKey( key : char; x, y : integer ) : boolean;
    procedure onShutdown;
    property BoundingBox : boolean read m_bBoundingBox;
    property Skeleton : boolean read m_bSkeleton;
    property WireFrame : boolean read m_bWireFrame;
    property Light : boolean read m_bLight;
  end;

var
  TheMenu : TMenu;

implementation

uses
  demo,
  model;

const
  MENUITEM_Y : array[ 0..4 ] of integer = ( 228, 200, 94, 66, 38 );
  MENUITEM_HEIGHT : array[ 0..4 ] of integer = ( 28, 28, 106, 28, 28 );
  MENUITEM_MOTION_X : array[ 0..2 ] of integer = ( 42, 80, 118 );
  MENUITEM_MOTION_Y : array[ 0..2 ] of integer = ( 168, 102, 168 );

constructor TMenu.Create;
begin
  m_bMotionMovement := false;
  m_menuX := 4;
  m_menuY := 4;
  m_bSkeleton := false;
  m_bBoundingBox := false;
  m_bWireframe := false;
  m_bLight := true;
  m_actionTimespan[0] := 0.0;
  m_actionTimespan[1] := 0.0;
  m_nextTimespan := 0.0;
  m_lodX := 4;
  m_lodY := 4;
  m_bLodMovement := false;
end;

function TMenu.onInit( width, height : integer ) : boolean;
var
  strFilename : string;
begin
  result := false;
  // load the menu texture
  strFilename := TheDemo.Datapath + 'menu.raw';
  if not theDemo.loadTexture( strFilename, m_textureId ) then
    exit;
  // load the lodxture
  strFilename := theDemo.Datapath + 'lod.raw';
  if not theDemo.loadTexture( strFilename, m_lodTextureId ) then
    exit;
  result := true;
end;

procedure TMenu.OnUpdate( elapsedSeconds : single );
begin
  // calculate new timespan for f/x 1
  if ( m_actionTimespan[ 0 ] > 0.0 ) then
  begin
    m_actionTimespan[ 0 ] := m_actionTimespan[ 0 ] - elapsedSeconds;
    if ( m_actionTimespan[ 0 ] < 0.0 ) then
      m_actionTimespan[ 0 ] := 0.0;
  end;

  // calculate new timespan for f/x 2
  if ( m_actionTimespan[ 1 ] > 0.0 ) then
  begin
    m_actionTimespan[ 1 ] := m_actionTimespan[ 1 ] - elapsedSeconds;
    if ( m_actionTimespan[ 1 ] < 0.0 ) then
      m_actionTimespan[ 1 ] := 0.0;
  end;
  // calculate new timespan for 'next model'
  if ( m_nextTimespan > 0.0 ) then
  begin
    m_nextTimespan := m_nextTimespan - elapsedSeconds;
    if ( m_nextTimespan < 0.0 ) then
      m_nextTimespan := 0.0;
  end;
end;

function TMenu.onMouseButtonDown( button, x, y : integer ) : boolean;
var
  menuItem : integer;
begin
  result := true;
  // get activated menu item
  menuItem := getMenuItem( x, y );
  // handle 'idle' button
  if ( menuItem = STATE_IDLE ) then
  begin
    theDemo.Model.setState( STATE_IDLE, 0.3 );
    exit;
  end;

  // handle 'fancy' button
  if ( menuItem = STATE_FANCY ) then
  begin
    theDemo.Model.setState( STATE_FANCY, 0.3 );
    exit;
  end;
  // handle 'motion' button/controller
  if ( menuItem = STATE_MOTION ) then
  begin
    theDemo.Model.setState( STATE_MOTION, 0.3 );
    calculateMotionBlend( x, y );
    m_bMotionMovement := true;
    exit;
  end;
  // handle 'f/x 1' button
  if ( menuItem = 3 ) then
  begin
    theDemo.Model.executeAction( 0 );
    m_actionTimespan[ 0 ] := 1.0;
  end;
  // handle 'f/x 2' button
  if ( menuItem = 4 ) then
  begin
    theDemo.Model.executeAction( 1 );
    m_actionTimespan[ 1 ] := 1.0;
  end;
  // handle 'skeleton' button
  if ( menuItem = 5 ) then
  begin
    if m_bBoundingBox then
    begin
      m_bSkeleton := false;
      m_bBoundingBox := false;
    end
    else if m_bSkeleton then
      m_bBoundingBox := true
    else
      m_bSkeleton := not m_bSkeleton;
  end;
  // handle 'wireframe' button
  if ( menuItem = 6 ) then
  begin
    m_bWireframe := not m_bWireframe;
  end;
  // handle 'light' button
  if ( menuItem = 7 ) then
  begin
    m_bLight := not m_bLight;
  end;
  // handle 'next model' button
  if ( menuItem = 8 ) then
  begin
    theDemo.nextModel;
    m_nextTimespan := 0.3;
  end;
  // handle lod bar
  if ( menuItem = 9 ) then
  begin
    calculateLodLevel( x, y );
    m_bLodMovement := true;
    exit;
  end;
  result := isInside( x, y );
end;

function TMenu.onMouseButtonUp( button, x, y : integer ) : boolean;
begin
  result := true;
  if ( m_bMotionMovement ) then
  begin
    m_bMotionMovement := false;
    exit;
  end;
  if ( m_bLodMovement ) then
  begin
    m_bLodMovement := false;
    exit;
  end;
  result := false;
end;

function TMenu.onMouseMove( x, y : integer ) : boolean;
begin
  result := true;
  if ( m_bMotionMovement ) then
  begin
    // update motion blend factors
    calculateMotionBlend( x, y );
    exit;
  end;
  if ( m_bLodMovement ) then
  begin
    // update lod level
    calculateLodLevel( x, y );
    exit;
  end;
  result := false;
end;

procedure TMenu.onResize( width, height : integer );
begin
  // adjust menu position
  m_menuX := width - 132;
  // adjust lod position
  m_lodX := width div 2 - 128;
end;

procedure TMenu.onRender;
var
  state : integer;
  startY, endY : integer;
  lodLevel : single;
  motionBlend : array[ 0..2 ] of single;
  motionX, motionY : integer;
begin
  // get the current animation state of the model
  state := theDemo.Model.State;
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, m_textureId );
  glColor3f( 1.0, 1.0, 1.0 );
  glBegin( GL_QUADS );

  // render the base menu
  glTexCoord2f( 0.5, 1.0 );
  glVertex2i( m_menuX, m_menuY );
  glTexCoord2f( 1.0, 1.0 );
  glVertex2i( m_menuX + 128, m_menuY );
  glTexCoord2f( 1.0, 0.0 );
  glVertex2i( m_menuX + 128, m_menuY + 256 );
  glTexCoord2f( 0.5, 0.0 );
  glVertex2i( m_menuX, m_menuY + 256 );

  // render all active states
  startY := MENUITEM_Y[ state ];
  endY := startY + MENUITEM_HEIGHT[ state ];
  glTexCoord2f( 0.0, 1.0 - startY / 256.0 );
  glVertex2i( m_menuX, m_menuY + startY );
  glTexCoord2f( 0.5, 1.0 - startY / 256.0 );
  glVertex2i( m_menuX + 128, m_menuY + startY );
  glTexCoord2f( 0.5, 1.0 - endY / 256.0 );
  glVertex2i( m_menuX + 128, m_menuY + endY );
  glTexCoord2f( 0.0, 1.0 - endY / 256.0 );
  glVertex2i( m_menuX, m_menuY + endY );

  if ( m_actionTimespan[ 0 ] > 0.0 ) then
  begin
    startY := MENUITEM_Y[ 3 ];
    endY := startY + MENUITEM_HEIGHT[ 3 ];
    glTexCoord2f( 0.0, 1.0 - startY / 256.0 );
    glVertex2i( m_menuX, m_menuY + startY );
    glTexCoord2f( 0.5, 1.0 - startY / 256.0 );
    glVertex2i( m_menuX + 128, m_menuY + startY );
    glTexCoord2f( 0.5, 1.0 - endY / 256.0 );
    glVertex2i( m_menuX + 128, m_menuY + endY );
    glTexCoord2f( 0.0, 1.0 - endY / 256.0 );
    glVertex2i( m_menuX, m_menuY + endY );
  end;

  if ( m_actionTimespan[ 1 ] > 0.0 ) then
  begin
    startY := MENUITEM_Y[ 4 ];
    endY := startY + MENUITEM_HEIGHT[ 4 ];
    glTexCoord2f( 0.0, 1.0 - startY / 256.0 );
    glVertex2i( m_menuX, m_menuY + startY );
    glTexCoord2f( 0.5, 1.0 - startY / 256.0 );
    glVertex2i( m_menuX + 128, m_menuY + startY );
    glTexCoord2f( 0.5, 1.0 - endY / 256.0 );
    glVertex2i( m_menuX + 128, m_menuY + endY );
    glTexCoord2f( 0.0, 1.0 - endY / 256.0 );
    glVertex2i( m_menuX, m_menuY + endY );
  end;

  if ( m_bSkeleton ) then
  begin
    glTexCoord2f( 0.0, 1.0 );
    glVertex2i( m_menuX, m_menuY );
    glTexCoord2f( 0.125, 1.0 );
    glVertex2i( m_menuX + 32, m_menuY );
    glTexCoord2f( 0.125, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 32, m_menuY + 35 );
    glTexCoord2f( 0.0, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX, m_menuY + 35 );
  end;

  if ( m_bWireframe ) then
  begin
    glTexCoord2f( 0.125, 1.0 );
    glVertex2i( m_menuX + 32, m_menuY );
    glTexCoord2f( 0.25, 1.0 );
    glVertex2i( m_menuX + 64, m_menuY );
    glTexCoord2f( 0.25, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 64, m_menuY + 35 );
    glTexCoord2f( 0.125, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 32, m_menuY + 35 );
  end;

  if ( m_bLight ) then
  begin
    glTexCoord2f( 0.25, 1.0 );
    glVertex2i( m_menuX + 64, m_menuY );
    glTexCoord2f( 0.375, 1.0 );
    glVertex2i( m_menuX + 96, m_menuY );
    glTexCoord2f( 0.375, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 96, m_menuY + 35 );
    glTexCoord2f( 0.25, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 64, m_menuY + 35 );
  end;

  if ( m_nextTimespan > 0.0 ) then
  begin
    glTexCoord2f( 0.375, 1.0 );
    glVertex2i( m_menuX + 96, m_menuY );
    glTexCoord2f( 0.5, 1.0 );
    glVertex2i( m_menuX + 128, m_menuY );
    glTexCoord2f( 0.5, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 128, m_menuY + 35 );
    glTexCoord2f( 0.375, 1.0 - 35.0 / 256.0 );
    glVertex2i( m_menuX + 96, m_menuY + 35 );
  end;

  glEnd( );

  // get the current lod level of the model
  lodLevel := theDemo.Model.LodLevel;

  glBindTexture( GL_TEXTURE_2D, m_lodTextureId );

  glColor3f( 1.0, 1.0, 1.0 );
  glBegin( GL_QUADS );
    // render the base lod
  glTexCoord2f( 0.0, 1.0 );
  glVertex2i( m_lodX, m_lodY );
  glTexCoord2f( 1.0, 1.0 );
  glVertex2i( m_lodX + 256, m_lodY );
  glTexCoord2f( 1.0, 0.5 );
  glVertex2i( m_lodX + 256, m_lodY + 32 );
  glTexCoord2f( 0.0, 0.5 );
  glVertex2i( m_lodX, m_lodY + 32 );

    // render the current lod level
  glTexCoord2f( ( 247 - lodLevel * 200 ) / 256.0, 0.5 );
  glVertex2i( m_lodX + 247 - round( lodLevel * 200 ), m_lodY );
  glTexCoord2f( 1.0, 0.5 );
  glVertex2i( m_lodX + 256, m_lodY );
  glTexCoord2f( 1.0, 0.0 );
  glVertex2i( m_lodX + 256, m_lodY + 32 );
  glTexCoord2f( ( 247 - lodLevel * 200 ) / 256.0, 0.0 );
  glVertex2i( m_lodX + 247 - round( lodLevel * 200 ), m_lodY + 32 );

  glEnd( );

  glDisable( GL_TEXTURE_2D );

  // render motion triangle
  if ( state = STATE_MOTION ) then
  begin
    // get current blending factors
    theDemo.Model.getMotionBlend( motionBlend );
    // calculate the current motion point
    motionX := round( motionBlend[ 0 ] * MENUITEM_MOTION_X[ 0 ] + motionBlend[ 1 ] * MENUITEM_MOTION_X[ 1 ] + motionBlend[ 2 ] * MENUITEM_MOTION_X[ 2 ] );
    motionY := round( motionBlend[ 0 ] * MENUITEM_MOTION_Y[ 0 ] + motionBlend[ 1 ] * MENUITEM_MOTION_Y[ 1 ] + motionBlend[ 2 ] * MENUITEM_MOTION_Y[ 2 ] );

    glLineWidth( 2.0 );

    glBegin( GL_LINES );
      glColor3f( 1.0, 1.0, 0.0 );
      glVertex2i( m_menuX + MENUITEM_MOTION_X[ 0 ], m_menuY + MENUITEM_MOTION_Y[ 0 ] );
      glVertex2i( m_menuX + motionX, m_menuY + motionY );
      glColor3f( 0.0, 1.0, 1.0 );
      glVertex2i( m_menuX + MENUITEM_MOTION_X[ 1 ], m_menuY + MENUITEM_MOTION_Y[ 1 ] );
      glVertex2i( m_menuX + motionX, m_menuY + motionY );
      glColor3f( 1.0, 0.0, 1.0 );
      glVertex2i( m_menuX + MENUITEM_MOTION_X[ 2 ], m_menuY + MENUITEM_MOTION_Y[ 2 ] );
      glVertex2i( m_menuX + motionX, m_menuY + motionY );
    glEnd( );

    glLineWidth( 1.0 );
  end;
end;

function TMenu.getMenuItem( x, y : integer ) : integer;
var
  itemId : integer;
begin
  // check if the point is inside the menu
  if ( not isInside( x, y ) ) then
  begin
    result := -1;
    exit;
  end;
  // check for the lod bar
  if ( ( x - m_lodX >= 0 ) and ( x - m_lodX < 256 ) and ( y - m_lodY >= 0 ) and ( y - m_lodY < 32 ) ) then
  begin
    result := 9;
    exit;
  end;
  // check for each menu item
  for itemId := 0 to 4 do
  begin
    if ( y - m_menuY >= MENUITEM_Y[ itemId ] ) and ( y - m_menuY < MENUITEM_Y[ itemId ] + MENUITEM_HEIGHT[ itemId ] ) then
    begin
      result := itemId;
      exit;
    end;
  end;
  // test for flag menu items
  if ( y - m_menuY >= 0 ) and ( y - m_menuY < 35 ) then
  begin
    result := 5 + ( x - m_menuX ) div 32;
    exit;
  end;

  result := -1;
end;

procedure TMenu.calculateMotionBlend( x, y : integer );
var
  motionBlend : array[ 0..2 ] of single;
  factor : single;
begin
  // convert to local coordinates
  dec( x, m_menuX );
  dec( y, m_menuY );

  // check if point is inside motion area
  if ( ( y >= MENUITEM_Y[ STATE_MOTION ] ) and ( y < MENUITEM_Y[ STATE_MOTION ] + MENUITEM_HEIGHT[ STATE_MOTION ] ) ) then
  begin
    // calculate baryzentric coordinates inside motion triangle
    motionBlend[ 0 ] := 1.0 - ( ( x - MENUITEM_MOTION_X[ 0 ] ) + ( MENUITEM_MOTION_Y[ 0 ] - y ) / 1.732 ) / 76.0;
    // clamp first to range [0.0 - 1.0]
    if ( motionBlend[ 0 ] < 0.0 ) then
      motionBlend[ 0 ] := 0.0;
    if ( motionBlend[ 0 ] > 1.0 ) then
      motionBlend[ 0 ] := 1.0;

    motionBlend[ 1 ] := 1.0 - ( y - MENUITEM_MOTION_Y[ 1 ] ) / 66.0;

    // clamp second to range [0.0 - 1.0]
    if ( motionBlend[ 1 ] < 0.0 ) then
      motionBlend[ 1 ] := 0.0;
    if ( motionBlend[ 1 ] > 1.0 ) then
      motionBlend[ 1 ] := 1.0;

    // clamp sum of first and second to range [0.0 - 1.0]
    if ( motionBlend[ 0 ] + motionBlend[ 1 ] > 1.0 ) then
    begin
      factor := motionBlend[ 0 ] + motionBlend[ 1 ];
      motionBlend[ 0 ] := motionBlend[ 0 ] / factor;
      motionBlend[ 1 ] := motionBlend[ 1 ] / factor;
    end;

    motionBlend[ 2 ] := 1.0 - motionBlend[ 0 ] - motionBlend[ 1 ];

    // clamp third to range [0.0 - 1.0]
    if ( motionBlend[ 2 ] < 0.0 ) then
      motionBlend[ 2 ] := 0.0;

    // set new motion blend factors
    theDemo.Model.setMotionBlend( motionBlend, 0.1 );
  end;
end;

procedure TMenu.calculateLodLevel( x, y : integer );
var
  lodLevel : single;
begin
  // convert to local coordinates
  dec( x, m_lodX );
//  dec(y, m_lodY);
  // calculate the new lod level from the local coordinates
  lodLevel := ( 247 - x ) / 200.0;
  // clamp the value to [0.0, 1.0]
  if ( lodLevel < 0.0 ) then
    lodLevel := 0.0;
  if ( lodLevel > 1.0 ) then
    lodLevel := 1.0;
  // set new motion blend factors
  theDemo.Model.LodLevel := lodLevel;
end;

function TMenu.isInside( x, y : integer ) : boolean;
begin
  result := true;
  if ( x - m_menuX >= 0 ) and ( x - m_menuX < 128 ) and ( y - m_menuY >= 0 ) and ( y - m_menuY < 256 ) then
    exit;
  if ( x - m_lodX >= 0 ) and ( x - m_lodX < 256 ) and ( y - m_lodY >= 0 ) and ( y - m_lodY < 32 ) then
    exit;
  result := false;
end;

function TMenu.onKey( key : char; x, y : integer ) : boolean;
begin
  result := false;
end;

procedure TMenu.onShutdown;
begin

end;

end.

