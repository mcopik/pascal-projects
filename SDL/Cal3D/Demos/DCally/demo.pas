unit demo;

interface

uses
  SysUtils,
  Classes,
  sdl,
  sdl_image,
  gl,
  glu,
  glext,
  menu,
  model;

type
  TDemo = class
  private
    m_width : integer;
    m_height : integer;
    m_bFullscreen : boolean;
    m_fpsDuration : single;
    m_fpsFrames : integer;
    m_fps : integer;
    m_cursorTextureId : integer;
    m_logoTextureId : integer;
    m_fpsTextureId : integer;
    m_mouseX : integer;
    m_mouseY : integer;
    m_tiltAngle : single;
    m_twistAngle : single;
    m_distance : single;
    m_bLeftMouseButtonDown : boolean;
    m_bRightMouseButtonDown : boolean;
    m_lastTick : integer;
    m_strDatapath : string;
    m_vectorModel : array of TModel;
    m_currentModel : integer;
    m_bPaused : boolean;
    function getModel : TModel;
  protected
    function LoadModel( modelname : string ) : boolean; // Delphi version only
  public
    constructor Create;
    destructor Destroy; override;
    function LoadTexture( strFilename : string; var id : integer ) : boolean;
    procedure OnIdle;
    procedure onMouseButtonDown( button, x, y : integer );
    procedure onMouseButtonUp( button, x, y : integer );
    procedure onMouseMove( x, y : integer );
    procedure setDimension( width, height : integer );
    procedure OnRender;
    procedure onKey( key : char; x, y : integer );
    function OnInit : boolean;
    procedure onShutdown;
    procedure nextModel;
    property Model : TModel read GetModel;
    property width : integer read m_width;
    property height : integer read m_height;
    property fullscreen : boolean read m_bFullscreen;
    property Datapath : string read m_strDatapath;
  end;

var
  TheDemo : TDemo;

implementation

uses
  logger;

var
  lightPosition : array[ 0..3 ] of single = ( 0.0, 0.0, 0.0, 1.0 );
  lightColorAmbient : array[ 0..3 ] of single = ( 0.3, 0.3, 0.3, 1.0 );
  lightColorDiffuse : array[ 0..3 ] of single = ( 0.52, 0.5, 0.5, 1.0 );
  lightColorSpecular : array[ 0..3 ] of single = ( 0.1, 0.1, 0.1, 1.0 );

constructor TDemo.Create;
var
  arg : integer;
begin
  inherited;
  TheMenu := TMenu.Create;
  m_width := 640;
  m_height := 480;
  m_bFullscreen := false;
  m_fpsDuration := 0.0;
  m_fpsFrames := 0;
  m_fps := 0;
  m_mouseX := 0;
  m_mouseY := 0;
  m_tiltAngle := -70.0;
  m_twistAngle := -45.0;
  m_distance := 270.0;
  m_bLeftMouseButtonDown := false;
  m_bRightMouseButtonDown := false;
  m_lastTick := SDL_GetTicks;
  m_currentModel := 0;
  m_bPaused := false;

  m_strDatapath := 'data/';

  // show some information
  Log.LogStatus( 'o----------------------------------------------------------------o', '' );
  Log.LogStatus( '|                       The Cally Demo 2.0                       |', '' );
  Log.LogStatus( '|     Orignial version (C) 2001 Bruno ''Beosil'' Heidelberger      |', '' );
  Log.LogStatus( '|             Delphi version (C) 2001 Paul TOTH                  |', '' );
  Log.LogStatus( 'o----------------------------------------------------------------o', '' );
  Log.LogStatus( '| This program is free software; you can redistribute it and/or  |' , '' );
  Log.LogStatus( '| modify it under the terms of the GNU General Public License as |', '' );
  Log.LogStatus( '| published by the Free Software Foundation; either version 2 of |' , '' );
  Log.LogStatus( '| the License, or (at your option) any later version.            |' , '' );
  Log.LogStatus( 'o----------------------------------------------------------------o' , '' );

  // parse the command line arguments
  arg := 1;
  while arg <= ParamCount do
  begin
    // check for fullscreen flag
    if ParamStr( arg ) = '--fullscreen' then
      m_bFullscreen := true
    else
    if ParamStr( arg ) = '--window' then
        m_bFullscreen := false
      else
    if ( ParamStr( arg ) = '--dimension' ) and ( ParamCount - arg > 2 ) then
        begin
          inc( arg );
          m_width := StrToInt( paramstr( arg ) );
          inc( arg );
          m_height := StrToInt( paramstr( arg ) );
          if ( m_width <= 0 ) or ( m_height <= 0 ) then
          begin
            Log.LogStatus( 'Invalid dimension!', '' );
            halt;
          end;
        end
        else
        begin
          Log.LogStatus( 'Usage: ' + paramstr( 0 ) + ' [--fullscreen] [--window] [--dimension width height]' , '' );
          halt;
        end;
    inc( arg );
  end;
end;

procedure TDemo.OnIdle;
var
  tick : integer;
  elapsedSeconds : single;
begin
  // get the current tick value
  tick := SDL_GetTicks;
  // calculate the amount of elapsed seconds
  elapsedSeconds := ( tick - m_lastTick ) / 1000.0;
  if elapsedSeconds = 0 then
    exit;
  // adjust fps counter
  m_fpsDuration := m_fpsDuration + elapsedSeconds;
  if ( m_fpsDuration >= 1.0 ) then
  begin
    m_fps := round( m_fpsFrames / m_fpsDuration );
    m_fpsDuration := 0.0;
    m_fpsFrames := 0;
  end;
  // update the current model
  if ( not m_bPaused ) then
    m_vectorModel[ m_currentModel ].onUpdate( elapsedSeconds );
  // update the menu
  theMenu.onUpdate( elapsedSeconds );
  // current tick will be last tick next round
  m_lastTick := tick;
  // update the screen
  glFlush;
end;

procedure TDemo.onMouseButtonDown( button, x, y : integer );
begin
  // let the menu handle mouse buttons first
  if not theMenu.onMouseButtonDown( button, x, y ) then
  begin
    // update mouse button states
    if ( button = SDL_BUTTON_LEFT ) then
    begin
      m_bLeftMouseButtonDown := true;
    end;

    if ( button = SDL_BUTTON_RIGHT ) then
    begin
      m_bRightMouseButtonDown := true;
    end;
  end;

  // update internal mouse position
  m_mouseX := x;
  m_mouseY := y;
end;

procedure TDemo.onMouseButtonUp( button, x, y : integer );
begin
  // let the menu handle mouse buttons first
  if not theMenu.onMouseButtonUp( button, x, y ) then
  begin
    // update mouse button states
    if ( button = SDL_BUTTON_LEFT ) then
    begin
      m_bLeftMouseButtonDown := false;
    end;
    if ( button = SDL_BUTTON_RIGHT ) then
    begin
      m_bRightMouseButtonDown := false;
    end;
  end;
  // update internal mouse position
  m_mouseX := x;
  m_mouseY := y;
end;

procedure TDemo.onMouseMove( x, y : integer );
begin
  // let the meu handle mouse buttons first
  if not theMenu.onMouseMove( x, y ) then
  begin
    // update twist/tilt angles
    if ( m_bLeftMouseButtonDown ) then
    begin
      // calculate new angles
      m_twistAngle := m_twistAngle + ( x - m_mouseX );
      m_tiltAngle := m_tiltAngle - ( y - m_mouseY );
    end;

    // update distance
    if ( m_bRightMouseButtonDown ) then
    begin
      // calculate new distance
      m_distance := m_distance - ( y - m_mouseY ) / 3.0;
      if ( m_distance < 0.0 ) then
        m_distance := 0.0;
    end;
  end;

  // update internal mouse position
  m_mouseX := x;
  m_mouseY := y;
end;

procedure TDemo.setDimension( width, height : integer );
begin
  // store new width and height values
  m_width := width;
  m_height := height;

  // set new viewport dimension
  glViewport( 0, 0, m_width, m_height );

  // adjust menu
  theMenu.onResize( width, height );
end;

procedure TDemo.onRender;
var
  renderScale : single;
  digit : integer;
  digitId : integer;
  x : integer;
  tx : single;
begin
  // clear all the buffers
  glClearColor( 0.0, 0.0, 0.2, 0.0 );
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  // get the render scale of the model
  renderScale := m_vectorModel[ m_currentModel ].RenderScale;

  // set the projection transformation
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity( );
  gluPerspective( 45.0, m_width / m_height, renderScale * 50.0, renderScale * 5000.0 );
  //gluPerspective( 45.0, m_width / m_height, 1.0, renderScale * 800.0 );

  // set the model transformation
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity( );

  // set the light position and attributes
  glLightfv( GL_LIGHT0, GL_POSITION, @lightPosition );
  glLightfv( GL_LIGHT0, GL_AMBIENT, @lightColorAmbient );
  glLightfv( GL_LIGHT0, GL_DIFFUSE, @lightColorDiffuse );
  glLightfv( GL_LIGHT0, GL_SPECULAR, @lightColorSpecular );

  // set camera position
  glTranslatef( 0.0, 0.0, -m_distance * renderScale );
  glRotatef( m_tiltAngle, 1.0, 0.0, 0.0 );
  glRotatef( m_twistAngle, 0.0, 0.0, 1.0 );
  glTranslatef( 0.0, 0.0, -90.0 * renderScale );

  // render model
  m_vectorModel[ m_currentModel ].onRender;

  // switch to orthogonal projection for 2d stuff
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity( );
  glOrtho( 0, m_width, 0, m_height, -1.0, 1.0 );

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity( );

  // we will render some alpha-blended textures
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  // render menu
  theMenu.onRender;

  glEnable( GL_TEXTURE_2D );

  // render the logo
  glBindTexture( GL_TEXTURE_2D, m_logoTextureId );

  glColor3f( 1.0, 1.0, 1.0 );
  glBegin( GL_QUADS );
    glTexCoord2f( 0.0, 1.0 );
    glVertex2i( 0, 0 );
    glTexCoord2f( 1.0, 1.0 );
    glVertex2i( 128, 0 );
    glTexCoord2f( 1.0, 0.0 );
    glVertex2i( 128, 128 );
    glTexCoord2f( 0.0, 0.0 );
    glVertex2i( 0, 128 );
  glEnd( );

  // render the fps counter
  glBindTexture( GL_TEXTURE_2D, m_fpsTextureId );

  glBegin( GL_QUADS );
  digit := m_fps;
  for digitId := 2 downto 0 do
  begin
    x := 29 + digitId * 16;
    tx := ( digit mod 10 ) * 0.0625;
    glTexCoord2f( tx, 1.0 );
    glVertex2i( x, 94 );
    glTexCoord2f( tx + 0.0625, 1.0 );
    glVertex2i( x + 16, 94 );
    glTexCoord2f( tx + 0.0625, 0.0 );
    glVertex2i( x + 16, 110 );
    glTexCoord2f( tx, 0.0 );
    glVertex2i( x, 110 );
    digit := digit div 10;
  end;
  glEnd( );

  // render the cursor
  glBindTexture( GL_TEXTURE_2D, m_cursorTextureId );

  glBegin( GL_QUADS );
    glTexCoord2f( 0.0, 1.0 );
    glVertex2i( m_mouseX, m_mouseY - 32 );
    glTexCoord2f( 1.0, 1.0 );
    glVertex2i( m_mouseX + 32, m_mouseY - 32 );
    glTexCoord2f( 1.0, 0.0 );
    glVertex2i( m_mouseX + 32, m_mouseY );
    glTexCoord2f( 0.0, 0.0 );
    glVertex2i( m_mouseX, m_mouseY );
  glEnd( );

  glDisable( GL_TEXTURE_2D );
  glDisable( GL_BLEND );

  // swap the front- and back-buffer
  SDL_GL_SwapBuffers;

  // increase frame counter
  inc( m_fpsFrames );
end;

procedure TDemo.onKey( key : char; x, y : integer );
begin
  // test for quit event
  if ( ( key = #27 ) or ( key = 'q' ) or ( key = 'Q' ) ) then
    halt( 0 );
  // test for pause event
  if ( key = ' ' ) then
    m_bPaused := not m_bPaused;
  // let the menu handle the rest
  theMenu.onKey( key, x, y );
end;

function TDemo.LoadModel( modelname : string ) : boolean;
var
  model : TModel;
  i : integer;
begin
  result := false;
  // load model
  Log.LogStatus( 'Loading ''' + modelname + ''' model ...' , '' );
  Model := TModel.Create;
  if not Model.onInit( m_strDatapath + modelname + '.cfg' ) then
  begin
    Model.Free;
    Log.LogStatus( 'Model initialization failed! (' + modelname + ')' , '' );
    exit;
  end;
  i := Length( m_vectorModel );
  SetLength( m_vectorModel, i + 1 );
  m_vectorModel[ i ] := Model;

  result := true;
end;

function TDemo.onInit : boolean;
var
  strFilename : string;
begin
  result := false;
  // load the cursor texture
  strFilename := m_strDatapath + 'cursor.raw';
  if not loadTexture( strFilename, m_cursorTextureId ) then
    exit;
  // load the logo texture
  strFilename := m_strDatapath + 'logo.raw';
  if not loadTexture( strFilename, m_logoTextureId ) then
    exit;
  // load the fps texture
  strFilename := m_strDatapath + 'fps.raw';
  if not loadTexture( strFilename, m_fpsTextureId ) then
    exit;
  // initialize models
  LoadModel( 'cally' );
  // Uncomment the following lines if you have the other data.
  {LoadModel( 'skeleton' );
  LoadModel( 'paladin' );
  LoadModel( 'hero' );
  LoadModel( 'heroine' );
  LoadModel( 'thug' );}

  if Length( m_vectorModel ) = 0 then
    exit;
  // initialize menu
  if not theMenu.onInit( m_width, m_height ) then
  begin
    Log.LogStatus( 'Menu initialization failed!' , '' );
    exit;
  end;
  // we're done
  Log.LogStatus( 'Initialization done.' , '' );
  Log.LogStatus( 'Quit the demo by pressing ''q'' or ESC' , '' );
  result := true;
end;

function TDemo.loadTexture( strFilename : string; var id : integer ) : boolean;
var
  stream : TFileStream;
  width, height, depth : integer;
  buffer : pchar;
  ext : string;
  Picture : PSDL_Surface;
begin
  // initialize the id
  Id := 0;
  result := false;
  ext := ExtractFileExt( strFilename );
  if ( ext = '.raw' ) then
  begin
    // open the texture file
    stream := TFileStream.Create( strFileName, fmOpenRead );
    try
     // read the width, height and depth of the texture
      stream.readbuffer( width, sizeof( width ) );
      stream.Readbuffer( height, sizeof( height ) );
      stream.readbuffer( depth, sizeof( depth ) );
     // allocate a temporary buffer to hold the texture data
      getmem( buffer, width * height * depth );
     // load the texture
      stream.readbuffer( buffer^, width * height * depth );
    finally
      stream.free;
    end;
    // generate the texture
    glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
    glGenTextures( 1, @Id );
    glBindTexture( GL_TEXTURE_2D, Id );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    if depth = 3 then
      glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, Buffer )
    else
      glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Buffer );

    result := true;
  end
  else
  begin
    Picture := IMG_Load( PChar( strFilename ) );
    if Picture <> nil then
    begin
      // generate the texture
      glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
      glGenTextures( 1, @Id );
      glBindTexture( GL_TEXTURE_2D, Id );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
      if ( Picture.format.BytesPerPixel = 3 ) then
      begin
        glTexImage2D( GL_TEXTURE_2D, 0, 3, Picture.w, Picture.h, 0, GL_BGR, GL_UNSIGNED_BYTE, Picture.pixels );
        gluBuild2DMipmaps( GL_TEXTURE_2D, 3, Picture.w, Picture.h, GL_BGR, GL_UNSIGNED_BYTE, Picture.pixels );
      end
      else
      begin
        glTexImage2D( GL_TEXTURE_2D, 0, GL_BGRA, Picture.w, Picture.h, 0, GL_BGRA, GL_UNSIGNED_BYTE, Picture.pixels );
        gluBuild2DMipmaps( GL_TEXTURE_2D, GL_BGRA, Picture.w, Picture.h, GL_BGRA, GL_UNSIGNED_BYTE, Picture.pixels );
      end;
      SDL_FreeSurface( Picture );

      result := true;
    end;
  end;
end;

function TDemo.GetModel : TModel;
begin
  result := m_vectorModel[ m_currentModel ];
end;


procedure TDemo.nextModel;
begin
  m_currentModel := ( m_currentModel + 1 ) mod Length( m_vectorModel );
end;

procedure TDemo.onShutdown;
var
  modelId, modelCount : integer;
begin
  // shut the menu down
  theMenu.onShutdown;

  // shut all models down
  modelCount := Length( m_vectorModel );
  for modelId := 0 to modelCount - 1 do
  begin
    m_vectorModel[modelId].onShutdown;
    m_vectorModel[modelId].Free;
  end;
end;

destructor TDemo.Destroy;
begin

  inherited;
end;

end.

