unit model;

interface

uses
  Classes,
  SysUtils,
  sdl,
  sdl_image,
  gl,
  glu,
  glext,
  dcal3d;

const
  STATE_IDLE = 0;
  STATE_FANCY = 1;
  STATE_MOTION = 2;

type
  TModel = class
  private
    m_state : integer;
    m_calCoreModel : CalCoreModel;
    m_calModel : CalModel;
    m_animationId : array[ 0..15 ] of integer;
    m_animationCount : integer;
    m_meshId : array[ 0..31 ] of integer;
    m_meshCount : integer;
    m_textureId : array[ 0..31 ] of integer;
    m_textureCount : integer;
    m_motionBlend : array[ 0..2 ] of single;
    m_renderScale : single;
    m_lodLevel : single;
    m_path : string;
    procedure setLodLevel( value : single );
    function loadTexture( strFilename : string ) : integer;
  protected
    procedure renderSkeleton;
    procedure renderBoundingBox;
    procedure renderMesh( Wireframe, Light : boolean );
  public
    constructor Create;
    destructor Destroy; override;

    procedure OnUpdate( elapsedSeconds : single );
    procedure OnRender;
    function OnInit( strFilename : string ) : boolean;
    procedure onShutdown;
    procedure SetState( state : integer; delay : single );
    procedure executeAction( action : integer );
    procedure setMotionBlend( MotionBlend : array of single; delay : single );
    procedure getMotionBlend( var MotionBlend : array of single );
    property RenderScale : single read m_RenderScale;
    property Lodlevel : single read m_lodlevel write setLodLevel;
    property State : integer read m_state;
    property Path : string read m_path write m_path;
  end;

implementation

uses
  Menu,
  logger;

function StrToFloat( s : string ) : single;
var
  i : integer;
  f : integer;
begin
  result := 0;
  f := length( s ) + 1;
  for i := 1 to length( s ) do
  begin
    case s[ i ] of
      '0'..'9' : result := 10 * result + ( ord( s[ i ] ) - ord( '0' ) );
      '.' : f := i + 1;
    else
      raise Exception.Create( 'Invalide float value ' );
    end;
  end;
  for i := f to length( s ) do
    result := result / 10;
end;

constructor TModel.Create;
begin
  inherited;
  m_state := STATE_IDLE;
  m_motionBlend[ 0 ] := 0.6;
  m_motionBlend[ 1 ] := 0.1;
  m_motionBlend[ 2 ] := 0.3;
  m_animationCount := 0;
  m_meshCount := 0;
  m_renderScale := 1.0;
  m_lodLevel := 1.0;
end;

procedure TModel.OnUpdate( elapsedSeconds : single );
begin
  m_calModel.update( elapsedSeconds );
end;

procedure TModel.OnRender;
begin
  // set global OpenGL states
  glEnable( GL_DEPTH_TEST );
  glShadeModel( GL_SMOOTH );

  // check if we need to render the skeleton
  if ( TheMenu.Skeleton ) then
    renderSkeleton;

  // check if we need to render the skeleton
  if ( TheMenu.BoundingBox ) then
    renderBoundingBox;

  // check if we need to render the mesh
  if ( not TheMenu.Skeleton ) or TheMenu.Wireframe then
  begin
    renderMesh( TheMenu.Wireframe, theMenu.Light );
  end;

  // clear global OpenGL states
  glDisable( GL_DEPTH_TEST );
end;

function TModel.OnInit( strFilename : string ) : boolean;
var
  ifstream : TextFile;
  strPath : string;
  animationcount, MaterialCount, MapCount, MeshCount : integer;
  strKey, strData : string;
  p : integer;
  materialId : integer;
  CoreMaterial : CalCoreMaterial;
  mapId : integer;
  s : string;
  textureId : integer;
  meshId : integer;
begin
  // open the model configuration file
{$I-}
  AssignFile( ifstream, strFileName );
  reset( ifstream );
  if ioresult <> 0 then
  begin
    result := false;
    exit;
  end;
  // create a core model instance
  m_calcoreModel := CalCoreModel.Create( 'dummy' );

  // initialize the data path
  strPath := m_path;

  // initialize the animation count
  animationCount := 0;

  // parse all lines from the model configuration file
  while not eof( ifstream ) do
  begin
    // read the next model configuration line
    readln( ifstream, strKey );
    if strKey = '' then
      continue;
    if strKey[ 1 ] = '#' then
      continue;
    p := pos( '=', strKey );
    strData := copy( strKey, p + 1, length( strKey ) );
    SetLength( strKey, p - 1 );

    // handle the model creation
    if ( strKey = 'scale' ) then
    begin
      // set rendering scale factor
      m_renderScale := StrToFloat( strData );
    end
    else if ( strKey = 'path' ) then
    begin
      // set the new path for the data files
      strPath := strData;
    end
    else if ( strKey = 'skeleton' ) then
    begin
      // load core skeleton
      Log.LogStatus( 'Loading skeleton ''' + strData + '''...', '' );
      m_calCoreModel.loadCoreSkeleton( strPath + strData );
    end
    else if ( strKey = 'animation' ) then
    begin
      // load core animation
      Log.LogStatus( 'Loading animation ''' + strData + '''...', '' );
      m_animationId[ animationCount ] := m_calCoreModel.loadCoreAnimation( strPath + strData );
      inc( animationCount );
    end
    else if ( strKey = 'mesh' ) then
    begin
      // load core mesh
      Log.LogStatus( 'Loading mesh ''' + strData + '''...', '' );
      m_calCoreModel.loadCoreMesh( strPath + strData );
    end
    else if ( strKey = 'material' ) then
    begin
      // load core material
      Log.LogStatus( 'Loading material ''' + strData + '''...', '' );
      m_calCoreModel.loadCoreMaterial( strPath + strData );
    end
    else
    begin
      Log.LogStatus( strFilename + '( ' + strKey + ' ): Invalid syntax.', '' );
      result := false;
      exit;
    end;
  end;

  // explicitely close the file
  closefile( ifstream );

  // load all textures and store the opengl texture id in the corresponding map in the material
  MaterialCount := m_calCoreModel.getCoreMaterialCount;
  for materialId := 0 to MaterialCount - 1 do
  begin
    // get the core material
    CoreMaterial := m_calCoreModel.getCoreMaterial( materialId );

    // loop through all maps of the core material
    MapCount := CoreMaterial.getMapCount;
    for mapId := 0 to MapCount - 1 do
    begin
      // get the filename of the texture
      s := CoreMaterial.getMapFileName( mapId );

      // load the texture from the file
      textureId := loadTexture( strPath + s );

      // store the opengl texture id in the user data of the map
      CoreMaterial.setMapUserData( mapId, TextureId );
    end;
  end;

  // make one material thread for each material
  // NOTE: this is not the right way to do it, but this viewer can't do the right
  // mapping without further information on the model etc.
  MaterialCount := m_calCoreModel.getCoreMaterialCount;
  for materialId := 0 to MaterialCount - 1 do
  begin
    // create the a material thread
    m_calCoreModel.createCoreMaterialThread( materialId );

    // initialize the material thread
    m_calCoreModel.setCoreMaterialId( materialId, 0, materialId );
  end;

  // Calculate Bounding Boxes
  m_calCoreModel.CoreSkeleton.calculateBoundingBoxes( m_calCoreModel );

  // create the model instance from the loaded core model
  m_CalModel := CalModel.Create( m_CalCoreModel );

  // attach all meshes to the model
  MeshCount := m_calCoreModel.getCoreMeshCount;
  for meshId := 0 to MeshCount - 1 do
  begin
    m_calModel.attachMesh( meshId );
  end;

  // set the material set of the whole model
  m_calModel.setMaterialSet( 0 );

  // set initial animation state
  m_state := STATE_MOTION;
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION ], m_motionBlend[ 0 ], 0.0 );
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 1 ], m_motionBlend[ 1 ], 0.0 );
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 2 ], m_motionBlend[ 2 ], 0.0 );

  result := true;
end;

procedure TModel.SetState( state : integer; delay : single );
begin
  // check if this is really a new state
  if ( state <> m_state ) then
  begin
    if ( state = STATE_IDLE ) then
    begin
      m_calModel.getMixer.blendCycle( m_animationId[ STATE_IDLE ], 1.0, delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_FANCY ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION + 1 ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION + 2 ], delay );
      m_state := STATE_IDLE;
    end
    else if ( state = STATE_FANCY ) then
    begin
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_IDLE ], delay );
      m_calModel.getMixer.blendCycle( m_animationId[ STATE_FANCY ], 1.0, delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION + 1 ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_MOTION + 2 ], delay );
      m_state := STATE_FANCY;
    end
    else if ( state = STATE_MOTION ) then
    begin
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_IDLE ], delay );
      m_calModel.getMixer.clearCycle( m_animationId[ STATE_FANCY ], delay );
      m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION ], m_motionBlend[ 0 ], delay );
      m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 1 ], m_motionBlend[ 1 ], delay );
      m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 2 ], m_motionBlend[ 2 ], delay );
      m_state := STATE_MOTION;
    end;
  end;
end;

procedure TModel.executeAction( action : integer );
begin
  case action of
    0 : m_calModel.getMixer.executeAction( m_animationId[ 5 ], 0.3, 0.3 );
    1 : m_calModel.getMixer.executeAction( m_animationId[ 6 ], 0.3, 0.3 );
  end;
end;

procedure TModel.setLodLevel( value : single );
begin
  m_lodLevel := value;

  // set the new lod level in the cal model renderer
  m_calModel.setLodLevel( m_lodLevel );
end;

procedure TModel.setMotionBlend( MotionBlend : array of single; delay : single );
begin
  m_motionBlend[ 0 ] := MotionBlend[ 0 ];
  m_motionBlend[ 1 ] := MotionBlend[ 1 ];
  m_motionBlend[ 2 ] := MotionBlend[ 2 ];

  m_calModel.getMixer.clearCycle( m_animationId[ STATE_IDLE ], delay );
  m_calModel.getMixer.clearCycle( m_animationId[ STATE_FANCY ], delay );
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION ], m_motionBlend[ 0 ], delay );
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 1 ], m_motionBlend[ 1 ], delay );
  m_calModel.getMixer.blendCycle( m_animationId[ STATE_MOTION + 2 ], m_motionBlend[ 2 ], delay );

  m_state := STATE_MOTION;
end;

procedure TModel.getMotionBlend( var MotionBlend : array of single );
begin
  motionBlend[ 0 ] := m_MotionBlend[ 0 ];
  motionBlend[ 1 ] := m_MotionBlend[ 1 ];
  motionBlend[ 2 ] := m_MotionBlend[ 2 ];
end;

function TModel.loadTexture( strFilename : string ) : integer;
var
  stream : TFileStream;
  width, height, depth : integer;
  Buffer : pchar;
  y : integer;
  ext : string;
  Picture : PSDL_Surface;
begin
  result := -1;
  ext := ExtractFileExt( strFilename );
  if ( ext = '.raw' ) then
  begin
     // open the texture file
    stream := TFileStream.Create( strFileName, 0 );

    // load the dimension of the texture
    stream.ReadBuffer( width, sizeof( width ) );
    stream.ReadBuffer( height, sizeof( height ) );
    stream.ReadBuffer( depth, sizeof( depth ) );

    // allocate a temporary buffer to load the texture to
    getmem( Buffer, 2 * width * height * depth );

    // load the texture
    stream.ReadBuffer( Buffer^, width * height * depth );

    // explicitely close the file
    stream.Free;

    // flip texture around y-axis (-> opengl-style)
    for y := 0 to height - 1 do
    begin
      move( Buffer[ ( height - y - 1 ) * width * depth ], Buffer[ ( height + y ) * width * depth ], width * depth );
    end;

    // generate texture
    glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
    glGenTextures( 1, @Result );
    glBindTexture( GL_TEXTURE_2D, Result );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    if depth = 3 then
      glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, @Buffer[ width * height * depth ] )
    else
      glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Buffer[ width * height * depth ] );
    // free the allocated memory
    FreeMem( buffer );
  end
  else
  begin
    Picture := IMG_Load( PChar( strFilename ) );
    if Picture <> nil then
    begin
      // generate texture
      glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
      glGenTextures( 1, @Result );
      glBindTexture( GL_TEXTURE_2D, Result );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
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
    end;
  end;
end;

procedure DrawBoneLine( x1, y1, z1, x2, y2, z2 : single );
begin
  glVertex3f( x1, y1, z1 );
  glVertex3f( x2, y2, z2 );
end;

procedure DrawBonePoint( x, y, z : single );
begin
  glVertex3f( x, y, z );
end;

procedure TModel.renderSkeleton;
begin
  // draw the bone lines
  glLineWidth( 3.0 );
  glColor3f( 1.0, 1.0, 1.0 );
  glBegin( GL_LINES );
  m_calModel.getSkeleton.ForEachBoneLine( DrawBoneLine );
  glEnd;
  glLineWidth( 1.0 );

  // draw the bone points
  glPointSize( 4.0 );
  glBegin( GL_POINTS );
  glColor3f( 0.0, 0.0, 1.0 );

  m_calModel.getSkeleton.ForEachBonePoint( DrawBonePoint );
  glEnd;
  glPointSize( 1.0 );
end;


procedure TModel.renderMesh( Wireframe, Light : boolean );
var
  pCalRenderer : CalRenderer;
  meshCount : integer;
  submeshCount : integer;
  meshColor : array[ 0..3 ] of byte;
  materialColor : array[ 0..3 ] of float;
  shininess : float;
//  vertexCount:integer;
  textureCoordinateCount : integer;
  faceCount : integer;
  intsize : integer;
  meshId : integer;
  submeshId : integer;
  meshVertices : TVertices; //:array[0..30000,0..2] of float;
  meshNormals : TNormals; //array[0..30000,0..2] of float;
  meshFaces : vectorFaces; //array[0..50000,0..2] of integer;
  meshTextureCoordinates : TextureCoordinates; //array[0..30000,0..1] of float;
begin
  meshVertices := nil;
  meshNormals := nil;
  meshFaces := nil;
  meshTextureCoordinates := nil;

  // get the renderer of the model
  pCalRenderer := m_calModel.getRenderer;

  // begin the rendering loop
  if ( not pCalRenderer.beginRendering ) then
    exit;

  // set wireframe mode if necessary
  if ( Wireframe ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );

  // set the global OpenGL states
  glEnable( GL_DEPTH_TEST );
  glShadeModel( GL_SMOOTH );

  // set the lighting mode if necessary
  if ( Light ) then
  begin
    glEnable( GL_LIGHTING );
    glEnable( GL_LIGHT0 );
  end;

  // we will use vertex arrays, so enable them
  glEnableClientState( GL_VERTEX_ARRAY );
  glEnableClientState( GL_NORMAL_ARRAY );

  // get the number of meshes
  meshCount := pCalRenderer.getMeshCount;

  // render all meshes of the model
  for meshId := 0 to meshCount - 1 do
  begin

    // get the number of submeshes
    submeshCount := pCalRenderer.getSubmeshCount( meshId );

    // render all submeshes of the mesh
    for submeshId := 0 to submeshCount - 1 do
    begin
      // select mesh and submesh for further data access
      if pCalRenderer.selectMeshSubmesh( meshId, submeshId ) then
      begin
        // set the material ambient color
        pCalRenderer.getAmbientColor( meshColor[ 0 ] );
        materialColor[ 0 ] := meshColor[ 0 ] / 255.0;
        materialColor[ 1 ] := meshColor[ 1 ] / 255.0;
        materialColor[ 2 ] := meshColor[ 2 ] / 255.0;
        materialColor[ 3 ] := meshColor[ 3 ] / 255.0;
        glMaterialfv( GL_FRONT, GL_AMBIENT, @materialColor[ 0 ] );

        // set the material diffuse color
        pCalRenderer.getDiffuseColor( meshColor[ 0 ] );
        materialColor[ 0 ] := meshColor[ 0 ] / 255.0;
        materialColor[ 1 ] := meshColor[ 1 ] / 255.0;
        materialColor[ 2 ] := meshColor[ 2 ] / 255.0;
        materialColor[ 3 ] := meshColor[ 3 ] / 255.0;
        glMaterialfv( GL_FRONT, GL_DIFFUSE, @materialColor[ 0 ] );

        // set the vertex color if we have no lights
        if ( not Light ) then
          glColor4fv( @materialColor );

        // set the material specular color
        pCalRenderer.getSpecularColor( meshColor[ 0 ] );
        materialColor[ 0 ] := meshColor[ 0 ] / 255.0;
        materialColor[ 1 ] := meshColor[ 1 ] / 255.0;
        materialColor[ 2 ] := meshColor[ 2 ] / 255.0;
        materialColor[ 3 ] := meshColor[ 3 ] / 255.0;
        glMaterialfv( GL_FRONT, GL_SPECULAR, @materialColor[ 0 ] );

        // set the material shininess factor
        shininess := pCalRenderer.getShininess;
        glMaterialfv( GL_FRONT, GL_SHININESS, @shininess );

        // get the transformed vertices of the submesh
        meshVertices := pCalRenderer.getVertices;

        // get the transformed normals of the submesh
        meshNormals := pCalRenderer.getNormals;

        // get the texture coordinates of the submesh
        meshTextureCoordinates := pCalRenderer.getTextureCoordinates( 0 );
        textureCoordinateCount := length( meshTextureCoordinates );

        // get the faces of the submesh
        meshFaces := pCalRenderer.getFaces;
        faceCount := length( meshFaces );

        // set the vertex and normal buffers
        glVertexPointer( 3, GL_FLOAT, 0, @meshVertices[ 0 ] );
        glNormalPointer( GL_FLOAT, 0, @meshNormals[ 0 ] );

        // set the texture coordinate buffer and state if necessary
        if ( ( pCalRenderer.getMapCount > 0 ) and ( textureCoordinateCount > 0 ) ) then
        begin
          glEnable( GL_TEXTURE_2D );
          glEnableClientState( GL_TEXTURE_COORD_ARRAY );
          glEnable( GL_COLOR_MATERIAL );

          // set the texture id we stored in the map user data
          glBindTexture( GL_TEXTURE_2D, pCalRenderer.getMapUserData( 0 ) );

          // set the texture coordinate buffer
          glTexCoordPointer( 2, GL_FLOAT, 0, @meshTextureCoordinates[ 0 ] );
          glColor3f( 1.0, 1.0, 1.0 );
        end;

        // draw the submesh
        intsize := SizeOf( Integer );
        if ( intsize = 2 ) then
          glDrawElements( GL_TRIANGLES, faceCount * 3, GL_UNSIGNED_SHORT, @meshFaces[ 0 ] )
        else
          glDrawElements( GL_TRIANGLES, faceCount * 3, GL_UNSIGNED_INT, @meshFaces[ 0 ] );

        // disable the texture coordinate state if necessary
        if ( ( pCalRenderer.getMapCount > 0 ) and ( textureCoordinateCount > 0 ) ) then
        begin
          glDisable( GL_COLOR_MATERIAL );
          glDisableClientState( GL_TEXTURE_COORD_ARRAY );
          glDisable( GL_TEXTURE_2D );
        end;

      end;
    end;
  end;

  meshVertices := nil;
  meshNormals := nil;
  meshFaces := nil;
  meshTextureCoordinates := nil;

  // clear vertex array state
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_VERTEX_ARRAY );

  // reset the lighting mode
  if ( Light ) then
  begin
    glDisable( GL_LIGHTING );
    glDisable( GL_LIGHT0 );
  end;

  // reset the global OpenGL states
  glDisable( GL_DEPTH_TEST );

  // reset wireframe mode if necessary
  if ( Wireframe ) then
  begin
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
  end;

  // end the rendering
  pCalRenderer.endRendering;
end;

procedure TModel.renderBoundingBox;
var
  boneId : integer;
  CoreBoneSize : integer;
  p : TVertices;
  BoundingBox : CalBoundingBox;
  pCalSkeleton : CalSkeleton;
  vectorCoreBone : CalBones;
begin
  SetLength( p, 8 );
  pCalSkeleton := m_calModel.getSkeleton;

  // Note :
  // You have to call coreSkeleton.calculateBoundingBoxes(calCoreModel)
  // during the initialisation (before calModel.create(calCoreModel))
  // if you want to use bounding boxes.
  pCalSkeleton.calculateBoundingBoxes;

  vectorCoreBone := pCalSkeleton.getVectorBone;

  glColor3f( 1.0, 1.0, 1.0 );
  glBegin( GL_LINES );

  CoreBoneSize := Length( vectorCoreBone );
  for boneId := 0 to CoreBoneSize - 1 do
  begin
    BoundingBox := vectorCoreBone[ boneId ].getBoundingBox;

    BoundingBox.computePoints( p );


    glVertex3f( p[ 0 ].x, p[ 0 ].y, p[ 0 ].z );
    glVertex3f( p[ 1 ].x, p[ 1 ].y, p[ 1 ].z );

    glVertex3f( p[ 0 ].x, p[ 0 ].y, p[ 0 ].z );
    glVertex3f( p[ 2 ].x, p[ 2 ].y, p[ 2 ].z );

    glVertex3f( p[ 1 ].x, p[ 1 ].y, p[ 1 ].z );
    glVertex3f( p[ 3 ].x, p[ 3 ].y, p[ 3 ].z );

    glVertex3f( p[ 2 ].x, p[ 2 ].y, p[ 2 ].z );
    glVertex3f( p[ 3 ].x, p[ 3 ].y, p[ 3 ].z );

    glVertex3f( p[ 4 ].x, p[ 4 ].y, p[ 4 ].z );
    glVertex3f( p[ 5 ].x, p[ 5 ].y, p[ 5 ].z );

    glVertex3f( p[ 4 ].x, p[ 4 ].y, p[ 4 ].z );
    glVertex3f( p[ 6 ].x, p[ 6 ].y, p[ 6 ].z );

    glVertex3f( p[ 5 ].x, p[ 5 ].y, p[ 5 ].z );
    glVertex3f( p[ 7 ].x, p[ 7 ].y, p[ 7 ].z );

    glVertex3f( p[ 6 ].x, p[ 6 ].y, p[ 6 ].z );
    glVertex3f( p[ 7 ].x, p[ 7 ].y, p[ 7 ].z );

    glVertex3f( p[ 0 ].x, p[ 0 ].y, p[ 0 ].z );
    glVertex3f( p[ 4 ].x, p[ 4 ].y, p[ 4 ].z );

    glVertex3f( p[ 1 ].x, p[ 1 ].y, p[ 1 ].z );
    glVertex3f( p[ 5 ].x, p[ 5 ].y, p[ 5 ].z );

    glVertex3f( p[ 2 ].x, p[ 2 ].y, p[ 2 ].z );
    glVertex3f( p[ 6 ].x, p[ 6 ].y, p[ 6 ].z );

    glVertex3f( p[ 3 ].x, p[ 3 ].y, p[ 3 ].z );
    glVertex3f( p[ 7 ].x, p[ 7 ].y, p[ 7 ].z );
  end;
  SetLength( p, 0 );
  glEnd;
end;

destructor TModel.Destroy;
begin

  inherited;
end;

procedure TModel.onShutdown;
begin
  // destroy the model instance
  m_calModel.Free;

  // destroy the core model instance
  m_calCoreModel.Free;
end;

end.

