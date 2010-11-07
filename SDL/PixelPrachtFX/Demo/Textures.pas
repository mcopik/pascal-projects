//----------------------------------------------------------------------------
//
// Author      : Jan Horn
// Email       : jhorn@global.co.za
// Website     : http://www.sulaco.co.za
//               http://home.global.co.za/~jhorn
// Version     : 1.03
// Date        : 1 May 2001
// Changes     : 28 July   2001 - Faster BGR to RGB swapping routine
//               2 October 2001 - Added support for 24, 32bit TGA files
//               28 April  2002 - Added support for compressed TGA files
//
// Description : A unit that used with OpenGL projects to load BMP, JPG and TGA
//               files from the disk or a resource file.
// Usage       : LoadTexture(Filename, TextureName, LoadFromResource);
//
//               eg : LoadTexture('logo.jpg', LogoTex, TRUE);
//                    will load a JPG texture from the resource included
//                    with the EXE. File has to be loaded into the Resource
//                    using this format  "logo JPEG logo.jpg"
//
// Changed by Lithander 27.08.02
//----------------------------------------------------------------------------
unit Textures;

interface

uses
  gl,glu, sdl, sdl_image;

function LoadTexture(Filename: String; var Texture: GLuint): Boolean; overload;

implementation

{------------------------------------------------------------------}
{  Swap bitmap format from BGR to RGB                              }
{------------------------------------------------------------------}
procedure SwapRGB(data : Pointer; Size : Integer);
asm
  mov ebx, eax
  mov ecx, size

@@loop :
  mov al,[ebx+0]
  mov ah,[ebx+2]
  mov [ebx+2],al
  mov [ebx+0],ah
  add ebx,3
  dec ecx
  jnz @@loop
end;


{------------------------------------------------------------------}
{  Create the Texture                                              }
{------------------------------------------------------------------}
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
var
  Texture : GLuint;
begin
  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
//  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);  {Texture does NOT blend with object background}

  { Select a filtering type. BiLinear filtering produces very good results with little performance impact
    GL_NEAREST               - Basic texture (grainy looking texture)
    GL_LINEAR                - BiLinear filtering
    GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
    GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
  }  

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
//  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL

  result :=Texture;
end;




{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}
function LoadTGATexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var s: PSDL_Surface;
   w, h: Word;
begin
  result :=FALSE;
  s := IMG_Load(PChar(Filename));
  w := s.w;
  h := s.h;
  if s.format.BitsPerPixel = 24 then
  begin
    Texture :=CreateTexture(w, h, GL_RGB, s.pixels);
    Result := True;
  end;
  if s.format.BitsPerPixel = 32 then
  begin
    Texture :=CreateTexture(w, h, GL_RGBA, s.pixels);
    Result := True;
  end;
end;



function LoadTexture(Filename: String; var Texture : GLuint) : Boolean;
begin
   LoadTGATexture(Filename, Texture, FALSE);
end;

end.