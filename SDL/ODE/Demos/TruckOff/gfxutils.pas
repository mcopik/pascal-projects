unit gfxutils;
{******************************************************************}
{                                                                  }
{  $Id: gfxutils.pas,v 1.1 2004/03/08 21:29:15 savage Exp $                                      }
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the Ed Jones' ODE TruckOff Demo              }
{                                                                  }
{ Portions created by Ed Jones <ed.jones@oracle.com>,  are         }
{ Copyright(C) 2002-2004 Ed Jones.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : gfxutils.pas                       }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright(C) 2002-2004 Dominique Louis.                          }
{                                                                  }
{  $Log: gfxutils.pas,v $
{  Revision 1.1  2004/03/08 21:29:15  savage
{  ODE TruckOff Demo
{                                     }
{                                                                  }
{******************************************************************}

interface

uses
  gl,
  glu,
  odeimport;

const
  PIE_SLICES = 25; // How smooth to make round things

procedure guDrawCylinder( var qobj : GLUQuadricObj; clength, radius : GLFloat );
procedure guDrawBox( const sides : TdVector3 );
procedure guSetColor( r, g, b : GLFloat );
procedure guSetTransform( const pos : PdVector3; const R : PdMatrix3 );
procedure guUnsetTransform;

implementation

procedure guSetTransform( const pos : PdVector3; const R : PdMatrix3 );
var
  matrix : array[ 0..15 ] of GLFloat;
begin
  matrix[ 0 ] := R[ 0 ];
  matrix[ 1 ] := R[ 4 ];
  matrix[ 2 ] := R[ 8 ];
  matrix[ 3 ] := 0;

  matrix[ 4 ] := R[ 1 ];
  matrix[ 5 ] := R[ 5 ];
  matrix[ 6 ] := R[ 9 ];
  matrix[ 7 ] := 0;

  matrix[ 8 ] := R[ 2 ];
  matrix[ 9 ] := R[ 6 ];
  matrix[ 10 ] := R[ 10 ];
  matrix[ 11 ] := 0;

  matrix[ 12 ] := pos[ 0 ];
  matrix[ 13 ] := pos[ 1 ];
  matrix[ 14 ] := pos[ 2 ];
  matrix[ 15 ] := 1;

  glPushMatrix;
  glMultMatrixf( @matrix[ 0 ] );
end;

procedure guUnsetTransform;
begin
  glPopMatrix;
end;

// Draw a cylinder of length l and radius r, aligned along the z axis

procedure guDrawCylinder( var qobj : GLUQuadricObj; clength, radius : GLFloat );
begin
  glPushMatrix;
  glTranslatef( 0, 0, -clength * 0.5 );
  gluCylinder( @qobj, radius, radius, clength, PIE_SLICES, 1 );
  glPopMatrix;

  glPushMatrix;
  glTranslatef( 0, 0, clength * 0.5 );
  gluDisk( @qobj, 0, radius, PIE_SLICES, 1 );
  glPopMatrix;

  glPushMatrix;
  glTranslatef( 0, 0, -clength * 0.5 );
  glRotatef( 180.0, 0, 1, 0 );
  gluDisk( @qobj, 0, radius, PIE_SLICES, 1 );
  glPopMatrix;
end;

procedure guDrawBox( const sides : TdVector3 );
var
  lx, ly, lz : GLFloat;
begin
  lx := sides[ 0 ] * 0.5;
  ly := sides[ 1 ] * 0.5;
  lz := sides[ 2 ] * 0.5;

  // sides top and bottom
  glBegin( GL_TRIANGLE_STRIP );
    glNormal3f( -1, 0, 0 );
    glVertex3f( -lx, -ly, -lz );
    glVertex3f( -lx, -ly, lz );
    glVertex3f( -lx, ly, -lz );
    glVertex3f( -lx, ly, lz );
    glNormal3f( 0, 1, 0 );
    glVertex3f( lx, ly, -lz );
    glVertex3f( lx, ly, lz );
    glNormal3f( 1, 0, 0 );
    glVertex3f( lx, -ly, -lz );
    glVertex3f( lx, -ly, lz );
    glNormal3f( 0, -1, 0 );
    glVertex3f( -lx, -ly, -lz );
    glVertex3f( -lx, -ly, lz );
  glEnd;

  // front face
  glBegin( GL_TRIANGLE_FAN );
    glNormal3f( 0, 0, 1 );
    glVertex3f( -lx, -ly, lz );
    glVertex3f( lx, -ly, lz );
    glVertex3f( lx, ly, lz );
    glVertex3f( -lx, ly, lz );
  glEnd;

  // back face
  glBegin( GL_TRIANGLE_FAN );
    glNormal3f( 0, 0, -1 );
    glVertex3f( -lx, -ly, -lz );
    glVertex3f( -lx, ly, -lz );
    glVertex3f( lx, ly, -lz );
    glVertex3f( lx, -ly, -lz );
  glEnd;
end;

//void guDrawCylinder(GLUquadricObj *qobj,const float pos[3],const float R[12],float length,float radius){

//	guSetTransform(pos,R);
//	drawCylinder(qobj,length,radius,0);
//	guUnsetTransform();
//}

procedure guSetColor( r, g, b : GLFloat );
begin
  glColor3f( r, g, b );
end;

end.

