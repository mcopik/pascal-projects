unit dcal3d;
{
  $Id: dcal3d.pas,v 1.1 2005/01/03 19:29:07 savage Exp $

}
{******************************************************************************}
{                                                                              }
{       Borland Delphi Cal3D - skeletal based 3d character animation library   }
{       Conversion of the Cal3D Headers                                        }
{                                                                              }
{ Portions created by Bruno 'Beosil' Heidelberger  are                         }
{ Copyright (C) 2002 - 2003  Bruno 'Beosil' Heidelberger                       }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{Paul TOTH <tothpaul@free.fr>                            }
{                                                                              }
{ Portions created by Paul TOTH are                                      }
{ Copyright (C) 2001 - 2005 Paul TOTH.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au> }
{ Cal3D-0.6 (4. Aug. 2001) (C) 2001 Bruno 'Beosil' Heidelberger}
{                                                                              }
{ Obtained through:                                                            }
{ ----------------                                                             }
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
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The Cal3D Runtime libraris on Win32  : cal3d.dll on Linux : libcal3d.so    }
{   They are available from...                                                 }
{   http://cal3d.sourceforge.net.                                                       }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{ 24 November 2001, first lines of code :)             }
{ 10 December 2001, first beta release - animation scramble at end of cycle }
{ 12 December 2001, first release - less C style code, more Delphi style code }
{                   fix Vector & Quaternion assignments. }
{ 2004, Ported JEDI-SDL and updated to v0.09 }
(*
  $Log: dcal3d.pas,v $
  Revision 1.1  2005/01/03 19:29:07  savage
  Initial Cal3D v1.0 checkin.



*)

(* Translation notes :
  -------------------

  - Delphi classes are pointers :
  ----------- Delphi --------------+---------- C -------------------------------
  var                              | {
   MyInstance:TMyClass;            |   TMyClass *MyInstance;
  begin                            |
   MyInstance:=TMyClass.Create;    |   MyInstance=new TMyClass;
  ------------------------------------------+-----------------------------------
  - Delphi use a "." for classes fields, not "->"

  - Static classes aren't available under Delphi.
  ----------- C -------------------+---------- Delphi --------------------------
  {                                |
   TMyClass MyClasse;              |  need the same code as above !
                                   |
                                   |
  ---------------------------------+--------------------------------------------

  - Function returned value is called "Result", it can be read and write

  - There's no "return" keyword, use "Result:=Value; exit;"

  - C "FOR", look like a While for Delphi, Delphi "FOR" have no exit condition.
  -----------C---------------------------+--------Delphi------------------------
   for (i=0; i<10; i++) {                | for i:=0 to 9 do begin
   }                                     | end;
                                         |
   for (i=0; (i<10)&&(s[i]); i++, j++) { | i:=0; while (i<10)and(s[i]<>0) do begin
   }                                     | inc(i); inc(j); end;
                                         |
   for(; childCount > 0; childCount--) { | while ChildCount>0 do begin
   }                                     | dec(ChildCount); end;
  ------------------------------------------------------------------------------

  - null pointer is "nil"; pointer isn't integer, can't assign 0 to a pointer !

  - float => single

  - Delphi "Property" can replace get/set method :
  -----------C---------------------------+--------Delphi------------------------
   class MyClass {                       | MyClass=class
    private:                             |  private
     int m_Field;                        |   m_Field:integer;
    public:                              |  public
     int getField();                     |   property Field:integer read m_Field write m_Field;
     void setField(int value);           | end;
   }                                     | //you can also use get/set method !
                                         | //property Field:integer read getField write setField;
  ------------------------------------------------------------------------------
  - I've replaced :
   * vector class by Delphi dynamic arrays : reserve -> SetLength()
   * list by TList
   * map by dynamic arrays of record...
  ------------------------------------------------------------------------------

  Fix #1, Delphi need an Assign() method to copy class fields
  -----------C---------------------------+--------Delphi------------------------
   CalVector x(1,2,3);                   | x:=CalVector.Create(1,2,3);
   Calvector y=x;                        | y:=CalVector.Create(0,0,0);
                                         | y.Assign(x);

*)
interface

uses
  Classes,
  libxmlparser; // TList

const
  fmOpenRead = $0000;
  fmOpenWrite = $0001;
  fmOpenReadWrite = $0002;

  fmShareCompat = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead = $0030;

  fmShareDenyNone = $0040;

type
  float = single;

function fmod( a, b : float ) : float;

// global.h

// global typedefs
type
  CalUserData = integer;

const
// file magic cookies
  SKELETON_FILE_MAGIC : array[ 0..3 ] of char = 'CSF'#0;
  ANIMATION_FILE_MAGIC : array[ 0..3 ] of char = 'CAF'#0;
  MESH_FILE_MAGIC : array[ 0..3 ] of char = 'CMF'#0;
  MATERIAL_FILE_MAGIC : array[ 0..3 ] of char = 'CRF'#0;

  SKELETON_XMLFILE_MAGIC : array[ 0..3 ] of char = 'XSF'#0;
  ANIMATION_XMLFILE_MAGIC : array[ 0..3 ] of char = 'XAF'#0;
  MESH_XMLFILE_MAGIC : array[ 0..3 ] of char = 'XMF'#0;
  MATERIAL_XMLFILE_MAGIC : array[ 0..3 ] of char = 'XRF'#0;

// library version
  LIBRARY_VERSION = 910;

// file versions
  CURRENT_FILE_VERSION : integer = LIBRARY_VERSION;
  EARLIEST_COMPATIBLE_FILE_VERSION = 600;

type
// forwards
  CalAnimation = class;
  CalAnimationAction = class;
  CalAnimationCycle = class;
  CalBone = class;
  CalCoreAnimation = class;
  CalCoreBone = class;
  CalCoreKeyframe = class;
  CalCoreMaterial = class;
  CalCoreMesh = class;
  CalCoreModel = class;
  CalCoreSkeleton = class;
  CalCoreSubmesh = class;
  CalCoreTrack = class;
  CalError = class;
  CalCoreSubMorphTarget = class;
  CalMorphTargetMixer = class;
  CalCoreMorphAnimation = class;
//----------------- CalLoader=class;
  CalMesh = class;
  CalMixer = class;
  CalModel = class;
  CalQuaternion = class;
  CalRenderer = class;
//----------------- CalSaver=class;
  CalSkeleton = class;
  CalSubmesh = class;
  CalVector = class;
  CalPlane = class;
  CalMatrix = class;
  CalBoundingBox = class;
  CalPhysique = class;
  CalSpringSystem = class;

// vector
  CalAnimations = array of CalAnimation;
  CalBones = array of CalBone;
  CalCoreAnimations = array of CalCoreAnimation;
  CalCoreBones = array of CalCoreBone;
  CalCoreKeyframes = array of CalCoreKeyframe;
  CalCoreMaterials = array of CalCoreMaterial;
  CalCoreTracks = array of CalCoreTrack;
  CalCoreMeshes = array of CalCoreMesh;
  CalCoreSubmeshes = array of CalCoreSubmesh;
  CalMeshes = array of CalMesh;
  CalSubmeshes = array of CalSubmesh;
  CalCoreSubMorphTargets = array of CalCoreSubMorphTarget;
  CalCoreMorphAnimations = array of CalCoreMorphAnimation;
  CalVectors = array of CalVector;

  Integers = array of integer;
  Floats = array of float;

  TVertex = record
    x, y, z : float;
  end;
  TVertices = array of TVertex;

  TNormal = record
    x, y, z : float;
  end;
  TNormals = array of TNormal;

// .h
{$I animation.int}
{$I animation_action.int}
{$I animation_cycle.int}
{$I bone.int}
{$I coreanimation.int}
{$I corebone.int}
{$I corekeyframe.int}
{$I corematerial.int}
{$I coremesh.int}
{$I coremodel.int}
{$I coreskeleton.int}
{$I coresubmesh.int}
{$I coretrack.int}
{$I error.int}
{$I mesh.int}
{$I mixer.int}
{$I model.int}
{$I quaternion.int}
{$I renderer.int}
{$I skeleton.int}
{$I submesh.int}
{$I vector.int}
{$I matrix.int}
{$I physique.int}
{$I coresubmorphtarget.int}
{$I coremorphanimation.int}
{$I morphtargetmixer.int}
{$I springsystem.int}

implementation

// some C function unavailable under Delphi

uses
  Math,
  SysUtils,
  logger; // ArcCos()

function fmod( a, b : float ) : float;
begin
  result := a - ( b * int( a / b ) );
end;

function CalOpen( AFilename : string; magic : integer ) : TFileStream;
var
  head : record
    magic : integer;
    version : integer;
  end;
begin
  Result := TFileStream.create( AFilename, fmOpenRead {or fmShareDenyNone} );
  Result.ReadBuffer( head, sizeof( head ) );
  if head.magic <> magic then
  begin
    CalError.setLastError( INVALID_BINARY_FILE_FORMAT, 'DCal3D.PAS', 262, AFilename );
    Result.Free;
    Result := nil;
  end
  else if ( head.version < EARLIEST_COMPATIBLE_FILE_VERSION ) or ( head.version > CURRENT_FILE_VERSION ) then
  begin
    CalError.setLastError( INCOMPATIBLE_FILE_VERSION, 'DCal3D.PAS', 268, AFilename );
    Result.Free;
    Result := nil;
  end;
end;

// .c
{$I animation.imp}
{$I animation_action.imp}
{$I animation_cycle.imp}
{$I bone.imp}
{$I coreanimation.imp}
{$I corebone.imp}
{$I corekeyframe.imp}
{$I corematerial.imp}
{$I coremesh.imp}
{$I coremodel.imp}
{$I coreskeleton.imp}
{$I coresubmesh.imp}
{$I coretrack.imp}
{$I error.imp}
{$I mesh.imp}
{$I mixer.imp}
{$I model.imp}
{$I quaternion.imp}
{$I renderer.imp}
{$I skeleton.imp}
{$I submesh.imp}
{$I vector.imp}
{$I matrix.imp}
{$I physique.imp}
{$I coresubmorphtarget.imp}
{$I coremorphanimation.imp}
{$I morphtargetmixer.imp}
{$I springsystem.imp}

initialization
  DecimalSeparator := '.';

end.

