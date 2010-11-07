unit cal3d;
{
  $Id: cal3d.pas,v 1.2 2007/05/20 20:21:07 savage Exp $

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
{ The original files are : cal3d_wrapper.h                                     }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2002 - 2003 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
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
{   December  20 2002 - DL : Initial Translation                               }
{                                                                              }
(*
  $Log: cal3d.pas,v $
  Revision 1.2  2007/05/20 20:21:07  savage
  Initial Changes to Handle 64 Bits

  Revision 1.1  2005/01/03 19:29:07  savage
  Initial Cal3D v1.0 checkin.

  Revision 1.3  2004/05/26 13:32:44  savage
  Tidying up.

  Revision 1.2  2004/05/11 12:21:10  savage
  Changes for FPC


*)
{******************************************************************************}

{$WEAKPACKAGEUNIT ON}

{$i jedi-sdl.inc}

interface

uses
{$IFDEF __GPC__}
  system,
  gpc;
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFNDEF __GPC__}
  Windows;
  {$ENDIF}
{$ENDIF}

{$IFDEF Unix}
  {$IFDEF FPC}
    {$IFDEF Ver1_0}
    linux,
    {$ELSE}
    pthreads,
    baseunix,
    unix,
    {$ENDIF}
    x,
    xlib;
  {$ELSE}
  Types,
  Libc,
  Xlib;
  {$ENDIF}
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll;
{$ENDIF}

const
{$IFDEF WINDOWS}
  Cal3DLibName = 'cal3d.dll';
{$ENDIF}
{$IFDEF LINUX}
  Cal3DLibName = 'libcal3d.so';
{$ENDIF}
{$IFDEF MACOS}
  Cal3DLibName = 'libcal3d';
{$ENDIF}
{$IFDEF DARWIN}
  Cal3DLibName = 'libcal3d.dylib';
{$ENDIF}

//****************************************************************************//
// Forward declarations                                                       //
//****************************************************************************//
type
  CalAnimation = record
  end;
  //PCalAnimation = ^CalAnimation;
  PCalAnimation = Pointer;

  CalAnimationAction = record
  end;
  //PCalAnimationAction = ^CalAnimationAction;
  PCalAnimationAction = Pointer;

  CalAnimationCycle = record
  end;
  //PCalAnimationCycle = ^CalAnimationCycle;
  PCalAnimationCycle = Pointer;

  CalBone = record
  end;
  //PCalBone = ^CalBone;
  PCalBone = Pointer;

  CalCoreAnimation = record
  end;
  //PCalCoreAnimation = ^CalCoreAnimation;
  PCalCoreAnimation = Pointer;

  CalCoreBone = record
  end;
  //PCalCoreBone = ^CalCoreBone;
  PCalCoreBone = Pointer;

  CalCoreKeyframe = record
  end;
  //PCalCoreKeyframe = ^CalCoreKeyframe;
  PCalCoreKeyframe = Pointer;

  CalCoreMaterial = record
  end;
  //PCalCoreMaterial = ^CalCoreMaterial;
  PCalCoreMaterial = Pointer;

  CalCoreMesh = record
  end;
  //PCalCoreMesh = ^CalCoreMesh;
  PCalCoreMesh = Pointer;

  CalCoreModel = record
  end;
  //PCalCoreModel = ^CalCoreModel;
  PCalCoreModel = Pointer;

  CalCoreSkeleton = record
  end;
  //PCalCoreSkeleton = ^CalCoreSkeleton;
  PCalCoreSkeleton = Pointer;

  CalCoreSubmesh = record
  end;
  //PCalCoreSubmesh = ^CalCoreSubmesh;
  PCalCoreSubmesh = Pointer;

  CalCoreTrack = record
  end;
  //PCalCoreTrack = ^CalCoreTrack;
  PCalCoreTrack = Pointer;

  CalIndex = record
  end;
  //PCalIndex = ^CalIndex;
  PCalIndex = Pointer;

  CalLoader = record
  end;
  //PCalLoader = ^CalLoader;
  PCalLoader = Pointer;

  CalMatrix = record
  end;
  //PCalMatrix = ^CalMatrix;
  PCalMatrix = Pointer;

  CalMesh = record
  end;
  //PCalMesh = ^CalMesh;
  PCalMesh = Pointer;

  CalMixer = record
  end;
  //PCalMixer = ^CalMixer;
  PCalMixer = Pointer;

  CalModel = record
  end;
  //PCalModel = ^CalModel;
  PCalModel = Pointer;

  CalPhysique = record
  end;
  //PCalPhysique = ^CalPhysique;
  PCalPhysique = Pointer;

  CalPlatform = record
  end;
  //PCalPlatform = ^CalPlatform;
  PCalPlatform = Pointer;

  CalQuaternion = record
  end;
  //PCalQuaternion = ^CalQuaternion;
  PCalQuaternion = Pointer;

  CalRenderer = record
  end;
  //PCalRenderer = ^CalRenderer;
  PCalRenderer = Pointer;

  CalSaver = record
  end;
  //PCalSaver = ^CalSaver;
  PCalSaver = Pointer;

  CalSkeleton = record
  end;
  //PCalSkeleton = ^CalSkeleton;
  PCalSkeleton = Pointer;

  CalSpringSystem = record
  end;
  //PCalSpringSystem = ^CalSpringSystem;
  PCalSpringSystem = Pointer;

  CalSubmesh = record
  end;
  //PCalSubmesh = ^CalSubmesh;
  PCalSubmesh = Pointer;

  CalVector = record
  end;
  //PCalVector = ^CalVector;
  PCalVector = Pointer;

  CalUserData = Pointer;

//****************************************************************************//
// Substitute for the C++ 'bool' type                                         //
//****************************************************************************//

  {BOOL = (
    False = 0,
    True = 1
  );}



//****************************************************************************//
// CalAnimation wrapper functions declaration                                 //
//****************************************************************************//

  CalAnimationType = (
    ANIMATION_TYPE_NONE = 0,
    ANIMATION_TYPE_CYCLE,
    ANIMATION_TYPE_POSE,
    ANIMATION_TYPE_ACTION
  );

  CalAnimationState = (
    ANIMATION_STATE_NONE = 0,
    ANIMATION_STATE_SYNC,
    ANIMATION_STATE_ASYNC,
    ANIMATION_STATE_IN,
    ANIMATION_STATE_STEADY,
    ANIMATION_STATE_OUT
  );

function CalAnimation_Create( self : PCalAnimation; pCoreAnimation : PCalCoreAnimation ) : BOOL; cdecl; external Cal3DLibName;
{$EXTERNALSYM CalAnimation_Create}

procedure CalAnimation_Delete( self : PCalAnimation ); cdecl; external Cal3DLibName;
procedure CalAnimation_Destroy( self : PCalAnimation ); cdecl; external Cal3DLibName;
function CalAnimation_GetCoreAnimation( self : PCalAnimation) : PCalCoreAnimation; cdecl; external Cal3DLibName;
function CalAnimation_GetState( self : PCalAnimation ) : CalAnimationState; cdecl; external Cal3DLibName;
function CalAnimation_GetTime( self : PCalAnimation ) : single; cdecl; external Cal3DLibName;
function CalAnimation_GetType( self : PCalAnimation ) : CalAnimationType; cdecl; external Cal3DLibName;
function CalAnimation_GetWeight( self : PCalAnimation ) : single; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalAnimationAction wrapper functions declaration                           //
//****************************************************************************//

function CalAnimationAction_Create( self : PCalAnimationAction; pCoreAnimation : PCalCoreAnimation ) : BOOL; cdecl; external Cal3DLibName;
procedure CalAnimationAction_Delete( self : PCalAnimationAction ); cdecl; external Cal3DLibName;
procedure CalAnimationAction_Destroy( self : PCalAnimationAction ); cdecl; external Cal3DLibName;
function CalAnimationAction_Execute( self : PCalAnimationAction; delayIn : single; delayOut : single ) : BOOL; cdecl; external Cal3DLibName;
function CalAnimationAction_New : PCalAnimationAction; cdecl; external Cal3DLibName;
function CalAnimationAction_Update( self : PCalAnimationAction; deltaTime : single ) : BOOL; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalAnimationCycle wrapper functions declaration                            //
//****************************************************************************//

function CalAnimationCycle_Blend( self : PCalAnimationCycle; weight : single; delay : single ) : BOOL; cdecl; external Cal3DLibName;
function CalAnimationCycle_Create( self : PCalAnimationCycle; pCoreAnimation : PCalCoreAnimation ): BOOL; cdecl; external Cal3DLibName;
procedure CalAnimationCycle_Delete( self : PCalAnimationCycle ); cdecl; external Cal3DLibName;
procedure CalAnimationCycle_Destroy( self : PCalAnimationCycle ); cdecl; external Cal3DLibName;
function CalAnimationCycle_New : PCalAnimationCycle; cdecl; external Cal3DLibName;
procedure CalAnimationCycle_SetAsync( self : PCalAnimationCycle; time : single; duration : single ); cdecl; external Cal3DLibName;
function CalAnimationCycle_Update( self : PCalAnimationCycle; deltaTime : single ) : BOOL; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalBone wrapper functions declaration                                      //
//****************************************************************************//

procedure CalBone_BlendState( self : PCalBone; weight : single; pTranslation : PCalVector; pRotation : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalBone_CalculateState( self : PCalBone ); cdecl; external Cal3DLibName;
procedure CalBone_ClearState( self : PCalBone ); cdecl; external Cal3DLibName;
function CalBone_Create( self : PCalBone; pCoreBone : PCalCoreBone ) : BOOL; cdecl; external Cal3DLibName;
procedure CalBone_Delete( self : PCalBone ); cdecl; external Cal3DLibName;
procedure CalBone_Destroy( self : PCalBone ); cdecl; external Cal3DLibName;
function CalBone_GetCoreBone( self : PCalBone ) : PCalCoreBone; cdecl; external Cal3DLibName;
function CalBone_GetRotation( self : PCalBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalBone_GetRotationAbsolute( self : PCalBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalBone_GetRotationBoneSpace( self : PCalBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalBone_GetTranslation( self : PCalBone ) : PCalVector; cdecl; external Cal3DLibName;
function CalBone_GetTranslationAbsolute( self : PCalBone ) : PCalVector; cdecl; external Cal3DLibName;
function CalBone_GetTranslationBoneSpace( self : PCalBone ) : PCalVector; cdecl; external Cal3DLibName;
procedure CalBone_LockState( self : PCalBone ); cdecl; external Cal3DLibName;
function CalBone_New : PCalBone; cdecl; external Cal3DLibName;
procedure CalBone_SetSkeleton( self : PCalBone;  pSkeleton : PCalSkeleton ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreAnimation wrapper functions declaration                             //
//****************************************************************************//

function CalCoreAnimation_AddCoreTrack( self : PCalCoreAnimation; pCoreTrack : PCalCoreTrack ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreAnimation_Create( self : PCalCoreAnimation ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreAnimation_Delete( self : PCalCoreAnimation ); cdecl; external Cal3DLibName;
procedure CalCoreAnimation_Destroy( self : PCalCoreAnimation ); cdecl; external Cal3DLibName;
function CalCoreAnimation_GetCoreTrack( self : PCalCoreAnimation; coreBoneId : integer ) : PCalCoreTrack; cdecl; external Cal3DLibName;
function  CalCoreAnimation_GetDuration( self : PCalCoreAnimation ) : single; cdecl; external Cal3DLibName;
//  function std::list<CalCoreTrack *>& CalCoreAnimation_GetListCoreTrack( self : PCalCoreAnimation ); cdecl; external Cal3DLibName;
function CalCoreAnimation_New : PCalCoreAnimation; cdecl; external Cal3DLibName;
procedure CalCoreAnimation_SetDuration( self : PCalCoreAnimation; duration : single ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreBone wrapper functions declaration                                  //
//****************************************************************************//

function CalCoreBone_AddChildId( self : PCalCoreBone; childId : integer ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreBone_CalculateState( self : PCalCoreBone ); cdecl; external Cal3DLibName;
function CalCoreBone_Create( self : PCalCoreBone; strName : PChar ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreBone_Delete( self : PCalCoreBone ); cdecl; external Cal3DLibName;
procedure CalCoreBone_Destroy( self : PCalCoreBone ); cdecl; external Cal3DLibName;
//  function std::list<int>& CalCoreBone_GetListChildId( self : PCalCoreBone ); cdecl; external Cal3DLibName;
function CalCoreBone_GetName( self : PCalCoreBone ) : PChar; cdecl; external Cal3DLibName;
function CalCoreBone_GetParentId( self : PCalCoreBone ) : integer; cdecl; external Cal3DLibName;
function CalCoreBone_GetRotation( self : PCalCoreBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalCoreBone_GetRotationAbsolute( self : PCalCoreBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalCoreBone_GetRotationBoneSpace( self : PCalCoreBone ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalCoreBone_GetTranslation( self : PCalCoreBone ) : PCalVector; cdecl; external Cal3DLibName;
function CalCoreBone_GetTranslationAbsolute( self : PCalCoreBone ) : PCalVector; cdecl; external Cal3DLibName;
function CalCoreBone_GetTranslationBoneSpace( self : PCalCoreBone ) : PCalVector; cdecl; external Cal3DLibName;
function CalCoreBone_GetUserData( self : PCalCoreBone ) : CalUserData; cdecl; external Cal3DLibName;
function CalCoreBone_New : PCalCoreBone; cdecl; external Cal3DLibName;
procedure CalCoreBone_SetCoreSkeleton( self : PCalCoreBone; pCoreSkeleton : PCalCoreSkeleton ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetParentId( self : PCalCoreBone; parentId : integer ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetRotation( self : PCalCoreBone; pRotation : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetRotationBoneSpace( self : PCalCoreBone; pRotation : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetTranslation( self : PCalCoreBone; pTranslation : PCalVector ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetTranslationBoneSpace( self : PCalCoreBone; pTranslation : PCalVector ); cdecl; external Cal3DLibName;
procedure CalCoreBone_SetUserData( self : PCalCoreBone; userData : CalUserData ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreKeyframe wrapper functions declaration                              //
//****************************************************************************//

function CalCoreKeyframe_Create( self : PCalCoreKeyframe ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreKeyframe_Delete( self : PCalCoreKeyframe ); cdecl; external Cal3DLibName;
procedure CalCoreKeyframe_Destroy( self : PCalCoreKeyframe ); cdecl; external Cal3DLibName;
function CalCoreKeyframe_GetRotation( self : PCalCoreKeyframe ) : PCalQuaternion; cdecl; external Cal3DLibName;
function CalCoreKeyframe_GetTime( self : PCalCoreKeyframe )  : single; cdecl; external Cal3DLibName;
function CalCoreKeyframe_GetTranslation( self : PCalCoreKeyframe ) : PCalVector; cdecl; external Cal3DLibName;
function CalCoreKeyframe_New : PCalCoreKeyframe; cdecl; external Cal3DLibName;
procedure CalCoreKeyframe_SetRotation( self : PCalCoreKeyframe; pRotation : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalCoreKeyframe_SetTime( self : PCalCoreKeyframe; time : single ); cdecl; external Cal3DLibName;
procedure CalCoreKeyframe_SetTranslation( self : PCalCoreKeyframe; pTranslation : PCalVector ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreMaterial wrapper functions declaration                              //
//****************************************************************************//

function CalCoreMaterial_Create( self : PCalCoreMaterial ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreMaterial_Delete( self : PCalCoreMaterial ); cdecl; external Cal3DLibName;
procedure CalCoreMaterial_Destroy( self : PCalCoreMaterial ); cdecl; external Cal3DLibName;
//  function CalCoreMaterial::Color *CalCoreMaterial_GetAmbientColor(CalCoreMaterial  self : P); cdecl; external Cal3DLibName;
//  function CalCoreMaterial::Color *CalCoreMaterial_GetDiffuseColor(CalCoreMaterial  self : P); cdecl; external Cal3DLibName;
function CalCoreMaterial_GetMapCount( self : PCalCoreMaterial ) : integer; cdecl; external Cal3DLibName;
function CalCoreMaterial_GetMapFilename( self : PCalCoreMaterial; mapId : integer ) : PChar; cdecl; external Cal3DLibName;
function CalCoreMaterial_GetMapUserData( self : PCalCoreMaterial; mapId : integer ) : CalUserData; cdecl; external Cal3DLibName;
function CalCoreMaterial_GetShininess( self : PCalCoreMaterial )  : single; cdecl; external Cal3DLibName;
//  function CalCoreMaterial::Color *CalCoreMaterial_GetSpecularColor(CalCoreMaterial  self : P); cdecl; external Cal3DLibName;
function CalCoreMaterial_GetUserData( self : PCalCoreMaterial ) : CalUserData; cdecl; external Cal3DLibName;
//  function std::vector<Map>& CalCoreMaterial_GetVectorMap(CalCoreMaterial  self : P); cdecl; external Cal3DLibName;
function CalCoreMaterial_New : PCalCoreMaterial; cdecl; external Cal3DLibName;
function CalCoreMaterial_Reserve( self : PCalCoreMaterial; mapCount : integer ) : BOOL; cdecl; external Cal3DLibName;
//  procedure CalCoreMaterial_SetAmbientColor(CalCoreMaterial  self : P, CalCoreMaterial::Color *pAmbientColor); cdecl; external Cal3DLibName;
//  procedure CalCoreMaterial_SetDiffuseColor(CalCoreMaterial  self : P, CalCoreMaterial::Color *pDiffuseColor); cdecl; external Cal3DLibName;
//  function BOOL CalCoreMaterial_SetMap(CalCoreMaterial  self : P, : integer mapId, CalCoreMaterial::Map *pMap); cdecl; external Cal3DLibName;
function CalCoreMaterial_SetMapUserData( self : PCalCoreMaterial; mapId : integer; userData : CalUserData ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreMaterial_SetShininess( self : PCalCoreMaterial; shininess : single ); cdecl; external Cal3DLibName;
//  procedure CalCoreMaterial_SetSpecularColor(CalCoreMaterial  self : P, CalCoreMaterial::Color *pSpecularColor); cdecl; external Cal3DLibName;
procedure CalCoreMaterial_SetUserData( self : PCalCoreMaterial; userData : CalUserData ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreMesh wrapper functions declaration                                  //
//****************************************************************************//

function CalCoreMesh_AddCoreSubmesh( self : PCalCoreMesh; pCoreSubmesh : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalCoreMesh_Create( self : PCalCoreMesh ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreMesh_Delete( self : PCalCoreMesh ); cdecl; external Cal3DLibName;
procedure CalCoreMesh_Destroy( self : PCalCoreMesh ); cdecl; external Cal3DLibName;
function CalCoreMesh_GetCoreSubmesh( self : PCalCoreMesh; id : integer ) : PCalCoreSubmesh; cdecl; external Cal3DLibName;
function CalCoreMesh_GetCoreSubmeshCount( self : PCalCoreMesh ) : integer; cdecl; external Cal3DLibName;
//  function std::vector<CalCoreSubmesh *>& CalCoreMesh_GetVectorCoreSubmesh( self : PCalCoreMesh ); cdecl; external Cal3DLibName;
function CalCoreMesh_New : PCalCoreMesh; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreModel wrapper functions declaration                                 //
//****************************************************************************//

function CalCoreModel_AddCoreAnimation( self : PCalCoreModel; pCoreAnimation : PCalCoreAnimation ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_AddCoreMaterial( self : PCalCoreModel; pCoreMaterial : PCalCoreMaterial ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_AddCoreMesh( self : PCalCoreModel; pCoreMesh : PCalCoreMesh ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_Create(self : PCalCoreModel; strName : PChar ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_CreateCoreMaterialThread( self : PCalCoreModel; coreMaterialThreadId : integer ) : BOOL ; cdecl; external Cal3DLibName;
procedure CalCoreModel_Delete( self : PCalCoreModel ); cdecl; external Cal3DLibName;
procedure CalCoreModel_Destroy( self : PCalCoreModel ); cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreAnimation( self : PCalCoreModel; coreAnimationId : integer ) : PCalCoreAnimation; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreAnimationCount( self : PCalCoreModel) : integer; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreMaterial( self : PCalCoreModel; coreMaterialId : integer ) : PCalCoreMaterial; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreMaterialCount( self : PCalCoreModel) : integer; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreMaterialId( self : PCalCoreModel; coreMaterialThreadId : integer; coreMaterialSetId : integer ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreMesh( self : PCalCoreModel; coreMeshId : integer ) : PCalCoreMesh; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreMeshCount( self : PCalCoreModel ) : integer; cdecl; external Cal3DLibName;
function CalCoreModel_GetCoreSkeleton( self : PCalCoreModel ) : PCalCoreSkeleton; cdecl; external Cal3DLibName;
function CalCoreModel_GetUserData( self : PCalCoreModel ) : CalUserData; cdecl; external Cal3DLibName;
function CalCoreModel_LoadCoreAnimation( self : PCalCoreModel; strFilename : PChar ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_LoadCoreMaterial( self : PCalCoreModel; strFilename : PChar ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_LoadCoreMesh( self : PCalCoreModel; strFilename : PChar ): integer; cdecl; external Cal3DLibName;
function CalCoreModel_LoadCoreSkeleton( self : PCalCoreModel; strFilename : PChar ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_New : PCalCoreModel; cdecl; external Cal3DLibName;
function CalCoreModel_SaveCoreAnimation( self : PCalCoreModel; strFilename : PChar; coreAnimtionId : integer ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_SaveCoreMaterial( self : PCalCoreModel;  strFilename : PChar; coreMaterialId : integer ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_SaveCoreMesh( self : PCalCoreModel;  strFilename : PChar; coreMeshId : integer ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_SaveCoreSkeleton( self : PCalCoreModel;  strFilename : PChar ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreModel_SetCoreMaterialId( self : PCalCoreModel; coreMaterialThreadId : integer; coreMaterialSetId : integer ; coreMaterialId: integer ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreModel_SetCoreSkeleton( self : PCalCoreModel; pCoreSkeleton : PCalCoreSkeleton ); cdecl; external Cal3DLibName;
procedure CalCoreModel_SetUserData(  self : PCalCoreModel; userData : CalUserData ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreSkeleton wrapper functions declaration                              //
//****************************************************************************//

function CalCoreSkeleton_AddCoreBone( self : PCalCoreSkeleton; pCoreBone : PCalCoreBone ) : integer; cdecl; external Cal3DLibName;
procedure CalCoreSkeleton_CalculateState( self : PCalCoreSkeleton ); cdecl; external Cal3DLibName;
function CalCoreSkeleton_Create( self : PCalCoreSkeleton ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreSkeleton_Delete( self : PCalCoreSkeleton ); cdecl; external Cal3DLibName;
procedure CalCoreSkeleton_Destroy( self : PCalCoreSkeleton ); cdecl; external Cal3DLibName;
function CalCoreSkeleton_GetCoreBone( self : PCalCoreSkeleton; coreBoneId : integer ) : PCalCoreBone; cdecl; external Cal3DLibName;
function CalCoreSkeleton_GetCoreBoneId( self : PCalCoreSkeleton; strName : PChar ): integer; cdecl; external Cal3DLibName;
//  function std::list<int>& CalCoreSkeleton_GetListRootCoreBoneId(CalCoreSkeleton  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalCoreBone *>& CalCoreSkeleton_GetVectorCoreBone(CalCoreSkeleton  self : P); cdecl; external Cal3DLibName;
function CalCoreSkeleton_New : PCalCoreSkeleton; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreSubmesh wrapper functions declaration                               //
//****************************************************************************//

function CalCoreSubmesh_Create( self : PCalCoreSubmesh ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreSubmesh_Delete( self : PCalCoreSubmesh ); cdecl; external Cal3DLibName;
procedure CalCoreSubmesh_Destroy( self : PCalCoreSubmesh ); cdecl; external Cal3DLibName;
function CalCoreSubmesh_GetCoreMaterialThreadId( self : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalCoreSubmesh_GetFaceCount( self : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalCoreSubmesh_GetLodCount( self : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalCoreSubmesh_GetSpringCount( self : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
//  function std::vector<CalCoreSubmesh::Face>& CalCoreSubmesh_GetVectorFace(CalCoreSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalCoreSubmesh::PhysicalProperty>& CalCoreSubmesh_GetVectorPhysicalProperty(CalCoreSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalCoreSubmesh::Spring>& CalCoreSubmesh_GetVectorSpring(CalCoreSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<std::vector<CalCoreSubmesh::TextureCoordinate> >& CalCoreSubmesh_GetVectorVectorTextureCoordinate(CalCoreSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalCoreSubmesh::Vertex>& CalCoreSubmesh_GetVectorVertex(CalCoreSubmesh  self : P); cdecl; external Cal3DLibName;
function CalCoreSubmesh_GetVertexCount( self : PCalCoreSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalCoreSubmesh_New : PCalCoreSubmesh; cdecl; external Cal3DLibName;
function CalCoreSubmesh_Reserve( self : PCalCoreSubmesh; vertexCount : integer; textureCoordinateCount : integer; faceCount : integer; springCount : integer ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreSubmesh_SetCoreMaterialThreadId( self : PCalCoreSubmesh; coreMaterialThreadId : integer ); cdecl; external Cal3DLibName;
//  function BOOL CalCoreSubmesh_SetFace(CalCoreSubmesh  self : P, : integer faceId, CalCoreSubmesh::Face *pFace); cdecl; external Cal3DLibName;
procedure CalCoreSubmesh_SetLodCount( self : PCalCoreSubmesh; lodCount : integer ); cdecl; external Cal3DLibName;
//  function BOOL CalCoreSubmesh_SetPhysicalProperty(CalCoreSubmesh  self : P, : integer vertexId, CalCoreSubmesh::PhysicalProperty *pPhysicalProperty); cdecl; external Cal3DLibName;
//  function BOOL CalCoreSubmesh_SetSpring(CalCoreSubmesh  self : P, : integer springId, CalCoreSubmesh::Spring *pSpring); cdecl; external Cal3DLibName;
//  function BOOL CalCoreSubmesh_SetTextureCoordinate(CalCoreSubmesh  self : P, : integer vertexId, : integer textureCoordinateId, CalCoreSubmesh::TextureCoordinate *pTextureCoordinate); cdecl; external Cal3DLibName;
//  function BOOL CalCoreSubmesh_SetVertex(CalCoreSubmesh  self : P, : integer vertexId, CalCoreSubmesh::Vertex *pVertex); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalCoreTrack wrapper functions declaration                                 //
//****************************************************************************//

function CalCoreTrack_AddCoreKeyframe( self : PCalCoreTrack; pCoreKeyframe : PCalCoreKeyframe ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreTrack_Create( self : PCalCoreTrack ) : BOOL; cdecl; external Cal3DLibName;
procedure CalCoreTrack_Delete( self : PCalCoreTrack ); cdecl; external Cal3DLibName;
procedure CalCoreTrack_Destroy( self : PCalCoreTrack ); cdecl; external Cal3DLibName;
function CalCoreTrack_GetCoreBoneId( self : PCalCoreTrack ) : integer; cdecl; external Cal3DLibName;
//  function std::map< : single, CalCoreKeyframe *>& CalCoreTrack_GetMapCoreKeyframe(CalCoreTrack  self : P); cdecl; external Cal3DLibName;
function CalCoreTrack_GetState( self : PCalCoreTrack;  time : single; pTranslation : PCalVector; pRotation : PCalQuaternion ) : BOOL; cdecl; external Cal3DLibName;
function CalCoreTrack_New : PCalCoreTrack; cdecl; external Cal3DLibName;
function CalCoreTrack_SetCoreBoneId( self : PCalCoreTrack; coreBoneId : integer ) : BOOL; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalError wrapper functions declaration                                     //
//****************************************************************************//
type
  CalErrorCode = (
  ERROR_CODE_OK = 0,
  ERROR_CODE_INTERNAL,
  ERROR_CODE_INVALID_HANDLE,
  ERROR_CODE_MEMORY_ALLOCATION_FAILED,
  ERROR_CODE_FILE_NOT_FOUND,
  ERROR_CODE_INVALID_FILE_FORMAT,
  ERROR_CODE_FILE_PARSER_FAILED,
  ERROR_CODE_INDEX_BUILD_FAILED,
  ERROR_CODE_NO_PARSER_DOCUMENT,
  ERROR_CODE_INVALID_ANIMATION_DURATION,
  ERROR_CODE_BONE_NOT_FOUND,
  ERROR_CODE_INVALID_ATTRIBUTE_VALUE,
  ERROR_CODE_INVALID_KEYFRAME_COUNT,
  ERROR_CODE_INVALID_ANIMATION_TYPE,
  ERROR_CODE_FILE_CREATION_FAILED,
  ERROR_CODE_FILE_WRITING_FAILED,
  ERROR_CODE_INCOMPATIBLE_FILE_VERSION,
  ERROR_CODE_NO_MESH_IN_MODEL,
  ERROR_CODE_MAX_ERROR_CODE
  );

function CalError_GetLastErrorCode : CalErrorCode; cdecl; external Cal3DLibName;
function CalError_GetLastErrorDescription : PChar; cdecl; external Cal3DLibName;
function CalError_GetLastErrorFile : PChar; cdecl; external Cal3DLibName;
function CalError_GetLastErrorLine : integer ; cdecl; external Cal3DLibName;
function CalError_GetLastErrorText : PChar; cdecl; external Cal3DLibName;
procedure CalError_PrintLastError; cdecl; external Cal3DLibName;
procedure CalError_SetLastError(code : CalErrorCode; strFile : PChar ; line: integer; strText : PChar ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalLoader wrapper functions declaration                                    //
//****************************************************************************//

procedure CalLoader_Delete( self : PCalLoader ); cdecl; external Cal3DLibName;
function CalLoader_LoadCoreAnimation( self : PCalLoader; strFilename : PChar ) : PCalCoreAnimation; cdecl; external Cal3DLibName;
function CalLoader_LoadCoreMaterial( self : PCalLoader; strFilename : PChar ) : PCalCoreMaterial; cdecl; external Cal3DLibName;
function CalLoader_LoadCoreMesh( self : PCalLoader; strFilename : PChar ): PCalCoreMesh; cdecl; external Cal3DLibName;
function CalLoader_LoadCoreSkeleton( self : PCalLoader; strFilename : PChar ) : PCalCoreSkeleton; cdecl; external Cal3DLibName;
function CalLoader_New : PCalLoader; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalMesh wrapper functions declaration                                      //
//****************************************************************************//

function CalMesh_Create( self : PCalMesh; pCoreMesh : PCalCoreMesh ) : BOOL; cdecl; external Cal3DLibName;
procedure CalMesh_Delete( self : PCalMesh ); cdecl; external Cal3DLibName;
procedure CalMesh_Destroy( self : PCalMesh ); cdecl; external Cal3DLibName;
function CalMesh_GetCoreMesh( self : PCalMesh ) : PCalCoreMesh; cdecl; external Cal3DLibName;
function CalMesh_GetSubmesh( self : PCalMesh; id : integer ) : PCalSubmesh; cdecl; external Cal3DLibName;
function CalMesh_GetSubmeshCount( self : PCalMesh ) : integer; cdecl; external Cal3DLibName;
//  function std::vector<CalSubmesh *>& CalMesh_GetVectorSubmesh(CalMesh  self : P); cdecl; external Cal3DLibName;
function CalMesh_New: PCalMesh; cdecl; external Cal3DLibName;
procedure CalMesh_SetLodLevel( self : PCalMesh; lodLevel : single ); cdecl; external Cal3DLibName;
procedure CalMesh_SetMaterialSet( self : PCalMesh; setId : integer ); cdecl; external Cal3DLibName;
procedure CalMesh_SetModel( self : PCalMesh; pModel : PCalModel ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalMixer wrapper functions declaration                                     //
//****************************************************************************//

function CalMixer_BlendCycle( self : PCalMixer; id: integer; weight : single; delay : single ) : BOOL; cdecl; external Cal3DLibName;
function CalMixer_ClearCycle( self : PCalMixer; id : integer; delay : single ) : BOOL; cdecl; external Cal3DLibName;
function CalMixer_Create( self : PCalMixer; pModel : PCalModel ) : BOOL; cdecl; external Cal3DLibName;
procedure CalMixer_Delete( self : PCalMixer ); cdecl; external Cal3DLibName;
procedure CalMixer_Destroy( self : PCalMixer ); cdecl; external Cal3DLibName;
function CalMixer_ExecuteAction( self : PCalMixer; id : integer; delayIn : single; delayOut : single ) : BOOL; cdecl; external Cal3DLibName;
function CalMixer_New : PCalMixer; cdecl; external Cal3DLibName;
procedure CalMixer_UpdateAnimation( self : PCalMixer; deltaTime : single ); cdecl; external Cal3DLibName;
procedure CalMixer_UpdateSkeleton( self : PCalMixer ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalModel wrapper functions declaration                                     //
//****************************************************************************//

function CalModel_AttachMesh( self : PCalModel; coreMeshId : integer ): BOOL; cdecl; external Cal3DLibName;
function CalModel_Create( self : PCalModel; pCoreModel : PCalCoreModel ) : BOOL; cdecl; external Cal3DLibName;
procedure CalModel_Delete( self : PCalModel ); cdecl; external Cal3DLibName;
procedure CalModel_Destroy( self : PCalModel ); cdecl; external Cal3DLibName;
function CalModel_DetachMesh( self : PCalModel; coreMeshId : integer ) : BOOL; cdecl; external Cal3DLibName;
function CalModel_GetCoreModel( self : PCalModel ) : PCalCoreModel; cdecl; external Cal3DLibName;
function CalModel_GetMesh( self : PCalModel; coreMeshId : integer ) : PCalMesh; cdecl; external Cal3DLibName;
function CalModel_GetMixer( self : PCalModel ) : PCalMixer; cdecl; external Cal3DLibName;
function CalModel_GetPhysique( self : PCalModel ) : PCalPhysique; cdecl; external Cal3DLibName;
function CalModel_GetRenderer( self : PCalModel ) : PCalRenderer; cdecl; external Cal3DLibName;
function CalModel_GetSkeleton( self : PCalModel ) : PCalSkeleton; cdecl; external Cal3DLibName;
function CalModel_GetSpringSystem( self : PCalModel ) : PCalSpringSystem; cdecl; external Cal3DLibName;
function CalModel_GetUserData( self : PCalModel ) : CalUserData; cdecl; external Cal3DLibName;
//  function std::vector<CalMesh *>& CalModel_GetVectorMesh(CalModel  self : P); cdecl; external Cal3DLibName;
function CalModel_New : PCalModel; cdecl; external Cal3DLibName;
procedure CalModel_SetLodLevel( self : PCalModel; lodLevel : single ); cdecl; external Cal3DLibName;
procedure CalModel_SetMaterialSet( self : PCalModel; setId : integer ); cdecl; external Cal3DLibName;
procedure CalModel_SetUserData( self : PCalModel; userData : CalUserData ); cdecl; external Cal3DLibName;
procedure CalModel_Update( self : PCalModel; deltaTime : single ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalPhysique wrapper functions declaration                                  //
//****************************************************************************//

function CalPhysique_CalculateNormals( self : PCalPhysique; pSubmesh : PCalSubmesh; pNormalBuffer : PSingle ) : integer; cdecl; external Cal3DLibName;
function CalPhysique_CalculateVertices( self : PCalPhysique; pSubmesh : PCalSubmesh; pVertexBuffer : PSingle ) : integer; cdecl; external Cal3DLibName;
function CalPhysique_CalculateVerticesAndNormals( self : PCalPhysique; pSubmesh : PCalSubmesh; pVertexBuffer : PSingle ): integer; cdecl; external Cal3DLibName;
function CalPhysique_CalculateVerticesNormalsAndTexCoords( self : PCalPhysique; pSubmesh : PCalSubmesh; pVertexBuffer : PSingle; NumTexCoords : integer ): integer; cdecl; external Cal3DLibName;
function CalPhysique_Create( self : PCalPhysique; pModel : PCalModel ) : BOOL; cdecl; external Cal3DLibName;
procedure CalPhysique_Delete( self : PCalPhysique ); cdecl; external Cal3DLibName;
procedure CalPhysique_Destroy( self : PCalPhysique); cdecl; external Cal3DLibName;
function CalPhysique_New : PCalPhysique; cdecl; external Cal3DLibName;
procedure CalPhysique_Update( self : PCalPhysique); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalPlatform wrapper functions declaration                                  //
//****************************************************************************//

//****************************************************************************//
// CalQuaternion wrapper functions declaration                                //
//****************************************************************************//

procedure CalQuaternion_Blend( self : PCalQuaternion; d : single; pQ : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_Clear( self : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_Conjugate( self : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_Delete( self : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_Equal( self : PCalQuaternion; pQ : PCalQuaternion ); cdecl; external Cal3DLibName;
function  CalQuaternion_Get( self : PCalQuaternion ) : PSingle; cdecl; external Cal3DLibName;
procedure CalQuaternion_Multiply( self : PCalQuaternion; pQ : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_MultiplyVector( self : PCalQuaternion; pV : PCalVector ); cdecl; external Cal3DLibName;
function CalQuaternion_New : PCalQuaternion; cdecl; external Cal3DLibName;
procedure CalQuaternion_Op_Multiply( pResult : PCalQuaternion; pQ : PCalQuaternion; pR : PCalQuaternion ); cdecl; external Cal3DLibName;
procedure CalQuaternion_Set( self : PCalQuaternion; qx : single; qy : single; qz : single; qw : single ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalRenderer wrapper functions declaration                                  //
//****************************************************************************//

function CalRenderer_BeginRendering( self : PCalRenderer ) : BOOL; cdecl; external Cal3DLibName;
function CalRenderer_Create( self : PCalRenderer; pModel : PCalModel ) : BOOL; cdecl; external Cal3DLibName;
procedure CalRenderer_Delete( self : PCalRenderer ); cdecl; external Cal3DLibName;
procedure CalRenderer_Destroy( self : PCalRenderer ); cdecl; external Cal3DLibName;
procedure CalRenderer_EndRendering( self : PCalRenderer ); cdecl; external Cal3DLibName;
procedure CalRenderer_GetAmbientColor( self : PCalRenderer; pColorBuffer : PChar ); cdecl; external Cal3DLibName;
procedure CalRenderer_GetDiffuseColor( self : PCalRenderer; pColorBuffer : PChar ); cdecl; external Cal3DLibName;
function CalRenderer_GetFaceCount( self : PCalRenderer ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetFaces( self : PCalRenderer; pFaceBuffer : PCalIndex ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetMapCount( self : PCalRenderer ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetMapUserData( self : PCalRenderer; mapId : integer ) : CalUserData; cdecl; external Cal3DLibName;
function CalRenderer_GetMeshCount( self : PCalRenderer ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetNormals( self : PCalRenderer;  pNormalBuffer : PSingle ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetShininess( self : PCalRenderer ) : single; cdecl; external Cal3DLibName;
procedure CalRenderer_GetSpecularColor( self : PCalRenderer; pColorBuffer : PChar ); cdecl; external Cal3DLibName;
function CalRenderer_GetSubmeshCount( self : PCalRenderer; meshId : integer ) : integer; cdecl; external Cal3DLibName;
function CalRenderer_GetTextureCoordinates( self : PCalRenderer; mapId : integer;  pTextureCoordinateBuffer : PSingle ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetVertexCount( self : PCalRenderer ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetVertices( self : PCalRenderer; pVertexBuffer : PSingle ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetVerticesAndNormals( self : PCalRenderer;  pVertexBuffer : PSingle ): integer; cdecl; external Cal3DLibName;
function CalRenderer_GetVerticesNormalsAndTexCoords( self : PCalRenderer; pVertexBuffer : PSingle; NumTexCoords : integer ): integer; cdecl; external Cal3DLibName;
function CalRenderer_New : PCalRenderer; cdecl; external Cal3DLibName;
function CalRenderer_SelectMeshSubmesh( self : PCalRenderer; meshId : integer ; submeshId : integer ) : BOOL; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalSaver wrapper functions declaration                                     //
//****************************************************************************//

procedure CalSaver_Delete( self : PCalSaver ); cdecl; external Cal3DLibName;
function CalSaver_New : PCalSaver; cdecl; external Cal3DLibName;
function CalSaver_SaveCoreAnimation( self : PCalSaver; strFilename : PChar; pCoreAnimation : PCalCoreAnimation ) : BOOL; cdecl; external Cal3DLibName;
function CalSaver_SaveCoreMaterial( self : PCalSaver; strFilename : PChar ; pCoreMaterial : PCalCoreMaterial ) : BOOL; cdecl; external Cal3DLibName;
function CalSaver_SaveCoreMesh( self : PCalSaver; strFilename : PChar; pCoreMesh : PCalCoreMesh ) : BOOL; cdecl; external Cal3DLibName;
function CalSaver_SaveCoreSkeleton( self : PCalSaver; strFilename : PChar; pCoreSkeleton : PCalCoreSkeleton ) : BOOL; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalSkeleton wrapper functions declaration                                  //
//****************************************************************************//

procedure CalSkeleton_CalculateState( self : PCalSkeleton ); cdecl; external Cal3DLibName;
procedure CalSkeleton_ClearState( self : PCalSkeleton ); cdecl; external Cal3DLibName;
function CalSkeleton_Create( self : PCalSkeleton; pCoreSkeleton : PCalCoreSkeleton ): BOOL; cdecl; external Cal3DLibName;
procedure CalSkeleton_Delete( self : PCalSkeleton ); cdecl; external Cal3DLibName;
procedure CalSkeleton_Destroy( self : PCalSkeleton ); cdecl; external Cal3DLibName;
function CalSkeleton_GetBone( self : PCalSkeleton; boneId : integer ) : PCalBone; cdecl; external Cal3DLibName;
function CalSkeleton_GetCoreSkeleton( self : PCalSkeleton ) : PCalCoreSkeleton; cdecl; external Cal3DLibName;
//  function std::vector<CalBone *>& CalSkeleton_GetVectorBone(CalSkeleton  self : P); cdecl; external Cal3DLibName;
procedure CalSkeleton_LockState( self : PCalSkeleton ); cdecl; external Cal3DLibName;
function CalSkeleton_New : PCalSkeleton; cdecl; external Cal3DLibName;

// DEBUG-CODE
function CalSkeleton_GetBonePoints( self : PCalSkeleton; pPoints : PSingle ) : integer; cdecl; external Cal3DLibName;
function CalSkeleton_GetBonePointsStatic( self : PCalSkeleton; pPoints : PSingle ) : integer; cdecl; external Cal3DLibName;
function CalSkeleton_GetBoneLines( self : PCalSkeleton; pPoints : PSingle ) : integer; cdecl; external Cal3DLibName;
function CalSkeleton_GetBoneLinesStatic( self : PCalSkeleton; pPoints : PSingle ) : integer; cdecl; external Cal3DLibName;

//****************************************************************************//
// CalSpringSystem wrapper functions declaration                              //
//****************************************************************************//

procedure CalSpringSystem_CalculateForces( self : PCalSpringSystem; pSubmesh : PCalSubmesh; deltaTime : single ); cdecl; external Cal3DLibName;
procedure CalSpringSystem_CalculateVertices( self : PCalSpringSystem; pSubmesh : PCalSubmesh; deltaTime : single ); cdecl; external Cal3DLibName;
function CalSpringSystem_Create( self : PCalSpringSystem; pModel : PCalModel ) : BOOL; cdecl; external Cal3DLibName;
procedure CalSpringSystem_Delete( self : PCalSpringSystem ); cdecl; external Cal3DLibName;
procedure CalSpringSystem_Destroy( self : PCalSpringSystem ); cdecl; external Cal3DLibName;
function CalSpringSystem_New : PCalSpringSystem; cdecl; external Cal3DLibName;
procedure CalSpringSystem_Update( self : PCalSpringSystem; deltaTime : single ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalSubmesh wrapper functions declaration                                   //
//****************************************************************************//

function CalSubmesh_Create( self : PCalSubmesh; pCoreSubmesh : PCalCoreSubmesh ) : BOOL; cdecl; external Cal3DLibName;
procedure CalSubmesh_Delete( self : PCalSubmesh ); cdecl; external Cal3DLibName;
procedure CalSubmesh_Destroy( self : PCalSubmesh ); cdecl; external Cal3DLibName;
function CalSubmesh_GetCoreSubmesh( self : PCalSubmesh ) : PCalCoreSubmesh; cdecl; external Cal3DLibName;
function CalSubmesh_GetCoreMaterialId( self : PCalSubmesh ): integer; cdecl; external Cal3DLibName;
function CalSubmesh_GetFaceCount( self : PCalSubmesh ): integer; cdecl; external Cal3DLibName;
function CalSubmesh_GetFaces( self : PCalSubmesh; pFaceBuffer : PCalIndex ): integer; cdecl; external Cal3DLibName;

//  function std::vector<CalVector>& CalSubmesh_GetVectorNormal(CalSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalSubmesh::PhysicalProperty>& CalSubmesh_GetVectorPhysicalProperty(CalSubmesh  self : P); cdecl; external Cal3DLibName;
//  function std::vector<CalVector>& CalSubmesh_GetVectorVertex(CalSubmesh  self : P); cdecl; external Cal3DLibName;
function CalSubmesh_GetVertexCount( self : PCalSubmesh ) : integer; cdecl; external Cal3DLibName;
function CalSubmesh_HasInternalData( self : PCalSubmesh ) : BOOL; cdecl; external Cal3DLibName;
function CalSubmesh_New : PCalSubmesh; cdecl; external Cal3DLibName;
procedure CalSubmesh_SetCoreMaterialId( self : PCalSubmesh; coreMaterialId : integer ); cdecl; external Cal3DLibName;
procedure CalSubmesh_SetLodLevel( self : PCalSubmesh; lodLevel : single ); cdecl; external Cal3DLibName;

//****************************************************************************//
// CalVector wrapper functions declaration                                    //
//****************************************************************************//

procedure CalVector_Add( self : PCalVector; pV : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Blend( self : PCalVector; d : single; pV: PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Clear( self : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Delete( self : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Equal( self : PCalVector; pV : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_InverseScale( self : PCalVector; d : single ); cdecl; external Cal3DLibName;
function  CalVector_Get( self : PCalVector ) : PSingle; cdecl; external Cal3DLibName;
function  CalVector_Length( self : PCalVector) :single; cdecl; external Cal3DLibName;
function CalVector_New : PCalVector; cdecl; external Cal3DLibName;
function CalVector_Normalize( self : PCalVector)  : single; cdecl; external Cal3DLibName;
procedure CalVector_Op_Add(pResult : PCalVector; pV : PCalVector; pU : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Op_Subtract( pResult : PCalVector; pV : PCalVector; pU : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_CalVector_Op_Scale( pResult : PCalVector; pV : PCalVector; d : single ); cdecl; external Cal3DLibName;
procedure CalVector_CalVector_Op_InverseScale( pResult : PCalVector; pV : PCalVector; d : single ); cdecl; external Cal3DLibName;
function  CalVector_Op_Scalar( pV : PCalVector; pU : PCalVector ): single; cdecl; external Cal3DLibName;
procedure CalVector_Op_Cross( pResult : PCalVector; pV : PCalVector; pU : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Scale( self : PCalVector; d : single ); cdecl; external Cal3DLibName;
procedure CalVector_Set( self : PCalVector; vx : single; vy : single; vz : single ); cdecl; external Cal3DLibName;
procedure CalVector_Subtract( self : PCalVector; pV : PCalVector ); cdecl; external Cal3DLibName;
procedure CalVector_Transform( self : PCalVector; pQ : PCalQuaternion ); cdecl; external Cal3DLibName;

implementation

end.
