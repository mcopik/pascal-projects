unit hvdi;
{
  $Id: hvdi.pas,v 1.5 2007/05/29 21:30:11 savage Exp $

}
{******************************************************************************}
{                                                                              }
{       Borland Delphi HawkVoice                                               }
{       Conversion of the HawkVoice  Headers                                   }
{                                                                              }
{ Portions created by <> are                }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : hvdi.h                                               }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dean Ellis <dean@delpigamer.com>                            }
{                                                                              }
{ Portions created by Dean Ellis are                                      }
{ Copyright (C) 2000 - 2004 Dean Ellis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
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
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The HawkVoice Runtime libraris on Win32  : hvdi.dll on Linux :  libHVDI.so }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{******************************************************************************}

{$I jedi-sdl.inc}

interface

const HVDI_MAJOR_VERSION = 0;
const HVDI_MINOR_VERSION  = 91;
const HVDI_VERSION_STRING  = 'HVDI 0.91 beta';


{$IFNDEF NL_INVALID}
const NL_INVALID           =   (-1);
const NL_FALSE             =   (0);
const NL_TRUE              =   (1);
{$ENDIF}

{$IFDEF HVDI_OLD_API}
const hvdiCreateEncoderState =  hvdiNewEncState;
const hvdiCreateDecoderState = hvdiNewDecState;
const hvdiFreeEncoderState   = hvdiDeleteEncState;
const hvdiFreeDecoderState   = hvdiDeleteDecState;
const hvdiSetCodec(x, y)     = hvdiEncStateSetCodec(y, x);
const hvdiIsVoicePacket      = hvdiPacketIsVoice;
const hvdiDecodePacket       = hvdiPacketDecode;
//const hvdiEncodePacket(a, b, c, d, e, f) hvdiPacketEncode(c, b, a, d, e, f);
{$ENDIF}

(* We will use HVDI or hvdi to prefix all HawkVoiceDI defines and functions *)

type BF_LONG  = LongInt ;

const BF_ROUNDS	=16;
const BF_BLOCK	=8;

type

bf_key_st = record
  P: array[0..(BF_ROUNDS+2)-1] of BF_LONG ;
  S: array[0..(4*256)-1] of BF_LONG ;
end;
BF_KEY = bf_key_st;


hcrypt_key_st = record
    bf:BF_KEY;
    iv: array[0..7] of Byte;
    digest: array[0..15] of Byte;
    n: integer;
end;
hcrypt_key =  hcrypt_key_st;
phcrypt_key = ^hcrypt_key;

hcrypt_salt_st = record
    data: array[0..15] of char; (* 128 bit salt for encryption key generation *)
end; //hcrypt_salt;
hcrypt_salt = hcrypt_salt_st;
phcrypt_salt = ^hcrypt_salt;

//typedef struct hvdi_vox_st hvdi_vox;
hvdi_vox_st = record
    rate: Integer;           //* HVDI_VOX_FAST, HVDI_VOX_MEDIUM, or HVDI_VOX_SLOW */
    noisethreshold: Integer; //* The actual threshold used by hvdiVOX */
    samplecount:Integer;    //* init to 0; used internally by hvdiVOX */
end;
hvdi_vox = hvdi_vox_st;
phvdi_vox = ^hvdi_vox;

//typedef struct hvdi_enc_state_st hvdi_enc_state;
hvdi_enc_state_st = record
    codec: Byte;      //* the codec used with the last packet */
    sequence: SmallInt;   //* the sequence number of the last packet */
    state: Pointer;     //* the codec state */
    vox: phvdi_vox;  //* the VOX structure for auto VOX */
end;
hvdi_enc_state = hvdi_enc_state_st;
phvdi_enc_state= ^hvdi_enc_state;

//typedef struct hvdi_dec_state_st hvdi_dec_state;
hvdi_dec_state_st = record
    codec: Byte;      //* the codec used with the last packet */
    sequence:SmallInt;   //* the sequence number of the last packet */
    state: Pointer;     //* the codec state */
end;
hvdi_dec_state = hvdi_dec_state_st;
phvdi_dec_state= ^hvdi_dec_state;
//typedef struct hvdi_agc_st hvdi_agc;
hvdi_agc_st = record
    sample_max: word;
    counter: Integer;
    igain: Longint;
    ipeak: Integer;
    silence_counter: Integer;
end;
hvdi_agc = hvdi_agc_st;
phvdi_agc = ^hvdi_agc;
//typedef struct hvdi_rate_st hvdi_rate;
hvdi_rate_st = record
    lcmrate: Longint;		    //* least common multiple of rates */
    inskip, outskip: Longint;    //* LCM increments for I & O rates */
    total: Longint;
    intot, outtot: Longint;      //* total samples in terms of LCM rate */
    lastsamp: Longint;
end;
hvdi_rate = hvdi_rate_st;
phvdi_rate = ^hvdi_rate;


(* The basic codecs, from hawkvoice.h *)
const HV_2_4K_CODEC     =  $0001; (* LPC-10 2.4 Kbps codec *)
const HV_4_8K_CODEC     = $0002;  (* LPC 4.8 Kbps codec *)
const HV_13_2K_CODEC    = $0003;  (* GSM 13.2 Kbps codec *)
const HV_32K_CODEC      = $0004;  (* Intel/DVI ADPCM 32 Kbps codec *)
const HV_64K_CODEC      = $0005;  (* G.711 u-law 64 Kbps codec *)
const HV_1_4K_CODEC     = $0006;  (* OpenLPC 1.4 Kbps codec *)
const HV_1_8K_CODEC     = $0007;  (* OpenLPC 1.8 Kbps codec *)
const HV_4_5K_CODEC     = $0008;  (* CELP 4.5 Kbps codec *)
const HV_3_0K_CODEC     = $0009;  (* CELP 3.0 Kbps codec *)
const HV_2_3K_CODEC       = $000a;  (* CELP 2.3 Kbps codec *)
const HV_VBR_2_4K_CODEC   = $000b;  (* Variable Bit Rate LPC-10 2.4 Kbps max codec*)

const HV_SILENCE_CODEC    = $001f;  (* Silence codec, used internally *)

(* Alternate codec names *)
const HV_LPC10_CODEC    = HV_2_4K_CODEC;
const HV_LPC_CODEC      = HV_4_8K_CODEC;
const HV_GSM_CODEC      = HV_13_2K_CODEC;
const HV_ADPCM_32_CODEC = HV_32K_CODEC;
const HV_PCM_64_CODEC   = HV_64K_CODEC;
const HV_G_711_CODEC    = HV_64K_CODEC;
const HV_ULAW_CODEC     = HV_64K_CODEC;
const HV_LPC_1_4_CODEC  = HV_1_4K_CODEC;
const HV_LPC_1_8_CODEC  = HV_1_8K_CODEC;
const HV_CELP_4_5_CODEC = HV_4_5K_CODEC;
const HV_CELP_3_0_CODEC = HV_3_0K_CODEC;
const HV_CELP_2_3_CODEC = HV_2_3K_CODEC;
const HV_VBR_LPC10_CODEC= HV_VBR_2_4K_CODEC;

(* VOX options *)
(* How many samples of silence to wait after voice stops. *)
(* You can use any value, these are just for reference. *)
const HVDI_VOX_FAST     = 4000;    (* 1/2 second *)
const HVDI_VOX_MEDIUM   = 8000;    (* 1 second *)
const HVDI_VOX_SLOW     =12000;    (* 1 1/2 seconds *)

(* hvdiHint options*)
const HVDI_NORMAL         = $0001;  (* Normal encoding/decoding speed, best quality, arg ignored *)
const HVDI_FAST           = $0002;  (* Faster encoding/decoding, some loss of quality
                                       with some codecs, arg ignored *)
const HVDI_FASTEST        = $0003;  (* Fastest possible encoding/decoding, more loss of quality,
                                       arg ignored *)
const HVDI_CELP_CODEBOOK  = $0004;  (* Directly change the CELP encoding codebook length, arg
                                       valid range 32 to 256 *)
const HVDI_SEQUENCE       = $0005;  (* Determines if the sequence number is sent in the packet.
                                       To disable, arg = 0, to enable (default), arg != 0 *)
const HVDI_AUTO_VOX       = $0006;  (* Enables automatic VOX processing inside hvdiPacketEncode.
                                       Default VOX setting is 300 unless first set by HVD_VOX_LEVEL *)
const HVDI_VOX_LEVEL      = $0007;  (* Sets the threshhold level when using HVDI_AUTO_VOX, arg
                                       valid range 0 to 1000, default 300 *)
const HVDI_VOX_SPEED      = $0008;  (* Sets the VOX speed, default HVDI_VOX_FAST *)
const HVDI_COMFORT_NOISE  = $0009;  (* Enables sending silence packets by the encoder, and creates
                                       white noise in the decoder *)
const HVDI_NOISE_LEVEL    = $000A;  (* Sets the decoder comfort noise level, 0 to 1000, default 100 *)

(* HawkVoiceDI API *)

var


  hvdiNewEncState: function: phvdi_enc_state ;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiNewDecState: function: phvdi_dec_state;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDeleteEncState: procedure(state:phvdi_enc_state);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDeleteDecState: procedure(state:phvdi_dec_state );{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiEncStateSetCodec: function(state:phvdi_enc_state;codec:Byte): Integer;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDecStateGetCodec: function(state:phvdi_dec_state): PChar;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiPacketIsVoice: function(var packet: Byte;length: Integer): Integer;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiPacketDecode: function(var packet: Byte;paclen: integer;var buffer: Smallint;
                                 buflen: Integer;key:phcrypt_key;state: phvdi_dec_state): Integer;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiPacketEncode: function(var packet: Byte;buflen: Integer;var buffer: Smallint;
                                 paclen: Integer;key:phcrypt_key;pstate:phvdi_enc_state): Integer;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiNewVOX: function(vox_speed, noisethreshold: integer): phvdi_vox;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiVOX:function(vox:phvdi_vox;var buffer: SmallInt;buflen: Integer): Integer;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDeleteVOX:procedure(vox:phvdi_vox);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiNewRate: function(inrate, outrate: Integer): phvdi_rate;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiRateFlow:procedure(rate:phvdi_rate;var inbuf, outbuf: SmallInt;var inlen: Integer;var outlen: Integer);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDeleteRate: procedure(var rate:phvdi_rate);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiNewAGC: function(level: single): phvdi_agc;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiAGC: procedure(agc:phvdi_agc;var buffer: SmallInt;len: integer);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiDeleteAGC: procedure(agc:phvdi_agc);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiMix:procedure(var outbuf: SmallInt; {short **}inbuf: Pointer;number, inlen: Integer);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hvdiHint: procedure(name, arg: Integer);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptNewSalt:function: phcrypt_salt;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptDeleteKey:procedure(key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptNewKey:function(const astring: Pchar; const salt: phcrypt_salt):phcrypt_key;{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptDeleteSalt: procedure(key:phcrypt_salt);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptEncryptPacket: procedure(bufferin: Pointer; bufferout: Pointer; buflen: Integer;
                                   key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptEncryptStream: procedure(bufferin: Pointer; bufferout: Pointer; buflen: Integer;
                                   key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptDecryptPacket: procedure(bufferin:Pointer;bufferout: Pointer;buflen: Integer;
                                   key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptDecryptStream: procedure(bufferin:Pointer;bufferout: Pointer;buflen: Integer;
                                   key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptSignPacket: procedure(bufferin: Pointer;buflen: Integer;key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}
  hcryptAuthenticatePacket: procedure(bufferin: Pointer;buflen: Integer;key:phcrypt_key);{$IFDEF WINDOWS}stdcall;{$ELSE} cdecl;{$ENDIF}



function nlSwapl(D: LongWord): Longword;
function nlSwaps(D: Word): Word;

const IsHawkVoiceInitialized: Boolean = False;


implementation

uses
  moduleloader;

const
{$IFDEF WINDOWS}
  HawkVoiceDLL = 'hvdi.dll';
{$ENDIF}
{$IFDEF UNIX}
{$IFDEF DARWIN}
  HawkVoiceDLL = 'hvdi.dylib';
{$ELSE}
  HawkVoiceDLL = 'libHVDI.so';
{$ENDIF}
{$ENDIF}


function nlSwaps(D: Word): Word;
begin
//  if SDL_BYTEORDER = SDL_BIG_ENDIAN then
    Result := ((Word(D) and $00FF) shl 8) or ((Word(D) and $FF00) shr 8)
//  else
//    Result := D;
end;

function nlSwapl(D: LongWord): Longword;
begin
//  if SDL_BYTEORDER = SDL_BIG_ENDIAN then
    Result := (((D and $000000FF) shl 24) or ((D and $0000FF00) shl 8)  or ((D and $00FF0000) shr 8)  or ((D and $FF000000) shr 24) )
//  else
//    Result := D;
end;


var
  vHawkVoiceHandle : TModuleHandle;

function InitHawkVoice(ADllName : PChar): Boolean;
begin
  if ADllName = '' then
   ADllName := HawkVoiceDLL;

  IsHawkVoiceInitialized := LoadModule( vHawkVoiceHandle, ADllName );
  Result := IsHawkVoiceInitialized;

  if IsHawkVoiceInitialized then
  begin
    @hvdiNewEncState := GetModuleSymbol(vHawkVoiceHandle, 'hvdiNewEncState');
    @hvdiNewDecState := GetModuleSymbol(vHawkVoiceHandle, 'hvdiNewDecState');
    @hvdiDeleteEncState := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDeleteEncState');
    @hvdiDeleteDecState := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDeleteDecState');
    @hvdiEncStateSetCodec := GetModuleSymbol(vHawkVoiceHandle, 'hvdiEncStateSetCodec');
    @hvdiDecStateGetCodec := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDecStateGetCodec');
    @hvdiPacketIsVoice := GetModuleSymbol(vHawkVoiceHandle, 'hvdiPacketIsVoice');
    @hvdiPacketDecode := GetModuleSymbol(vHawkVoiceHandle, 'hvdiPacketDecode');
    @hvdiPacketEncode := GetModuleSymbol(vHawkVoiceHandle, 'hvdiPacketEncode');
    @hvdiNewVOX := GetModuleSymbol(vHawkVoiceHandle, 'hvdiNewVOX');
    @hvdiVOX := GetModuleSymbol(vHawkVoiceHandle, 'hvdiVOX');
    @hvdiDeleteVOX := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDeleteVOX');
    @hvdiNewRate := GetModuleSymbol(vHawkVoiceHandle, 'hvdiNewRate');
    @hvdiRateFlow := GetModuleSymbol(vHawkVoiceHandle, 'hvdiRateFlow');
    @hvdiDeleteRate := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDeleteRate');
    @hvdiNewAGC := GetModuleSymbol(vHawkVoiceHandle, 'hvdiNewAGC');
    @hvdiAGC := GetModuleSymbol(vHawkVoiceHandle, 'hvdiAGC');
    @hvdiDeleteAGC := GetModuleSymbol(vHawkVoiceHandle, 'hvdiDeleteAGC');
    @hvdiMix := GetModuleSymbol(vHawkVoiceHandle, 'hvdiMix');
    @hvdiHint := GetModuleSymbol(vHawkVoiceHandle, 'hvdiHint');
    @hcryptNewSalt := GetModuleSymbol(vHawkVoiceHandle,'hcryptNewSalt');
    @hcryptDeleteKey := GetModuleSymbol(vHawkVoiceHandle, 'hcryptDeleteKey');
    @hcryptNewKey := GetModuleSymbol(vHawkVoiceHandle, 'hcryptNewKey');
    @hcryptDeleteSalt := GetModuleSymbol(vHawkVoiceHandle, 'hcryptDeleteSalt');
    @hcryptEncryptPacket := GetModuleSymbol(vHawkVoiceHandle, 'hcryptEncryptPacket');
    @hcryptEncryptStream := GetModuleSymbol(vHawkVoiceHandle, 'hcryptEncryptStream');
    @hcryptDecryptPacket := GetModuleSymbol(vHawkVoiceHandle, 'hcryptDecryptPacket');
    @hcryptDecryptStream := GetModuleSymbol(vHawkVoiceHandle, 'hcryptDecryptStream');
    @hcryptSignPacket := GetModuleSymbol(vHawkVoiceHandle, 'hcryptSignPacket');
    @hcryptAuthenticatePacket := GetModuleSymbol(vHawkVoiceHandle, 'hcryptAuthenticatePacket');
  end;
end;


procedure CloseHawkVoice;
begin
  IsHawkVoiceInitialized := False;
  UnLoadModule( vHawkVoiceHandle );
  @hvdiNewEncState := nil;
  @hvdiNewDecState := nil;
  @hvdiDeleteEncState := nil;
  @hvdiDeleteDecState := nil;
  @hvdiEncStateSetCodec := nil;
  @hvdiDecStateGetCodec := nil;
  @hvdiPacketIsVoice := nil;
  @hvdiPacketDecode := nil;
  @hvdiPacketEncode := nil;
  @hvdiNewVOX := nil;
  @hvdiVOX := nil;
  @hvdiDeleteVOX := nil;
  @hvdiNewRate := nil;
  @hvdiRateFlow := nil;
  @hvdiDeleteRate := nil;
  @hvdiNewAGC := nil;
  @hvdiAGC := nil;
  @hvdiDeleteAGC := nil;
  @hvdiMix := nil;
  @hvdiHint := nil;
  @hcryptNewSalt := nil;
  @hcryptDeleteKey := nil;
  @hcryptNewKey := nil;
  @hcryptDeleteSalt := nil;
  @hcryptEncryptPacket := nil;
  @hcryptEncryptStream := nil;
  @hcryptDecryptPacket := nil;
  @hcryptDecryptStream := nil;
  @hcryptSignPacket := nil;
  @hcryptAuthenticatePacket := nil;
end;

initialization

  InitHawkVoice( HawkVoiceDLL );

finalization

  CloseHawkVoice;

end.
