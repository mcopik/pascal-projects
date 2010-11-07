program HawkVoiceDemo;
{$APPTYPE CONSOLE}
uses
  SysUtils,Classes,
  hvdi in '..\Pas\hvdi.pas',
  ulaw in '..\Pas\ulaw.pas';

const NL_MAX_PACKET_LENGTH  = 16384;
const NUM_SAMPLES   =  2880;

const AU_U_LAW          =  1;
const AU_PCM_16_BIT     =  3;
const AU_NOT_SUPPORTED  =  0;

type

  TAUHeader = record
    magic, hsize, dsize, emode, rate, nchan: Longint;
  end;


{------------------------------------------------------------------------------}
{Writes the header for the .au audio format}
{------------------------------------------------------------------------------}
procedure WriteHeader(Stream: TStream);
var header: TAUHeader;
begin
  header.magic := nlSwapl($2e736e64);
  header.hsize := nlSwapl(sizeof(TAUHeader));
  header.dsize := nlSwapl($ffffffff);
  header.emode := nlSwapl(AU_U_LAW);
  header.rate := nlSwapl(8000);
  header.nchan := nlSwapl(1);
  Stream.write(header, sizeof(header));
end;

{------------------------------------------------------------------------------}
{Reads the header for the .au audio format}
{------------------------------------------------------------------------------}
function ReadHeader(Stream: TStream): Integer;
var header: TAUHeader;
begin
  Result := AU_NOT_SUPPORTED;
  Stream.ReadBuffer(header, sizeof(header));
  header.emode := nlSwapl(header.emode);
  if(header.emode = AU_U_LAW) or (header.emode = AU_PCM_16_BIT) then
  begin
    Result := header.emode;
  end;
end;

{------------------------------------------------------------------------------}
{Swaps the samples }
{------------------------------------------------------------------------------}
procedure SwapSamples(var buffer: SmallInt;len: integer);
type TSmallIntArray= array[0..MAxint div 4] of SmallInt;
     PSmallIntArray = ^TSmallIntArray;
var i: Integer;
begin
  for i :=0 to len-1 do
  begin
     PSmallIntArray(@buffer)^[i] := nlSwaps(PSmallIntArray(@buffer)^[i]);
  end;
end;

{------------------------------------------------------------------------------}
{Read the sampes from the audio file}
{------------------------------------------------------------------------------}
function ReadSamples(Stream: TStream;_type: Integer;var buffer: SmallInt;len: Integer): Integer;
var count: integer;
    temp: array[0..NUM_SAMPLES-1] of Byte;
begin
  if(_type = AU_PCM_16_BIT) then
  begin
    (* no conversion needed, just swap *)
    count :=  Stream.Read(buffer, sizeof(SmallInt) * len);
    count := count div 2;
    swapSamples(buffer, count);
    result := count;
  end
  else
  begin
    (* convert the u-law samples to 16 bit *)
    count := Stream.Read(temp, len);
    ulawDecode(temp[0], buffer, count);
    result := count;
  end;
end;

{------------------------------------------------------------------------------}
{Write the ULaw encoded samples to the file}
{------------------------------------------------------------------------------}
procedure WriteSamples(Stream: TStream;const buffer: array of SmallInt;len: Integer);
var  temp: array[0..NUM_SAMPLES*3] of Byte;
begin
  ulawEncode(buffer, temp[0], len);
  Stream.write(temp, len);
end;

var _type: integer;
    buflen: Integer;
    samples: array[0..NUM_SAMPLES-1] of SmallInt;
    encodedlen, paclen, valid, decodedlen: Integer;
    packet:array[0..NL_MAX_PACKET_LENGTH-1] of Byte;
    decoded:array[0..(NUM_SAMPLES * 2)-1] of SmallInt;
    salt: phcrypt_salt;
    key: phcrypt_key;
    agc: phvdi_agc;
    vox: phvdi_vox;
    infile, outfile: TFileStream;
    encstate: phvdi_enc_state;
    decstate: phvdi_dec_state;
    selectedCodec: Byte;
    keyinput: string;

begin
  if ParamCount <> 2 then
  begin
    Writeln('NL Hawk Voice Demo');
    Writeln('Usage:');
    Writeln('HawkVoiceDemo <inputfile> <outputfile>');
    Writeln('Input and Output file must be .au files!');
    Exit;
  end;
  if IsHawkVoiceInitialized then
  begin
    Writeln('NL Hawk Voice Available');
    salt := hcryptNewSalt();

    key :=  hcryptNewKey('HawkVoiceROCKS!!', salt);

    inFile := TFileStream.Create(ParamStr(1), fmOpenRead);
    _type := readHeader(infile);

    if _type = AU_NOT_SUPPORTED then
    begin
      Writeln('AU Format Not Supported.');
      Exit; 
    end;

    hvdiHint(HVDI_NORMAL, 0);

    outfile := TFileStream.Create(ParamStr(2), fmCreate or fmOpenWrite);
    writeHeader(outfile);
    encstate := hvdiNewEncState();
    decstate := hvdiNewDecState();

    Writeln('Select CODEC to Test');
    Writeln;
    Writeln('1: LPC-10 2.4 Kbps codec ');
    Writeln('2: LPC 4.8 Kbps codec ');
    Writeln('3: GSM 13.2 Kbps codec ');
    Writeln('4: Intel/DVI ADPCM 32 Kbps codec ');
    Writeln('5: G.711 u-law 64 Kbps codec ');
    Writeln('6: OpenLPC 1.4 Kbps codec ');
    Writeln('7: OpenLPC 1.8 Kbps codec ');
    Writeln('8: CELP 4.5 Kbps codec ');
    Writeln('9: CELP 3.0 Kbps codec ');
    Writeln('10: CELP 2.3 Kbps codec ');
    Writeln('11: Variable Bit Rate LPC-10 2.4 Kbps max codec');
    Readln(keyinput);
    case StrToIntDef(keyinput, 1) of
      1:selectedCodec:= HV_2_4K_CODEC;
      2:selectedCodec:= HV_4_8K_CODEC;
      3:selectedCodec:= HV_13_2K_CODEC;
      4:selectedCodec:= HV_32K_CODEC;
      5:selectedCodec:= HV_64K_CODEC;
      6:selectedCodec:= HV_1_4K_CODEC;
      7:selectedCodec:= HV_1_8K_CODEC;
      8:selectedCodec:= HV_4_5K_CODEC;
      9:selectedCodec:= HV_3_0K_CODEC;
      10:selectedCodec:= HV_2_3K_CODEC;
      11:selectedCodec:= HV_VBR_2_4K_CODEC;
    else
      selectedCodec:= HV_LPC_1_8_CODEC;
    end;

    hvdiEncStateSetCodec(encstate, selectedCodec);

    agc := hvdiNewAGC(0.9);
    vox := hvdiNewVOX(HVDI_VOX_FAST, 600);

    // hints go here
    hvdiHint(HVDI_FASTEST, 0);
    hvdiHint(HVDI_SEQUENCE, 1);
    hvdiHint(HVDI_AUTO_VOX, 1);
    hvdiHint(HVDI_VOX_LEVEL, 300);
    hvdiHint(HVDI_COMFORT_NOISE, 1);
    hvdiHint(HVDI_NOISE_LEVEL, 100);

    buflen := NUM_SAMPLES;
    paclen := NL_MAX_PACKET_LENGTH-1;


    Writeln('Encoding '+ExtractFileName(ParamStr(1)));

    while buflen = NUM_SAMPLES do
    begin
     Write('.');
     buflen := readSamples(infile, _type, samples[0], NUM_SAMPLES);
     hvdiAGC(agc, samples[0], buflen);
     hvdiVOX(vox, samples[0], buflen);

     encodedlen := hvdiPacketEncode(packet[0], buflen, samples[0], paclen, key, encstate);
     valid := hvdiPacketIsVoice(packet[0], encodedlen);
     if valid = NL_TRUE then
     begin
       decodedlen := hvdiPacketDecode(packet[0], encodedlen, decoded[0], NUM_SAMPLES * 2, key, decstate);
        writeSamples(outfile, decoded, decodedlen);
     end;
    end;
    Writeln('Complete');

    hvdiDeleteDecState(decstate);
    hvdiDeleteEncState(encstate);

    hvdiDeleteVOX(vox);
    hvdiDeleteAGC(agc);

    outfile.Free;
    infile.FRee;

    hcryptDeleteKey(key);
    hcryptDeleteSalt(salt);
  end
  else
  begin
    Writeln('Could not load Hawk Voice Library.');
  end;
end.
