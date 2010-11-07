unit ulaw;

interface

//int ulawEncode(const short *in, unsigned char *out, int length);

function ulawEncode(const ain: array of SmallInt;var aout: Byte;length: Integer): Integer;
//int ulawDecode(const unsigned char *in, short *out, int length);
function ulawDecode(const ain: array of Byte; var aout: SmallInt;length: Integer): Integer;

function audio_s2u(x: SmallInt): Byte;
function audio_u2s(x: Byte): SmallInt;

function rawULawEncode(sample: smallInt): byte;

implementation

{------------------------------------------------------------------------------}
{Function to encode a SmallInt into a Byte using the ULaw algorithm.}
{------------------------------------------------------------------------------}
function rawULawEncode(sample: smallInt): byte;
const BIAS = 132;//0x84
const CLIP = 32635;//32767-BIAS
var
  sign,temp,mantis: smallInt;
  exp: Byte;
begin
  sign:= sample and $8000;
  if(sign <> 0) then begin
    sample:= sample * -1;
    sign:= $80;
  end;
  if(sample > CLIP) then sample:= CLIP;

  sample:= sample+ BIAS;
  temp:= sample shl 1;
  for exp:=7 downto 1 do begin
    if ((temp and $8000) <> 0) then break;
    temp:= temp shl 1;
  end;

  temp:= sample shr (exp + 3);
  mantis:= temp and $000f;
  result:= not (sign or (exp shl 4) or mantis);
end;

const u2s: array[0..255] of Word = (
        33280, 34308, 35336, 36364, 37393, 38421, 39449, 40477,
        41505, 42534, 43562, 44590, 45618, 46647, 47675, 48703,
        49474, 49988, 50503, 51017, 51531, 52045, 52559, 53073,
        53587, 54101, 54616, 55130, 55644, 56158, 56672, 57186,
        57572, 57829, 58086, 58343, 58600, 58857, 59114, 59371,
        59628, 59885, 60142, 60399, 60656, 60913, 61171, 61428,
        61620, 61749, 61877, 62006, 62134, 62263, 62392, 62520,
        62649, 62777, 62906, 63034, 63163, 63291, 63420, 63548,
        63645, 63709, 63773, 63838, 63902, 63966, 64030, 64095,
        64159, 64223, 64287, 64352, 64416, 64480, 64544, 64609,
        64657, 64689, 64721, 64753, 64785, 64818, 64850, 64882,
        64914, 64946, 64978, 65010, 65042, 65075, 65107, 65139,
        65163, 65179, 65195, 65211, 65227, 65243, 65259, 65275,
        65291, 65308, 65324, 65340, 65356, 65372, 65388, 65404,
        65416, 65424, 65432, 65440, 65448, 65456, 65464, 65472,
        65480, 65488, 65496, 65504, 65512, 65520, 65528,     0,
        32256, 31228, 30200, 29172, 28143, 27115, 26087, 25059,
        24031, 23002, 21974, 20946, 19918, 18889, 17861, 16833,
        16062, 15548, 15033, 14519, 14005, 13491, 12977, 12463,
        11949, 11435, 10920, 10406,  9892,  9378,  8864,  8350,
         7964,  7707,  7450,  7193,  6936,  6679,  6422,  6165,
         5908,  5651,  5394,  5137,  4880,  4623,  4365,  4108,
         3916,  3787,  3659,  3530,  3402,  3273,  3144,  3016,
         2887,  2759,  2630,  2502,  2373,  2245,  2116,  1988,
         1891,  1827,  1763,  1698,  1634,  1570,  1506,  1441,
         1377,  1313,  1249,  1184,  1120,  1056,   992,   927,
          879,   847,   815,   783,   751,   718,   686,   654,
          622,   590,   558,   526,   494,   461,   429,   397,
          373,   357,   341,   325,   309,   293,   277,   261,
          245,   228,   212,   196,   180,   164,   148,   132,
          120,   112,   104,    96,    88,    80,    72,    64,
           56,    48,    40,    32,    24,    16,    8,      0
);

function audio_s2u(x: SmallInt): Byte;
begin
  Result := rawULawEncode(x);
end;

function audio_u2s(x: Byte): SmallInt;
begin
  Result := u2s[Integer(x)];
end;

{------------------------------------------------------------------------------}
{Encode an array of SmallInt's into an array of Bytes}
{------------------------------------------------------------------------------}
function ulawEncode(const ain: array of SmallInt;var aout: Byte;length: Integer): Integer;
type TByteArray= array[0..Maxint div 4] of Byte;
     PByteArray = ^TByteArray;
var i: Integer;
begin
    for i:=0 to length-1 do
    begin
        PByteArray(@aout)^[i] := audio_s2u(ain[i]);
    end;
    Result := length;
end;
{------------------------------------------------------------------------------}
{Decode an array of Bytes into an array of smallints}
{------------------------------------------------------------------------------}
function ulawDecode(const ain: array of Byte;var aout: SmallInt;length: Integer): Integer;
type TSmallIntArray= array[0..Maxint div 4] of SmallInt;
     PSmallIntArray = ^TSmallIntArray;
var i: Integer;
begin
    for i:=0 to length-1 do
    begin
        PSmallIntArray(@aout)^[i] := audio_u2s(ain[i]);
    end;
    Result := length;
end;

end.

