{******************************************************************************}
{                                                                              }
{ Linux zlib API Interface Unit                                                }
{                                                                              }
{                                                                              }
{ Translator: Matthias Thoma                                                   }
{                                                                              }
{                                                                              }
{ Portions created by Jean-loup Gailly and Mark Adler are                      }
{ Copyright (C) 1995-1998 Jean-loup Gailly (jloup@gzip.org) and                }
{ Mark Adler (madler@alumni.cal­tech.edu)                                      }
{                                                                              }
{ The original file is: zlib.h, released 9 July 1998.                          }
{ The original Pascal code is: ZLib.pas, released 01 Feb 2001.                 }
{ The initial developer of the Pascal code is Matthias Thoma                   }
{ (ma.thoma@gmx.de).                                                           }
{                                                                              }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2001 Matthias Thoma.                                           }
{                                                                              }
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
{******************************************************************************}

unit ZLib;

interface

uses
  LibC;

{$HPPEMIT '#include <zlib.h>'}

{
  zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.3, July 9th, 1998

  Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files ftp://ds.internic.net/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).
}

const
  ZLIB_VERSION = '1.1.4';
{$EXTERNALSYM ZLIB_VERSION}

{ The 'zlib' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

     Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap'ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio.

     The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input. }


type
  TAlloc = function(opaque: Pointer; Items, Size: Integer): Pointer; cdecl;
  TFree = procedure(opaque, address: Pointer); cdecl;


type
  PZStreamRec = ^TZStreamRec;
  z_stream = packed record
    next_in: PChar; // next input byte
    avail_in: Cardinal; // number of bytes available at next_in
    total_in: Cardinal; // total nb of input bytes read so far

    next_out: PChar; // next output byte should be put here
    avail_out: Cardinal; // remaining free space at next_out
    total_out: Cardinal; // total nb of bytes output so far

    msg: PChar; // last error message, NULL if no error

    internal_state: Pointer; // not visible by applications

    zalloc: TAlloc; // used to allocate the internal state
    zfree: TFree; // used to free the internal state
    opaque: Pointer; // private data object passed to zalloc and zfree

    data_type: Integer; //  best guess about the data type: ascii or binary
    adler: Cardinal; // adler32 value of the uncompressed data
    reserved: Cardinal; // reserved for future use
  end;
  TZStreamRec = z_stream;
{$EXTERNALSYM z_stream}
  z_streamp = PZStreamRec;
{$EXTERNALSYM z_streamp}

{  The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   If zlib is used in a multi-threaded application, zalloc and zfree must be
   thread safe.

   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step). }

                              { constants }

const
  Z_NO_FLUSH = 0;
{$EXTERNALSYM Z_NO_FLUSH}
  Z_PARTIAL_FLUSH = 1; // will be removed, use Z_SYNC_FLUSH instead
{$EXTERNALSYM Z_PARTIAL_FLUSH}
  Z_SYNC_FLUSH = 2;
{$EXTERNALSYM Z_SYNC_FLUSH}
  Z_FULL_FLUSH = 3;
{$EXTERNALSYM Z_FULL_FLUSH}
  Z_FINISH = 4;
{$EXTERNALSYM Z_FINISH}

// Allowed flush values; see deflate() below for details

  Z_OK = 0;
{$EXTERNALSYM Z_OK}
  Z_STREAM_END = 1;
{$EXTERNALSYM Z_STREAM_END}
  Z_NEED_DICT = 2;
{$EXTERNALSYM Z_NEED_DICT}
  Z_ERRNO = -1;
{$EXTERNALSYM Z_ERRNO}
  Z_STREAM_ERROR = -2;
{$EXTERNALSYM Z_STREAM_ERROR}
  Z_DATA_ERROR = -3;
{$EXTERNALSYM Z_DATA_ERROR}
  Z_MEM_ERROR = -4;
{$EXTERNALSYM Z_MEM_ERROR}
  Z_BUF_ERROR = -5;
{$EXTERNALSYM Z_BUF_ERROR}
  Z_VERSION_ERROR = -6;
{$EXTERNALSYM Z_VERSION_ERROR}

{ Return codes for the compression/decompression functions. Negative
  values are errors, positive values are used for special but normal events. }

  Z_NO_COMPRESSION = 0;
{$EXTERNALSYM Z_NO_COMPRESSION}
  Z_BEST_SPEED = 1;
{$EXTERNALSYM Z_BEST_SPEED}
  Z_BEST_COMPRESSION = 9;
{$EXTERNALSYM Z_BEST_COMPRESSION}
  Z_DEFAULT_COMPRESSION = -1;
{$EXTERNALSYM Z_DEFAULT_COMPRESSION}

// compression levels
  Z_FILTERED = 1;
{$EXTERNALSYM Z_FILTERED}
  Z_HUFFMAN_ONLY = 2;
{$EXTERNALSYM Z_HUFFMAN_ONLY}
  Z_DEFAULT_STRATEGY = 0;
{$EXTERNALSYM Z_DEFAULT_STRATEGY}

// compression strategy; see deflateInit2() below for details
  Z_BINARY = 0;
{$EXTERNALSYM Z_BINARY}
  Z_ASCII = 1;
{$EXTERNALSYM Z_ASCII}
  Z_UNKNOWN = 2;
{$EXTERNALSYM Z_UNKNOWN}

// Possible values of the data_type field
  Z_DEFLATED = 8;
{$EXTERNALSYM Z_DEFLATED}

// The deflate compression method (the only one supported in this version)
  Z_NULL = 0; // for initializing zalloc, zfree, opaque
{$EXTERNALSYM Z_NULL}

                        { basic functions }

function zlibVersion: PChar; cdecl;
{$EXTERNALSYM zlibVersion}

{ The application can compare zlibVersion and ZLIB_VERSION for consistency.
   If the first character differs, the library code actually used is
   not compatible with the zlib.h header file used by the application.
   This check is automatically made by deflateInit and inflateInit. }

function deflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;

{$EXTERNALSYM deflate}
{ deflate compresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce some
  output latency (reading input without producing any output) except when
  forced to flush.

    The detailed semantics are as follows. deflate performs one or both of the
  following actions:

  - Compress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in and avail_in are updated and
    processing will resume at this point for the next call of deflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly. This action is forced if the parameter flush is non zero.
    Forcing flush frequently degrades the compression ratio, so this parameter
    should be set only when necessary (in interactive applications).
    Some output may be provided even if flush is not set.

  Before the call of deflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating avail_in or avail_out accordingly; avail_out
  should never be zero before the call. The application can consume the
  compressed output when it wants, for example when the output buffer is full
  (avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
  and with zero avail_out, it must be called again after making room in the
  output buffer because there might be more output pending.

    If the parameter flush is set to Z_SYNC_FLUSH, all pending output is
  flushed to the output buffer and the output is aligned on a byte boundary, so
  that the decompressor can get all input data available so far. (In particular
  avail_in is zero after the call if enough output space has been provided
  before the call.)  Flushing may degrade compression for some compression
  algorithms and so it should be used only when necessary.

    If flush is set to Z_FULL_FLUSH, all output is flushed as with
  Z_SYNC_FLUSH, and the compression state is reset so that decompression can
  restart from this point if previous compressed data has been damaged or if
  random access is desired. Using Z_FULL_FLUSH too often can seriously degrade
  the compression.

    If deflate returns with avail_out == 0, this function must be called again
  with the same value of the flush parameter and more output space (updated
  avail_out), until the flush is complete (deflate returns with non-zero
  avail_out).

    If the parameter flush is set to Z_FINISH, pending input is processed,
  pending output is flushed and deflate returns with Z_STREAM_END if there
  was enough output space; if deflate returns with Z_OK, this function must be
  called again with Z_FINISH and more output space (updated avail_out) but no
  more input data, until it returns with Z_STREAM_END or an error. After
  deflate has returned Z_STREAM_END, the only possible operations on the
  stream are deflateReset or deflateEnd.

    Z_FINISH can be used immediately after deflateInit if all the compression
  is to be done in a single step. In this case, avail_out must be at least
  0.1% larger than avail_in plus 12 bytes.  If deflate does not return
  Z_STREAM_END, then it must be called again as described above.

    deflate() sets strm->adler to the adler32 checksum of all input read
  so far (that is, total_in bytes).

    deflate() may update data_type if it can make a good guess about
  the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
  binary. This field is only for information purposes and does not affect
  the compression algorithm in any manner.

    deflate() returns Z_OK if some progress has been made (more input
  processed or more output produced), Z_STREAM_END if all input has been
  consumed and all output has been produced (only when flush is set to
  Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
  if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible
  (for example avail_in or avail_out was zero). }

function deflateEnd(var strm: TZStreamRec): Integer; cdecl;

{$EXTERNALSYM deflateEnd}
{  All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
   stream state was inconsistent, Z_DATA_ERROR if the stream was freed
   prematurely (some input or output was discarded). In the error case,
   msg may be set but then points to a static string (which must not be
   deallocated). }

{
ZEXTERN int ZEXPORT deflateInit OF((z_streamp strm, int level));

     Initializes the internal stream state for compression. The fields
   zalloc, zfree and opaque must be initialized before by the caller.
   If zalloc and zfree are set to Z_NULL, deflateInit updates them to
   use default allocation functions.

     The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
   1 gives best speed, 9 gives best compression, 0 gives no compression at
   all (the input data is simply copied a block at a time).
   Z_DEFAULT_COMPRESSION requests a default compromise between speed and
   compression (currently equivalent to level 6).

     deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if level is not a valid compression level,
   Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
   with the version assumed by the caller (ZLIB_VERSION).
   msg is set to null if there is no error message.  deflateInit does not
   perform any compression: this will be done by deflate().
}

function inflateEnd(var strm: TZStreamRec): Integer; cdecl;
{$EXTERNALSYM inflateEnd}

{  All dynamically allocated data structures for this stream are freed.
   This function discards any unprocessed input and does not flush any
   pending output.

     deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
   stream state was inconsistent, Z_DATA_ERROR if the stream was freed
   prematurely (some input or output was discarded). In the error case,
   msg may be set but then points to a static string (which must not be
   deallocated). }

{
ZEXTERN int ZEXPORT inflateInit OF((z_streamp strm));

     Initializes the internal stream state for decompression. The fields
   next_in, avail_in, zalloc, zfree and opaque must be initialized before by
   the caller. If next_in is not Z_NULL and avail_in is large enough (the exact
   value depends on the compression method), inflateInit determines the
   compression method from the zlib header and allocates all data structures
   accordingly; otherwise the allocation will be deferred to the first call of
   inflate.  If zalloc and zfree are set to Z_NULL, inflateInit updates them to
   use default allocation functions.

     inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_VERSION_ERROR if the zlib library version is incompatible with the
   version assumed by the caller.  msg is set to null if there is no error
   message. inflateInit does not perform any decompression apart from reading
   the zlib header if present: this will be done by inflate().  (So next_in and
   avail_in may be modified, but next_out and avail_out are unchanged.)}

function inflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;
{$EXTERNALSYM inflate}

{ inflate decompresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may some
  introduce some output latency (reading input without producing any output)
  except when forced to flush.

  The detailed semantics are as follows. inflate performs one or both of the
  following actions:

  - Decompress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in is updated and processing
    will resume at this point for the next call of inflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly.  inflate() provides as much output as possible, until there
    is no more input data or no more space in the output buffer (see below
    about the flush parameter).

  Before the call of inflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating the next_* and avail_* values accordingly.
  The application can consume the uncompressed output when it wants, for
  example when the output buffer is full (avail_out == 0), or after each
  call of inflate(). If inflate returns Z_OK and with zero avail_out, it
  must be called again after making room in the output buffer because there
  might be more output pending.

    If the parameter flush is set to Z_SYNC_FLUSH, inflate flushes as much
  output as possible to the output buffer. The flushing behavior of inflate is
  not specified for values of the flush parameter other than Z_SYNC_FLUSH
  and Z_FINISH, but the current implementation actually flushes as much output
  as possible anyway.

    inflate() should normally be called until it returns Z_STREAM_END or an
  error. However if all decompression is to be performed in a single step
  (a single call of inflate), the parameter flush should be set to
  Z_FINISH. In this case all pending input is processed and all pending
  output is flushed; avail_out must be large enough to hold all the
  uncompressed data. (The size of the uncompressed data may have been saved
  by the compressor for this purpose.) The next operation on this stream must
  be inflateEnd to deallocate the decompression state. The use of Z_FINISH
  is never required, but can be used to inform inflate that a faster routine
  may be used for the single inflate() call.

     If a preset dictionary is needed at this point (see inflateSetDictionary
  below), inflate sets strm-adler to the adler32 checksum of the
  dictionary chosen by the compressor and returns Z_NEED_DICT; otherwise
  it sets strm->adler to the adler32 checksum of all output produced
  so far (that is, total_out bytes) and returns Z_OK, Z_STREAM_END or
  an error code as described below. At the end of the stream, inflate()
  checks that its computed adler32 checksum is equal to that saved by the
  compressor and returns Z_STREAM_END only if the checksum is correct.

    inflate() returns Z_OK if some progress has been made (more input processed
  or more output produced), Z_STREAM_END if the end of the compressed data has
  been reached and all uncompressed output has been produced, Z_NEED_DICT if a
  preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
  corrupted (input stream not conforming to the zlib format or incorrect
  adler32 checksum), Z_STREAM_ERROR if the stream structure was inconsistent
  (for example if next_in or next_out was NULL), Z_MEM_ERROR if there was not
  enough memory, Z_BUF_ERROR if no progress is possible or if there was not
  enough room in the output buffer when Z_FINISH is used. In the Z_DATA_ERROR
  case, the application may then call inflateSync to look for a good
  compression block. }

                        { Advanced functions }

{    The following functions are needed only in some special applications. }

{
ZEXTERN int ZEXPORT deflateInit2 OF((z_streamp strm,
                                     int  level,
                                     int  method,
                                     int  windowBits,
                                     int  memLevel,
                                     int  strategy));

     This is another version of deflateInit with more compression options. The
   fields next_in, zalloc, zfree and opaque must be initialized before by
   the caller.

     The method parameter is the compression method. It must be Z_DEFLATED in
   this version of the library.

     The windowBits parameter is the base two logarithm of the window size
   (the size of the history buffer).  It should be in the range 8..15 for this
   version of the library. Larger values of this parameter result in better
   compression at the expense of memory usage. The default value is 15 if
   deflateInit is used instead.

     The memLevel parameter specifies how much memory should be allocated
   for the internal compression state. memLevel=1 uses minimum memory but
   is slow and reduces compression ratio; memLevel=9 uses maximum memory
   for optimal speed. The default value is 8. See zconf.h for total memory
   usage as a function of windowBits and memLevel.

     The strategy parameter is used to tune the compression algorithm. Use the
   value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
   filter (or predictor), or Z_HUFFMAN_ONLY to force Huffman encoding only (no
   string match).  Filtered data consists mostly of small values with a
   somewhat random distribution. In this case, the compression algorithm is
   tuned to compress them better. The effect of Z_FILTERED is to force more
   Huffman coding and less string matching; it is somewhat intermediate
   between Z_DEFAULT and Z_HUFFMAN_ONLY. The strategy parameter only affects
   the compression ratio but not the correctness of the compressed output even
   if it is not set appropriately.

      deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as an invalid
   method). msg is set to null if there is no error message.  deflateInit2 does
   not perform any compression: this will be done by deflate().
}

function deflateSetDictionary(var strm: TZStreamRec; var dictionary: Byte; dictLength: Integer): Integer; cdecl;

{$EXTERNALSYM deflateSetDictionary}
{
     Initializes the compression dictionary from the given byte sequence
   without producing any compressed output. This function must be called
   immediately after deflateInit, deflateInit2 or deflateReset, before any
   call of deflate. The compressor and decompressor must use exactly the same
   dictionary (see inflateSetDictionary).

     The dictionary should consist of strings (byte sequences) that are likely
   to be encountered later in the data to be compressed, with the most commonly
   used strings preferably put towards the end of the dictionary. Using a
   dictionary is most useful when the data to be compressed is short and can be
   predicted with good accuracy; the data can then be compressed better than
   with the default empty dictionary.

     Depending on the size of the compression data structures selected by
   deflateInit or deflateInit2, a part of the dictionary may in effect be
   discarded, for example if the dictionary is larger than the window size in
   deflate or deflate2. Thus the strings most likely to be useful should be
   put at the end of the dictionary, not at the front.

     Upon return of this function, strm->adler is set to the Adler32 value
   of the dictionary; the decompressor may later use this value to determine
   which dictionary has been used by the compressor. (The Adler32 value
   applies to the whole dictionary even if only a subset of the dictionary is
   actually used by the compressor.)

     deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent (for example if deflate has already been called for this stream
   or if the compression method is bsort). deflateSetDictionary does not
   perform any compression: this will be done by deflate().
}

function deflateCopy(var dest: TZStreamRec; var source: TZStreamRec): Integer; cdecl;

{$EXTERNALSYM deflateCopy}
{
     Sets the destination stream as a complete copy of the source stream.

     This function can be useful when several compression strategies will be
   tried, for example when there are several ways of pre-processing the input
   data with a filter. The streams that will be discarded should then be freed
   by calling deflateEnd.  Note that deflateCopy duplicates the internal
   compression state which can be quite large, so this strategy is slow and
   can consume lots of memory.

     deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
   (such as zalloc being NULL). msg is left unchanged in both source and
   destination.
}

function deflateReset(var strm: TZStreamRec): Integer; cdecl;
{$EXTERNALSYM deflateReset}

{  This function is equivalent to deflateEnd followed by deflateInit,
   but does not free and reallocate all the internal compression state.
   The stream will keep the same compression level and any other attributes
   that may have been set by deflateInit2.

      deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).}

function deflateParams(var strm: TZStreamRec; level: Integer; strategy: Integer): Integer; cdecl;
{$EXTERNALSYM deflateParams}

{  Dynamically update the compression level and compression strategy.  The
   interpretation of level and strategy is as in deflateInit2.  This can be
   used to switch between compression and straight copy of the input data, or
   to switch to a different kind of input data requiring a different
   strategy. If the compression level is changed, the input available so far
   is compressed with the old level (and may be flushed); the new level will
   take effect only at the next call of deflate().

     Before the call of deflateParams, the stream state must be set as for
   a call of deflate(), since the currently available input may have to
   be compressed and flushed. In particular, strm->avail_out must be non-zero.

     deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
   stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
   if strm->avail_out was zero.}

{
ZEXTERN int ZEXPORT inflateInit2 OF((z_streamp strm,
                                     int  windowBits));

     This is another version of inflateInit with an extra parameter. The
   fields next_in, avail_in, zalloc, zfree and opaque must be initialized
   before by the caller.

     The windowBits parameter is the base two logarithm of the maximum window
   size (the size of the history buffer).  It should be in the range 8..15 for
   this version of the library. The default value is 15 if inflateInit is used
   instead. If a compressed stream with a larger window size is given as
   input, inflate() will return with the error code Z_DATA_ERROR instead of
   trying to allocate a larger window.

      inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
   memLevel). msg is set to null if there is no error message.  inflateInit2
   does not perform any decompression apart from reading the zlib header if
   present: this will be done by inflate(). (So next_in and avail_in may be
   modified, but next_out and avail_out are unchanged.)
}

function inflateSetDictionary(var strm: TZStreamRec; var dictionary: byte; dictLength: Cardinal): Integer; cdecl;
{$EXTERNALSYM inflateSetDictionary}

{  Initializes the decompression dictionary from the given uncompressed byte
   sequence. This function must be called immediately after a call of inflate
   if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
   can be determined from the Adler32 value returned by this call of
   inflate. The compressor and decompressor must use exactly the same
   dictionary (see deflateSetDictionary).

     inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
   parameter is invalid (such as NULL dictionary) or the stream state is
   inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
   expected one (incorrect Adler32 value). inflateSetDictionary does not
   perform any decompression: this will be done by subsequent calls of
   inflate().}

function inflateSync(var strm: TZStreamRec): Integer; cdecl;
{$EXTERNALSYM inflateSync}

{  Skips invalid compressed data until a full flush point (see above the
  description of deflate with Z_FULL_FLUSH) can be found, or until all
  available input is skipped. No output is provided.

    inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
  if no more input was provided, Z_DATA_ERROR if no flush point has been found,
  or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
  case, the application may save the current current value of total_in which
  indicates where valid compressed data was found. In the error case, the
  application may repeatedly call inflateSync, providing more input each time,
  until success or end of the input data.
}
function inflateReset(var strm: TZStreamRec): Integer; cdecl;
{$EXTERNALSYM inflateReset}

{
     This function is equivalent to inflateEnd followed by inflateInit,
   but does not free and reallocate all the internal decompression state.
   The stream will keep attributes that may have been set by inflateInit2.

      inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
   stream state was inconsistent (such as zalloc or state being NULL).
}

                        { utility functions }

{
     The following utility functions are implemented on top of the
   basic stream-oriented functions. To simplify the interface, some
   default options are assumed (compression level and memory usage,
   standard memory allocation functions). The source code of these
   utility functions can easily be modified if you need special options.
}

function compress(dest: Pointer; var destLen: Cardinal; source: Pointer; var sourceLen: Cardinal): Integer; cdecl;
{$EXTERNALSYM compress}

{  Compresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be at least 0.1% larger than
   sourceLen plus 12 bytes. Upon exit, destLen is the actual size of the
   compressed buffer.
     This function can be used to compress a whole file at once if the
   input file is mmap'ed.
     compress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer.}

function compress2(dest: Pointer; var destLen: Cardinal; source: Pointer; var sourceLen: Cardinal; level: Integer): Integer; cdecl;
{
     Compresses the source buffer into the destination buffer. The level
   parameter has the same meaning as in deflateInit.  sourceLen is the byte
   length of the source buffer. Upon entry, destLen is the total size of the
   destination buffer, which must be at least 0.1% larger than sourceLen plus
   12 bytes. Upon exit, destLen is the actual size of the compressed buffer.

     compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_BUF_ERROR if there was not enough room in the output buffer,
   Z_STREAM_ERROR if the level parameter is invalid.
}

function uncompress(dest: Pointer; var destLen: Cardinal; source: Pointer; var sourceLen: Cardinal): Integer; cdecl;
{$EXTERNALSYM uncompress}

{
     Decompresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be large enough to hold the
   entire uncompressed data. (The size of the uncompressed data must have
   been saved previously by the compressor and transmitted to the decompressor
   by some mechanism outside the scope of this compression library.)
   Upon exit, destLen is the actual size of the compressed buffer.
     This function can be used to decompress a whole file at once if the
   input file is mmap'ed.

     uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer, or Z_DATA_ERROR if the input data was corrupted.
}

type
  TGZFile = TIOFile;

function gzopen(path: PChar; mode: PChar): TGZFile; cdecl;
{$EXTERNALSYM gzopen}

{
     Opens a gzip (.gz) file for reading or writing. The mode parameter
   is as in fopen ("rb" or "wb") but can also include a compression level
   ("wb9") or a strategy: 'f' for filtered data as in "wb6f", 'h' for
   Huffman only compression as in "wb1h". (See the description
   of deflateInit2 for more information about the strategy parameter.)

     gzopen can be used to read a file which is not in gzip format; in this
   case gzread will directly read from the file without decompression.

     gzopen returns NULL if the file could not be opened or if there was
   insufficient memory to allocate the (de)compression state; errno
   can be checked to distinguish the two cases (if errno is zero, the
   zlib error is Z_MEM_ERROR).  }

function gzdopen(fd: Integer; mode: PChar): TGZFile; cdecl;
{$EXTERNALSYM gzdopen}

{
     gzdopen() associates a gzFile with the file descriptor fd.  File
   descriptors are obtained from calls like open, dup, creat, pipe or
   fileno (in the file has been previously opened with fopen).
   The mode parameter is as in gzopen.
     The next call of gzclose on the returned gzFile will also close the
   file descriptor fd, just like fclose(fdopen(fd), mode) closes the file
   descriptor fd. If you want to keep fd open, use gzdopen(dup(fd), mode).
     gzdopen returns NULL if there was insufficient memory to allocate
   the (de)compression state.
}

function gzsetparams(gzfile: TGZFile; level: Integer; strategy: Integer): Integer; cdecl;
{$EXTERNALSYM gzsetparams}

{
   Dynamically update the compression level or strategy. See the description
   of deflateInit2 for the meaning of these parameters.
   gzsetparams returns Z_OK if success, or Z_STREAM_ERROR if the file was not
   opened for writing.
}

function gzread(gzfile: TGZFile; buf: Pointer; len: Cardinal): Integer; cdecl;
{$EXTERNALSYM gzread}

{
     Reads the given number of uncompressed bytes from the compressed file.
   If the input file was not in gzip format, gzread copies the given number
   of bytes into the buffer.
     gzread returns the number of uncompressed bytes actually read (0 for
   end of file, -1 for error). }

function gzwrite(gzfile: TGZFile; buf: Pointer; len: Cardinal): Integer; cdecl;
{$EXTERNALSYM gzwrite}
{
     Writes the given number of uncompressed bytes into the compressed file.
   gzwrite returns the number of uncompressed bytes actually written
   (0 in case of error).
}

// Note: NOT Converted!
// ZEXTERN int ZEXPORTVA   gzprintf OF((gzFile file, const char *format, ...));
{
     Converts, formats, and writes the args to the compressed file under
   control of the format string, as in fprintf. gzprintf returns the number of
   uncompressed bytes actually written (0 in case of error).
}

function gzputs(gzfile: TGZFile; s: PChar): Integer; cdecl;
{$EXTERNALSYM gzputs}
{
      Writes the given null-terminated string to the compressed file, excluding
   the terminating null character.
      gzputs returns the number of characters written, or -1 in case of error.
}

function gzgets(gzfile: TGZFile; buf: Pointer; len: Integer): PChar; cdecl;
{$EXTERNALSYM gzgets}
{
      Reads bytes from the compressed file until len-1 characters are read, or
   a newline character is read and transferred to buf, or an end-of-file
   condition is encountered.  The string is then terminasted with a null
   character.
      gzgets returns buf, or Z_NULL in case of error.
}

function gzputc(gzfile: TGZFile; c: Integer): Integer; cdecl;
{$EXTERNALSYM gzputc}
{
      Writes c, converted to an unsigned char, into the compressed file.
   gzputc returns the value that was written, or -1 in case of error.
}

function gzgetc(gzfile: TGZFile): Integer; cdecl;
{$EXTERNALSYM gzgetc}
{
      Reads one byte from the compressed file. gzgetc returns this byte
   or -1 in case of end of file or error.
}

function gzflush(gzfile: TGZFile; flush: Integer): Integer; cdecl;
{$EXTERNALSYM gzflush}
{
     Flushes all pending output into the compressed file. The parameter
   flush is as in the deflate() function. The return value is the zlib
   error number (see function gzerror below). gzflush returns Z_OK if
   the flush parameter is Z_FINISH and all output could be flushed.
     gzflush should be called only when strictly necessary because it can
   degrade compression.
}

function gzseek(gzfile: TGZFile; offset: Integer; whence: Integer): Integer; cdecl;
{$EXTERNALSYM gzseek}
{
      Sets the starting position for the next gzread or gzwrite on the
   given compressed file. The offset represents a number of bytes in the
   uncompressed data stream. The whence parameter is defined as in lseek(2);
   the value SEEK_END is not supported.
     If the file is opened for reading, this function is emulated but can be
   extremely slow. If the file is opened for writing, only forward seeks are
   supported; gzseek then compresses a sequence of zeroes up to the new
   starting position.

      gzseek returns the resulting offset location as measured in bytes from
   the beginning of the uncompressed stream, or -1 in case of error, in
   particular if the file is opened for writing and the new starting position
   would be before the current position.
}

function gzrewind(gzfile: TGZFile): Integer; cdecl;
{$EXTERNALSYM gzrewind}
{
     Rewinds the given file. This function is supported only for reading.

   gzrewind(file) is equivalent to (int)gzseek(file, 0L, SEEK_SET)
}

function gztell(gzfile: TGZFile): Integer; cdecl;
{$EXTERNALSYM gztell}
{
     Returns the starting position for the next gzread or gzwrite on the
   given compressed file. This position represents a number of bytes in the
   uncompressed data stream.

   gztell(file) is equivalent to gzseek(file, 0L, SEEK_CUR)
}

function gzeof(gzfile: TGZFile): Integer; cdecl;
{$EXTERNALSYM gzeof}
{
     Returns 1 when EOF has previously been detected reading the given
   input stream, otherwise zero.
}

function gzclose(gzfile: TGZFile): Integer; cdecl;
{$EXTERNALSYM gzclose}
{
     Flushes all pending output if necessary, closes the compressed file
   and deallocates all the (de)compression state. The return value is the zlib
   error number (see function gzerror below).
}

function gzerror(gzfile: TGZFile; var errnum: Integer): PChar; cdecl;
{$EXTERNALSYM gzerror}
{    Returns the error message for the last error which occurred on the
   given compressed file. errnum is set to zlib error number. If an
   error occurred in the file system and not in the compression library,
   errnum is set to Z_ERRNO and the application may consult errno
   to get the exact error code. }

                        { checksum functions }

{
     These functions are not related to compression but are exported
   anyway because they might be useful in applications using the
   compression library.
}

function adler32(adler: Cardinal; buf: Pointer; len: Cardinal): Cardinal; cdecl;
{$EXTERNALSYM adler32}
(*
     Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NULL, this function returns
   the required initial value for the checksum.
   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
   much faster. Usage example:

     uLong adler = adler32(0L, Z_NULL, 0);

     while (read_buffer(buffer, length) != EOF) {
       adler = adler32(adler, buffer, length);
     }
     if (adler != original_adler) error();
*)

function crc32(crc: Cardinal; buf: Pointer; len: Cardinal): Cardinal; cdecl;
{$EXTERNALSYM crc32}
(*
     Update a running crc with the bytes buf[0..len-1] and return the updated
   crc. If buf is NULL, this function returns the required initial value
   for the crc. Pre- and post-conditioning (one's complement) is performed
   within this function so it shouldn't be done by the application.
   Usage example:

     uLong crc = crc32(0L, Z_NULL, 0);

     while (read_buffer(buffer, length) != EOF) {
       crc = crc32(crc, buffer, length);
     }
     if (crc != original_crc) error();
*)

                         { various hacks, don't look :) }

{ deflateInit and inflateInit are macros to allow checking the zlib version
  and the compiler's view of z_stream: }

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar; stream_size: Integer): Integer; cdecl;
{$EXTERNALSYM deflateInit_}
function inflateInit_(var strm: TZStreamRec; version: PChar; stream_size: Integer): Integer; cdecl;
{$EXTERNALSYM inflateInit_}
function deflateInit2_(var strm: TZStreamRec; level, method, windowBits, memLevel, strategy: Integer; version: PChar; stream_size: Integer): Integer; cdecl;
{$EXTERNALSYM deflateInit2_}
function inflateInit2_(var strm: TZStreamRec; windowBits: Integer; version: PChar; stream_size: Integer): Integer; cdecl;
{$EXTERNALSYM inflateInit2_}

implementation
const
  ZLibModuleName = 'libz.so';


function zlibVersion;          external ZLibModuleName name 'zlibVersion';
function deflate;              external ZLibModuleName name 'deflate';
function deflateEnd;           external ZLibModuleName name 'deflateEnd';
function inflateEnd;           external ZLibModuleName name 'inflateEnd';
function inflate;              external ZLibModuleName name 'inflate';
function deflateSetDictionary; external ZLibModuleName name 'deflateSetDictionary';
function deflateCopy;          external ZLibModuleName name 'deflateCopy';
function deflateReset;         external ZLibModuleName name 'deflateReset';
function deflateParams;        external ZLibModuleName name 'deflateParams';
function inflateSetDictionary; external ZLibModuleName name 'inflateSetDictionary';
function inflateSync;          external ZLibModuleName name 'inflateSetDictionary';
function inflateReset;         external ZLibModuleName name 'inflateReset';
function compress;             external ZLibModuleName name 'compress';
function compress2;            external ZLibModuleName name 'compress2';
function uncompress;           external ZLibModuleName name 'uncompress';
function gzopen;               external ZLibModuleName name 'gzopen';
function gzdopen;              external ZLibModuleName name 'gzdopen';
function gzsetparams;          external ZLibModuleName name 'gzsetparams';
function gzread;               external ZLibModuleName name 'gzread';
function gzwrite;              external ZLibModuleName name 'gzwrite';
function gzputs;               external ZLibModuleName name 'gzputs';
function gzgets;               external ZLibModuleName name 'gzgets';
function gzputc;               external ZLibModuleName name 'gzputc';
function gzerror;              external ZLibModuleName name 'gzerror';
function gzclose;              external ZLibModuleName name 'gzclose';
function gzeof;                external ZLibModuleName name 'gzeof';
function gztell;               external ZLibModuleName name 'gztell';
function gzrewind;             external ZLibModuleName name 'gzrewind';
function gzseek;               external ZLibModuleName name 'gzseek';
function gzflush;              external ZLibModuleName name 'gzflush';
function gzgetc;               external ZLibModuleName name 'gzgetc';
function adler32;              external ZLibModuleName name 'adler32';
function crc32;                external ZLibModuleName name 'crc32';
function deflateInit_;         external ZLibModuleName name 'deflateInit_';
function inflateInit_;         external ZLibModuleName name 'inflateInit_';
function deflateInit2_;        external ZLibModuleName name 'deflateInit2_';
function inflateInit2_;        external ZLibModuleName name 'inflateInit2_';
end.
