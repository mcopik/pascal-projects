{------------------------------------------------------------------------------}
{This is a simple webupdate application which used the SDLWeb units.
 It works by downloading a txt file which is an ini file.

 This ini file contains the current version number, the location of the
 update file and the name of the update file.

 The version number is compared to the one passed in if this is less than the
 one in the ini file the new file is downloaded.

 How you extract the data from the update file is project specific. it would
 be best to have this file zipped or compressed in some way. It would also be
 advisable to add some sort of security to this application.

 This example uses

 SDL.dll or the equivalent on Linux or MacOS
 SDL_net.dll or the equivalent on Linux or MacOS
 SDL_ttf.dll or the equivalent on Linux or MacOS
 SDL_image.dll or the equivalent on Linux or MacOS
}
{------------------------------------------------------------------------------}
program webupdate;

{$IFOPT D+}{$APPTYPE CONSOLE}{$ENDIF}

uses
  SysUtils,
  Classes,
  sdl,
  sdlweb,
  sdlwebhttp,
  sdlwebftp,
  sdl_ttf,
  sdl_image;

const
  clLightBlue : TSDL_Color = ( r : 207; g : 208; b : 245; unused : 0 );

var
  UpdateFile, Response, Version : string;
  Connection : TSDLWebConnection;
  Screen, BackGround : PSDL_Surface;
  Font : PTTF_Font;

{------------------------------------------------------------------------------}
{<Method Comments>}
{------------------------------------------------------------------------------}

procedure DownloadProgress( Progress, Total : UInt32 ); cdecl;
var
  Text : PSDL_Surface;
  s : string;
  Rect : TSDL_Rect;
begin
  SDL_BlitSurface( BackGround, nil, Screen, nil );
  s := Format( 'Download %d %% Complete', [ Trunc( ( Progress / Total ) * 100 ) ] );
  Text := TTF_RenderText_Blended( Font, PChar( s ), clLightBlue );
  Rect.x := ( Screen.w - Text.w ) div 2;
  Rect.y := ( Screen.h - Text.h ) div 2;
  Rect.w := Text.w;
  Rect.h := Text.h;
  SDL_BlitSurface( Text, nil, Screen, @Rect );
  SDL_FreeSurface(Text);
  SDL_Flip( Screen );
end;

{------------------------------------------------------------------------------}
{<Method Comments>}
{------------------------------------------------------------------------------}

procedure WriteProgress( s : string; PauseFor : Integer = 1500 );
var
  Text : PSDL_Surface;
  Rect : TSDL_Rect;
begin
  SDL_BlitSurface( BackGround, nil, Screen, nil );
  Text := TTF_RenderText_Blended( Font, PChar( s ), clLightBlue );
  Rect.x := ( Screen.w - Text.w ) div 2;
  Rect.y := ( Screen.h - Text.h ) div 2;
  Rect.w := Text.w;
  Rect.h := Text.h;
  SDL_BlitSurface( Text, nil, Screen, @Rect );
  SDL_FreeSurface(Text);
  SDL_Flip( Screen );
  SDL_Delay( PauseFor );
end;

{------------------------------------------------------------------------------}
{Download a file from a location and save it in the current directory with the
 same filename}
{------------------------------------------------------------------------------}

procedure DownloadUpdate( location, outfilename : string );
var
  Local : TSDLWebConnection;
  Stream : TStream;
begin
  SDLWeb_ConnectToSite( location, wcHTTP, Local );
  if SDLWeb_Connected( Local ) then
  begin
    Stream := TFileStream.Create( outfilename, fmCreate );
    try
      SDL_Web_HTTP_GetPageToStream( Local, location + outfilename, Stream, DownloadProgress );
    finally
      Stream.Free;
      SDLWeb_Disconnect( Local );
    end;
  end;
end;

begin
  if ParamCount <> 2 then
  begin
    Writeln( 'webupdate ' );
    Writeln;
    Writeln( 'usgage :' );
    Writeln( 'webupdate <current version> <ini-file url>' );
    Exit;
  end;
  Version := ParamStr( 1 );
  UpdateFile := ParamStr( 2 );
  SDL_Init( SDL_INIT_VIDEO );   
  SDL_putenv('SDL_VIDEO_CENTERED=1');
  Screen := SDL_SetVideoMode( 400, 50, 32, SDL_DOUBLEBUF or SDL_NOFRAME );
  SDL_WM_SetCaption( 'Example Web Update', 'Example Web Update' );
  BackGround := IMG_Load( './images/background.png' );
  TTF_Init;
  Font := TTF_OpenFont( './fonts/Adventure Subtitles.ttf', 14 );
  SDLWeb_Init;
  try
    WriteProgress( 'Connecting to ' + UpdateFile );
    SDLWeb_ConnectToSite( UpdateFile, wcHTTP, Connection );
    if SDLWeb_Connected( Connection ) then
    begin
      try
        WriteProgress( 'Connected' );
        if SDL_Web_HTTP_GetPageToString( Connection, UpdateFile, Response ) then
        begin
          // check version info
          with TStringList.Create do
          begin
            Text := Response;
            WriteProgress( 'Checking for new Version' );
            if StrToFloat( Version ) < StrToFloat( Values[ 'Version' ] ) then
            begin
              // an update is available.
              WriteProgress( 'New Version available. Beginning Download.' );
              Downloadupdate( Values[ 'URL' ], Values[ 'FileName' ] );
              WriteProgress( 'Download Complete.', 5000 );
            end
            else
            begin
              WriteProgress( 'Your system is up to date.', 5000 );
            end;
            Free;
          end;
        end
        else
        begin
          WriteProgress( 'Error Retrieving Version Information.' );
        end;
      finally
        SDLWeb_Disconnect( Connection );
      end;
    end
    else
    begin
      WriteProgress( 'Error Connecting to Site.' );
    end;
  finally
    TTF_Quit;
    SDL_FreeSurface( BackGround );
    SDL_FreeSurface( Screen );
    SDLWeb_Quit;
    SDL_Quit;
  end;
end.
