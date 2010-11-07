program playflic;
{$APPTYPE CONSOLE}
uses
  sdl, Classes, SysUtils,
  SDL_flic in 'SDL_flic.pas';

var flic: PFLI_animation;
    Screen: PSDL_Surface;
    ticks: UInt32;
    Event: TSDL_Event;
    Stream: TStream;
const Done: Boolean = False;

begin
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_NOPARACHUTE);
  try
     // Stream is attached to the flic animation so we don't need to free it this is done
     // when the amination closes.
    Stream := TFileStream.Create(ParamStr(1), fmOpenRead);
    flic := FLI_Open(Stream);
    screen := SDL_SetVideoMode(flic.width, flic.height, 0, SDL_SWSURFACE);
    try
      ticks := SDL_GetTicks();
      while not Done do
      begin
        if not FLI_NextFrame(flic) then
        begin
          Done := True;
        end;
        SDL_BlitSurface(flic.surface, nil, screen, nil);
        SDL_UpdateRect(screen, 0, 0, 0, 0);
        while (SDL_PollEvent(@event) = 1) do
        begin
          case Event.type_ of
            SDL_QUITEV: Done := True;
          end;
        end;
       ticks := SDL_GetTicks() - ticks;
       if (ticks < flic.delay) then
          SDL_Delay(flic.delay - ticks);
       ticks := SDL_GetTicks();
      end;
    finally
      FLI_Close(flic);
      SDL_FreeSurface(Screen);
    end;
  finally
    SDL_Quit;
  end;
end.