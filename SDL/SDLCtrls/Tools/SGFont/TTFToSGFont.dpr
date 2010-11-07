program TTFToSGFont;

uses
  SDL,
  SDLDraw,
  SDLGUI,
  SysUtils,
  SDLFont,
  SDLForm,
  Main;

var Quit: Boolean;
    event: TSDL_Event;
    Gui: TSDLGui;
    Font: TSDLRasterFont;
begin
  SDLScreen := TSDLScreen.Create('SDL TTF to SGFont',640,480,32,False);
  if SDLScreen.Surface=nil then
  begin
    SDLScreen.Free;
    SDL_Quit;
    Exit;
  end;

  Gui := TSDLGui.Create(nil);
  Font := TSDLRasterFont.Create(nil);
  Font.Load('Book','..'+PathDelim+'Font.png');
  GlobalFont := Font;
  Gui.HintFont := Font;
  Form1 := TForm1.Create(Gui);

  Quit := true;
  while Quit do
  begin
    Gui.Update;
    while SDL_PollEvent(@event)>0 do
    begin
      Gui.ProcessEvent(Event);
      if event.type_=SDL_QUITEV then
        Quit := False;
    end;
    SDLScreen.Flip;
    SDLScreen.Clear(nil,0);
  end;

  Gui.Free;
  Font.Free;

  SDL_Quit;
end.
