program SDLFDesign;

{$R *.res}

uses
  SysUtils, sdl, SDLDraw, sdlgui, SDLFont, gl, glu, Main, SDLDialogs;

var Quit: Boolean;
    event: TSDL_Event;
    Gui: TSDLGui;
    Font: TSDLRasterFont;
begin
  SDLScreen := TSDLScreen.Create('SDL Form Designer',800,600,32,False);
  if SDLScreen.Surface=nil then
  begin
    SDLScreen.Free;
    SDL_Quit;
    Exit;
  end;
  if SDLScreen.OpenGLScreen then DefaultInitGL;

  Gui := TSDLGui.Create(nil);
  Font := TSDLRasterFont.Create(nil);
  Font.Load('Arial','../Data/Font.png');
  GlobalFont := Font;
  Gui.HintFont := Font;
  MainForm := TMainForm.Create(Gui);

  Quit := true;
  while Quit do
  begin
    while SDL_PollEvent(@event)>0 do
    begin
      Gui.ProcessEvent(Event);
      if (event.type_= SDL_KEYDOWN)and(event.key.keysym.sym = SDLK_ESCAPE) then
        MainForm.ExitClick(nil,0,0);
      if event.type_ = SDL_QUITEV then
       Quit := False;
    end;
    SDLScreen.Clear(nil,0);
    Gui.Update;
    SDLScreen.Flip;
  end;

  Gui.Free;
  Font.Free;

  SDLScreen.Free;

  SDL_Quit;
end.
