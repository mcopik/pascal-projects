program SdlCtrls;

uses
  sdl,
  sdlgui,
  SDLFont,
  Main,
  SysUtils,
  gl,
  glu,
  Classes,
  SDLDraw;

var Quit: Boolean;
    event: TSDL_Event;
    Gui: TSDLGui;
    Font: TSDLRasterFont;
    //Font: TSDLTTFFont;
    t: Integer;
begin
  SDLScreen := TSDLGLScreen.Create('SDL Controls Example',640,480,32,False);
  if SDLScreen.Surface=nil then
  begin
    SDLScreen.Free;
    SDL_Quit;
    Exit;
  end;

  if SDLScreen.OpenGLScreen then DefaultInitGL;
  Randomize;

  Gui := TSDLGui.Create(nil);
  Font := TSDLRasterFont.Create(nil);
  Font.Load('Book','Font.png');
  {Font := TSDLTTFFont.Create(nil);
  Font.Load('Arial','Arial.ttf',14);}
  GlobalFont := Font;
  Gui.HintFont := Font;
  Form1 := TForm1.Create(Gui);
  Form2 := TForm2.Create(Gui);

  Quit := true;
  while Quit do
  begin
    while SDL_PollEvent(@event)>0 do
    begin
      Gui.ProcessEvent(Event);
      if (event.type_= SDL_KEYDOWN)and
         (event.key.keysym.sym = SDLK_ESCAPE)or
         (event.type_ = SDL_QUITEV) then
       Quit := False;
    end;
    t := SDLScreen.FPS;
    if Assigned(Form1)and Assigned(Form1.SLabel) then
      Form1.SLabel.Caption := 'FPS: '#13#10+IntToStr(t)+' Alpha Image';
    SDLScreen.Clear(nil,0);
    Gui.Update;
    SDLScreen.Flip;
  end;

  Gui.Free;
  Font.Free;
  Images.Free;

  SDLScreen.Free;

  SDL_Quit;
end.
