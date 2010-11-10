program project1;


uses
  Classes, SysUtils,
  sdl,sdlutils,sdl_ttf,Game_Objects;//Graphic_Functions,Game_Variables,Game_Objects,Game_Functions;

  var game:TGame;
  begin
  if Game.Init('bunkry.cfg') <> TRUE  then
  begin

       Game.Load_Data('data.cfg');

       //Game.Loop();
       Game.Close();
  end
  else
  Game.Close();

  end.
