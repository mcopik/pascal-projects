program project1;


uses
  Classes, SysUtils, sdl, sdlutils, sdl_ttf, Game_Objects, menu;

  var game:TGame;

{$R *.res}

begin
  if Game.Init('bunkry.cfg') <> TRUE  then
  begin

       if Game.LoadData('data.cfg') <> TRUE then
       begin
            Game.Loop();
            Game.Close();
       end
       else
       begin
       WriteLn('Terminating...');
       Game.Close();
       end;
  end
  else
  begin
       WriteLn('Terminating...');
       Game.Close();
  end;
end.
