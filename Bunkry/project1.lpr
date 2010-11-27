program project1;


uses
  Classes, SysUtils,
  sdl,sdlutils,sdl_ttf,Game_Objects;//Graphic_Functions,Game_Variables,Game_Objects,Game_Functions;

  var game:TGame;

{$R *.res}

  begin
  //game := TGame.Create();
  //if game = NIL then
  //WriteLn('fuck');
  //WriteLn(IntToStr(PtrUInt(game)));
  if Game.Init('bunkry.cfg') <> TRUE  then
  begin

       if Game.Load_Data('data.cfg') <> TRUE then
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
  //game.Free;
  end.
