unit game_objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl,sdl_ttf,
  game_variables,graphic_functions,game_functions,player,map,font,weapon;
   type

    PGame= ^TGame;

    PMenu = ^TMenu;
    FActionPointer = procedure (keysym:TSDL_keysym;game:PGame);
    TMenu = object
          Actions : array of FActionPointer;
          Children: array of PMenu;
          Number:  integer;
          MenuPosition : integer;
          Captions: array of string;
          Parent: PMenu;
          procedure MenuDown();
          procedure MenuUp();
          function Next():PMenu;
          procedure Draw(game:PGame);
    end;


    TGame = object
          Width,Height,Bpp:integer;
          Caption:string;

          mainScreen: PSDL_SURFACE;
          background: PSDL_SURFACE;
          menuBackground: PSDL_SURFACE;

          GameStatus : (gMenu,gAiming,gShooting,gHit,gQuit);

          Menu:PMenu;
          MenuActive:PMenu;

          Weapons: array of TWeapon;
          WeaponsNumber:integer;
          WeaponsAmount:array of integer;
          WeaponActive:integer;

          Players: array of TPlayer;
          Enemies: array of TPlayer;
          PlayersNumber,PlayerActive,EnemiesNumber:integer;

          Fonts: array of TFont;
          FontsNumber,Font:integer;

          Maps: array of TMap;
          MapsNumber,Map:integer;

          procedure Loop();
          function Init(path:string):boolean;
          procedure ProceedEvents();
          procedure Close();
          procedure DrawScreen();
          function LoadData(path1:string): boolean;
          procedure ChangeFont(x:integer);
          procedure Start();
          procedure Update();
          function GetActivePlayer():PPlayer;
    end;

    function AddMenu(captions_:array of string;actions_:array of FActionPointer):PMenu;
    procedure MenuFree(pt:PMenu);

implementation

   procedure MenuTypicalAction(keysym:TSDL_KEYSYM;game:PGame);
   begin
        if keysym.sym = SDLK_RETURN then
        begin
             if game^.MenuActive^.Children[game^.MenuActive^.MenuPosition] <> NIL then
             game^.MenuActive := game^.MenuActive^.Next();
        end;
   end;

   procedure MenuQuit(keysym:TSDL_KEYSYM;game:PGame);
   begin
        if keysym.sym = SDLK_RETURN then
        begin
             game^.gameStatus := gQuit;
        end;
   end;

   procedure MenuStart(keysym:TSDL_KEYSYM;game:PGame);
   begin
        if keysym.sym = SDLK_RETURN then
        begin
             game^.Start();
        end;
   end;

   procedure MenuBack(keysym:TSDL_KEYSYM;game:PGame);
   begin
        if keysym.sym = SDLK_RETURN then
        begin
             game^.MenuActive := game^.MenuActive^.Parent;
        end;
   end;

   procedure MenuChangePlayers(keysym:TSDL_KEYSYM;game:PGame);
   begin
        case keysym.sym of
             SDLK_LEFT:
             begin
                  if ((game^.PlayersNumber > 2) or ((game^.PlayersNumber = 2) and (game^.EnemiesNumber > 0))) then
                  begin
                       game^.PlayersNumber := game^.PlayersNumber - 1;
                       game^.MenuActive^.Captions[game^.MenuActive^.MenuPosition] := 'Liczba graczy <'+IntToStr(game^.PlayersNumber)+'>';
                  end
             end;
             SDLK_RIGHT:
             begin
                  if ((game^.PlayersNumber + game^.EnemiesNumber < 4) and (game^.PlayersNumber < 4)) then
                  begin
                       game^.PlayersNumber := game^.PlayersNumber + 1;
                       game^.MenuActive^.Captions[game^.MenuActive^.MenuPosition] := 'Liczba graczy <'+IntToStr(game^.PlayersNumber)+'>';
                  end
             end;
        end;
   end;

   procedure MenuChangeEnemies(keysym:TSDL_KEYSYM;game:PGame);
   begin
        case keysym.sym of
             SDLK_LEFT:
             begin
                  if ((game^.EnemiesNumber > 1) or ((game^.EnemiesNumber = 1) and (game^.PlayersNumber >= 2))) then
                  begin
                       game^.EnemiesNumber := game^.EnemiesNumber - 1;
                       game^.MenuActive^.Captions[game^.MenuActive^.MenuPosition] := 'Liczba graczy komputerowych <'+IntToStr(game^.EnemiesNumber)+'>';
                  end
             end;
             SDLK_RIGHT:
             begin
                  if ((game^.PlayersNumber + game^.EnemiesNumber < 4) and (game^.EnemiesNumber < 3)) then
                  begin
                       game^.EnemiesNumber := game^.EnemiesNumber + 1;
                       game^.MenuActive^.Captions[game^.MenuActive^.MenuPosition] := 'Liczba graczy komputerowych <'+IntToStr(game^.EnemiesNumber)+'>';
                  end
             end;
        end;
   end;

   procedure MenuChangeWeapons(keysym:TSDL_KEYSYM;game:PGame);
   var amount,position:integer;
   begin
        position := game^.MenuActive^.MenuPosition;
        amount := game^.WeaponsAmount[position];
        case keysym.sym of
             SDLK_LEFT:
             begin
                  if amount > -1 then
                  begin
                       game^.WeaponsAmount[position] := amount - 1;
                       if amount <> 0 then
                       begin
                            game^.MenuActive^.Captions[position] := game^.Weapons[position].Name +' <'+IntToStr(amount-1)+'>';
                       end
                       else
                           game^.MenuActive^.Captions[position] := game^.Weapons[position].Name +' <∞>';
                  end
             end;
             SDLK_RIGHT:
             begin
                       game^.WeaponsAmount[position] := amount + 1;
                       game^.MenuActive^.Captions[position] := game^.Weapons[position].Name +' <'+IntToStr(amount+1)+'>';
             end;
        end;
   end;

   function TGame.GetActivePlayer():PPlayer;
   begin
        if PlayerActive < PlayersNumber then
        begin
             GetActivePlayer := @Players[PlayerActive];
        end
        else
            GetActivePlayer := @Enemies[PlayerActive-PlayersNumber];
   end;

   procedure TGame.Update();
   var player:PPlayer;
   begin
        case GameStatus of
             gAiming:
             begin
                  player := GetActivePlayer();
                  if player^.AngleChange = DECR then
                  begin
                       if player^.Angle > 0 then
                       begin
                            player^.Angle := player^.Angle -1;
                       end
                       else
                           player^.DisableAngleChange();
                  end
                  else if player^.AngleChange = INCR then
                  begin
                       if player^.Angle < 180 then
                       begin
                            player^.Angle := player^.Angle +1;
                       end
                       else
                           player^.DisableAngleChange();
                  end
                  else if player^.PowerChange = DOWN then
                  begin
                       if player^.Power > 0 then
                       begin
                            player^.Power := player^.Power -1;
                       end
                       else
                           player^.DisablePowerChange();
                  end
                  else if player^.PowerChange = UP then
                  begin
                       if player^.Power < 100 then
                       begin
                            player^.Power := player^.Power +1;
                       end
                       else
                           player^.DisablePowerChange();
                  end
             end;
             gShooting:
             begin
                  Weapons[WeaponActive].Calculate();
                  if ((Weapons[WeaponActive].X >= 0) and (Weapons[WeaponActive].X <= Width-1) and (Weapons[WeaponActive].Y <= Height-1) and (Weapons[WeaponActive].Y >= 0)) then
                  begin
                       //collision check
                  end
                  else
                  begin
                       Inc(PlayerActive);
                       WriteLn(PlayerActive,PlayersNumber);
                       if PlayerActive >= (EnemiesNumber + PlayersNumber) then
                       PlayerActive := 0;
                       gameStatus := gAiming;
                  end;
             end;
        end;
   end;

   procedure TMenu.Draw(game:PGame);
   var i:integer;
   begin
        Draw_Surface(0,0,game^.menuBackground,game^.mainScreen);

        game^.Fonts[game^.Font].Resize(60);
        Set_Color(WHITE,@game^.Fonts[game^.Font].color);
        game^.Fonts[game^.Font].Write(game^.mainScreen,0,100,'B U N K R Y',CENTER);
        game^.Fonts[game^.Font].Resize(15);
        game^.Fonts[game^.Font].Write(game^.mainScreen,0,180,'Marcin Copik, Inf I, Grupa 3',CENTER);
        game^.Fonts[game^.Font].Resize(30);

        for i := 0 to Number-1do
        begin
             if i = MenuPosition then
             begin
                  Set_Color(YELLOW,@game^.Fonts[game^.Font].color);
                  game^.Fonts[game^.Font].Write(game^.mainScreen,0,game^.Height DIV 2 +40*i,Captions[i],CENTER);
                  Set_Color(WHITE,@game^.Fonts[game^.Font].color);
             end
             else
                  game^.Fonts[game^.Font].Write(game^.mainScreen,0,game^.Height DIV 2+40*i,Captions[i],CENTER);
        end;
   end;

   procedure TGame.Start();
   var i,j:integer;
       colors_:array[0..3] of colors = (WHITE,BLUE,YELLOW,RED);
       tab:array of integer;
   begin
        Set_Color(BLACK,@color);
        Fill_Surface(mainScreen,@color);
        Fonts[Font].Resize(40);
        Fonts[Font].Write(mainScreen,0,(Height div 2) -60,'Inicjalizacja mapy',CENTER);
        SDL_Flip(mainScreen);

        Maps[Map].InitLand();

        SetLength(Players,PlayersNumber);
        SetLength(tab,PlayersNumber);

        for i := 0 to PlayersNumber-1 do
        tab[i] := random(Width-20);

        for i := 0 to PlayersNumber-1 do
        begin
             Set_Color(colors_[i],@color);
             Players[i].Init('Gracz '+IntToStr(i+1),tab[i],color.r,color.g,color.b,@Maps[Map]);
             SetLength(Players[i].Weapons,WeaponsNumber);
             for j := 0 to WeaponsNumber-1 do
             Players[i].Weapons[j] := WeaponsAmount[j];
        end;

        if EnemiesNumber <> 0 then
        begin
             SetLength(Enemies,EnemiesNumber);
             SetLength(tab,EnemiesNumber);
             for i := PlayersNumber to PlayersNumber+EnemiesNumber-1 do
             begin
                  tab[i-PlayersNumber] := random(Width-20);
                  Set_Color(colors_[i],@color);
                  Enemies[i-PlayersNumber].Init('Komputer '+IntToStr(i+1),tab[i-PlayersNumber],color.r,color.g,color.b,@Maps[Map]);
             end;
        end;
        PlayerActive := 0;
        WeaponActive := 0;
        Maps[Map].GenerateLand();
        gameStatus := gAiming;
   end;

   procedure TGame.Loop();
   var start_,dt,acc : UInt32;
   begin
        start_ := SDL_GetTicks();
        acc := 0;
        dt := 0;
        repeat
              dt := SDL_GetTicks() - start_;
              start_ := start_ + dt;
              acc := acc + dt;
              ProceedEvents();
              while Int(acc) > GetTime() do
              begin
                   Update();
                   acc := acc - GetTime();
                   end;
              DrawScreen();
        until (gameStatus = gQuit);
   end;

   procedure TGame.DrawScreen();
   var i:integer;
   begin
        case gameStatus of
           gMenu:
                 MenuActive^.Draw(@self);
           gAiming:
           begin
                Maps[Map].Draw(mainScreen);
                for i := 0 to PlayersNumber-1 do
                Players[i].Draw(mainScreen);
                for i := 0 to EnemiesNumber-1 do
                Enemies[i].Draw(mainScreen);

                GetActivePlayer()^.Draw(mainScreen,Weapons[GetActivePlayer()^.WeaponActive].Name,@Fonts[font]);
                {if PlayerActive < PlayersNumber then
                begin
                     Players[PlayerActive].Draw(mainScreen,Weapons[Players[PlayerActive].WeaponActive].Name,@Fonts[Font]);
                end
                else
                    Enemies[PlayerActive-PlayersNumber].Draw(mainScreen,Weapons[Enemies[PlayerActive-PlayersNumber].WeaponActive].Name,@Fonts[Font]);
           }end;
           gShooting:
           begin
                Maps[Map].Draw(mainScreen);
                for i := 0 to PlayersNumber-1 do
                Players[i].Draw(mainScreen);
                for i := 0 to EnemiesNumber-1 do
                Enemies[i].Draw(mainScreen);

                GetActivePlayer()^.Draw(mainScreen,Weapons[GetActivePlayer()^.WeaponActive].Name,@Fonts[font]);

                Weapons[WeaponActive].Draw(mainScreen);
           end;
        end;
        SDL_Flip(mainScreen);
   end;

   procedure TGame.ChangeFont(x:integer);
   begin
        if x <= FontsNumber then
        Font := x;
   end;

   function TGame.Init(path:string): boolean;
   var pt: TFile;temp:string;
   begin
           Init := FALSE;
           pt.Init(path);
           Width := pt.FindInteger(string('WIDTH'));
           Height := pt.FindInteger(string('HEIGHT'));
           BPP := pt.FindInteger('BPP');
           Caption := pt.FindString('CAPTION');
           temp := pt.FindString('MENU_BACKGROUND');
           pt.CloseFile();

           if ((Width <= 0) or (Height <= 0) or (BPP <= 0) or (Caption = 'ERROR') or (temp = 'ERROR'))then
           begin
                WriteLn('Bledne dane w pliku ',path);
                Init := TRUE;
           end;

           if Init = FALSE then
           begin
           if SDL_Init(SDL_INIT_VIDEO) <> -1 then
           begin
                        mainScreen := SDL_SetVideoMode(Width, Height, BPP, SDL_SWSURFACE);
                        if mainScreen <> NIL then
                        begin
                             if TTF_Init() <> -1 then
                             begin
                             SDL_WM_SetCaption(pchar(Caption),NIL);
                             Init := FALSE;
                             end
                             else
                             begin
                             WriteLn('Blad inicjalizacji modulu TrueType Fonts');
                             Init := TRUE;
                             end;
                        end
                        else
                        begin
                        WriteLn('Blad utworzenia okna');
                        Init := TRUE;
                        end;
           end
           else
           begin
                   WriteLn('Blad inicjalizacji SDL');
                   Init := TRUE;
           end;
           menuBackground := Load_Image(temp,TRUE);
           gameStatus := gMenu;
           if Init = FALSE then
           WriteLn('Utworzenie okna ',Width,'x',Height);
           end;
   end;

   procedure TGame.ProceedEvents();
   var player:PPlayer;
   begin
           while( SDL_PollEvent(@event)>0) do
           begin
                case event.type_ of
                SDL_KEYDOWN:
                begin
                     case GameStatus of
                          gMenu:
                          begin
                               case event.key.keysym.sym of
                                    SDLK_DOWN:
                                    MenuActive^.MenuDown();
                                    SDLK_UP:
                                    MenuActive^.MenuUp();
                                    else
                                    MenuActive^.Actions[MenuActive^.MenuPosition](event.key.keysym,@self);
                               end;
                          end;
                          gAiming:
                          begin
                               player := GetActivePlayer();
                               case event.key.keysym.sym of
                                    SDLK_LEFT:
                                    player^.EnableAngleChange(DECR);
                                    SDLK_RIGHT:
                                    player^.EnableAngleChange(INCR);
                                    SDLK_UP:
                                    player^.EnablePowerChange(UP);
                                    SDLK_DOWN:
                                    player^.EnablePowerChange(DOWN);
                                    SDLK_RETURN:
                                    begin
                                         Weapons[WeaponActive].Shoot(player^.X,player^.Y,player^.Angle,player^.Power);
                                         gameStatus := gShooting;
                                         Maps[Map].Explosion(780,580,50);
                                    end;
                               end;
                          end;
                     end;
                end;
                SDL_KEYUP:
                begin
                     case GameStatus of
                          gAiming:
                          begin
                               player := GetActivePlayer();
                               case event.key.keysym.sym of
                                    SDLK_LEFT:
                                    player^.DisableAngleChange();
                                    SDLK_RIGHT:
                                    player^.DisableAngleChange();
                                    SDLK_UP:
                                    player^.DisablePowerChange();
                                    SDLK_DOWN:
                                    player^.DisablePowerChange();
                               end;
                          end;
                     end;
                end;
                SDL_QUITEV:
                gameStatus := gQuit;
                end;

           end;
   end;

   function TGame.LoadData(path1:string):boolean;
   var pt:TFile;
       i,temp,temp2,temp3,temp4,temp5,temp6,temp7,temp8:integer;
       line1:string;
       tab: array of string;
       tab_: array of FActionPointer;
   begin
             LoadData := FALSE;
             pt.Init(path1);
             temp := pt.FindInteger('WEAPONS_NUMBER');
             if temp <> -1 then
             begin
                  SetLength(Weapons,temp);
                  WeaponsNumber := temp;
                  for i:=1 to temp do
                  begin
                       temp2 := pt.FindInteger(string('WEAPON'+IntToStr(i)+'_SPEED'));
                       temp3 := pt.FindInteger(string('WEAPON'+IntToStr(i)+'_DAMAGE'));
                       line1 := pt.FindString(string('WEAPON'+IntToStr(i)+'_NAME'));
                       if((temp2 <> -1) and (temp3 <> -1) and (line1 <> 'ERROR')) then
                       begin
                            Weapons[i-1].Init(string(line1),temp3,temp2);
                       end
                       else
                       begin
                            WriteLn('Bledne dane w pliku ',path1);
                            LoadData := TRUE;
                       end;
                  end;
                  if LoadData = FALSE then
                  WriteLn('Zaladowane bronie');
             end
             else
             begin
                  WriteLn('Bledne dane broni w pliku ',path1,' - brak/zla definicja zmiennej WEAPONS_NUMBER');
                  LoadData := TRUE;
             end;

             if LoadData = FALSE then
             begin
             temp := pt.FindInteger('FONTS_NUMBER');
             if temp <> -1 then
             begin
                  SetLength(Fonts,temp);
                  FontsNumber := temp;
                  Font := 0;
                  for i:=1 to temp do
                  begin
                       temp2 := pt.FindInteger(string('FONT'+IntToStr(i)+'_SIZE'));
                       line1 := pt.FindString(string('FONT'+IntToStr(i)+'_PATH'));
                       if((temp2 <> -1) and (line1 <> 'ERROR')) then
                       begin
                            Fonts[i-1].Load(temp2,line1);

                            if Fonts[i-1].Font = NIL  then
                            begin
                                 WriteLn('Blad w trakcie wczytywania czcionki');
                                 LoadData := TRUE;
                            end;
                       end
                       else
                       begin
                            WriteLn('Bledne dane czcionek w pliku ',path1);
                            LoadData := TRUE;
                       end;
                  end;
                  if LoadData = FALSE then
                  WriteLn('Zaladowane czcionki');
             end
             else
             begin
                  WriteLn('Bledne dane w pliku ',path1,' - brak/zla definicja zmiennej FONTS_NUMBER');
                  LoadData := TRUE;
             end;
             end;

             if LoadData = FALSE then
             begin
             temp := pt.FindInteger('MAPS_NUMBER');
             if temp <> -1 then
             begin
                  SetLength(Maps,temp);
                  MapsNumber := temp;
                  Map := 0;
                  for i:=1 to temp do
                  begin
                       temp2 := pt.FindInteger(string('MAP'+IntToStr(i)+'_MIN'));
                       temp3 := pt.FindInteger(string('MAP'+IntToStr(i)+'_MAX'));
                       temp4 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORR'));
                       temp5 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORG'));
                       temp6 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORB'));
                       temp7 := pt.FindInteger(string('MAP'+IntToStr(i)+'_SCALE'));
                       temp8 := pt.FindInteger(string('MAP'+IntToStr(i)+'_SIZE'));
                       line1 := pt.FindString(string('MAP'+IntToStr(i)+'_BACKGROUND'));
                       if((temp2 <> -1) and (temp3 <> -1)and(temp4 <> -1) and (temp5 <> -1)and (temp6 <> -1) and (temp7 <> -1) and(temp8 <> -1))then
                       begin
                            if line1 = 'FILL' then
                            begin
                                 Maps[i-1].Init(temp2,temp3,temp4,temp5,temp6,temp7,temp8,mainScreen);
                                 temp4 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORR'));
                                 temp5 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORG'));
                                 temp6 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORB'));
                                 Maps[i-1].SetBackground(temp4,temp5,temp6);
                            end
                            else if line1 = 'PATH' then
                            begin
                                 Maps[i-1].Init(temp2,temp3,temp4,temp5,temp6,temp7,temp8,mainScreen);

                                 line1 := pt.FindString(string('MAP'+IntToStr(i)+'_BACKGROUNDPATH'));
                                 Maps[i-1].SetBackground(line1);
                            end
                            else
                            begin
                                 WriteLn('Bledne dane w pliku',path1,' - zla definicja tla mapy');
                                 LoadData := TRUE;
                            end;
                       end
                       else
                       begin
                            WriteLn('Bledne dane map w pliku ',path1);
                            LoadData := TRUE;
                       end;
                  end;
                  if LoadData = FALSE then
                  WriteLn('Zaladowane mapy');
             end
             else
             begin
                  WriteLn('Bledne dane w pliku ',path1,' - brak/zla definicja zmiennej MAPS_NUMBER');
                  LoadData := TRUE;
             end;
             end;

             if LoadData = FALSE then
             begin
                  SetLength(tab,2);
                  tab[0] := 'Nowa gra';
                  tab[1] := 'Wyjście';
                  SetLength(tab_,2);
                  tab_[0] :=@MenuTypicalAction;
                  tab_[1] :=@MenuQuit;
                  Menu := AddMenu(tab,tab_);

                  SetLength(tab,4);
                  SetLength(tab_,4);
                  tab[0] := 'Liczba graczy <2>';
                  tab[1] := 'Liczba graczy komputerowych <2>';
                  tab[2] := 'Dalej';
                  tab[3] := 'Wstecz';
                  tab_[0] := @MenuChangePlayers;
                  tab_[1] := @MenuChangeEnemies;
                  tab_[2] := @MenuTypicalAction;
                  tab_[3] := @MenuBack;
                  Menu^.Children[0] := AddMenu(tab,tab_);
                  Menu^.Children[1] := NIL;
                  Menu^.Children[0]^.Parent := Menu;

                  SetLength(tab,WeaponsNumber+2);
                  SetLength(tab_,WeaponsNumber+2);
                  SetLength(WeaponsAmount,WeaponsNumber);
                  for i := 0 to WeaponsNumber-1 do
                  begin
                       WeaponsAmount[i] := 0;
                       tab[i] := Weapons[i].Name + ' <0>';
                       tab_[i] := @MenuChangeWeapons;
                  end;
                  tab[WeaponsNumber] := 'Zacznij grę';
                  tab[WeaponsNumber+1] := 'Wstecz';
                  tab_[WeaponsNumber] := @MenuStart;
                  tab_[WeaponsNumber+1] := @MenuBack;
                  Menu^.Children[0]^.Children[2] := AddMenu(tab,tab_);
                  Menu^.Children[0]^.Children[2]^.Parent := Menu^.Children[0];

                  PlayersNumber := 2;
                  EnemiesNumber := 2;

                  if Menu <> NIL then
                  begin
                       Menu^.MenuPosition := 0;
                       MenuActive := Menu;
                       WriteLn('Zaladowane menu');
                  end
                  else
                  begin
                      LoadData := TRUE;
                      WriteLn('Blad w trakcie tworzenia menu');
                  end;
             end
             else
             LoadData := TRUE;

             pt.CloseFile();
   end;

   procedure TGame.Close();
   var i:integer;
   begin

        for i := 1 to FontsNumber do
             Fonts[i-1].Close();

        MenuFree(Menu);

        for i := 1 to MapsNumber do
            Maps[i-1].Close();

        SDL_FreeSurface(mainScreen);
        SDL_FreeSurface(background);
        SDL_FreeSurface(menuBackground);
        TTF_Quit;
        SDL_Quit;
   end;

   procedure TMenu.MenuDown();
   begin
         MenuPosition := MenuPosition + 1;
         if MenuPosition = Number then
         MenuPosition := 0;
   end;

   procedure TMenu.MenuUp();
   begin
         MenuPosition := MenuPosition - 1;
         if MenuPosition = -1 then
         MenuPosition := Number-1;
   end;

   function TMenu.Next():PMenu;
   begin
         Next := Children[MenuPosition];
   end;

   function AddMenu(captions_:array of string;actions_:array of FActionPointer):PMenu;
   var  i:integer;
         pt3:  PMenu;
   begin
             new(pt3);
             pt3^.Number := Length(captions_);
             SetLength(pt3^.Captions,pt3^.Number);
             SetLength(pt3^.Actions,pt3^.Number);
             SetLength(pt3^.Children,pt3^.Number);
             for i := 0 to pt3^.Number-1 do
             begin
                  pt3^.Captions[i] := captions_[i];
                  pt3^.Actions[i] := actions_[i];
             end;
             pt3^.MenuPosition := 0;
             AddMenu := pt3;
   end;

   procedure MenuFree(pt:PMenu);
   var  i:integer;
   begin
         if pt <> NIL then
         begin
              for i := 0 to pt^.Number-1 do
              begin
                   if pt^.Children[i] <> NIL then
                   MenuFree(pt^.Children[i]);
              end;
              dispose(pt);
         end;
   end;

end.

