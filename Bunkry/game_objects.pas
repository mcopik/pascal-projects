unit Game_Objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl,sdlutils,sdl_ttf,
  Game_Variables,Graphic_Functions,Game_Functions,menu;
  type

    TWeapon = object
    Name:string;
    Damage, Speed:real;
    X,Y,Angle:integer;
    Direction: boolean; //FALSE-LEWO;TRUE-PRAWO
    procedure Draw(surface:PSDL_SURFACE);
    function Calculate():integer;
    function GetY():integer;
    procedure Shoot(x1,y1,angle1:integer;direction1:boolean);
    procedure Init(name1:string;damage1,speed1:real);
    end;

    TMap = object
    Heights : array of integer;
    Min,Max,W,H,Scale,Size :integer;
    Color: TSDL_Color;
    Background:(FILL,BPATH);
    backgroundFill:TSDL_Color;
    BackgroundPath:string;
    mapSurface:PSDL_Surface;
    backgroundSurface:PSDL_SURFACE;
    procedure Init(min_,max_,r,g,b,scale_,map_size:integer;screen:PSDL_SURFACE);
    procedure InitLand();
    procedure GenerateLand();
    procedure Draw(screen:PSDL_SURFACE);
    procedure SetBackground(r,g,b:integer);
    procedure SetBackground(path1:string);
    procedure Explosion(x,y,power:integer);
    function  OnScreenX(x:integer):integer;
    function  OnScreenY(x:integer):integer;
    procedure Close();
    end;
    PMap = ^TMap;

    TFont = object
    Font: PTTF_Font;
    Path:string;
    Size:integer;
    Color:TSDL_COLOR;
    procedure Load(size1:integer;path1:string);
    procedure Resize(size1:integer);
    procedure Close();
    procedure Write(surf:PSDL_SURFACE;y:integer;text:string;type_:textPosition);overload;
    procedure Write(surf:PSDL_SURFACE;x,y:integer;text:string);overload;
    end;

    TPlayer = object
    Name:string;
    X,Y,Angle,Power,Health:integer;
    Color:TSDL_COLOR;
    procedure Draw(screen:PSDL_SURFACE);
    procedure Init(name_:string;x_,r,g,b:integer;map:PMap);
    end;

    TGame = object
    Width,Height,Bpp:integer;
    Caption:string;

    mainScreen: PSDL_SURFACE;
    background: PSDL_SURFACE;
    menuBackground:       PSDL_SURFACE;

    GameStatus : (gMenu,gAiming,gShooting,gHit,gQuit);

    Menu:PMenu;
    MenuActive:PMenu;

    Weapons: array of TWeapon;
    WeaponsNumber:integer;

    Players: array of TPlayer;
    PlayersNumber,PlayerActive:integer;

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
    procedure Change_Font(x:integer);
    procedure Start(number:integer);
    end;

implementation

   procedure TGame.Start(number:integer);
   var i:integer;
       colors_:array[0..3] of colors = (WHITE,BLUE,YELLOW,RED);
       tab:array of integer;
   begin
        gameStatus := gAiming;
        Maps[Map].InitLand();

        SetLength(Players,number);
        SetLength(tab,number);

        for i := 0 to number-1 do
        tab[i] := random(Width-20);

        for i := 0 to number-1 do
        begin
             Set_Color(colors_[i],@color);
             Players[i].Init('Gracz '+IntToStr(i+1),tab[i],color.r,color.g,color.b,@Maps[Map]);
        end;

        PlayersNumber := number;
        PlayerActive := 0;
        Maps[Map].GenerateLand();
   end;

   procedure TGame.Loop();
   var now,start_ : UInt32;
   begin

   repeat
        now := 0;
        start_ := SDL_GetTicks;
        case gameStatus of
              gMenu:
              begin
                   ProceedEvents();
              end;
              gAiming:
              begin
                   ProceedEvents();
              end;
              gShooting:
              begin
                   ProceedEvents();
              end;
              gHit:
              begin
                   ProceedEvents();
              end;
        end;
        DrawScreen();
        now :=SDL_GetTicks;
        if Get_Delay(FPS)-now+start_ > 0 then
        SDL_Delay(Get_Delay(FPS)-now+start_);
   until (gameStatus = gQuit);
   end;

   procedure TGame.DrawScreen();
   var i:integer;
   begin
        case gameStatus of
           gMenu:
           begin
                Draw_Surface(0,0,menuBackground,mainScreen);

                Fonts[Font].Resize(60);
                Set_Color(WHITE,@Fonts[Font].color);
                Fonts[Font].Write(mainScreen,100,'B U N K R Y',CENTER);
                Fonts[Font].Resize(15);
                Fonts[Font].Write(mainScreen,180,'Marcin Copik, Inf I, Grupa 3',CENTER);
                Fonts[Font].Resize(30);

                for i := 0 to MenuActive^.Number-1do
                begin
                     if i = MenuActive^.MenuPosition then
                     begin
                     Set_Color(YELLOW,@Fonts[Font].color);
                     Fonts[Font].Write(mainScreen,Height DIV 2 +40*i,MenuActive^.Captions[i],CENTER);
                     Set_Color(WHITE,@Fonts[Font].color);
                     end
                     else
                     Fonts[Font].Write(mainScreen,Height DIV 2+40*i,MenuActive^.Captions[i],CENTER);
                end;

           end;
           gAiming:
           begin
                Maps[Map].Draw(mainScreen);
                for i := 0 to PlayersNumber-1 do
                Players[i].Draw(mainScreen);
                Fonts[Font].Resize(15);
                Set_Color(WHITE,@Fonts[Font].color);
                Fonts[Font].Write(mainScreen,40,Players[PlayerActive].Name,LEFT);
                Fonts[Font].Write(mainScreen,60,'Siła∞'+IntToStr(Players[PlayerActive].Power),LEFT);
                Fonts[Font].Write(mainScreen,80,'Kąt: '+IntToStr(Players[PlayerActive].Angle),LEFT);
                Fonts[Font].Write(mainScreen,100,'Życie: '+IntToStr(Players[PlayerActive].Health)+'%',LEFT);
           end;
           end;
           SDL_Flip(mainScreen);
   end;

   procedure TGame.Change_Font(x:integer);
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
   begin
           while( SDL_PollEvent(@event)>0) do
           begin
                case event.type_ of
                SDL_KEYUP:
                begin
                     case GameStatus of
                          gMenu:
                          begin
                               case event.key.keysym.sym of
                                    SDLK_DOWN:
                                    MenuActive^.MenuDown();
                                    SDLK_UP:
                                    MenuActive^.MenuUp();
                                    SDLK_RETURN:
                                    begin
                                         case MenuActive^.MenuAction() of
                                         0:
                                         MenuActive := MenuActive^.Next();
                                         1:
                                         Start(MenuActive^.Actions[MenuActive^.MenuPosition].Players);
                                         2:
                                         gameStatus := GQuit;
                                         end;
                                    end;
                               end;
                          end;
                     end;
                end;
                SDL_KEYDOWN:
                begin

                end;
                SDL_QUITEV:
                gameStatus := gQuit;
                end;

           end;
   end;

   function TGame.LoadData(path1:string):boolean;
   var pt:TFile;
       i,temp,temp2,temp3,temp4,temp5,temp6,temp7,temp8:integer;
       line1,line2:string;
   begin
             LoadData := FALSE;
             pt.Init(path1);
             temp := pt.FindInteger('WEAPONS_NUMBER');
             if temp <> -1 then
             begin
                  SetLength(Weapons,temp);
                  WeaponsNumber := temp-1;
                  for i:=1 to temp do
                  begin
                       temp2 := pt.FindInteger(string('WEAPON'+IntToStr(i)+'_SPEED'));
                       temp3 := pt.FindInteger(string('WEAPON'+IntToStr(i)+'_DAMAGE'));
                       line1 := pt.FindString(string('WEAPON'+IntToStr(i)+'_NAME'));
                       line2 := pt.FindString(string('WEAPON'+IntToStr(i)+'_TYPE'));
                       if((temp2 <> -1) and (temp3 <> -1) and (line1 <> 'ERROR') and (line2 <> 'ERROR')) then
                       begin
                            if line2 = 'BULLET' then
                            begin
                            Weapons[i-1].Init(string(line1),temp2,temp3);//,BULLET);
                            end
                            {else if line2 = 'ROCKET' then
                            begin
                            Weapons[i-1].Init(string(line1),temp2,temp3,ROCKET);
                            end}
                            else
                            begin
                            WriteLn('Bledne dane w pliku ',path1,' - nieznany typ broni');
                            LoadData := TRUE;
                            end;
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


                                 {if ((temp4 <> -1) and (temp5 <> -1)) then
                                 begin
                                      Maps[i-1].InitLand(temp4,temp5);
                                 end
                                 else
                                 begin
                                      WriteLn('Bledne dane w pliku ',path1,' - zla definicja wlasciwosci mapy');
                                      LoadData := TRUE;
                                 end;}

                                 temp4 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORR'));
                                 temp5 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORG'));
                                 temp6 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORB'));
                                 Maps[i-1].SetBackground(temp4,temp5,temp6);
                            end
                            else if line1 = 'PATH' then
                            begin
                                 Maps[i-1].Init(temp2,temp3,temp4,temp5,temp6,temp7,temp8,mainScreen);

                                 {if ((temp4 <> -1) and (temp5 <> -1)) then
                                 begin
                                      Maps[i-1].InitLand(temp4,temp5);
                                 end
                                 else
                                 begin
                                      WriteLn('Bledne dane w pliku ',path1,' - zla definicja wlasciwosci mapy');
                                      LoadData := TRUE;
                                 end;}

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
                  Menu := MenuLoad(0,pt);
                  if Menu <> NIL then
                  begin
                       Menu^.MenuPosition := 0;
                       MenuActive := Menu;
                       WriteLn('Zaladowane menu');
                  end
             end
             else
             LoadData := TRUE;

             //Maps[Map].Explosion(500,500,50);
             pt.CloseFile();
   end;

   procedure TGame.Close();
   var i:integer;
   begin

        for i := 1 to FontsNumber do
             Fonts[i-1].Close();

        Menu_Free(Menu);

        for i := 1 to MapsNumber do
            Maps[i-1].Close();

        SDL_FreeSurface(mainScreen);
        SDL_FreeSurface(background);
        SDL_FreeSurface(menuBackground);
        TTF_Quit;
        SDL_Quit;
   end;

   // TFONT FUNCTIONS

   procedure TFont.Load(size1:integer;path1:string);
   begin
        Path := path1;
        Size := size1;
        Font := TTF_OpenFont(pchar(Path),Size);
   end;

   procedure TFont.Resize(size1:integer);
   begin
        Close();
        Size := size1;
        Font := TTF_OpenFont(pchar(Path),Size);
   end;

   procedure TFont.Close();
   begin
        TTF_CloseFont(Font);
   end;

   procedure TFont.Write(surf:PSDL_SURFACE;y:integer;text:string;type_:textPosition);overload;
   var temp,msg:PSDL_Surface;
   begin
        msg := TTF_RenderUTF8_Solid(Font,pchar(text),Color);

        temp := SDL_DisplayFormat(msg);
        SDL_SetColorKey(temp, SDL_SRCCOLORKEY,PUint32(temp^.pixels)^);

        case type_ of
             LEFT:
             Draw_Surface(30 ,y,temp,surf);
             RIGHT:
             Draw_Surface(surf^.w - (msg^.w DIV 2) ,y,temp,surf);
             CENTER:
             Draw_Surface((surf^.w DIV 2) - (msg^.w DIV 2) ,y,temp,surf);
        end;

        SDL_FreeSurface(msg);
        SDL_FreeSurface(temp);
   end;

   procedure TFont.Write(surf:PSDL_SURFACE;x,y:integer;text:string);overload;
   var temp,msg:PSDL_Surface;
   begin
        msg := TTF_RenderText_Solid(Font,pchar(text),Color);

        temp := SDL_DisplayFormat(msg);
        SDL_SetColorKey(temp, SDL_SRCCOLORKEY ,PUint32(temp^.pixels)^);

        Draw_Surface(x,y,temp,surf);

        SDL_FreeSurface(msg);
        SDL_FreeSurface(temp);
   end;

   // TWEAPON FUNCTIONS

   procedure TWeapon.Draw(surface:PSDL_SURFACE);
   begin

   end;

   procedure TWeapon.Shoot(x1,y1,angle1:integer;direction1:boolean);
   begin

   end;

   function TWeapon.Calculate():integer;
   begin

   end;

   function TWeapon.GetY():integer;
   begin

   end;

   procedure TWeapon.Init(name1:string;damage1,speed1:real);
   begin
        Name := name1;
        Damage := damage1;
        Speed:= speed1;
        X := 0;
        Y := 0;
        Angle := 0;
        Direction := FALSE;
   end;

   // TMAP FUNCTIONS

   procedure TMap.Init(min_,max_,r,g,b,scale_,map_size:integer;screen:PSDL_SURFACE);
   begin
        Min := min_;
        Max := max_;
        Color.r := r;
        Color.g := g;
        Color.b := b;
        W := screen^.w;
        H := screen^.h;
        Scale := scale_;
        Size := map_size;
        SetLength(Heights,W);

        mapSurface := SDL_DisplayFormat(screen);
        SDL_SetColorKey(mapSurface, SDL_SRCCOLORKEY,SDL_MapRGB(mapSurface^.format,TRANSPARENCY[1],TRANSPARENCY[2],TRANSPARENCY[3]));
   end;

   procedure TMap.InitLand();
   var i,j,number,sum:integer;
   begin

        //Scale := scale_;
        //MapSize := map_siz;
        randomize();
        repeat
              Heights[0] := random(Max-Min);
        until Heights[0] > Min;

        for i := 1 to w-1 do
        begin
             repeat
             Heights[i] := trunc(Heights[i-1] + random * scale - scale / 2);
             until ((Heights[i] > Min)and(Heights[i]<Max));
        end;

        for i := 1 to (w-1)  do
        begin
             number := 0;
             sum := 0;
             for j := i-size to i+size do
             begin
                  if ((j >= 0) and (j < w)) then
                  begin
                  inc(number);
                  sum := sum + Heights[j];
                  end;

             end;
             Heights[i] := sum DIV number;
        end;


   end;

   procedure TMap.GenerateLand();
   var i:integer;
   begin

        SDL_FillRect(mapSurface,NIL,SDL_MapRGB(mapSurface^.format,TRANSPARENCY[1],TRANSPARENCY[2],TRANSPARENCY[3]));

        for i := 0 to W-1 do
        begin
             SDL_DrawLine(mapSurface, i, H - 1, i, H - 1 - Heights[i], SDL_MapRGB(mapSurface^.format,Color.r,Color.g,Color.b));
        end;
   end;

   procedure TMap.Draw(screen:PSDL_SURFACE);
   begin
        Draw_Surface(0,0,backgroundSurface,screen);
        Draw_Surface(0,0,mapSurface,screen);
   end;

   procedure TMap.Close();
   begin
        SDL_FreeSurface(backgroundSurface);
        SDL_FreeSurface(mapSurface);
   end;

   procedure TMap.Explosion(x,y,power:integer);
   var i,alfa:integer;
   begin
        for i := 0 to 2*power do
        begin
             alfa := trunc(sqrt((2*power*i) - i*i));
             SDL_DrawLine(mapSurface,x-alfa,y-power+i,x+alfa,y-power+i,SDL_MapRGB(mapSurface^.format,TRANSPARENCY[1],TRANSPARENCY[2],TRANSPARENCY[3]));
        end;
   end;

   function TMap.OnScreenX(x:integer):integer;
   begin
        if x < 0 then
        begin
        OnScreenX := 0;
        end
        else if x >= W then
        begin
        OnScreenX := W-1;
        end
        else
        OnScreenX := x;
   end;

   function TMap.OnScreenY(x:integer):integer;
   begin
        if x < 0 then
        begin
        OnScreenY := 0;
        end
        else if x >= H then
        begin
        OnScreenY := H-1;
        end
        else
        OnScreenY := x;
   end;

   procedure TMap.SetBackground(r,g,b:integer);
   begin
        Background:= FILL;
        BackgroundFill.r := r;
        BackgroundFill.g := g;
        BackgroundFill.b := b;
   end;

   procedure TMap.SetBackground(path1:string);
   begin
        Background:= BPATH;
        BackgroundPath := path1;
        backgroundSurface := Load_Image(path1,FALSE);
   end;

   // TPLAYER FUNCTIONS

   procedure TPlayer.Draw(screen:PSDL_SURFACE);
   var i,alfa:integer;
   begin
        for i := 0 to 5 do
        SDL_DrawLine(screen,X-10,Y-i,X+10,Y-i,SDL_MapRGB(screen^.format,Color.r,Color.g,Color.b));

        for i := 0 to 10 do
        begin
             alfa := trunc(sqrt((2*10*i) - i*i));
             SDL_DrawLine(screen,X-alfa,Y-power+i-15,X+alfa,Y-power+i-15,SDL_MapRGB(screen^.format,Color.r,Color.g,Color.b));
        end;

        SDL_DrawLine(screen,X,Y-15,trunc(X+10*cos((Angle/180)*PI)),trunc(Y-15-10*sin((Angle/180)*PI)),SDL_MapRGB(screen^.format,Color.r,Color.g,Color.b));

   end;

   procedure TPlayer.Init(name_:string;x_,r,g,b:integer;map:PMap);
   var  i,lowest:integer;
   begin
        Name := name_;
        Color.r := r;
        Color.g := g;
        Color.b := b;
        //repeat
              {repeat
                    temp := random(map^.W);
              until temp < map^.W-20;
              WriteLn(temp);}

              //sum := 0;
              lowest := map^.H;
              for i := 0 to 19 do
              begin
                   //sum := map^.Heights[temp+i];
                   if map^.Heights[x_+i] < lowest then
                   lowest := map^.Heights[x_+i];
              end;
              //sum := sum DIV 20;
        //until sum < 10;

        for i := 0 to 20 do
        map^.Heights[x_+i] := lowest;

        X := x_+10;
        Y := map^.H -map^.Heights[x_];
       { temp := round((map^.Heights[X] - map^.Heights[X+20]) /20);

        if temp <> 0 then
        begin
             for i := 1 to 10 do
             begin
             map^.Heights[X+10+i] := map^.Heights[X] - temp*i;
             WriteLn(map^.Heights[X+10+i]);
             end;
        end;   }

        Angle := 90;
        Health := 100;
   end;

end.

