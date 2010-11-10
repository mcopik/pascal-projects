unit Game_Objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,sdl,sdl_ttf,Game_Variables,Graphic_Functions,Game_Functions;
  type
    TWeapon = class
    Name:pchar;
    Damage, Speed:real;
    X,Y,Angle:integer;
    Direction: boolean; //FALSE-LEWO;TRUE-PRAWO
    Kind : weaponType;
    procedure Draw(surface:PSDL_SURFACE);
    function Calculate():integer;
    function GetY():integer;
    procedure Shoot(x1,y1,angle1:integer;direction1:boolean);
    constructor Create(name1:pchar;damage1,speed1:real;kind1:weaponType);
    end;

    TMap = object
    Heights : array of integer;
    MaxLow,MaxHigh :integer;
    procedure Init();
    procedure Draw();
    end;

    TFont = object
    Font: PTTF_FONT;
    Path:pchar;
    Size:integer;
    Color:TSDL_COLOR;
    procedure Load(size1:integer;path1:pchar);
    procedure Resize(size1:integer);
    procedure Close();
    procedure Write(surf:PSDL_SURFACE;x,y:integer;text:pchar);
    end;

    TFile = object
    Path: pchar;
    Handle: Text;
    procedure Init(path1:pchar);
    procedure CloseFile();
    function FindInteger(name:pchar):integer;
    function FindString(name:pchar):pchar;
    end;

    TGame = object
    Width,Height,Bpp:integer;
    Caption:pchar;
    gameStatus : (gMenu,gAiming,gShooting,gHit,gQuit);
    mainScreen: PSDL_SURFACE;
    background: PSDL_SURFACE;
    Weapons: array of TWeapon;

    //Players: array of TPlayer;
    PlayersNumber,WhoIsTurn:integer;

    Fonts: array of TFont;
    FontsNumber,Font:integer;
    Map: TMap;
    procedure Loop();
    function Init(path1:pchar):boolean;
    procedure ProceedEvents();
    procedure Close();
    procedure DrawScreen();
    procedure Load_Data(path1:pchar);
    procedure Change_Font(x:integer);
    end;

implementation
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

   constructor TWeapon.Create(name1:pchar;damage1,speed1:real;kind1:weaponType);
   begin
        Kind := kind1;
        Name := name1;
        Damage := damage1;
        Speed:= speed1;
        X := 0;
        Y := 0;
        Angle := 0;
        Direction := FALSE;
   end;

   procedure TMap.Init();
   begin

   end;

   procedure TMap.Draw();
   begin

   end;

   procedure TFont.Load(size1:integer;path1:pchar);
   begin
        if Font <> NIL then
           Close();
        Size := size1;
        Path := path1;
        Font := TTF_OpenFont(Path,Size);
   end;

   procedure TFont.Resize(size1:integer);
   begin
        Close();
        Size := size1;
        Font := TTF_OpenFont(Path,Size);

   end;

   procedure TFont.Close();
   begin
        TTF_CloseFont(Font);
   end;

   procedure TFont.Write(surf:PSDL_SURFACE;x,y:integer;text:pchar);
   var msg:PSDL_SURFACE;
   begin
        msg := TTF_RenderText_Solid(Font,text,Color);
        Draw_Surface(x,y,msg,surf);
   end;

   procedure TFile.Init(path1:pchar);
   begin
        Assign(Handle,path1);
   end;

   function TFile.FindInteger(name:pchar):integer;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        while not EOF(Handle) do
        begin
             x := 0;
             ReadLn(Handle,line);
             for i := 1 to Length(line) do
             begin
                  if line[i] = name[x] then
                  begin
                       x := x + 1;
                       if x = (Length(name)) then
                       begin
                            x := i;
                            Break;
                       end;
                  end
                  else
                  x := 0;
             end;
             temp := '';

             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       x := x + 2;
                       repeat
                             temp := temp + line[x];
                            Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                       x := 0;
                       for i := Length(temp) downto 1 do
                       begin
                            x := x + (Ord(temp[i])-48)*POW(10,Length(temp)-i);
                       end;
                       FindInteger := x;
                       Break;
                  end
                  else
                  begin
                       WriteLn('Zmienna ',name,' znaleziona, blad w deklaracji');
                       FindInteger := -1;
                       Break;
                  end;
             end;
        end;

        if EOF(Handle) then
        begin
        WriteLn('Zmienna ',name,' nie znaleziona');
        FindInteger := -1;
        end;
   end;

   function TFile.FindString(name:pchar):pchar;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        while not EOF(Handle) do
        begin
             x := 0;
             ReadLn(Handle,line);
             for i := 1 to Length(line) do
             begin
                  if line[i] = name[x] then
                  begin
                       x := x + 1;
                       if x = (Length(name)) then
                       begin
                            x := i;
                            Break;
                       end;
                  end
                  else
                  x := 0;
             end;
             temp := '';

             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       x := x + 2;
                       repeat
                             temp := temp + line[x];
                             Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                       FindString := Pchar(temp);
                       Break;
                  end
                  else
                  begin
                       WriteLn('Zmienna ',name,' znaleziona, blad w deklaracji');
                       FindString := 'ERROR';
                       Break;
                  end;
             end;
        end;

        if EOF(Handle) then
        begin
        WriteLn('Zmienna ',name,' nie znaleziona');
        FindString := 'ERROR';
        end;
   end;

   procedure TFile.CloseFile();
   begin
        Close(Handle);
   end;

   function TGame.Init(path1:pchar): boolean;
   var pointer: ^TFile;
   begin
                GetMem(pointer,sizeof(TFile));
           pointer^.Init(path1);
           Width := pointer^.FindInteger('WIDTH');
           Height := pointer^.FindInteger('HEIGHT');
           BPP := pointer^.FindInteger('BPP');
           Caption := pointer^.FindString('CAPTION');
           pointer^.CloseFile();
           FreeMem(pointer,sizeof(TFile));

           if ((Width <= 0) and (Height <= 0) and (BPP <= 0) and (Caption = 'ERROR'))then
           begin
                WriteLn('Bledne dane w pliku bunkry.cfg');
                Init := FALSE;
           end;

           if SDL_Init(SDL_INIT_VIDEO) <> -1 then
           begin
                        mainScreen := SDL_SetVideoMode(Width, Height, BPP, SDL_SWSURFACE);
                        if mainScreen <> NIL then
                        begin
                             if TTF_Init() <> -1 then
                             begin
                             SDL_WM_SetCaption(Caption,NIL);
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
           gameStatus := gMenu;
           Init := FALSE;
   end;

   procedure TGame.Loop();
   begin
        repeat
         SDL_Delay(Get_Delay(FPS));
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
       until (gameStatus = gQuit);
   end;

   procedure TGame.DrawScreen();
   begin
        case gameStatus of
           gMenu:
           begin
                Set_Color(BLACK,@Fonts[Font].color);
                Fill_Surface(mainScreen,@color);
                Set_Color(WHITE,@Fonts[Font].color);
                Fonts[Font].Resize(60);
                Fonts[Font].Write(mainScreen,250,100,'B U N K R Y');
                Fonts[Font].Resize(15);
                Fonts[Font].Write(mainScreen,330,170,'Marcin Copik, Inf I, Grupa 3');
                Fonts[Font].Resize(20);
                Fonts[Font].Write(mainScreen,330,300,'1  -  Nowa Gra');
                Fonts[Font].Write(mainScreen,330,340,'2  -  Wyjdz');
           end;
           end;
           SDL_Flip(mainScreen);
   end;

   procedure TGame.Change_Font(x:integer);
   begin
        if x <= FontsNumber then
        Font := x;
   end;

   procedure TGame.ProceedEvents();
   begin
           while( SDL_PollEvent(@event)>0) do
           begin
                case event.type_ of
                SDL_KEYUP:
                begin

                end;
                SDL_KEYDOWN:
                begin

                end;
                SDL_QUITEV:
                gameStatus := gQuit;
                end;

           end;
   end;

   procedure TGame.Load_Data(path1:pchar);
   begin
           SetLength(weapons,1);
           weapons[0] := TWeapon.Create('Pocisk kalibru 45 mm',0,0,BULLET);
   end;

   procedure TGame.Close();
   begin
        SDL_FreeSurface(mainScreen);
        SDL_FreeSurface(background);
        TTF_Quit;
        SDL_Quit;
   end;

end.

