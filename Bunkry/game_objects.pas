unit Game_Objects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,sdl,sdl_ttf,Game_Variables,Graphic_Functions,Game_Functions;
  type
    TWeapon = class
    Name:string;
    Damage, Speed:real;
    X,Y,Angle:integer;
    Direction: boolean; //FALSE-LEWO;TRUE-PRAWO
    Kind : weaponType;
    procedure Draw(surface:PSDL_SURFACE);
    function Calculate():integer;
    function GetY():integer;
    procedure Shoot(x1,y1,angle1:integer;direction1:boolean);
    procedure Init(name1:string;damage1,speed1:real;kind1:weaponType);
    end;

    TMap = class
    Heights : array of integer;
    Min,Max :integer;
    Color: TSDL_Color;
    Background:(FILL,BPATH);
    Background_Fill:TSDL_Color;
    Background_Path:string;
    procedure Init(mi,ma,r,g,b,width:integer);
    procedure Draw();
    procedure SetBackground(r,g,b:integer);
    procedure SetBackground(path1:string);
    end;

    TFont = class
    Font: PTTF_FONT;
    Path:string;
    Size:integer;
    Color:TSDL_COLOR;
    msg:PSDL_SURFACE;
    procedure Load(size1:integer;path1:string);
    procedure Resize(size1:integer);
    procedure Close();
    procedure Write(surf:PSDL_SURFACE;x,y:integer;text:string);
    end;

    TFile = class
    Path: string;
    Handle: Text;
    procedure Init(path1:string);
    procedure CloseFile();
    function FindInteger(name:string):integer;
    function FindString(name:string):string;
    end;

    TGame = class
    Width,Height,Bpp:integer;
    Caption:string;
    gameStatus : (gMenu,gAiming,gShooting,gHit,gQuit);
    mainScreen: PSDL_SURFACE;
    background: PSDL_SURFACE;
    Weapons: array of TWeapon;
    WeaponsNumber:integer;

    //Players: array of TPlayer;
    //PlayersNumber,WhoIsTurn:integer;

    Fonts: array of TFont;
    FontsNumber,Font:integer;

    Maps: array of TMap;
    MapsNumber,Map:integer;
    procedure Loop();
    constructor ini();
    function Init(path1:string):boolean;
    procedure ProceedEvents();
    procedure Close();
    procedure DrawScreen();
    function Load_Data(path1:string): boolean;
    procedure Change_Font(x:integer);
    end;

implementation
   constructor TGame.ini();
   begin
   end;
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

   procedure TWeapon.Init(name1:string;damage1,speed1:real;kind1:weaponType);
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

   procedure TMap.Init(mi,ma,r,g,b,width:integer);
   var i :integer;
   begin
        Min := mi;
        Max := ma;
        Color.r := r;
        Color.g := g;
        Color.b := b;
        WriteLn(width);
        SetLength(Heights,width);
        randomize();
        for i := 1 to width do
        begin
             repeat
             Heights[i-1] := random(Max);
             until Heights[i-1] < Min;
             //WriteLn(Heights[i]);
        end;
   end;

   procedure TMap.Draw();
   begin

   end;

   procedure TMap.SetBackground(r,g,b:integer);
   begin
        Background:= FILL;
        Background_Fill.r := r;
        Background_Fill.g := g;
        Background_Fill.b := b;
   end;

   procedure TMap.SetBackground(path1:string);
   begin
        Background:= BPATH;
        Background_Path := path1;
   end;

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

   procedure TFont.Write(surf:PSDL_SURFACE;x,y:integer;text:string);
   begin
        msg := TTF_RenderText_Solid(Font,pchar(text),Color);
        Draw_Surface(x,y,msg,surf);
   end;

   procedure TFile.Init(path1:string);
   begin
        Path := path1;
        Assign(Handle,pchar(path1));
   end;

   function TFile.FindInteger(name:string):integer;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        //FileMode := fmOpenRead;
        FindInteger := -1;
        while not EOF(Handle) do
        begin
             x := 1;
             ReadLn(Handle,line);
             //WriteLn(line);
             if(Length(line) > 0) then
             begin
             for i := 1 to Length(line) do
             begin
                  //WriteLn(line,' ',i,' ',name,' ',x);
                  if line[i] = name[x] then
                  begin
                       if x = (Length(name)) then
                       begin
                            x := i;
                            Break;
                       end;
                       x := x + 1;

                  end
                  else
                  x := 1;
             end;
             temp := '';
             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       i := x + 2;
                       x := i;
                       repeat
                             //WriteLn(temp,' ',line[x],' ',x);
                             //WriteLn(temp+line[x]);
                             //temp := String(temp + line[x]);
                             //WriteLn(temp,' ',line[x],' ',x);
                             Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                       temp := Copy(line,i,x-i);
                       //WriteLn(temp);
                       x := 0;
                       for i := Length(temp) downto 1 do
                       begin
                            x := x + (Ord(temp[i])-48)*POW(10,Length(temp)-i);
                       end;
                       //WriteLn(temp,' ',x,' ',x*10,' ',x + 5);
                       FindInteger := x;
                       Break;
                  end
                  else
                  begin
                       WriteLn('Zmienna ',name,' znaleziona, blad w deklaracji');
                       FindInteger := -2;
                       Break;
                  end;
             end;
             end;
        end;

        if FindInteger = -1 then
        begin
        WriteLn('Zmienna ',name,' nie znaleziona');
        end
        else if FindInteger = -2 then
        begin
        FindInteger := -1;
        end;
        WriteLn(FindInteger,'ok');
   end;

   function TFile.FindString(name:string):string;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        FileMode := fmOpenRead;
        while not EOF(Handle) do
        begin
             x := 1;
             ReadLn(Handle,line);
             //WriteLn(line);
             if Length(line)>0 then
             begin
             for i := 1 to Length(line)do
             begin
                  if line[i] = name[x] then
                  begin
                       if x = Length(name) then
                       begin
                            //WriteLn(line[i],' ',i,' ',name[x]);
                            x := i;
                            Break;
                       end;
                       x := x + 1;

                  end
                  else
                  x := 1;
             end;
             temp := '';
             //WriteLn(line,' ',i,' ',name,' ',x);
             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       x := x + 2;
                       i := i + 2;
                       repeat
                             //temp := temp + line[x];
                             Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                     //  WriteLn(line,' ',i,' ',x);
                       temp := Copy(line,i,x-i);
                       //WriteLn(temp);
                       FindString := temp;
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

   function TGame.Init(path1:string): boolean;
   var pt: TFile;
   begin
           Init := FALSE;
           pt := TFile.Create(); //new(pt);
           pt.Init(path1);
           Width := pt.FindInteger(string('WIDTH'));
           Height := pt.FindInteger(string('HEIGHT'));
           BPP := pt.FindInteger('BPP');
           Caption := pt.FindString('CAPTION');
           pt.CloseFile();
           pt.Free();
           //dispose(pt);
           if ((Width <= 0) or (Height <= 0) or (BPP <= 0) or (Caption = 'ERROR'))then
           begin
                WriteLn('Bledne dane w pliku ',path1);
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
           gameStatus := gMenu;
           if Init = FALSE then
           WriteLn('Utworzenie okna ',Width,'x',Height);
           end;
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
       //WriteLn(Weapons[1].Name);
   end;

   procedure TGame.DrawScreen();
   begin
        case gameStatus of
           gMenu:
           begin
                Set_Color(BLACK,@Fonts[0].color);
                Fill_Surface(mainScreen,@color);
                Set_Color(WHITE,@Fonts[0].color);
                Fonts[0].Resize(60);
                //Fonts[0].Write(mainScreen,250,100,'B U N K R Y');
                {Fonts[Font]^.Resize(15);
                Fonts[Font]^.Write(mainScreen,330,170,'Marcin Copik, Inf I, Grupa 3');
                Fonts[Font]^.Resize(20);
                Fonts[Font]^.Write(mainScreen,330,300,'1  -  Nowa Gra');
                Fonts[Font]^.Write(mainScreen,330,340,'2  -  Wyjdz');}
           end;
           gAiming:
           //Maps[Map].Draw();
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

   function TGame.Load_Data(path1:string):boolean;
   var pt:TFile;
       i,temp,temp2,temp3,temp4,temp5,temp6,temp7:integer;
       line1,line2:string;
   begin
             Load_Data := FALSE;
             //new(pt);
             pt := TFile.Create();
             pt.Init(path1);
             //WriteLn(IntToStr(PtrUInt(pt)));
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
                       line2 := pt.FindString(string('WEAPON'+IntToStr(i)+'_TYPE'));
                       if((temp2 <> -1) and (temp3 <> -1) and (line1 <> 'ERROR') and (line2 <> 'ERROR')) then
                       begin
                            //new(weapons[i]);
                            Write(temp,i);
                            Weapons[i-1] := TWeapon.Create();
                            if line2 = 'BULLET' then
                            begin
                            Weapons[i-1].Init(string(line1),temp2,temp3,BULLET);
                            end
                            else if line2 = 'ROCKET' then
                            begin
                            Weapons[i-1].Init(string(line1),temp2,temp3,ROCKET);
                            end
                            else
                            begin
                            WriteLn('Bledne dane w pliku ',path1,' - nieznany typ broni');
                            Load_Data := TRUE;
                            end;
                       end
                       else
                       begin
                            WriteLn('Bledne dane w pliku ',path1);
                            Load_Data := TRUE;
                       end;
                  end;
                  if Load_Data = FALSE then
                  WriteLn('Zaladowane bronie');
             end
             else
             begin
                  WriteLn('Bledne dane broni w pliku ',path1,' - brak/zla definicja zmiennej WEAPONS_NUMBER');
                  Load_Data := TRUE;
             end;

             if Load_Data = FALSE then
             begin
             temp := pt.FindInteger('FONTS_NUMBER');
             if temp <> -1 then
             begin
                  SetLength(Fonts,temp);
                  FontsNumber := temp;
                  Font := 1;
                  for i:=1 to temp do
                  begin
                       temp2 := pt.FindInteger(string('FONT'+IntToStr(i)+'_SIZE'));
                       line1 := pt.FindString(string('FONT'+IntToStr(i)+'_PATH'));
                       //new(Fonts[i]);
                       WriteLn(temp2,line1);
                       if((temp2 <> -1) and (line1 <> 'ERROR')) then
                       begin
                            Fonts[i-1] := TFont.Create();
                            Fonts[i-1].Load(temp2,line1);

                            if Fonts[i-1].Font = NIL  then
                            begin
                                 WriteLn('Blad w trakcie wczytywania czcionki');
                                 Load_Data := TRUE;
                            end;
                       end
                       else
                       begin
                            WriteLn('Bledne dane czcionek w pliku ',path1);
                            Load_Data := TRUE;
                       end;
                  end;
                  if Load_Data = FALSE then
                  WriteLn('Zaladowane czcionki');
             end
             else
             begin
                  WriteLn('Bledne dane w pliku ',path1,' - brak/zla definicja zmiennej FONTS_NUMBER');
                  Load_Data := TRUE;
             end;
             end;
             //WriteLn(pt^.Path);
             //WriteLn(IntToStr(PtrUInt(pt)));
             //pt.CloseFile();
             //pt.Init(path1);
             WriteLn(temp);
             temp7 := pt.FindInteger('FONT1_SIZE');
             WriteLn(temp7);
             WriteLn(Load_Data);
             if Load_Data = FALSE then
             begin
             temp7 := pt.FindInteger('MAPS_NUMBER');
             //temp := 1;
             WriteLn('temp ',temp7);
             if temp7 <> -1 then
             begin
                  SetLength(Maps,temp7);
                  MapsNumber := temp7;
                  Map := 1;
                  for i:=1 to temp7 do
                  begin
                       Maps[i-1] := TMap.Create;
                       temp2:= pt.FindInteger(string('MAP'+IntToStr(i)+'_MIN'));
                       temp3 := pt.FindInteger(string('MAP'+IntToStr(i)+'_MAX'));
                       temp4 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORR'));
                       temp5 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORG'));
                       temp6 := pt.FindInteger(string('MAP'+IntToStr(i)+'_COLORB'));
                       line1 := pt.FindString(string('MAP'+IntToStr(i)+'_BACKGROUND'));
                       //new(Maps[i]);
                       if((temp2 <> -1) and (temp3 <> -1)and(temp4 <> -1) and (temp5 <> -1)and (temp6 <> -1)) then
                       begin
                            if line1 = 'FILL' then
                            begin
                                 Maps[i-1].Init(temp2,temp3,temp4,temp5,temp6,Width);
                                 temp4 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORR'));
                                 temp5 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORG'));
                                 temp6 := pt.FindInteger(string('MAP'+IntToStr(i)+'_BACKGROUNDCOLORB'));
                                 Maps[i-1].SetBackground(temp4,temp5,temp6);
                            end
                            else if line1 = 'PATH' then
                            begin
                                 Maps[i-1].Init(temp2,temp3,temp4,temp5,temp6,Width);
                                 line1 := pt.FindString(string('MAP'+IntToStr(i)+'_BACKGROUNDPATH'));
                                 Maps[i-1].SetBackground(line1);
                            end
                            else
                            begin
                                 WriteLn('Bledne dane w pliku',path1,' - zla definicja tla mapy');
                                 Load_Data := TRUE;
                            end;
                       end
                       else
                       begin
                            WriteLn('Bledne dane map w pliku ',path1);
                            Load_Data := TRUE;
                       end;
                  end;
                  if Load_Data = FALSE then
                  WriteLn('Zaladowane mapy');
             end
             else
             begin
                  WriteLn('Bledne dane w pliku ',path1,' - brak/zla definicja zmiennej MAPS_NUMBER');
                  Load_Data := TRUE;
             end;
             end;
             pt.Free();
             //dispose(pt);
   end;



   procedure TGame.Close();
   var i,j:integer;
   begin

        for i := 1 to FontsNumber do
        begin
             SDL_FreeSurface(Fonts[i-1].msg);
             Fonts[i-1].Close();
             Fonts[i-1].Free;
             //dispose(Fonts[i]);
        end;
        for i := 1 to WeaponsNumber do
        begin
             Weapons[i-1].Free;
             //dispose(Fonts[i]);
        end;
        for i := 1 to MapsNumber do
        begin
             Maps[i-1].Free;
             //dispose(Fonts[i]);
        end;
       { WriteLn('ok');
        for i := 1 to WeaponsNumber do
            dispose(Weapons[i]);
        WriteLn('ok2');
        for i := 1 to MapsNumber do
        begin
             for j := 1 to Width do
             begin
                  //dispose(Maps[i]^.Heights[j]);
             WriteLn(j);
             end;
             dispose(Maps[i]);
        end;   }
        SDL_FreeSurface(mainScreen);
        SDL_FreeSurface(background);
        TTF_Quit;
        SDL_Quit;
   end;

end.

