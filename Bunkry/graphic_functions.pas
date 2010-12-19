unit graphic_functions;



interface

uses
  sdl,sdl_image,game_variables;
    function Load_Image(path:string;transp:boolean):PSDL_SURFACE;
    procedure Draw_Surface(x,y:integer;src,dst:PSDL_SURFACE);
    procedure Set_Color(col:colors;color:PSDL_COLOR);
    procedure Fill_Surface(surf:PSDL_SURFACE;color:PSDL_COLOR);
implementation

              procedure Draw_Surface(x,y:integer;src,dst:PSDL_SURFACE);
              begin
                   rect.x := x;
                   rect.y := y;
                   SDL_BlitSurface(src,NIL,dst,@rect);
              end;

              procedure Set_Color(col:colors;color:PSDL_COLOR);
              begin
                   case col of
                   WHITE:
                   begin
                   color^.r := 255;
                   color^.g := 255;
                   color^.b := 255;
                   end;
                   BLACK:
                   begin
                   color^.r := 0;
                   color^.g := 0;
                   color^.b := 0;
                   end;
                   YELLOW:
                   begin
                   color^.r := 255;
                   color^.g := 255;
                   color^.b := 0;
                   end;
                   BLUE:
                   begin
                   color^.r := 0;
                   color^.g := 255;
                   color^.b := 255;
                   end;
                   RED:
                   begin
                   color^.r := 255;
                   color^.g := 0;
                   color^.b := 0;
                   end;
                   end;
              end;

              procedure Fill_Surface(surf:PSDL_SURFACE;color:PSDL_COLOR);
              begin
                   SDL_FillRect(surf,NIL,SDL_MapRGB(surf^.format,color^.r,color^.g,color^.b));
              end;

              function Load_Image(path:string;transp:boolean):PSDL_SURFACE;
              var temp,return:PSDL_SURFACE;
              begin
                   temp := IMG_Load(PChar(path));
                   if temp <> NIL then
                   begin
                        if transp = TRUE then
                        SDL_SetColorKey(temp, SDL_SRCCOLORKEY,PUint32(temp^.pixels)^);

                        return := SDL_DisplayFormat(temp);
                        SDL_FreeSurface(temp);
                        Load_Image := return;
                   end
                   else
                   Load_Image := NIL;
              end;

end.

