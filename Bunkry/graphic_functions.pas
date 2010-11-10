unit Graphic_Functions;



interface

uses
  sdl,sdlutils,sdl_ttf,Game_Variables;

    //procedure Write_Text(surf:PSDL_SURFACE;font:PTTF_FONT;color:PSDL_COLOR;x,y:integer;text:pchar);
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
                   end;
              end;

              procedure Fill_Surface(surf:PSDL_SURFACE;color:PSDL_COLOR);
              begin
                   SDL_FillRect(surf,NIL,SDL_MapRGB(surf^.format,color^.r,color^.g,color^.b));
              end;

end.

