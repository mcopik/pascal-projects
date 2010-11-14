unit Game_Variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl;
type   //surfacePointer=PSDL_SURFACE;
       //fontPointer=PTTF_FONT;
       //colorPointer=PSDL_Color;
       weaponType=(BULLET,ROCKET);
       colors=(WHITE,BLUE,GREEN,BLACK);

var
   event : TSDL_Event;
   rect: TSDL_Rect;
   color: TSDL_Color;
const
     FPS:integer = 25;
implementation

end.

