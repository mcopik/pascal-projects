unit Game_Variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  sdl;
type
       colors=(WHITE,BLUE,GREEN,BLACK,YELLOW,RED);
       textPosition=(NONE,LEFT,RIGHT,CENTER);

var
       event : TSDL_Event;
       rect: TSDL_Rect;
       color: TSDL_Color;
const
       FPS:integer = 25;
       TRANSPARENCY:array[1..3] of integer = (255,0,255);
implementation

end.

