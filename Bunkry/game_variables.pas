unit game_variables;


interface

uses
  Classes, SysUtils,
  sdl;
type
       colors=(WHITE,BLUE,GREEN,BLACK,YELLOW,RED);
       textPosition=(LEFT,RIGHT,CENTER);
       changeType=(NONE,DECR,INCR,UP,DOWN);

var
       event : TSDL_Event;
       rect: TSDL_Rect;
       color: TSDL_Color;
const
       FPS:integer = 30;
       TRANSPARENCY:array[1..3] of integer = (255,0,255);
implementation

end.

