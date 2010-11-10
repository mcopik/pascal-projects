unit Game_Functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  function Get_Delay(fps:integer): integer;
  function POW(a,b:integer):integer;
implementation

function Get_Delay(fps:integer): integer;
      begin
           Get_Delay := trunc(1000/FPS);
      end;
function POW(a,b:integer):integer;
var temp:integer;
    i: integer;
begin
     temp := 1;
     for i := 1 to b do
     temp := temp * a;
     POW := temp;
end;

end.

