unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation
         var i : Real;
         function pole(var a:Real):Real;
         begin
           pole := (0.001*(sin(a)+sin(a+0.001)))/2;
         end;
              //var i:Real;
              var suma:Real;
begin
     i := 0;
     suma := 0;
     while  i < PI/2 do
     begin
              suma := suma + pole(i);
              i := i + 0.001;
     end;
   WriteLn(suma);
   WriteLn(sin((1/2)*PI));
end.

end.

end.

