unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation
           var i, j: integer;
           const m=3;n=2;
   var mnozne: array[1..m] of integer;
   mnozniki: array[1..n] of integer;
   iloczyny: array[1..m, 1..n] of integer; {tablica dwuwymiarowa}

begin
   writeln('Podaj ', m, ' liczb (mnożne):');
   for i := 1 to m do
      read(mnozne[i]);
   readln; {chroni przed bałaganem}

   writeln('Teraz podaj ', n, ' liczb (mnożniki):');
   for i := 1 to n do
      read(mnozniki[i]);
   readln; {chroni przed bałaganem}

   {obliczanie}
   for i := 1 to m do
      for j := 1 to n do
         iloczyny[i, j] := mnozne[i] * mnozniki[j];

   {drukowanie wyniku}
   for i := 1 to m do
   begin
      for j := 1 to n do
         write(iloczyny[i, j]: 4);
      writeln;
   end;

   readln;
end.

end.

