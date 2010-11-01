unit Unit6;

{$mode objfpc}

interface

uses
  Classes, SysUtils;
var z,n,i :integer;

type pointer= ^ulamek;
    ulamek=object

     x,y :Integer;
     znak: Boolean;


         function GetX():integer;
         function GetY():integer;
      procedure Dodaj( p1:pointer);
      procedure Odejmij(p1:pointer);
      procedure Mnoz(p1:pointer);
      procedure Dziel(p1:pointer);
      constructor Create(x1,y1:integer);
  end;
  type pointer2 = ^list_r;
       list_r = record
    x:integer;
    prev,next:pointer2;
  end;

  type list = object
    head:pointer2;
    constructor Create();
    procedure Add(x:integer);
  end;

  var tab: array of integer;
  implementation
  function NWD(x,y:integer):integer;
  begin

      if y > x then
      begin
             z := x;
             x := y;
             y := z;
             end;
      while(y<> 0) do
      begin
          z := x MOD y;
          x := y;
          y:= z;
          end;
          NWD := x;
          end;

  constructor ulamek.Create(x1,y1:integer);
  begin
       x := x1;
       y := y1;
  end;

  function ulamek.GetX():integer;
  begin
      GetX := x;
  end;
  function ulamek.GetY():integer;
  begin
      GetY := y;
  end;
  procedure ulamek.Dodaj(p1:pointer);
  begin
         z := y*p1^.GetY();
         //if znak = TRUE then
         x := x*p1^.GetY() + p1^.GetX()*y;
         //else
         //x := x*p1^.GetY() - p1^.GetX()*y;
         y := z;
         z := NWD(x,y);
         x := x DIV z;
         y := y DIV z;
    end;
    procedure ulamek.Odejmij(p1:pointer);
    begin
         z := y*p1^.GetY();
         x := x*p1^.GetY() - p1^.GetX()*y;
         y := z;
         z := NWD(x,y);
         x := x DIV z;
         y := y DIV z;
      end;
   procedure ulamek.Mnoz(p1:pointer);
   begin
       x := x*p1^.GetX();
       y := y*p1^.GetY();
       z := NWD(x,y);
       x := x DIV z;
       y := y DIV z;
     end;
   procedure ulamek.Dziel(p1:pointer);
   begin
       x := x*p1^.GetY();
       y := y*p1^.GetX();
       z := NWD(x,y);
       x := x DIV z;
       y := y DIV z;
     end;

         begin
          WriteLn('Program do obliczania calkowitych pierwiastkow wielomianu n-tego stopnia');
          WriteLn('za pomoca twierdzenia Gaussa');
          WriteLn('Autor:Marcin Copik, grupa 3 I-go roku Informatyki, Politechnika Slaska');
          WriteLn('Podaj stopien wielomianu:');
          Read(n);
          SetLength(tab,n);
          WriteLn('Podaj wspolczynniki wielomianu');
          for i := 0 to n do
          begin
              Read(z);
              tab[i] := z;
          end;
          {Wczytane wspolczynniki
          Czas znalezc wszystkie dzielniki wyrazu wolnego i wspolczynnika przy najwyzszej potedze}


end.

