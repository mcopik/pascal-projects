unit Unit6;

{$mode objfpc}

interface

uses
  Classes, SysUtils;
var z,n,i :integer;

type pointer= ^ulamek;
     tabp = ^array of ulamek;
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
    tail:pointer2;
    constructor Create(x,y:integer);
    destructor Delete();
    destructor Delete();
    procedure Add(x:integer);
  end;

  //type tabtype= array of integer;
  //type tabp = ^tabtype;
  var temp: pointer2;
  dzielnikiA,dzielnikiB: list;
  var tab:array of integer;
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
  function Horner(x:pointer;n:integer;tab:tabp):boolean;
  begin
       z := tab[0];
       for i:= 1 to n do
       begin
           z :=
       end;
  end;

  constructor list.Create(x,y:integer);
  begin
      GetMem(head,sizeof(list_r));
      GetMem(tail,sizeof(list_r));
      head^.prev := NIL;
      head^.next := tail;
      head^.x := x;
      tail^.next := NIL;
      tail^.prev := head;
      tail^.x := y;
  end;

  destructor list.Delete();
  begin
      //FreeMem(head,sizeof(list_r));
      temp := head;
      while temp <> NIL do
      begin
          temp := temp^.next;
          FreeMem(temp^.prev,sizeof(list_r));
      end;
  end;

  procedure list.Add(x:integer);
  begin
      GetMem(tail^.prev^.next,sizeof(list_r));
      tail^.prev^.next^.x := x;
      tail^.prev^.next^.prev := tail^.prev;
      tail^.prev^.next^.next := tail;
      tail^.prev := tail^.prev^.next;
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
         x := x*p1^.GetY() + p1^.GetX()*y;
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

     constructor list.Create();
     begin
     GetMem(head,sizeof(list_r));
     head^.prev := NIL;
     end;

     procedure list.Add(x:integer);
     begin
     end;

     destructor list.Delete();
     begin
     FreeMem(head,sizeof(list_r));
     end;

         begin
          WriteLn('Program do obliczania calkowitych pierwiastkow wielomianu n-tego stopnia');
          WriteLn('za pomoca twierdzenia Gaussa');
          WriteLn('Autor:Marcin Copik, grupa 3 I-go roku Informatyki, Politechnika Slaska');
          WriteLn('Podaj stopien wielomianu:');
          Read(n);
          SetLength(tab,n+1);
          WriteLn('Podaj wspolczynniki wielomianu');
          for i := 0 to n do
          begin
              Read(z);
              tab[i] := z;
          end;
          //wyszukanie wszystkich dzielnikow wyrazu wolnego i pierwszego
          dzielnikiA.Create(1,tab[0]);
          for i:=2 to trunc(sqrt(tab[0])) do
          begin
          if tab[0] MOD i = 0 then
          dzielnikiA.Add(i);
          end;
          dzielnikiB.Create(1,tab[n-1]);
          for i:=2 to trunc(sqrt(tab[n-1])) do
          begin
          if tab[n-1] MOD i = 0 then
          dzielnikiB.Add(i);
          end;


          {temp := dzielnikiA.head;
          while temp <> NIL do
          begin
              WriteLn(temp^.x);
              temp := temp^.next;
          end;    }

          {Wczytane wspolczynniki
          Czas znalezc wszystkie dzielniki wyrazu wolnego i wspolczynnika przy najwyzszej potedze}


end.

