unit tape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type

  pointer=^Tape_Field;

  Tape_Field =record
  X:char;
  Prev,Next:pointer;
  end;

  TTape = object
  Head:pointer;
  Tail:pointer;
  procedure Create();
  procedure Delete();
  procedure Add(x:char);
  procedure Add_End(x:char);
  procedure WriteToTape(x:char;pos:integer);
  function Get(x:integer):char;
  end;

implementation

procedure TTape.Create();
begin
     GetMem(Head,sizeof(Tape_Field));
     GetMem(Tail,sizeof(Tape_Field));
     Head^.Prev := NIL;
     Head^.Next := Tail;
     Tail^.Prev := Head;
     Tail^.Next := NIL;
end;

procedure TTape.WriteToTape(x:char;pos:integer);
var i:integer;
    temp:pointer;
begin
     temp:=Head;
     for i := 0 to pos do
     temp := temp^.Next;
     temp^.X := x;
end;

procedure TTape.Add(x:char);
var temp:pointer;
begin
     GetMem(temp,sizeof(Tape_Field));
     temp^.Next := Head^.Next;
     temp^.Prev := Head;
     Head^.Next^.Prev := temp;
     Head^.Next := temp;
     temp^.X := x;
end;

procedure TTape.Add_End(x:char);
var temp:pointer;
begin
     GetMem(temp,sizeof(Tape_Field));
     temp^.Next := Tail;
     temp^.Prev := Tail^.Prev;
     Tail^.Prev^.Next := temp;
     Tail^.Prev := temp;
     temp^.X := x;
end;

function TTape.Get(x:integer):char;
var i:integer;
    temp:pointer;
begin
     temp:=Head;
     for i := 0 to x do
     temp := temp^.Next;
     Get := temp^.X;
end;

procedure TTape.Delete();
var temp:pointer;
begin
     temp := Head^.Next;
     FreeMem(Head,sizeof(Tape_Field));
     while temp <> Tail do
     begin
        FreeMem(temp,sizeof(Tape_Field));
        temp := temp^.Next;
     end;
     FreeMem(Tail,sizeof(Tape_Field));
end;
end.

