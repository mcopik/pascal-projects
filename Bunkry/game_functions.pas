unit game_functions;

{$mode objfpc}{$H+}

interface
uses SysUtils,game_variables;
  function GetTime(): integer;
  //function POW(a,b:integer):integer;
  type
    TFile = object
    Path: string;
    Handle: Text;
    procedure Init(path1:string);
    procedure CloseFile();
    function FindInteger(name:string):integer;
    function FindString(name:string):string;
  end;
implementation

function GetTime(): integer;
begin
     GetTime := trunc(1000/FPS);
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

procedure TFile.Init(path1:string);
   begin
        Path := path1;
        Assign(Handle,pchar(path1));
   end;

   function TFile.FindInteger(name:string):integer;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        FindInteger := -1;
        while not EOF(Handle) do
        begin
             x := 1;
             ReadLn(Handle,line);
             if(Length(line) > 0) then
             begin
             for i := 1 to Length(line) do
             begin
                  if line[i] = name[x] then
                  begin
                       if x = (Length(name)) then
                       begin
                            x := i;
                            Break;
                       end;
                       x := x + 1;

                  end
                  else
                  x := 1;
             end;
             temp := '';
             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       i := x + 2;
                       x := i;
                       repeat
                             Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                       temp := Copy(line,i,x-i);
                       x := 0;
                       for i := Length(temp) downto 1 do
                       begin
                            x := x + (Ord(temp[i])-48)*POW(10,Length(temp)-i);
                       end;
                       FindInteger := x;
                       Break;
                  end
                  else
                  begin
                       WriteLn('Zmienna ',name,' znaleziona, blad w deklaracji');
                       FindInteger := -2;
                       Break;
                  end;
             end;
             end;
        end;

        if FindInteger = -1 then
        begin
        WriteLn('Zmienna ',name,' nie znaleziona');
        end
        else if FindInteger = -2 then
        begin
        FindInteger := -1;
        end;
   end;

   function TFile.FindString(name:string):string;
   var x,i:integer;
       line,temp:string;
   begin
        Reset(Handle);
        FileMode := fmOpenRead;
        while not EOF(Handle) do
        begin
             x := 1;
             ReadLn(Handle,line);
             if Length(line)>0 then
             begin
             for i := 1 to Length(line)do
             begin
                  if line[i] = name[x] then
                  begin
                       if x = Length(name) then
                       begin
                            x := i;
                            Break;
                       end;
                       x := x + 1;

                  end
                  else
                  x := 1;
             end;
             temp := '';
             if i <> Length(line) then
             begin
                  if line[x+1] = '=' then
                  begin
                       x := x + 2;
                       i := i + 2;
                       repeat
                             Inc(x);
                       until ((line[x]=';') or (x=(Length(line)+1)));
                       temp := Copy(line,i,x-i);
                       FindString := temp;
                       Break;
                  end
                  else
                  begin
                       WriteLn('Zmienna ',name,' znaleziona, blad w deklaracji');
                       FindString := 'ERROR';
                       Break;
                  end;
             end;
             end;
        end;

        if EOF(Handle) then
        begin
        WriteLn('Zmienna ',name,' nie znaleziona');
        FindString := 'ERROR';
        end;
   end;

   procedure TFile.CloseFile();
   begin
        Close(Handle);
   end;

end.

