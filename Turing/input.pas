unit input;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, machine;
type pointer=^TTuring;
procedure Get_Input(Turing:pointer);
implementation

procedure Get_Input(Turing:pointer);
var temp:integer;
k,g:char;
i,j:integer;
stemp:string;
tempch:array of char;
begin
  //SYMBOLS
    WriteLn('Podaj liczbe symboli - symbol $ bedzie traktowany jako symbol pusty');
    ReadLn(temp);
    SetLength(tempch,temp+1);
    WriteLn('Podaj symbole');
    for i := 1 to temp do
    begin
         ReadLn(k);
         tempch[i-1] := k;
    end;
    tempch[temp] := '$';
    Turing^.SetSymbols(temp+1,tempch);

  //HEADS
  WriteLn('Podaj liczbe stanow glowicy. Przedostatni stan bedzie traktowany jako stan koncowy,ostatni jako stan bledny.');
  repeat
  ReadLn(temp);
  until temp >= 3;
  Turing^.SetHeadsNumber(temp);

  Turing^.Init();

  //TABELA DZIALANIA
  WriteLn('Wczytywanie tabeli rozkazow dla maszyny');
    for i := 0 to Turing^.HeadStatesNumber-3 do
    begin
         for j := 0 to Turing^.SymbolsNumber-1 do
         begin
              WriteLn('Stan glowicy q'+IntToStr(i)+', symbol '+Turing^.Symbols[j]);
              repeat
              Write('Podaj symbol do zapisania: ');
              ReadLn(k);
              until Turing^.IsSymbol(k);
              repeat
              Write('Podaj symbol ruchu L/P/N: ');
              ReadLn(g);
              until ((g = 'L')or(g='N')or(g='P'));
              repeat
              Write('Podaj numer stanu, w ktory ma przejsc glowica: ');
              ReadLn(temp);
              until ((temp>=0)or(temp<Turing^.HeadStatesNumber));
              Turing^.SetOrder(i,j,k,g,temp);
         end;
    end;


  //TAPE
    WriteLn('Podaj dlugosc danych na tasmie');
    ReadLn(temp);
    SetLength(tempch,temp);
    WriteLn('Podaj symbole na tasmie');
    i := 0;
    repeat
         ReadLn(k);
         if Turing^.IsSymbol(k) = TRUE then
         begin
         tempch[i] := k;
         Inc(i);
         end
         else
         begin
         WriteLn('Bledny symbol');
         end;
    until i = temp;
    Turing^.SetTape(temp,tempch);

  WriteLn('Podaj miejsce startu glowicy - L(lewo)/P(prawo)');
  repeat
  ReadLn(k);
  until ((k = 'L') or (k='P'));
  if k = 'L' then
  begin
  Turing^.SetPosition(FALSE);
  end
  else
  Turing^.SetPosition(TRUE);

  WriteLn('Podaj opoznienie z jakim maszyna ma wykonywac kroki(w ms); 0 - brak opoznienia;ujemna wartosc - zapis do pliku');
  ReadLn(temp);
  if temp >= 0 then
  begin
  Turing^.SetDelay(temp);
  end
  else
  begin
  WriteLn('Podaj nazwe pliku do zapisu');
  ReadLn(stemp);
  Turing^.SetPath(stemp);
  end;
end;

end.

