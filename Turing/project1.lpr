program project1;


uses sysutils, machine, input;

var Turing:TTuring;
{ Turing_Machine }

begin
  WriteLn('Symulator maszyny Turinga.');
  WriteLn('Marcin Copik, Grupa 3, Inf I 2010,Pol. Slaska');
  Get_Input(@Turing);
  WriteLn('Maszyna Turinga zaczyna prace');
  Turing.Run();
end.

