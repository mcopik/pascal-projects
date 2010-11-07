program FilterTest;

uses
  {$IFDEF WIN32}
  Forms,
  {$ENDIF}
  {$IFDEF UNIX}
  QForms,
  {$ENDIF}
  Filtering in 'Filtering.pas' {Form1},
  SDLFilter in '../../Pas/SDLFilter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
