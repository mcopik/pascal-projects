unit Main;

interface

uses sdl, sdlgui, SDLForm, SDLDraw, SDLLabel, SDLGauge,
     SDLButton, SDLScroll, SDLComboBox;

type
  TForm1 = class(TSDLForm)
    btColor: TSDLButton;
    btClose: TSDLButton;
    btQuit: TSDLButton;
    SLabel : TSDLLabel;
    procedure Button1Click(Sender: TSDLComponent;X,Y: Integer);
    procedure Button2Click(Sender: TSDLComponent;X,Y: Integer);
    procedure Button3Click(Sender: TSDLComponent;X,Y: Integer);
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
  end;

  TForm2 = class(TSDLForm)
    Gauge: TSDLGauge;
    procedure sbAlphaChange(Sender: TSDLComponent);
  public
    constructor Create(AParent: TSDLObject); override;
  end;

var Form1 : TForm1;
    Form2: TForm2;
    Images: TSDLImages;

implementation

uses SysUtils, Classes, SDLEdit, SDLMenus, SDLCheck, SDLMemo,
     SDLListBox;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TSDLComponent; X, Y: Integer);
begin
  {FocusColor := Random($FFFFFF);
  Color := Random($FFFFFF);}
  Font.Color := Random($FFFF00)+$FF;
end;

procedure TForm1.Button2Click(Sender: TSDLComponent; X, Y: Integer);
begin
  Kill;
end;

procedure TForm1.Button3Click(Sender: TSDLComponent; X, Y: Integer);
var qe : TSDL_Event;
begin
  FillChar(qe,SizeOf(qe),0);
  qe.type_ := SDL_QUITEV;
  SDL_PushEvent(@qe);
end;

constructor TForm1.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('Form1.xfm');
  Images := RImages;
end;

destructor TForm1.Destroy;
begin
  if DestroyRImages then Images := nil;
  inherited;
  Form1 := nil;
end;

{ TForm2 }

constructor TForm2.Create(AParent: TSDLObject);
begin
  inherited;
  RImages := Images;
  LoadCompFromFile('Form2.xfm');
end;

procedure TForm2.sbAlphaChange(Sender: TSDLComponent);
begin
  with Sender as TSDLScrollBar do
  begin
    Gauge.Progress := Position;
    if Assigned(Image) then
      Image.Alpha := Position*255 div 100;
  end;
end;

end.
