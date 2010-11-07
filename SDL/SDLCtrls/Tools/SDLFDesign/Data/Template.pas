unit Unit1;

interface

uses
  sdlgui, SDLDraw, SDLForm;

type
  TSDLForm1 = class(TSDLForm)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
  end;

var
  SDLForm1: TSDLForm1;

implementation

constructor TSDLForm1.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile({Insert FileName});
end;

end.