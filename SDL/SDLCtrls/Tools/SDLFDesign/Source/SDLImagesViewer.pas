unit SDLImagesViewer;

interface

uses SDLLabel, SDLButton, SDLListBox, 
  sdlgui, SDLDraw, SDLForm;

type
  TImgsView = class(TSDLForm)
    SDLLabel1: TSDLLabel;
    btClose: TSDLButton;
    SDLListBox1: TSDLListBox;
    procedure btCloseClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
    function SetFocus: Boolean; override;
    procedure Show;
  end;

var
  ImgsView: TImgsView;

implementation

uses SysUtils;

constructor TImgsView.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('../Data/ImgsViewer.xfm');
  Visible := False;
end;

procedure TImgsView.btCloseClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  LoseFocus;
  Visible := False;
end;

procedure TImgsView.Show;
begin
  Visible := True;
  SetFocus;
end;

function TImgsView.SetFocus: Boolean;
var i: Integer;
    t: string;
begin
  Result := inherited SetFocus;
  SDLListBox1.Items.Clear;
  with Gui.DesignForm as TSDLForm do
    if Assigned(RImages) then
      for i := 0 to RImages.Count-1 do
        with RImages.Items[i] do
        begin
          t := IntToStr(i)+'. '+Name+' ';
          t := t+IntToStr(PatternWidth)+'x'+IntToStr(PatternHeight);
          SDLListBox1.Items.Add(t);
        end;
end;

end.
