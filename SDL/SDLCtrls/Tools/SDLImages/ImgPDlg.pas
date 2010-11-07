unit ImgPDlg;

interface

uses SDLLabel, SDLButton, SDLEdit, SDLCheck, 
  sdlgui, SDLDraw, SDLForm;

type
  TImgPropsDlg = class(TSDLForm)
    btCancel: TSDLButton;
    btOK: TSDLButton;
    cbTrans: TSDLCheckBox;
    cbUseAlpha: TSDLCheckBox;
    edAlpha: TSDLEdit;
    edName: TSDLEdit;
    edPattH: TSDLEdit;
    edPattW: TSDLEdit;
    edTransColor: TSDLEdit;
    SDLLabel1: TSDLLabel;
    SDLLabel2: TSDLLabel;
    SDLLabel3: TSDLLabel;
    SDLLabel4: TSDLLabel;
    SDLLabel5: TSDLLabel;
    procedure btCancelClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btOKClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
  end;

var
  ImgPropsDlg: TImgPropsDlg;

implementation

constructor TImgPropsDlg.Create(AParent: TSDLObject);
begin
  inherited;
  LoadFromFile('ImgPDlg.xfm');
end;

end.
