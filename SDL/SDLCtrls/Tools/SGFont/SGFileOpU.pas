unit SGFileOpU;

interface

uses SDLLabel, SDLButton, SDLFileCtrls,
  sdlgui, SDLDraw, SDLForm;

type
  TSDLForm1 = class(TSDLForm)
    SDLLabel5: TSDLLabel;
    SDLLabel4: TSDLLabel;
    SDLLabel3: TSDLLabel;
    SDLLabel2: TSDLLabel;
    SDLLabel1: TSDLLabel;
    btRemove: TSDLButton;
    btAdd: TSDLButton;
    btChoose: TSDLButton;
    procedure btChooseClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btAddClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btRemoveClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
    OpenDialog: TSDLOpenDialog;
    procedure SetFileName(Sender: TSDLOpenDialog;const FileName: string);
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
  end;

var
  SDLForm1: TSDLForm1;

implementation

uses Classes, SysUtils;

procedure TSDLForm1.btAddClick(Sender: TSDLComponent; AX, AY: Integer);
var Img,Dat: TFileStream;
    p1,p2: PByte;
    s: Integer;
begin
  SDLLabel5.Caption := 'Status: No File';
  if SDLLabel2.Caption='' then Exit;
  SDLLabel5.Caption := 'Status: No Data File';
  if not FileExists(SDLLabel4.Caption) then Exit;
  Img := TFileStream.Create(SDLLabel2.Caption,fmOpenReadWrite);
  Dat := TFileStream.Create(SDLLabel4.Caption,fmOpenRead);
  s := Dat.Size;
  GetMem(p1, s);
  GetMem(p2, s);
  try
    Img.Seek(-s,soFromEnd);
    Img.Read(p1^,s);
    Dat.Read(p2^,s);
    Dat.Position := 0;
    Img.Seek(0, soFromEnd);
    if CompareMem(p1,p2,s) then
      SDLLabel5.Caption := 'Status: Data already there'
    else
    begin
      Img.CopyFrom(Dat,s);
      SDLLabel5.Caption := 'Status: Data added successfuly';
    end;
  finally
    FreeMem(p1);
    FreeMem(p2);
    Img.Free;
    Dat.Free;
  end;
end;

procedure TSDLForm1.btChooseClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  OpenDialog.Execute(SetFileName);
end;

procedure TSDLForm1.btRemoveClick(Sender: TSDLComponent; AX, AY: Integer);
var Img,Dat: TFileStream;
    i: Integer;
begin
  SDLLabel5.Caption := 'Status: No File';
  if SDLLabel2.Caption='' then Exit;
  if not FileExists(SDLLabel2.Caption) then Exit;
  Img := TFileStream.Create(SDLLabel2.Caption,fmOpenReadWrite);
  Dat := TFileStream.Create(SDLLabel4.Caption,fmCreate);
  try                        
    Img.Seek(-SizeOf(Integer),soFromEnd);
    Img.Read(i,SizeOf(Integer));
    Img.Seek(i,soFromEnd);
    Dat.CopyFrom(Img,-i);
    SDLLabel5.Caption := 'Status: Extracting successful';
  finally
    Img.Free;
    Dat.Free;
  end;
end;

constructor TSDLForm1.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('FontOp.xfm');
  OpenDialog := TSDLOpenDialog.Create(AParent);
  OpenDialog.Filter := '*.bmp;*.png;*.jpg;*.tga;*.tif;*.gif;*.pcx|'+
      '*.bmp|*.png|*.jpg|*.tga|*.tif|*.gif|*.pcx|All files (*.*)';
  SetFocus;
end;

procedure TSDLForm1.SetFileName(Sender: TSDLOpenDialog;
  const FileName: string);
begin
  SDLLabel2.Caption := FileName;
  SDLLabel4.Caption := Copy(FileName,1,Length(FileName)-3)+'dat';
end;

end.
