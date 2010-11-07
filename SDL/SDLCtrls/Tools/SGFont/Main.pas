unit Main;

interface

uses SDLLabel, SDLCheck, SDLEdit, SDLButton, SDLMemo, SDLFileCtrls, 
  sdlgui, SDLDraw, SDLForm;

type
  TForm1 = class(TSDLForm)
    edBColor: TSDLEdit;
    edFColor: TSDLEdit;
    lbStatus: TSDLLabel;
    SDLLabel3: TSDLLabel;
    SDLLabel2: TSDLLabel;
    SDLLabel1: TSDLLabel;
    rgCharset: TSDLRadioGroup;
    btSave: TSDLButton;
    cbOpenGL: TSDLCheckBox;
    cbSaveS: TSDLCheckBox;
    cbBold: TSDLCheckBox;
    edSize: TSDLEdit;
    btLoad: TSDLButton;
    Memo: TSDLMemo;
    procedure btLoadClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btSaveClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure cbBoldChange(Sender: TSDLComponent);
    procedure edBColorChange(Sender: TSDLComponent);
    procedure edFBCKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
    procedure edFColorChange(Sender: TSDLComponent);
    procedure edSizeChange(Sender: TSDLComponent);
    procedure rgCharsetClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
    FFName: string;
    OpenDialog: TSDLOpenDialog;
    SaveDialog: TSDLSaveDialog;
    procedure LoadTTF(Sender: TSDLOpenDialog;const FileName: string);
    procedure SaveFont(Sender: TSDLOpenDialog;const FileName: string);
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses Classes, SysUtils, SDLFont, sdl;

const
  LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`1234567890-=~!@#$%^&*()_+[]\{}|;'':",./<>?';
  ASCIIKEYS = '!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';

procedure TForm1.btLoadClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  OpenDialog.Execute(LoadTTF);
end;

procedure TForm1.btSaveClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if (Memo.Font=GlobalFont)or
     (Length(Memo.Text)<2) then Exit;
  SaveDialog.Execute(SaveFont);
end;

procedure TForm1.cbBoldChange(Sender: TSDLComponent);
begin
  if Memo.Font=GlobalFont then Exit;
  with Memo.Font as TSDLTTFFont do
    if cbBold.Checked then
      Style := [fsBold]
    else
      Style := [];
  Memo.Font := Memo.Font; //Refresh Wrapmanager
end;

constructor TForm1.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('TTFToSGFont.xfm');
  Memo.Text := LETTERS;
  OpenDialog := TSDLOpenDialog.Create(AParent);
  OpenDialog.Filter := 'TTF Font (*.ttf)';
  SaveDialog := TSDLSaveDialog.Create(AParent);
  SaveDialog.DefaultExt := 'bmp';
  SaveDialog.Filter := 'Bitmap (*.bmp)';
  SetFocus;
end;

destructor TForm1.Destroy;
begin
  if Memo.Font<>GlobalFont then
    Memo.Font.Free;
  inherited;
end;

procedure TForm1.edBColorChange(Sender: TSDLComponent);
begin
  Memo.Color := StrToIntDef(edBColor.Text,0);
  Memo.FocusColor := Memo.Color;
end;

procedure TForm1.edFBCKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
var txt: string;
begin
  if Key<>SDLK_RETURN then Exit;
  Key := 0;
  txt := edFColor.Text;
  edFColor.Text := edBColor.Text;
  edBColor.Text := txt;
end;

procedure TForm1.edFColorChange(Sender: TSDLComponent);
begin
  if Memo.Font <> GlobalFont then
    Memo.Font.Color := StrToIntDef(edFColor.Text,0);
end;

procedure TForm1.edSizeChange(Sender: TSDLComponent);
var size: Integer;
begin
  if Memo.Font=GlobalFont then Exit;
  size := StrToIntDef(edSize.Text,0);
  if size<8 then size := 20;
  TSDLTTFFont(Memo.Font).Size := size;
  Memo.Font := Memo.Font; //Refresh TWrapManager
end;

procedure TForm1.LoadTTF(Sender: TSDLOpenDialog; const FileName: string);
var tf: TSDLTTFFont;
    size: Integer;
begin
  size := StrToIntDef(edSize.Text,0);
  if size<8 then size := 20;
  if Memo.Font<>GlobalFont then
    Memo.Font.Free;
  tf := TSDLTTFFont.Create(nil);
  if cbBold.Checked then
    tf.Style := [fsBold]
  else
    tf.Style := [];
  tf.Load('Book',FileName,size);
  tf.Color := StrToIntDef(edFColor.Text,0);
  Memo.Font := tf;
  Memo.Color := StrToIntDef(edBColor.Text,0);
  Memo.FocusColor := Memo.Color;
  FFName := ExtractFileName(FileName);
  Delete(FFName,Length(FFName)-3,4);
end;

procedure TForm1.rgCharsetClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if rgCharset.ItemIndex=0 then
    Memo.Text := LETTERS
  else if rgCharSet.ItemIndex=1 then
    Memo.Text := ASCIIKEYS;
  Memo.ReadOnly := rgCharset.ItemIndex<2;
end;

procedure TForm1.SaveFont(Sender: TSDLOpenDialog; const FileName: string);
var SSurf: PSDL_Surface;
    h,w,i: Integer;
    cw: array[0..256]of Word;
    fn,txt: string;
    Dat: TFileStream;
    FontData: TFontData;
    ReplaceColor: Boolean;
begin
  ReplaceColor := True;
  h := Memo.Font.TextHeight('M')-1;
  txt := Memo.Text;
  FontData.Height := h;
  FontData.CharacterNo := Length(txt);
  cw[0] := 0;
  for i := 1 to FontData.CharacterNo do
  begin
    w := Memo.Font.TextWidth(txt[i]);
    cw[i] := cw[i-1]+w;
  end;
  w := cw[FontData.CharacterNo];
  if cbOpenGL.Checked then
  begin
    if w<=1024 then w := 1024 else w := 2048;
    if h<=16 then h := 16
    else if h<=32 then h := 32
    else if h<=64 then h := 64
    else h := 128;
  end;
  SSurf := SDL_CreateRGBSurface(0,w,h,32,0,0,0,0);
  if SSurf=nil then
  begin
    lbStatus.Caption := 'Failed to create surface';
    Exit;
  end;
  FontData.BackColor := StrToIntDef(edBColor.Text,$FFFFFF);
  SDLScreen.Clear(SSurf,FontData.BackColor);
  w := 0;
  for i := 1 to FontData.CharacterNo do
    Memo.Font.TextOut(SSurf,cw[i-1],-1,txt[i]);
  SDL_SaveBMP(SSurf,PChar(FileName));
  SDL_FreeSurface(SSurf);
  if cbSaveS.Checked then
  begin
    fn := Copy(FileName,1,Length(FileName)-3)+'dat';
    Dat := TFileStream.Create(fn,fmCreate);
  end
  else
  begin
    Dat := TFileStream.Create(FileName,fmOpenReadWrite);
    Dat.Seek(0,soFromEnd);
  end;
  try
    w := Dat.Position;
    h := w;
    FontData.Version := RASTERFONTVER;
    FontData.FontColor := TSDLTTFFont(Memo.Font).Color;
    Dat.Write(w,SizeOf(Integer));
    Dat.Write(FontData,SizeOf(TFontData));
    Dat.Write(ReplaceColor,SizeOf(Boolean));
    Dat.Write(cw[1],FontData.CharacterNo*SizeOf(Word));  //Character's Widths
    WriteString(Dat,txt);                            //Characters
    WriteString(Dat,'');
    WriteString(Dat,FFName);
    w := w - Dat.Position - SizeOf(Integer);
    Dat.Write(w,SizeOf(Integer));
    Dat.Position := h;
    w := - w - SizeOf(Integer);
    Dat.Write(w,SizeOf(Integer));
    lbStatus.Caption := 'Font Saved';
  finally
    Dat.Free;
  end;
end;

end.
