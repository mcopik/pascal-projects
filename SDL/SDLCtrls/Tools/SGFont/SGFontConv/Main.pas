unit Main;

interface

uses SDLScroll, SDLLabel, SDLEdit, SDLCheck, SDLButton, SDLFileCtrls,
  sdlgui, SDLDraw, SDLForm;

type
  TFuncForm = class(TSDLForm)
    btOK: TSDLButton;
    edph: TSDLEdit;
    edpw: TSDLEdit;
    SDLLabel2: TSDLLabel;
    SDLLabel1: TSDLLabel;
    SDLRadioGroup1: TSDLRadioGroup;
    procedure btOKClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
  end;

  TForm1 = class(TSDLForm)
    SDLLabel4: TSDLLabel;
    PictureSB: TSDLScrollBar;
    ZoomRGroup: TSDLRadioGroup;
    SDLLabel3: TSDLLabel;
    SDLLabel2: TSDLLabel;
    SDLLabel1: TSDLLabel;
    SDLEdit1: TSDLEdit;
    cbRC: TSDLCheckBox;
    cbOpenGL: TSDLCheckBox;
    cbSaveS: TSDLCheckBox;
    btSpFunc: TSDLButton;
    btSave: TSDLButton;
    btLoad: TSDLButton;
    procedure btLoadClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btSaveClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btSpFuncClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure ColorClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure Form1Click(Sender: TSDLComponent; AX, AY: Integer);
    procedure SDLEdit1KeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
    procedure ZoomRGroupClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
    Picture: TSDLImage;
    FFName : string;
    FColor: PCardinal;
    CharsetIndex: Boolean;
    cw: array[1..256]of Word;
    function Copy1(Image: TSDLImage): Integer;
    function Copy2(Image: TSDLImage): Integer;
    function Copy3(Image: TSDLImage): Integer;
    procedure LoadImage(Sender: TSDLOpenDialog;const FileName: string);
    procedure SaveImage(Sender: TSDLOpenDialog;const FileName: string);
  protected
    procedure DoDraw; override;
  public
    { Public declarations }
    FuncForm: TFuncForm;
    BackColor: Cardinal;
    ForeColor: Cardinal;
    SpecialColor: Cardinal;
    OpenDialog: TSDLOpenDialog;
    SaveDialog: TSDLSaveDialog;
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses sdl, sdlutils, Classes, SysUtils, SDLDialogs, SDLFont;

const
  PictureRect: TSDL_Rect=(x:7; y:35; w:625; h:305);
  LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`1234567890-=~!@#$%^&*()_+[]\{}|;'':",./<>?';
  ASCIIKEYS = '!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';

procedure TForm1.btLoadClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  OpenDialog.Execute(LoadImage);
end;

procedure TForm1.btSaveClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if not Assigned(Picture.Surface) or(SDLEdit1.Text='') then
  begin
    SDLMessageDlg(nil,Gui,[dbOK],'Error','You must open an image and type in the charset.');
    Exit;
  end;
  SaveDialog.Execute(SaveImage);
end;

procedure TForm1.btSpFuncClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  FuncForm.Visible := True;
  FuncForm.SetFocus;
  FuncForm.Modal := True;
end;

procedure TForm1.ColorClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Sender.Tag = 1 then
    FColor := @ForeColor
  else FColor := @SpecialColor;
end;

//Copy first type with special color between letters
function TForm1.Copy1(Image: TSDLImage): Integer;
var i,w,h,cx: Integer;
    sr: TSDL_Rect;
begin
  h := Picture.Height;
  Image.Alloc(Picture.Width,h);
  Result := 0;
  cx := 0;
  for i := 1 to Length(SDLEdit1.Text) do
  begin
    while SDLScreen.GetRGBAPixel(Picture.Surface,Result,0)=SpecialColor do
    begin
      Inc(Result);
      if Result>=Picture.Width then Break;
    end;
    w := Result+1;
    while SDLScreen.GetRGBAPixel(Picture.Surface,w,0)<>SpecialColor do
    begin
      Inc(w);
      if w>=Picture.Width then Break;
    end;
    if w>=Picture.Width then Break;
    sr := SDLRect(Result,1,w-Result,h);
    Picture.DrawRect(Image.Surface,cx,0,sr);
    Inc(cx,w-Result);
    cw[i] := cx;
    Result := w+1;
  end;
  Result := cx;
end;

//Copy second type where specail color is above letters
function TForm1.Copy2(Image: TSDLImage): Integer;
var i,w,h,cx: Integer;
    sr: TSDL_Rect;
begin
  Image.Alloc(Picture.Width+100,Picture.Height-1);
  SDLScreen.Clear(Image.Surface,BackColor);
  Result := 0;
  cx := 0;
  h := Image.Height;
  for i := 1 to Length(SDLEdit1.Text) do
  begin
    while SDLScreen.GetRGBAPixel(Picture.Surface,Result,0)<>SpecialColor do
    begin
      Inc(Result);
      if Result>=Picture.Width then Break;
    end;
    w := Result+1;
    while SDLScreen.GetRGBAPixel(Picture.Surface,w,0)=SpecialColor do
    begin
      Inc(w);
      if w>=Picture.Width then Break;
    end;
    if w>=Picture.Width then Break;
    sr := SDLRect(Result,1,w-Result,h);
    Picture.DrawRect(Image.Surface,cx+1,0,sr);
    Inc(cx,w-Result+2);
    cw[i] := cx;
    Result := w+1;
  end;
  Result := cx;
end;

//Copy third type where letters are of fixed width
function TForm1.Copy3(Image: TSDLImage): Integer;
var i,w,h,cx: Integer;
    sr: TSDL_Rect;
begin
  Image.Alloc(Picture.Width+100,Picture.Height-1);
  SDLScreen.Clear(Image.Surface,BackColor);
  Result := 0;
  cx := 0;
  h := Image.Height;
  for i := 1 to Length(SDLEdit1.Text) do
  begin
    while SDLScreen.GetRGBAPixel(Picture.Surface,Result,0)<>SpecialColor do
    begin
      Inc(Result);
      if Result>=Picture.Width then Break;
    end;
    w := Result+1;
    while SDLScreen.GetRGBAPixel(Picture.Surface,w,0)=SpecialColor do
    begin
      Inc(w);
      if w>=Picture.Width then Break;
    end;
    if w>=Picture.Width then Break;
    sr := SDLRect(Result,1,w-Result,h);
    Picture.DrawRect(Image.Surface,cx+1,0,sr);
    Inc(cx,w-Result+2);
    cw[i] := cx;
    Result := w+1;
  end;
  Result := cx;
end;

constructor TForm1.Create(AParent: TSDLObject);
begin
  inherited;
  Picture := TSDLImage.Create(nil);
  LoadCompFromFile('Main.xfm');
  X := 0;
  Y := 0;
  Width := SDLScreen.SurfaceRect.w-1;
  Height := SDLScreen.SurfaceRect.h-1;
  OpenDialog := TSDLOpenDialog.Create(AParent);
  OpenDialog.Filter := '*.bmp;*.png;*.jpg;*.tga;*.tif;*.gif;*.pcx|'+
      '*.bmp|*.png|*.jpg|*.tga|*.tif|*.gif|*.pcx|All files (*)';
  SaveDialog := TSDLSaveDialog.Create(AParent);
  SaveDialog.Filter := 'Bitmap (*.bmp)|All files (*)';
  SaveDialog.DefaultExt := 'bmp';
  SetFocus;
  BackColor := $FFFFFF;
  SpecialColor := $FF00FF;
  FuncForm := TFuncForm.Create(AParent);
end;

destructor TForm1.Destroy;
begin
  Picture.Free;
  inherited;
  Form1 := nil;
end;

procedure TForm1.DoDraw;
var sr,dr,tr: TSDL_Rect;
    m: Integer;
begin
  inherited;
  SDLScreen.Draw3DControl(SDLScreen.Surface,PictureRect,FocusColor,BorderWidth,True);
  dr := SDLRect(340,380,50,50);
  SDLScreen.FillRect(SDLScreen.Surface,dr,BackColor);
  Inc(dr.x,70);
  SDLScreen.FillRect(SDLScreen.Surface,dr,ForeColor);
  Inc(dr.x,70);
  SDLScreen.FillRect(SDLScreen.Surface,dr,SpecialColor);
  if not Assigned(Picture.Surface) then Exit;

  m := 1 shl ZoomRGroup.ItemIndex;
  dr.x := PictureRect.x+2;
  dr.w := PictureRect.w-4;
  sr.x := PictureSB.Position;
  sr.y := 0;
  sr.w := PictureSB.LargeChange;
  if sr.w>Picture.Width then
    sr.w := Picture.Width;
  sr.h := Picture.Height;
  dr.h := sr.h*m;
  dr.y := PictureRect.y+(PictureRect.h-24-dr.h)div 2;
  if dr.w>Picture.Width*(ZoomRGroup.ItemIndex+1) then
   dr.w := Picture.Width*(ZoomRGroup.ItemIndex+1);
  tr := SDLScreen.ClipRect;
  SDLScreen.SetClipRect(SDLScreen.Surface,@PictureRect);
  Picture.StretchDrawRect(SDLScreen.Surface,sr,dr);
  SDLScreen.SetClipRect(SDLScreen.Surface,@tr);
end;

procedure TForm1.Form1Click(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Assigned(FColor) then
    FColor^ := SDLScreen.GetRGBAPixel(nil,AX,AY);
  FColor := nil;
end;

procedure TForm1.LoadImage(Sender: TSDLOpenDialog; const FileName: string);
begin
  Picture.Load(FileName);
  FFName := ExtractFileName(FileName);
  Delete(FFName,Length(FFName)-3,4);
  with Picture do
    BackColor := SDLScreen.GetRGBAPixel(Surface,0,Height-1);
  ZoomRGroup.ItemIndex := 0;
  ZoomRGroupClick(nil,0,0);
end;

procedure TForm1.SaveImage(Sender: TSDLOpenDialog; const FileName: string);
var TempImg,SaveImg: TSDLImage;
    w,h: Integer;
    Dat: TFileStream;
    fn: string;
    FontData : TFontData;
    ReplaceColor: Boolean;
begin
  SaveImg := TSDLImage.Create(nil);
  TempImg := TSDLImage.Create(nil);
  try
    case FuncForm.SDLRadioGroup1.ItemIndex of
    0: w := Copy1(TempImg);
    1: w := Copy2(TempImg);
    else w := Copy3(tempImg);
    end;
    h := TempImg.Height;
    FontData.Height := h;
    if cbOpenGL.Checked then
    begin
      if w<=512 then w := 512
      else if w<=1024 then w := 1024
      else w := 2048;
      if h<=16 then h := 16
      else if h<=32 then h := 32
      else if h<=64 then h := 64
      else h := 128;
    end;
    SaveImg.Alloc(w,h);
    TempImg.Draw(SaveImg.Surface,0,0,0);
    SaveImg.SaveToFile(FileName);
    FontData.BackColor := SDLScreen.GetRGBAPixel(SaveImg.Surface,0,0);
  finally
    SaveImg.Free;
    TempImg.Free;
  end;
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
    fn := SDLEdit1.Text;
    FontData.CharacterNo := Length(fn);
    w := Dat.Position;
    ReplaceColor := cbRC.Checked;
    h := w;
    FontData.Version := RASTERFONTVER;
    FontData.FontColor := ForeColor;
    Dat.Write(w,SizeOf(Integer));
    Dat.Write(FontData,SizeOf(TFontData));
    Dat.Write(ReplaceColor,SizeOf(Boolean));          
    Dat.Write(cw[1],FontData.CharacterNo*SizeOf(Word));  //Character's Widths
    WriteString(Dat,fn);                            //Characters
    WriteString(Dat,'');
    WriteString(Dat,FFName);
    w := w - Dat.Position - SizeOf(Integer);
    Dat.Write(w,SizeOf(Integer));
    Dat.Position := h;
    w := - w - SizeOf(Integer);
    Dat.Write(w,SizeOf(Integer));
  finally
    Dat.Free;
  end;
end;

procedure TForm1.SDLEdit1KeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
begin
  if Key = SDLK_RETURN then
  begin
    Key := 0;
    CharsetIndex := not CharsetIndex;
    if CharsetIndex then
      SDLEdit1.Text := LETTERS
    else
      SDLEdit1.Text := ASCIIKEYS;
  end;
end;

procedure TForm1.ZoomRGroupClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Picture.Surface=nil then Exit;
  with PictureSB do
  begin
    LargeChange := (PictureRect.w-4)div (1 shl ZoomRGroup.ItemIndex);
    Max := Picture.Width-LargeChange;
  end;
end;

{ TFuncForm }

procedure TFuncForm.btOKClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  Visible := False;
  Modal := False;
end;

constructor TFuncForm.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('FuncForm.xfm');
end;

end.
