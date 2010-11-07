unit Main;

interface

{ Version 00.02
  - Now supports Zlib compression under both Windows and Linux

  Image List format:
  Bytes - Meaning
    3   - 'SIL' ID
    2   - Version
    1   - Is it compressed
    //Compression can start only here
    4   - Number of Images
    for i := 1 to Number of Images do begin
    15  - SDLIData of image
    2   - Size of IName string
 Length(IName) - IName string
    4 - Size of Image Data
 SizeOf(ImgData) - Image Data
    end;
}

uses SDLButton, SDLMemo, SDLEdit, SDLLabel, SDLListBox, SDLMenus, SDLCheck,
     SDLFileCtrls, sdlgui, SDLDraw, SDLForm, Classes, SDLDialogs;

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
    procedure Show;
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
  end;

  TForm1 = class(TSDLForm)
    btDown: TSDLButton;
    btUp: TSDLButton;
    lbImageList: TSDLListBox;
    Menu: TSDLMenu;
    miAdd: TSDLMenuItem;
    miComp: TSDLMenuItem;
    miExit: TSDLMenuItem;
    miIgor: TSDLMenuItem;
    miNew: TSDLMenuItem;
    miOpen: TSDLMenuItem;
    miProps: TSDLMenuItem;
    miRemove: TSDLMenuItem;
    miSave: TSDLMenuItem;
    miSaveAs: TSDLMenuItem;
    miSort: TSDLMenuItem;
    Picture: TSDLLabel;
    procedure btDownClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btUpClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure lbItemsClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure lbItemsKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
    procedure miAddClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miCompClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miExitClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miNewClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miOpenClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miPropsClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miRemoveClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miSaveAsClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miSaveClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure miSortClick(Sender: TSDLComponent; AX, AY: Integer);
  private
    { Private declarations }
    PropsDlg: TImgPropsDlg;
    OpenDialog: TSDLOpenDialog;
    SaveDialog: TSDLSaveDialog;
    CurFileName: string;
    ListLoaded: Boolean;
    Modified: Boolean;
    Images: TSDLImages;
    procedure SetCFN(const Value: string);
    procedure NewList(Sender: TSDLForm; Button: TSDLDlgButton);
    procedure GetFN(Sender: TSDLForm; Button: TSDLDlgButton);
    procedure Quit(Sender: TSDLForm; Button: TSDLDlgButton);
    procedure SaveHeader(Stream: TStream);
    procedure SaveOver(Sender: TSDLOpenDialog;const FileName: string);
    procedure SaveNew(Sender: TSDLOpenDialog;const FileName: string);
    procedure SaveTo(Stream: TStream; Skip: Integer);
    procedure LoadIL(Sender: TSDLOpenDialog;const FileName: string);
    procedure AddImages(Sender: TSDLOpenDialog;const FileName: string);
  protected
    procedure DoDraw; override;
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses SysUtils, SDLFont, sdl,
{$IFDEF LINUX}
  aezlib;
{$ELSE}
  ZLibEx;
{$ENDIF}

type
  TSDLIData = record
    UseAlphaCh: Boolean;
    Alpha: Byte;
    PatternHeight: Integer;
    PatternWidth: Integer;
    Transparent: Boolean;
    TransparentColor: Cardinal;
  end;

  TSDLObj = class
  public
    SDLIData: TSDLIData;
    FName: string;
    IName: string;
    OImage: TSDLImage;
    ImageList: Boolean;
    ILIndex: Integer;
  end;

const CPT = 'SDL ImageList Creator - ';
      FNUntitled = 'Untitled.sil';
      CPTSTRING = 'Question!';
      QSTRING = 'Are you sure you don''t want to save this list?';

{ TImgPropsDlg }

procedure TImgPropsDlg.btCancelClick(Sender: TSDLComponent; AX,
  AY: Integer);
begin
  Modal := False;
  Visible := False;
  LoseFocus;
end;

procedure TImgPropsDlg.btOKClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  With Form1.lbImageList do
    with Items.Objects[ItemIndex] as TSDLObj do
    begin
      SDLIData.UseAlphaCh := cbUseAlpha.Checked;
      OImage.UseAlphaCh := cbUseAlpha.Checked;
      SDLIData.Transparent := cbTrans.Checked;
      OImage.Transparent := cbTrans.Checked;
      SDLIData.TransparentColor := StrToIntDef(edTransColor.Text,0);
      OImage.TransparentColor := SDLIData.TransparentColor;
      SDLIData.Alpha := StrToIntDef(edAlpha.Text,255);
      OImage.Alpha := SDLIData.Alpha;
      SDLIData.PatternWidth := StrToIntDef(edPattW.Text,0);
      OImage.PatternWidth := SDLIData.PatternWidth;
      SDLIData.PatternHeight := StrToIntDef(edPattH.Text,0);
      OImage.PatternHeight := SDLIData.PatternHeight;
      if IName<>edName.Text then
      begin
        IName := edName.Text;
        Items[ItemIndex] := IName;
        if Form1.miSort.Checked then Items.Sort;
      end;
      Form1.Picture.AnimCount := OImage.PatternCount;
      if Form1.Picture.AnimCount>1 then
        Form1.Picture.AnimSpeed := 3/1000;
    end;
  btCancelClick(nil,0,0);
end;

constructor TImgPropsDlg.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('ImgPDlg.xfm');
end;

procedure TImgPropsDlg.Show;
begin
  Visible := True;
  Modal := True;
  SetFocus;
end;

{ TForm1 }

procedure TForm1.AddImages(Sender: TSDLOpenDialog; const FileName: string);
var i: Integer;
    t: TSDLObj;
begin
  lbImageList.Items.BeginUpdate;
  for i := 0 to Sender.Files.Count-1 do
  begin
    t := TSDLObj.Create;
    t.FName := Sender.Files[i];
    t.IName := ExtractFileName(t.FName);
    Delete(t.IName,Length(t.IName)-3,4);
    t.SDLIData.Alpha := 255;
    t.OImage := Images.Add(t.IName);
    t.OImage.Load(t.FName);
    lbImageList.Items.AddObject(t.IName,t);
  end;
  with lbImageList do
  begin
    if miSort.Checked then Items.Sort;
    Items.EndUpdate;
  end;
  Modified := True;
end;

procedure TForm1.btDownClick(Sender: TSDLComponent; AX, AY: Integer);
var t: Integer;
begin
  if miSort.Checked or(lbImageList.Items.Count=0) then Exit;
  with lbImageList do
    if ItemIndex<Items.Count-1 then
    begin
      t := ItemIndex+1;
      Items.Move(t-1,t);
      ItemIndex := t;
    end;
  Modified := True;
end;

procedure TForm1.btUpClick(Sender: TSDLComponent; AX, AY: Integer);
var t: Integer;
begin
  if miSort.Checked or(lbImageList.Items.Count=0) then Exit;
  with lbImageList do
    if ItemIndex>0 then
    begin
      t := ItemIndex-1;
      Items.Move(t+1,t);
      ItemIndex := t;
    end;
  Modified := True;
end;

constructor TForm1.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('Main.xfm');
  SetCFN(FNUntitled);
  Images := TSDLImages.Create(TSDLImage);
  PropsDlg := TImgPropsDlg.Create(AParent);
  OpenDialog := TSDLOpenDialog.Create(AParent);
  SaveDialog := TSDLSaveDialog.Create(AParent);
  SaveDialog.Filter := 'SDL ImageList (*.sil)|All files(*)';
  SaveDialog.DefaultExt := 'sil';
  SetFocus;
end;

destructor TForm1.Destroy;
begin
  NewList(nil,dbYes);
  Images.Free;
  inherited;
end;

procedure TForm1.DoDraw;
var t: TSDL_Rect;
begin
  inherited;
  t := Picture.BoundsRect;
  InflateRect(t,2,2);
  SDLScreen.Draw3DControl(SDLScreen.Surface,t,FocusColor,2,True);
end;

procedure TForm1.GetFN(Sender: TSDLForm; Button: TSDLDlgButton);
begin
  if Button=dbNo then Exit;
  OpenDialog.FileLB.MultiSelect := False;
  OpenDialog.Filter := 'SDL ImageList (*.sil)|All files (*)';
  OpenDialog.Execute(LoadIL);
end;

procedure TForm1.lbItemsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if lbImageList.ItemIndex<0 then
  begin
    Picture.Image := nil;
    Exit;
  end;
  with lbImageList do
    Picture.Image := TSDLObj(Items.Objects[ItemIndex]).OImage;
  Picture.Caption := 'Width: '+IntToStr(Picture.Image.Surface.w)+
                  '  Height: '+IntToStr(Picture.Image.Surface.h);
end;

procedure TForm1.lbItemsKeyDown(Sender: TSDLControl; var Key: Word;
  Modifier: Cardinal);
begin
  if Key=SDLK_DELETE then
  begin
    miRemoveClick(nil,0,0);
    Key := 0;
  end
  else if Key=SDLK_RETURN then
  begin
    miPropsClick(nil,0,0);
    Key := 0;
  end;
end;

procedure TForm1.LoadIL(Sender: TSDLOpenDialog; const FileName: string);
var i: integer;
    t: TSDLObj;
    tfn: string;
begin
  tfn := FileName;
  NewList(nil,dbYes);  //New ImageList
  SetCFN(tfn);

  i := Images.LoadFromSil(FileName);
  if i<0 then
  begin
    SDLMessageDlg(nil,Gui,[dbOK],'Error','Error Loading File: '+IntToStr(i));
    Exit;
  end;
  miComp.Checked := i=1;
  lbImageList.Items.BeginUpdate;
  for i := 0 to Images.Count-1 do
  begin
    t := TSDLObj.Create;
    t.FName := FileName;
    t.IName := Images[i].Name;
    with t.SDLIData do
    begin
      UseAlphaCh := Images[i].UseAlphaCh;
      Alpha := Images[i].Alpha;
      PatternWidth := Images[i].PatternWidth;
      PatternHeight := Images[i].PatternHeight;
      Transparent := Images[i].Transparent;
      TransparentCOlor := Images[i].TransparentColor;
    end;
    t.OImage := Images[i];
    t.ImageList := True;
    t.ILIndex := i;
    lbImageList.Items.AddObject(t.IName,t);
  end;
  with lbImageList do
  begin
    if miSort.Checked then Items.Sort;
    Items.EndUpdate;
  end;
  ListLoaded := True;
  Modified := False;
end;

procedure TForm1.miAddClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  OpenDialog.FileLB.MultiSelect := True;
  OpenDialog.Filter := '*.bmp;*.png;*.jpg;*.tga;*.tif;*.gif;*.pcx|'+
      '*.bmp|*.png|*.jpg|*.tga|*.tif|*.gif|*.pcx|All files (*)';
  OpenDialog.Execute(AddImages);
end;

procedure TForm1.miCompClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  Modified := True;
end;

procedure TForm1.miExitClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  SDLMessageDlg(Quit,Gui,[dbYes,dbNo],CPTSTRING,'Are you sure you want to Exit?');
end;

procedure TForm1.miNewClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Modified then
    SDLMessageDlg(NewList,Gui,[dbYes,dbNo],CPTSTRING,QSTRING)
  else
    NewList(nil,dbYes);
end;

procedure TForm1.miOpenClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Modified then
    SDLMessageDlg(GetFN,Gui,[dbYes,dbNo],CPTSTRING,QSTRING)
  else GetFN(nil,dbYes);
end;

procedure TForm1.miPropsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if lbImageList.ItemIndex<0 then Exit;
  with lbImageList.Items.Objects[lbImageList.ItemIndex] as TSDLObj do
  begin
    PropsDlg.cbUseAlpha.Checked := SDLIData.UseAlphaCh;
    PropsDlg.cbTrans.Checked := SDLIData.Transparent;
    PropsDlg.edTransColor.Text := '$'+IntToHex(SDLIData.TransparentColor,6);
    PropsDlg.edAlpha.Text := IntToStr(SDLIData.Alpha);
    PropsDlg.edPattW.Text := IntToStr(SDLIData.PatternWidth);
    PropsDlg.edPattH.Text := IntToStr(SDLIData.PatternHeight);
    PropsDlg.edName.Text := IName;
  end;
  PropsDlg.Show;
  Modified := True;
end;

procedure TForm1.miRemoveClick(Sender: TSDLComponent; AX, AY: Integer);
var t: Integer;
begin
  if lbImageList.ItemIndex<0 then Exit;

  with lbImageList do
  begin
    t := ItemIndex;
    TSDLObj(Items.Objects[t]).OImage.Free;
    Items.Objects[t].Free;
    Items.Delete(t);
  end;
  lbItemsClick(nil,0,0);
  Modified := True;
end;

procedure TForm1.miSaveAsClick(Sender: TSDLComponent; AX, AY: Integer);
var i: Integer;
begin
  if not Modified or (lbImageList.Items.Count=0) then Exit;

  //Clear Memory
  Picture.Image := nil;
  Images.Clear;
  with lbImageList do
    for i := 0 to Items.Count-1 do
      with Items.Objects[i] as TSDLObj do
        OImage := nil;

  if ListLoaded then
    SaveDialog.Execute(SaveOver)
  else
    SaveDialog.Execute(SaveNew);
end;

procedure TForm1.miSaveClick(Sender: TSDLComponent; AX, AY: Integer);
var i: Integer;
begin
  if not Modified or (lbImageList.Items.Count=0) then Exit;

  //Clear Memory
  Picture.Image := nil;
  Images.Clear;
  with lbImageList do
    for i := 0 to Items.Count-1 do
      with Items.Objects[i] as TSDLObj do
        OImage := nil;

  if ListLoaded then
    SaveOver(nil,CurFileName)
  else
    SaveDialog.Execute(SaveNew);
end;

procedure TForm1.miSortClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if miSort.Checked then
    lbImageList.Items.Sort;
  Modified := True;
end;

procedure TForm1.NewList(Sender: TSDLForm; Button: TSDLDlgButton);
var i: Integer;
begin
  if Button=dbNo then Exit;
  SetCFN(FNUntitled);
  Picture.Image := nil;
  for i := 0 to lbImageList.Items.Count-1 do
  begin
    TSDLObj(lbImageList.Items.Objects[i]).OImage.Free;
    lbImageList.Items.Objects[i].Free;
  end;
  lbImageList.Items.Clear;
  lbItemsClick(nil,0,0);
  Modified := False;
  ListLoaded := False;
end;

procedure TForm1.Quit(Sender: TSDLForm; Button: TSDLDlgButton);
begin
  if Button<>dbYes then Exit;
  Gui.SendQuitSignal;
end;

procedure TForm1.SaveHeader(Stream: TStream);
var t: Integer;
    tb: Boolean;
begin
  Stream.Write('SIL',3);
  t := SILVER;
  Stream.Write(t,2);
  tb := miComp.Checked;
  Stream.Write(tb,1);
end;

procedure TForm1.SaveNew(Sender: TSDLOpenDialog; const FileName: string);
begin
  SetCFN(FileName);
  SaveTo(nil,0);
end;

procedure TForm1.SaveOver(Sender: TSDLOpenDialog; const FileName: string);
var nfn: string;
    Load: TFileStream;
    MemS,tmp: TMemoryStream;
    cmpS : TZDeCompressionStream;
    comp,del: Boolean;
    Size: Integer;
begin
  if FileName=CurFileName then
  begin
    del := True;
    nfn := Copy(CurFileName,1,Length(CurFileName)-3)+'tmp';
    RenameFile(CurFileName,nfn);
  end
  else
  begin
    del := False;
    nfn := CurFileName;
    CurFileName := FileName;
  end;

  MemS := nil; tmp := nil; cmpS := nil;
  Load := TFileStream.Create(nfn,fmOpenRead);

  try
   Load.Position := 5;
   Load.Read(comp,1);
   if comp then
   begin
     MemS := TMemoryStream.Create;
     tmp := TMemoryStream.Create;
     Size := Load.Size-6;
     tmp.CopyFrom(Load,Size);
     Load.Free; Load := nil;
     cmpS := TZDeCompressionStream.Create(tmp);
     MemS.CopyFrom(cmpS,0);
     //Free all unecessery memory
     cmpS.Free; cmpS := nil;
     tmp.Free; tmp := nil;
     SaveTo(MemS,4);
   end
   else
     SaveTo(Load,10);
  finally
    cmpS.Free;
    tmp.Free;
    Load.Free;
    MemS.Free;
  end;
  if del then DeleteFile(nfn);
end;

procedure TForm1.SaveTo(Stream: TStream; Skip: Integer);
  procedure SeekTo(Index: Integer);
  var n,p: Integer;
      t: Word;
  begin
    Stream.Position := Skip;
    for n := 1 to Index do
    begin
      Stream.Read(p,4);
      p := p+SizeOf(TSDLIData);
      Stream.Seek(p,soFromCurrent);
      Stream.Read(t,2); //Skip string
      Stream.Seek(t,soFromCurrent);
    end;
  end;

var Save,Img: TFileStream;
    MemS: TMemoryStream;
    cmpS : TZCompressionStream;
    i,Size: Integer;
begin
  Img := nil;
  cmpS := nil;
  Size := 0;
  if Skip>0 then
  begin
    Inc(Skip,SizeOf(TSDLIData));
    Stream.Position := Skip;
    Stream.Read(Size,2);
    Inc(Skip,Size+2);
  end;

  Save := TFileStream.Create(CurFileName,fmCreate);
  MemS := TMemoryStream.Create;
  try
    SaveHeader(Save);
    Size := lbImageList.Items.Count;
    MemS.Write(Size,4);
    for i := 0 to Size-1 do
      with TSDLObj(lbImageList.Items.Objects[i]) do
      begin
        MemS.Write(SDLIData,SizeOf(TSDLIData));
        WriteString(MemS,IName);
        if ImageList then
        begin
          SeekTo(ILIndex);
          Stream.Read(Size,4);
          MemS.Write(Size,4);
          MemS.CopyFrom(Stream,Size);
        end
        else
        begin
          Img := TFileStream.Create(FName,fmOpenRead);
          Size := Img.Size;
          MemS.Write(Size,4);
          MemS.CopyFrom(Img,0);
          Img.Free;
          Img := nil;
        end;
      end;
    if miComp.Checked then
    begin
      cmpS := TZCompressionStream.Create(Save,zcMax);
      cmpS.CopyFrom(MemS,0);
      cmpS.Free; cmpS := nil;
    end
    else
      Save.CopyFrom(MemS,0);
  finally
    cmpS.Free;
    MemS.Free;
    Save.Free;
    Img.Free;
  end;
  Modified := False;
  LoadIL(nil,CurFileName);
end;

procedure TForm1.SetCFN(const Value: string);
begin
  CurFileName := Value;
  Caption := CPT+ExtractFileName(Value);
end;

end.
