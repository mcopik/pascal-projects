unit SDLFileCtrls;

{*********************************************************************
             SDLFileCtrls v1.0b -  03.10.2004.
             
    Copyright (C) 2004  Igor Stojkovic

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Author:      Igor Stojkovic
Contact:     jimyiigor@ptt.yu  (please report any bugs)
             stojkovic@galeb.etf.bg.ac.yu
Description: TSDLFileListBox, TSDLDirectoryListBox, TSDLOpenDialog,
             TSDLSaveDialog and Windows only TSDLDriveCombo

If you have any suggestions or you made some modifications
please inform me.
   
**********************************************************************}

interface

uses sdl, sdlgui, SDLForm, SDLLabel, SDLButton, SDLEdit, SDLListBox,
     SDLComboBox, SDLFont, Classes {$IFDEF MSWINDOWS} ,Windows {$ENDIF};

type
  TSDLOpenDialog = class;
  TSDLODCallback = procedure(Sender: TSDLOpenDialog; const FileName: string)of object;

  TSDLFileListBox = class(TSDLListBox)
  private
    FDirectory: string;
    FMask: string;
    FFileEdit: TSDLEdit;
    procedure ReadCurDir;
    procedure SetDirectory(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetFileEdit(const Value: TSDLEdit);
    procedure SetMask(const Value: string);
  protected
    procedure Click(AX,AY: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    property FileEdit: TSDLEdit read FFileEdit write SetFileEdit;
  published
    property Directory: string read FDirectory write SetDirectory;
    property FileName: string read GetFileName write SetFileName;
    property Mask: string read FMask write SetMask;
  end;

  TSDLDirectoryListBox = class(TSDLListBox)
  private
    FDirectory: string;
    FFileLB: TSDLFileListBox;
    FDirLabel: TSDLLabel;
    procedure ReadCurDir;
    procedure SetDirectory(const Value: string);
    procedure SetFileLB(const Value: TSDLFIleListBox);
    procedure SetDirLabel(const Value: TSDLLabel);
  protected
    procedure DblClick(AX,AY: Integer); override;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
  public
    constructor Create(AParent: TSDLObject); override;
    property FileListBox: TSDLFileListBox read FFileLB write SetFileLB;
    property DirLabel: TSDLLabel read FDirLabel write SetDirLabel;
  published
    property Directory: string read FDirectory write SetDirectory;
  end;

  {$IFDEF MSWINDOWS}
  TSDLDriveCombo = class(TSDLComboBox)
  private
    FDrive: Char;
    FDirList: TSDLDirectoryListBox;
    procedure BuildList;
    procedure SetDrive(const Value: Char);
    procedure SetDirList(const Value: TSDLDirectoryListBox);
  protected
    procedure Change; override;
  public
    lbl: TSDLLabel;
    constructor Create(AParent: TSDLObject); override;
    property DirList: TSDLDirectoryListBox read FDirList write SetDirList;
  published
    property Drive: Char read FDrive write SetDrive;
  end;
  {$ENDIF}

  TSDLOpenDialog = class(TSDLForm)
  private
    FCallback : TSDLODCallback;
    FFilter: string;
    FFiles: TStringList;
    procedure FileEditKeyDown(Sender: TSDLControl; var Key: Word;
                              Modifier: TSDLMod);
    procedure btCancelClick(Sender: TSDLComponent;X,Y: Integer);
    procedure FilterChange(Sender: TSDLComponent);
    procedure SetFilter(const Value: string);
    procedure SetMSel(const Value: Boolean);
    function GetMSel: Boolean;
    function GetFileName: string;
  protected
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
    procedure btOpenClick(Sender: TSDLComponent;X,Y: Integer); virtual;
  public
    DirectoryLB: TSDLDirectoryListBox;
    FileLB: TSDLFileListBox;
    btOpen: TSDLButton;
    btCancel: TSDLButton;
    Labels: array[0..2]of TSDLLabel;
    FileEdit: TSDLEdit;
    FilterCombo: TSDLComboBox;
{$IFDEF MSWINDOWS}
    DriveCombo: TSDLDriveCombo;
{$ENDIF}
    procedure Execute(Callback: TSDLODCallback);
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    property Files: TStringList read FFiles;
  published
    property Filter: string read FFilter write SetFilter;
    property MultiSelect: Boolean read GetMSel write SetMSel;
    property FileName: string read GetFileName;
  end;

  TSDLSaveDialog = class(TSDLOpenDialog)
  private
    FDefaultExt: string;
  protected
    procedure btOpenClick(Sender: TSDLComponent;X,Y: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
  published
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;

  function GetRelativePath(const Source, Dest: string): string;

var AppPath: string;

implementation

uses SysUtils, SDLDraw;

//Source=C:\Jedi\SDL\Controls Dest=C:\Jedi\Demo\Test Result=..\..\Demo\Test
function GetRelativePath(const Source, Dest: string): string;
var i,p: Integer;
    s: String;
    pd: Char;
begin
  i := 1; p := 1;
  s := ExtractFilePath(Source);
  if AnsiPos('\',s)>0 then pd := '\'
  else pd := '/';
  while s[i]=Dest[i] do
  begin
    if (s[i]=pd) then p := i;
    if (Length(s)=i)or(Length(Dest)=i) then Break;
    Inc(i);
  end;
  s := Copy(s,p,Length(s)-p);
  Result := Copy(Dest,p+1,Length(Dest));
  p := AnsiPos(pd,s);
  while p>0 do
  begin
    Delete(s,1,p);
    p := AnsiPos(pd,s);
    Result := '..'+pd+Result;
  end;
end;

{ TSDLFileListBox }

procedure TSDLFileListBox.Click(AX, AY: Integer);
begin
  inherited;
  if Assigned(FFileEdit) then
    FFileEdit.Text := FileName;
end;

constructor TSDLFileListBox.Create(AParent: TSDLObject);
begin
  inherited;
  FMask := '*';
  Directory := AppPath;
end;

function TSDLFileListBox.GetFileName: string;
begin
  if ItemIndex>-1 then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

procedure TSDLFileListBox.ReadCurDir;
var Data: TSearchRec;
    MaskPtr,Ptr: PChar;
begin
  Items.Clear;

  MaskPtr := PChar(FMask);
  while MaskPtr<>nil do
  begin
    Ptr := StrScan(MaskPtr, ';');
    if Ptr <> nil then Ptr^ := #0;
    if FindFirst(MaskPtr, faAnyFile, Data) = 0 then
    begin
      repeat
        if (Data.Attr and faDirectory = 0)and
           (Data.Name[1]<>'.') then
          Items.Add(Data.Name);
      until FindNext(Data)<>0;
      FindClose(Data);
    end;
    if Ptr <> nil then
    begin
      Ptr^ := ';';
      Inc (Ptr);
    end;
    MaskPtr := Ptr;
  end;
  Items.Sort;
end;

procedure TSDLFileListBox.SetDirectory(const Value: string);
begin
  ChDir(Value);
  if IOResult=0 then FDirectory := Value;
  ReadCurDir;
  if Assigned(FFileEdit) then
    FFileEdit.Text := FileName;
end;

procedure TSDLFileListBox.SetFileEdit(const Value: TSDLEdit);
begin
  FFileEdit := Value;
  FFileEdit.Text := FileName;
end;

procedure TSDLFileListBox.SetFileName(const Value: string);
begin
  ItemIndex := Items.IndexOf(Value);
end;

procedure TSDLFileListBox.SetMask(const Value: string);
begin
  if Value=FMask then Exit;
  FMask := Value;
  ReadCurDir;
end;

{ TSDLDirectoryListBox }

constructor TSDLDirectoryListBox.Create(AParent: TSDLObject);
begin
  inherited;
  Directory := AppPath;
end;

procedure TSDLDirectoryListBox.DblClick(AX, AY: Integer);
var d: string;
begin
  inherited;
  if ItemIndex<0 then Exit;

  if Copy(Items[ItemIndex],1,2)='..' then
  begin
    if Length(FDirectory)>2 then   //For Linux'es ../
    begin
      d := Copy(FDirectory,1,Length(FDirectory)-1);
      Directory := ExtractFilePath(d);
    end;
  end
  else
  begin
    d := Items[ItemIndex];
    if d[Length(d)]=PathDelim then d := ''
    else d := PathDelim;
    Directory := Directory+Items[ItemIndex]+d;
  end;
end;

procedure TSDLDirectoryListBox.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  inherited;
  if Key=SDLK_RETURN then
  begin
    Key := 0;
    DblClick(0,0);
  end;
end;

procedure TSDLDirectoryListBox.ReadCurDir;
var Data: TSearchRec;
begin
  Items.Clear;

  if FindFirst('*', faAnyFile, Data) = 0 then
  begin
    repeat
      if Data.Attr and faDirectory<>0 then
        if (Data.Name='..')or(Data.Name[1]<>'.') then
          Items.Add(Data.Name);
    until FindNext(Data)<>0;
    FindClose(Data);
  end;
  Items.Sort;
  ItemIndex := 0;
end;

procedure TSDLDirectoryListBox.SetDirectory(const Value: string);
begin
  if Assigned(FileListBox) then
  begin
    FileListBox.SetDirectory(Value);
    FDirectory := FileListBox.Directory;
  end
  else
  begin
    ChDir(Value);
    if IOResult=0 then FDirectory := Value;
  end;
  ReadCurDir;
  if Assigned(FDirLabel) then FDirLabel.Caption := FDirectory;
end;

procedure TSDLDirectoryListBox.SetDirLabel(const Value: TSDLLabel);
begin
  FDirLabel := Value;
  FDirLabel.Caption := Directory;
end;

procedure TSDLDirectoryListBox.SetFileLB(const Value: TSDLFIleListBox);
begin
  FFileLB := Value;
  if FFileLb.Directory<>Directory then
    FFileLb.Directory := Directory;
end;

{ TSDLOpenDialog }

procedure TSDLOpenDialog.btCancelClick(Sender: TSDLComponent; X,
  Y: Integer);
begin
  Modal := False;
  LoseFocus;
  Visible := False;
  FCallback := nil;
end;

procedure TSDLOpenDialog.btOpenClick(Sender: TSDLComponent; X, Y: Integer);
var i : Integer;
    fn: string;
    cb: TSDLODCallBack;
begin
  if FileEdit.Text='' then Exit;
  for i := 0 to FileLB.Items.Count-1 do
    if FileLB.Selected[i] then
      FFiles.Add(FileLB.Directory+FileLB.Items[i]);
  fn := FileName;
  Modal := False;
  LoseFocus;
  Visible := False;
  cb := FCallBack;
  FCallBack := nil;
  if Assigned(cb) then cb(Self,fn);
end;

constructor TSDLOpenDialog.Create(AParent: TSDLObject);
const lblCaption: array[0..1]of string = ('File name:','Files of type:');
var i : Integer;
begin
  inherited;
  Visible := False;
  Width := 470;
  Height := 330;
  X := SDLScreen.SurfaceRect.w div 2 - 250;
  Y := SDLScreen.SurfaceRect.h div 2 - 170;
  Caption := 'Open File';
  FFiles := TStringList.Create;

  FileLB := TSDLFileListBox.Create(Self);
  with FileLB do
  begin
    X := 240;
    Y := 35;
    Width := 220;
    Height := 185;
    TabOrder := 1;
    OnDblClick := btOpenClick;
  end;
  DirectoryLB := TSDLDirectoryListBox.Create(Self);
  with DirectoryLB do
  begin
    X := 10;
    Y := 35;
    Width := 220;
    Height := 185;
    FileListBox := FileLB;
    TabOrder := 0;
  end;
  for i := 0 to 1 do
  begin
    Labels[i] := TSDLLabel.Create(Self);
    With Labels[i] do
    begin
      X := 5;
      Y := 226+i*35;
      Width := 100;
      Height := 30;
      Caption := lblCaption[i];
    end;
  end;
  Labels[2] := TSDLLabel.Create(Self);
  with Labels[2] do
  begin
    Alignment := taRight;
    Y := 296;
    Height := 30;
{$IFDEF MSWINDOWS}
    X := 180;
{$ELSE}
    X := 5;
{$ENDIF}
    Width := 460-X;
  end;
  DirectoryLB.DirLabel := Labels[2];
  FileEdit := TSDLEdit.Create(Self);
  with FileEdit do
  begin
    X := 110;
    Y := 226;
    Width := 230;
    Height := 30;
    OnKeyDown := FileEditKeyDown;
  end;
  FileLB.FileEdit := FileEdit;
  FilterCombo := TSDLComboBox.Create(Self);
  with FilterCombo do
  begin
    X := 110;
    Y := 261;
    Width := 230;
    Height := 30;
    Items.Add('*');
    OnChange := FilterChange;
    ItemIndex := 0;
  end;
{$IFDEF MSWINDOWS}
  DriveCombo := TSDLDriveCombo.Create(Self);
  with DriveCombo do
  begin
    X := 110;
    Y := 296;
    Width := 60;
    Height := 30;
    lbl.X := 5;
    lbl.Y := 296;
    lbl.Width := 100;
    lbl.Height := 30;
    DirList := DirectoryLB;
  end;
{$ENDIF}
  btOpen := TSDLButton.Create(Self);
  with btOpen do
  begin
    X := 350;
    Y := 226;
    Width := 110;
    Height := 30;
    Caption := 'Open';
    OnClick := btOpenClick;
  end;
  btCancel := TSDLButton.Create(Self);
  with btCancel do
  begin
    X := 350;
    Y := 261;
    Width := 110;
    Height := 30;
    Caption := 'Cancel';
    OnClick := btCancelClick;
  end;
end;

destructor TSDLOpenDialog.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TSDLOpenDialog.Execute(Callback: TSDLODCallback);
var t: string;
begin
  if Visible then Exit;
  Visible := True;
  SetFocus;
  Modal := True;
  FCallback := Callback;
  FFiles.Clear;
  t := GetCurrentDir;
  if t[Length(t)]<>PathDelim then
    t := t+PathDelim;
  DirectoryLB.Directory := t;
{$IFDEF MSWINDOWS}
  if Copy(t,2,2)=':\' then
    DriveCombo.Drive := t[1];
{$ENDIF}
  DirectoryLB.SetFocus;
end;

procedure TSDLOpenDialog.FileEditKeyDown(Sender: TSDLControl;
          var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_RETURN then
  begin
    Key := 0;
    if Length(FileEdit.Text)>0 then
      btOpenClick(nil,0,0);
  end;
end;

procedure TSDLOpenDialog.FilterChange(Sender: TSDLComponent);
var Mask: string;
    p: Integer;
begin
  Mask := FilterCombo.Text;
  if Mask='' then Exit;
  if Mask[Length(Mask)]<>')' then
  begin
    FileLB.Mask := Mask;
    Exit;
  end;
  p := AnsiPos('(',Mask);
  Delete(Mask,1,p);
  FileLB.Mask := Copy(Mask,1,Length(Mask)-1);
end;

function TSDLOpenDialog.GetFileName: string;
begin
  if AnsiPos(PathDelim,FileEdit.Text)>0 then
    Result := FileEdit.Text
  else
    Result := FileLB.Directory+FileEdit.Text;
end;

function TSDLOpenDialog.GetMSel: Boolean;
begin
  Result := FileLB.MultiSelect;
end;

procedure TSDLOpenDialog.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  inherited;
  if Key = SDLK_RETURN then
  begin
    Key := 0;
    btOpenClick(nil,0,0);
  end;
  if Key=SDLK_ESCAPE then
  begin
    Key := 0;
    btCancelClick(nil,0,0);
  end;
end;

procedure TSDLOpenDialog.SetFilter(const Value: string);
var FilterPtr,Ptr: PChar;
begin
  if Visible or (FFilter=Value) then Exit;
  FilterCombo.ItemIndex := -1;
  FilterCombo.Items.BeginUpdate;
  FilterCombo.Items.Clear;
  FFilter := Value;
  FilterPtr := PChar(FFilter);
  while FilterPtr<>nil do
  begin
    Ptr := StrScan(FilterPtr, '|');
    if Ptr <> nil then Ptr^ := #0;
    FilterCombo.Items.Add(FilterPtr);
    if Ptr <> nil then
    begin
      Ptr^ := '|';
      Inc(Ptr);
    end;
    FilterPtr := Ptr;
  end;
  FilterCombo.Items.EndUpdate;
  if FilterCombo.Items.Count>0 then
    FilterCombo.ItemIndex := 0;
end;

procedure TSDLOpenDialog.SetMSel(const Value: Boolean);
begin
  FileLb.MultiSelect := Value;
end;

{ TSDLSaveDialog }

procedure TSDLSaveDialog.btOpenClick(Sender: TSDLComponent; X, Y: Integer);
const QUESTION = 'Click again if you want to overwrite.';
begin
  if FileEdit.Text='' then
    FileEdit.Text := 'Untitled.'+DefaultExt
  else if ExtractFileExt(FileEdit.Text)='' then
    FileEdit.Text := FileEdit.Text+'.'+DefaultExt;
  if FileExists(FileName)and(Labels[2].Caption<>QUESTION) then
    Labels[2].Caption := QUESTION
  else
    inherited;
end;

constructor TSDLSaveDialog.Create(AParent: TSDLObject);
begin
  inherited;
  Caption := 'Save File';
  btOpen.Caption := 'Save';
end;

{ TSDLDriveCombo }
{$IFDEF MSWINDOWS}

procedure TSDLDriveCombo.BuildList;
var
  DriveNum: Integer;
  DriveChar: Char;
  DriveBits: set of 0..25;
begin
  { fill list }
  Items.Clear;
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    DriveChar := Char(DriveNum + Ord('A'));
    Items.Add(DriveChar+':');
  end;
end;

procedure TSDLDriveCombo.Change;
var d: char;
begin
  inherited;
  d := Items[ItemIndex][1];
  if DirList.Directory[1]=d then
  begin
    FDrive := d;
    Exit;
  end;
  DirList.Directory := d+':\';
  if DirList.Directory[1]=d then
    FDrive := d
  else
    ItemIndex := Items.IndexOf(FDrive+':');
end;

constructor TSDLDriveCombo.Create(AParent: TSDLObject);
begin
  inherited;
  BuildList;
  lbl := TSDLLabel.Create(AParent);
  lbl.Caption := 'Drive:'
end;

procedure TSDLDriveCombo.SetDirList(const Value: TSDLDirectoryListBox);
begin
  if FDirList = Value then Exit;
  FDirList := Value;
  Drive := FDirList.Directory[1];
end;

procedure TSDLDriveCombo.SetDrive(const Value: Char);
var p: Integer;
begin
  if FDrive=Value then Exit;
  p := Items.IndexOf(Value+':');
  if p<0 then Exit;
  ItemIndex := p;
end;

{$ENDIF}

initialization
begin
  AppPath := ExtractFilePath(ParamStr(0));
end;

end.
