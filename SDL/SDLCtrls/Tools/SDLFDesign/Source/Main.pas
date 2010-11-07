unit Main;

interface

uses sdlgui, SDLForm, SDLDraw, SDLButton, SDLMenus, SDLCodeEditor, SDLMenuDesigner,
     SDLImagesViewer, SDLFileCtrls, SDLDialogs;

type
  TMainForm = class(TSDLForm)
    btSelect: TSDLButton;
    Menu: TSDLMenu;
    procedure AddProcImplClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure AddButtonClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure SaveClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure SaveAsClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure NewClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure OpenClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure ExitClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure LockClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure HintsClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure ViewClick(Sender: TSDLComponent;AX,AY: Integer);
  private
    FAdding: Boolean;
    FAddingClass: TSDLObjectClass;
    FFormfn: string;
    FUnitfn: string;
    FMessageDlg: TSDLMessageDlg;
    FOpenDialog: TSDLOpenDialog;
    FSaveDialog: TSDLSaveDialog;
    procedure DlgCallBack(Sender: TSDLForm; Button: TSDLDlgButton);
    procedure ODCallBack(Sender: TSDLOpenDialog; const FileName: string);
    procedure SDCallBack(Sender: TSDLOpenDialog; const FileName: string);
    procedure LoadForm;
    procedure SaveForm;
    procedure NewForm;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure SetSelControl(Sender: TSDLComponent);
    procedure DesignFormClick(Sender: TSDLComponent;AX,AY: Integer);
  end;

var
  MainForm: TMainForm;

const DataPath = '../Data/';

implementation

uses sdl, Classes, SysUtils, Inspector, SDLCheck, SDLGauge, TypInfo;

const
  CONSTRING = 'Confirmation';
  QSTRING = 'Are you sure you don''t want to save current form?'; 

type
  TDesignForm = class(TSDLForm)
  protected
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); override;
  public
    constructor Create(AParent: TSDLObject); override;
  end;

var DesignForm: TDesignForm;

{ TDesignForm }

constructor TDesignForm.Create(AParent: TSDLObject);
begin
  inherited;
  X := 225;
  Y := 100;
  Width := SDLScreen.SurfaceRect.w-300;
  Height := 400;
  Caption := 'SDLForm1';
  Designing := True;
  Name := 'SDLForm1';
end;

procedure TDesignForm.KeyDown(var Key: Word; Modifier: TSDLMod);
begin
  if (Key=SDLK_DELETE)and(ObjInspector.SelectedControl<>Self) then
    with ObjInspector.SelectedControl do
      if Designing and (Parent.FieldAddress(Name)=nil) then
      begin
        ObjInspector.DelSelControl;
        MenuDesigner.AddingControl := ObjInspector.SelectedControl;
      end;
end;

{ TMainForm }

procedure TMainForm.AddButtonClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if Sender.Tag=-1 then
  begin
    FAdding := False;
    Exit;
  end;
  FAdding := True;
  FAddingClass := TSDLObjectClass(RegClasses.Objects[Sender.Tag]);
end;

procedure TMainForm.AddProcImplClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  CodeEditor.AddProcImplementation := TSDLMenuItem(Sender).Checked;
end;

constructor TMainForm.Create(AParent: TSDLObject);
var i,n,tx: Integer;
begin
  inherited;
  LoadCompFromFile(DataPath+'MainForm.xfm');
  Gui.DragEnabled := False;
  Width := SDLScreen.SurfaceRect.w;
  FFormfn := 'Untitled.xfm';
  FUnitfn := 'Untitled.pas';

  tx := 30;
  for i := RegClasses.Count-1 downto 0 do
  begin
    n := RImages.IndexOfItem(RegClasses[i]);
    if n<=0 then Continue;
    with TSDLButton.Create(Self) do
    begin
      X := tx;
      Y := 43;
      Width := 28;
      Height := 28;
      OnClick := AddButtonClick;
      GlyphIndex := n;
      Tag := i;
      Hint := RegClasses[i];
    end;
    Inc(tx,28);
  end;
  for i := 0 to ControlCount-1 do
    if Controls[i] is TSDLButton then
      TSDLButton(Controls[i]).GroupIndex := 1;
  CodeEditor := TSDLCodeEditor.Create(Gui);
  ObjInspector := TObjInspector.Create(AParent);
  ObjInspector.Name := 'ObjInspector';
  NewForm;
  Gui.SetSelControl := SetSelControl;
  MenuDesigner := TMenuDesigner.Create(Gui);
  ImgsView := TImgsView.Create(Gui);
  FMessageDlg := TSDLMessageDlg.Create(Gui);
  FOpenDialog := TSDLOpenDialog.Create(Gui);
  FSaveDialog := TSDLSaveDialog.Create(Gui);
end;

procedure TMainForm.DesignFormClick(Sender: TSDLComponent; AX, AY: Integer);
var ot : TSDLComponent;
    t: string;
begin
  if not FAdding then Exit;
  btSelect.Down := True;
  FAdding := False;
  t := FAddingClass.ClassName;
  Delete(t,1,1);
  ot := FAddingClass.Create(Sender);
  with ot do
  begin
    X := AX-Sender.WorldX;
    Y := AY-Sender.WorldY;
    Name := ObjInspector.GetValidNameFrom(t,True);
  end;
  if Sender is TSDLPanel then
    TSDLPanel(Sender).RefreshScroll;
  if CodeEditor.AddControlDeclaration(ot)>=0 then
    SetSelControl(ot)
  else
    ot.Free;
end;

procedure TMainForm.DlgCallBack(Sender: TSDLForm; Button: TSDLDlgButton);
begin
  if Button <> dbYes then Exit;
  case Sender.Tag of
  0:begin
      FOpenDialog.Caption := 'Select unit file to open.';
      FOpenDialog.Filter := 'Delphi Units (*.pas)|All Files (*)';
      FOpenDialog.Execute(ODCallBack);
    end;
  1: NewForm;
  2: Gui.SendQuitSignal;
  end;
end;

procedure TMainForm.ExitClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if FMessageDlg.Visible then Exit;
  FMessageDlg.Tag := 2;
  FMessageDlg.Show(DlgCallBack,[dbYes,dbNo],CONSTRING,'Are you sure you want to exit?');
end;

procedure TMainForm.HintsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  Gui.ShowHints := TSDLMenuItem(Sender).Checked;
end;

procedure TMainForm.LoadForm;
begin
  if (FUnitfn='')or(FFormfn='') then Exit;

  ObjInspector.cbControl.Items.Clear; //This will set SelectedControl to nil
  CodeEditor.EventInfo.Clear;
  CodeEditor.Memo.LoadFromFile(FUnitfn);
  DesignForm.Free;
  DesignForm := TDesignForm.Create(Gui);
  DesignForm.OnClick := DesignFormClick;
  Gui.DesignForm := CodeEditor.EventInfo;
  DesignForm.LoadCompFromFile(FFormfn);
  Gui.DesignForm := DesignForm;
  ObjInspector.SelectedControl := DesignForm;
end;

procedure TMainForm.LockClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  with Sender as TSDLMenuItem do
    Gui.DragEnabled :=  not Checked;
end;

procedure TMainForm.NewClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  FMessageDlg.Tag := 1;
  FMessageDlg.Show(DlgCallBack,[dbYes,dbNo],CONSTRING,QSTRING);
end;

procedure TMainForm.NewForm;
begin
  ObjInspector.cbControl.Items.Clear; //This will set SelectedControl to nil
  CodeEditor.EventInfo.Clear;
  CodeEditor.Memo.LoadFromFile(DataPath+'Template.pas');
  DesignForm.Free;
  DesignForm := TDesignForm.Create(Gui);
  DesignForm.OnClick := DesignFormClick;
  Gui.DesignForm := DesignForm;
  ObjInspector.SelectedControl := DesignForm;
end;

procedure TMainForm.ODCallBack(Sender: TSDLOpenDialog; const FileName: string);
begin
  if FOpenDialog.Tag=0 then
  begin
    FUnitFn := FileName;
    FOpenDialog.Tag := 1;
    FOpenDialog.Caption := 'Select form file to open.';
    FOpenDialog.Filter := 'SDL Forms (*.xfm)|All Files (*)';
    FOpenDialog.Execute(ODCallBack);
  end
  else
  begin
    FOpenDialog.Tag := 0;
    FFormfn := FileName;
    LoadForm;
  end;
end;

procedure TMainForm.OpenClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  FMessageDlg.Tag := 0;
  FMessageDlg.Show(DlgCallBack,[dbYes,dbNo],CONSTRING,QSTRING);
end;

procedure TMainForm.SaveAsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  FSaveDialog.Caption := 'Name of unit file to save?';
  FSaveDialog.Filter := 'Delphi Units (*.pas)|All Files (*)';
  FSaveDialog.DefaultExt := 'pas';
  FSaveDialog.Execute(SDCallBack);
end;

procedure TMainForm.SaveClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if FFormfn = 'Untitled.xfm' then
    SaveAsClick(nil,0,0)
  else
    SaveForm;
end;

procedure TMainForm.SaveForm;
var sl,dp: TStringList;
    i,s,p: Integer;
    t,ei,cs: string;
begin
  sl := TStringList.Create;
  dp := TStringList.Create;
  DesignForm.OnClick := nil;
  Gui.DesignForm := dp;
  DesignForm.SaveToStrings(sl,0);
  Gui.DesignForm := DesignForm;
  DesignForm.OnClick := DesignFormClick;

  with CodeEditor.EventInfo do
    for i := 0 to Count-1 do
    begin
      //get object declaration string: object MyObject: TSDLMyObject
      t := 'object '+TSDLComponent(Objects[i]).Name+': '+Objects[i].ClassName;
      //find declaration of object MyObject using string t
      p := StrToIntDef(dp[dp.IndexOfObject(Objects[i])],0);
      p := IndexOfString(sl,t,p)+1;
      if p=0 then Continue;
      //ei will contain string: OnSomeEvent = MyObjectSomeEvent
      s := AnsiPos('=',Strings[i]);
      ei := Copy(Strings[i],s+1,Length(Strings[i]));
      ei := ei+' = '+Copy(Strings[i],1,s-1);
      //get next string and count spaces on the begining for alignment
      cs := sl[p];
      s := 1;
      while cs[s]=' ' do Inc(s);
      //Add spaces to ei to align it
      ei := StringOfChar(' ',s-1)+ei;
      //Insert ei so list remains sorted
      while (AnsiCompareText(sl[p],ei)<0)and(AnsiPos('object',sl[p])<=0)and
            (AnsiPos('end',sl[p])<=0) do Inc(p);
      sl.Insert(p,ei);
    end;
  //Change Design Form declaration
  dp.Free;
  sl[0] := 'object '+DesignForm.Name+': T'+DesignForm.Name;
  sl.SaveToFile(FFormfn);
  sl.Free;
  ei := CodeEditor.Memo.Text;
  //Add Loading of correct file
  p := AnsiPos('LoadCompFromFile(',ei);
  if p>0 then
  begin
    p := p+17;
    s := p;
    while ei[s]<>';' do Inc(s);
    s := s-p-1;
    Delete(ei,p,s);
    Insert(''''+GetRelativePath(FUnitfn,FFormfn)+'''',ei,p);
  end;
  //Change unit declaration
  Delete(ei,1,AnsiPos(';',ei));
  t := Copy(FUnitfn,1,Length(FUnitfn)-4);
  s := Length(t);
  while (s>0)and(t[s]<>'/')and(t[s]<>'\') do Dec(s);
  if s>0 then Delete(t,1,s);
  CodeEditor.Memo.Text := 'unit '+t+';'+ei;
  CodeEditor.Memo.SaveToFile(FUnitfn);
end;

procedure TMainForm.SDCallBack(Sender: TSDLOpenDialog; const FileName: string);
begin
  if FSaveDialog.Tag=0 then
  begin
    FUnitFn := FileName;
    FSaveDialog.Tag := 1;
    FSaveDialog.Caption := 'Name of form file to save?';
    FSaveDialog.Filter := 'SDL Forms (*.xfm)|All Files (*)';
    FSaveDialog.DefaultExt := 'xfm';
    FSaveDialog.Execute(SDCallBack);
  end
  else
  begin
    FSaveDialog.Tag := 0;
    FFormfn := FileName;
    SaveForm;
  end;
end;

procedure TMainForm.SetSelControl(Sender: TSDLComponent);
begin
  if not FAdding then
  begin
    ObjInspector.SelectedControl := Sender;
    MenuDesigner.AddingControl := ObjInspector.SelectedControl;
  end
  else if Sender is TSDLPanel then
    DesignFormClick(Sender,Gui.SDLMouse.X,Gui.SDLMouse.Y);
end;

procedure TMainForm.ViewClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  case Sender.Tag of
  1: DesignForm.SetFocus;
  2: ObjInspector.SetFocus;
  3: CodeEditor.SetFocus;
  4: MenuDesigner.Show;
  5: ImgsView.Show;
  end;
end;

end.
