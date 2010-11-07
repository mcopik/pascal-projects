unit SDLDesignEditors;

interface

uses sdl, sdlgui, SDLDraw, SDLForm, SDLButton, SDLComboBox, SDLLabel, SDLEdit,
     SDLListBox, SDLMemo, SDLFileCtrls, clRTTI, TypInfo;

type
  //Base class for property editors
  TSDLPropertyEditor = class(TSDLLabel)
  private
    FPropInfo: TrtProperty;
  protected
    procedure DoAnimate(MoveCount: Integer); override;
    procedure Click(AX,AY: Integer); override;
    procedure SetPropInfo(const Value: TrtProperty); virtual;
    procedure RefreshEC; virtual;
  public
    function EditCtrl: TSDLControl; virtual;
    constructor Create(AParent: TSDLObject); override;
    property PropInfo: TrtProperty read FPropInfo write SetPropInfo;
  end;

  TSDLPropEditorClass = class of TSDLPropertyEditor;

  //Default property editor for signed and unsigned integers
  TSDLIntegerProperty = class(TSDLPropertyEditor)
  private
    FEditCtrl: TSDLEdit;
  protected
    procedure RefreshEC; override;
    procedure EditCtrlKeyDown(Sender: TSDLControl;
              var Key: Word; Modifier: TSDLMod);
    procedure EditCtrlExit(Sender: TSDLComponent); virtual;
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property editor for Char properties
  TSDLCharProperty = class(TSDLIntegerProperty)
  protected
    procedure RefreshEC; override;
    procedure EditCtrlExit(Sender: TSDLComponent); override;
  public
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property editor for enumerated properties
  TSDLEnumProperty = class(TSDLPropertyEditor)
  private
    FEditCtrl: TSDLComboBox;
    FNoChange: Boolean;
    procedure EditCtrlChange(Sender: TSDLComponent);
    procedure EditCtrlDblClick(Sender: TSDLComponent;AX,AY: Integer);
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
    procedure RefreshEC; override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property editor for string properties
  TSDLStringProperty = class(TSDLPropertyEditor)
  private
    FEditCtrl: TSDLEdit;
    procedure EditCtrlChange(Sender: TSDLComponent);
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property Editor for file names
  TSDLFileNameProperty = class(TSDLPropertyEditor)
  private
    FEditForm: TSDLOpenDialog;
    FbtDialog: TSDLButton;
    FEditCtrl: TSDLEdit;
    procedure EditCtrlKeyDown(Sender: TSDLControl;
              var Key: Word; Modifier: TSDLMod);
    procedure EditCtrlExit(Sender: TSDLComponent);
    procedure EditCtrlDblClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure ODExecute(Sender: TSDLOpenDialog; const FileName: string);
    procedure ODDestroy(Sender: TSDLComponent);
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    function EditCtrl: TSDLControl; override;
    destructor Destroy; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Base class for forms that some property editors use
  TSDLPropEditForm = class(TSDLForm)
  private
    FSP: TSDLPropertyEditor;
    btOK: TSDLButton;
    btCancel: TSDLButton;
  protected
    procedure ButtonClick(Sender: TSDLComponent;AX,AY: Integer); virtual;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure Show; virtual;
  end;

  //Base class for property editors that uses forms
  TSDLEditFormProperty = class(TSDLPropertyEditor)
  private
    FEditForm: TSDLPropEditForm;
    FbtDialog: TSDLButton;
    procedure EditCtrlDblClick(Sender: TSDLComponent;AX,AY: Integer);
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    destructor Destroy; override;
  end;

  //Property editor form for set properties
  TSDLSetEditForm = class(TSDLPropEditForm)
  private
    FListBox: TSDLListBox;
  protected
    procedure ButtonClick(Sender: TSDLComponent;AX,AY: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure Show; override;
  end;

  //Default property editor for set properties
  TSDLSetProperty = class(TSDLEditFormProperty)
  private
    FEditCtrl: TSDLEdit;
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
    procedure RefreshEC; override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Property editor form for TStringList properties
  TSDLStringsEditForm = class(TSDLPropEditForm)
  private
    FMemo: TSDLMemo;
  protected
    procedure ButtonClick(Sender: TSDLComponent;AX,AY: Integer); override;
  public
    constructor Create(AParent: TSDLObject); override;
    procedure Show; override;
  end;

  //Default property editor for TStrings properties
  TSDLStringsProperty = class(TSDLEditFormProperty)
  private
    FEditCtrl: TSDLEdit;
  protected
    procedure RefreshEC; override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property editor for Name property. DO NOT REGISTER
  TSDLNameProperty = class(TSDLPropertyEditor)
  private
    FEditCtrl: TSDLEdit;
    FInstance: TSDLComponent;
    procedure EditCtrlKeyDown(Sender: TSDLControl;
              var Key: Word; Modifier: TSDLMod);
    procedure EditCtrlExit(Sender: TSDLComponent);
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

  //Default property editor for events
  TSDLEventProperty = class(TSDLPropertyEditor)
  private
    FEditCtrl: TSDLEdit;
    FHasProc: Boolean;
    procedure EditCtrlDblClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure EditCtrlKeyDown(Sender: TSDLControl;
              var Key: Word; Modifier: TSDLMod);
    procedure EditCtrlExit(Sender: TSDLComponent);
    function GetEventIndex: Integer;
  protected
    procedure SetPropInfo(const Value: TrtProperty); override;
  public
    function EditCtrl: TSDLControl; override;
    constructor Create(AParent: TSDLObject); override;
  end;

function CreatePropEditor(APropInfo: TrtProperty; AParent: TSDLComponent;
         AY: Integer):Integer;
procedure SDLRegisterPropertyEditor(PropertyType: PTypeInfo;
          SDLClass: TSDLObjectClass; const PropertyName: string;
          EditorClass: TSDLPropEditorClass);

implementation

uses SysUtils, Classes, SDLFont, SDLCodeEditor, Inspector, SDLDialogs;

type
  PPropEditorInfo = ^TPropEditorInfo;
  TPropEditorInfo = record
    PropType : PTypeInfo;
    SDLClass : TSDLObjectClass;
    EditorClass: TSDLPropEditorClass;
  end;

var RegPropEditors: TStringList;

function IndexOfType(PropType: PTypeInfo; ASDLClass: TClass;
         const AName: string): Integer;
var i: Integer;
    pei: PPropEditorInfo;
begin
  Result := -1;
  for i := 0 to RegPropEditors.Count-1 do
  begin
    pei := PPropEditorInfo(RegPropEditors.Objects[i]);
    if pei.PropType = PropType then
    begin
      if (pei.SDLClass=ASDLClass)or
         (Assigned(ASDLClass)and ASDLClass.InheritsFrom(pei.SDLClass)) then
        if (Length(RegPropEditors[i])>0)and(AName = RegPropEditors[i]) then
        begin
          Result := i;
          Exit;
        end;
      if (Result=-1)and(pei.SDLClass=nil) then  //found type for any class
        Result := i;
    end;
  end;
end;

function CreatePropEditor(APropInfo: TrtProperty; AParent: TSDLComponent;
         AY: Integer):Integer;
var i: Integer;
    pec: TSDLPropEditorClass;
    t: TObject;
    c: string;
begin
  Result := AY+20;
  t := APropInfo;
  if t is TSDLComponent then
  begin
    pec := TSDLNameProperty;
    c := 'Name';
  end
  else
  begin
    i := IndexOfType(APropInfo.TypeInfo,APropInfo.ObjClassType,APropInfo.Name);
    if i<0 then
      if APropInfo.Kind = tkMethod then
        pec := TSDLEventProperty
      else
        pec := TSDLPropertyEditor
    else
      pec := PPropEditorInfo(RegPropEditors.Objects[i]).EditorClass;
    c := APropInfo.Name;
  end;
  if pec=TSDLEventProperty then
    with APropInfo.Instance as TSDLComponent do
      if Parent.FieldAddress(Name)<>nil then
      begin
        Result := AY;
        Exit;
      end;
  with pec.Create(AParent) do
  begin
    X := 2;
    Y := AY;
    Width := 87;
    Height := 18;
    Caption := c;
    if Length(c)>13 then
      Hint := Caption;
    PropInfo := APropInfo;
  end;
end;

procedure SDLRegisterPropertyEditor(PropertyType: PTypeInfo;
          SDLClass: TSDLObjectClass; const PropertyName: string;
          EditorClass: TSDLPropEditorClass);
var i: Integer;
    pei: PPropEditorInfo;
    t: string;
begin
  if RegPropEditors=nil then
    RegPropEditors := TStringList.Create;
  if Assigned(SDLClass) then t := PropertyName
  else t := '';
  i := IndexOfType(PropertyType,SDLClass,t);
  if i>=0 then
  begin
    pei := PPropEditorInfo(RegPropEditors.Objects[i]);
    if (pei.SDLClass = SDLClass)and(RegPropEditors[i]=t) then
    begin
      RegPropEditors.Delete(i);
      Dispose(pei);
    end;
  end;
  New(pei);
  pei.PropType := PropertyType;
  pei.SDLClass := SDLClass;
  pei.EditorClass := EditorClass;
  RegPropEditors.AddObject(t,TObject(pei));
end;

{ TSDLPropertyEditor }

procedure TSDLPropertyEditor.Click(AX, AY: Integer);
begin
  inherited;
  if EditCtrl<>nil then
    EditCtrl.SetFocus;
end;

constructor TSDLPropertyEditor.Create(AParent: TSDLObject);
begin
  inherited;
  Alignment := taLeft;
end;

procedure TSDLPropertyEditor.DoAnimate(MoveCount: Integer);
begin
  inherited;
  if EditCtrl<>nil then
    if not EditCtrl.HasFocus then
      RefreshEC;
end;

function TSDLPropertyEditor.EditCtrl: TSDLControl;
begin
  Result := nil;
end;

procedure TSDLPropertyEditor.RefreshEC;
begin
  //
end;

procedure TSDLPropertyEditor.SetPropInfo(const Value: TrtProperty);
begin
  FPropInfo := Value;
  if EditCtrl=nil then Exit;
  EditCtrl.X := Width+2;
  EditCtrl.Y := Y;
  EditCtrl.Width := Width+22;
  EditCtrl.Height := Height;
  EditCtrl.BorderWidth := 1;
end;

{ TSDLIntegerProperty }

constructor TSDLIntegerProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.MaxLength := 10;
  FEditCtrl.AutoSelect := True;
end;

function TSDLIntegerProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLIntegerProperty.EditCtrlExit(Sender: TSDLComponent);
var t: Integer;
begin
  if (PropInfo.Name='X')or(PropInfo.Name='Y')or
     (PropInfo.Name='Width')or(PropInfo.Name='Height') then
    if TSDLComponent(PropInfo.Instance).FLockedCoords then Exit;
  t := PropInfo.AsInteger;
  PropInfo.AsInteger := StrToIntDef(FEditCtrl.Text,t);
end;

procedure TSDLIntegerProperty.EditCtrlKeyDown(Sender: TSDLControl;
  var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_RETURN then
  begin
    EditCtrlExit(nil);
    Key := 0;
  end;
end;

procedure TSDLIntegerProperty.RefreshEC;
begin
  if AnsiPos('Color',PropInfo.Name)>0 then
    FEditCtrl.Text := '$'+IntToHex(Cardinal(PropInfo.AsInteger),6)
  else if PropInfo.TypeData.OrdType = otULong then
    FEditCtrl.Text := IntToStr(Cardinal(PropInfo.AsInteger))
  else
    FEditCtrl.Text := IntToStr(PropInfo.AsInteger);
end;

procedure TSDLIntegerProperty.SetPropInfo(const Value: TrtProperty);
begin
  if Assigned(FPropInfo) then Exit;
  inherited;
  RefreshEC;
  FEditCtrl.OnKeyDown := EditCtrlKeyDown;
  FEditCtrl.OnExit := EditCtrlExit;
end;

{ TSDLCharProperty }

constructor TSDLCharProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl.MaxLength := 1;
end;

procedure TSDLCharProperty.EditCtrlExit(Sender: TSDLComponent);
begin
  PropInfo.AsInteger := Ord(FEditCtrl.Text[1]);
end;

procedure TSDLCharProperty.RefreshEC;
begin
  FEditCtrl.Text := Chr(PropInfo.AsInteger and $FF);
end;

{ TSDLEnumProperty }

constructor TSDLEnumProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLComboBox.Create(AParent);
end;

function TSDLEnumProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLEnumProperty.EditCtrlChange(Sender: TSDLComponent);
begin
  if not FNoChange then
    PropInfo.AsString := FEditCtrl.Text;
end;

procedure TSDLEnumProperty.EditCtrlDblClick(Sender: TSDLComponent; AX,
  AY: Integer);
begin
  with FEditCtrl do
    if Items.Count = 2 then
      ItemIndex := 1-ItemIndex;
end;

procedure TSDLEnumProperty.RefreshEC;
begin
  FNoChange := True;
  FEditCtrl.Text := PropInfo.AsString;
  FNoChange := False;
end;

procedure TSDLEnumProperty.SetPropInfo(const Value: TrtProperty);
begin
  if Assigned(FPropInfo) then Exit;
  inherited;
  FEditCtrl.Items.Assign(PropInfo.EnumNames);
  RefreshEC;
  FEditCtrl.OnChange := EditCtrlChange;
  FEditCtrl.OnDblClick := EditCtrlDblClick;
end;

{ TSDLStringProperty }

constructor TSDLStringProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.AutoSelect := True;
end;

function TSDLStringProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLStringProperty.EditCtrlChange(Sender: TSDLComponent);
begin
  PropInfo.AsString := FEditCtrl.Text;
end;

procedure TSDLStringProperty.SetPropInfo(const Value: TrtProperty);
begin
  if Assigned(FPropInfo) then Exit;
  inherited;
  FEditCtrl.Text := PropInfo.AsString;
  FEditCtrl.OnChange := EditCtrlChange;
end;

{ TSDLFileNameProperty }

constructor TSDLFileNameProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FbtDialog := TSDLButton.Create(AParent);
  FEditForm := TSDLOpenDialog.Create(AParent.GUI);
  FEditForm.Filter := 'Image List (*.sil)|All Files (*)';
  FEditForm.OnDestroy := ODDestroy;
end;

destructor TSDLFileNameProperty.Destroy;
begin
  FEditForm.Free;
  inherited;
end;

function TSDLFileNameProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLFileNameProperty.EditCtrlDblClick(Sender: TSDLComponent; AX,
  AY: Integer);
begin
  FEditForm.Execute(ODExecute);
end;

procedure TSDLFileNameProperty.EditCtrlExit(Sender: TSDLComponent);
begin
  FPropInfo.AsString := FEditCtrl.Text;
end;

procedure TSDLFileNameProperty.EditCtrlKeyDown(Sender: TSDLControl;
  var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_RETURN then
  begin
    EditCtrlExit(nil);
    Key := 0;
  end;
end;

procedure TSDLFileNameProperty.ODDestroy(Sender: TSDLComponent);
begin
  FEditForm := nil;
end;

procedure TSDLFileNameProperty.ODExecute(Sender: TSDLOpenDialog;
  const FileName: string);
begin
  FEditCtrl.Text := FileName;
  FPropInfo.AsString := FileName;
end;

procedure TSDLFileNameProperty.SetPropInfo(const Value: TrtProperty);
begin
  inherited;
  FEditCtrl.OnKeyDown := EditCtrlKeyDown;
  FEditCtrl.OnExit := EditCtrlExit;
  FEditCtrl.OnDblClick := EditCtrlDblClick;
  FbtDialog.OnClick := EditCtrlDblClick;
  FbtDialog.BorderWidth := 1;
  FbtDialog.Caption := '...';
  with FEditCtrl do
  begin
    Width := Width-Height;
    FbtDialog.X := X+Width;
    FbtDialog.Y := Y;
    FbtDialog.Width := Height;
    FbtDialog.Height := Height;
  end;
end;

{ TSDLPropEditForm }

procedure TSDLPropEditForm.ButtonClick(Sender: TSDLComponent; AX,
  AY: Integer);
begin
  //Cancel.Tag = 2; OK.Tag = 1
  Visible := False;
  Modal := False;
  LoseFocus;
end;

constructor TSDLPropEditForm.Create(AParent: TSDLObject);
begin
  inherited Create(AParent.GUI);
  FSP := TSDLPropertyEditor(AParent);
  X := 250;
  Y := 100;
  Visible := False;
  btOK := TSDLButton.Create(Self);
  btCancel := TSDLButton.Create(Self);
  with btOK do
  begin
    Caption := 'OK';
    Tag := 1;
    OnClick := ButtonClick;
  end;
  with btCancel do
  begin
    Caption := 'Cancel';
    Tag := 2;
    OnClick := ButtonClick;
  end;
end;

destructor TSDLPropEditForm.Destroy;
begin
  TSDLEditFormProperty(FSP).FEditForm := nil;
  inherited;
end;

procedure TSDLPropEditForm.Show;
begin
  Visible := True;
  SetFocus;
  Modal := True;
end;

{ TSDLEditFormProperty }

destructor TSDLEditFormProperty.Destroy;
begin
  FEditForm.Free;
  inherited;
end;

procedure TSDLEditFormProperty.EditCtrlDblClick(Sender: TSDLComponent; AX,
  AY: Integer);
begin
  if Assigned(FEditForm) then
    FEditForm.Show;
end;

procedure TSDLEditFormProperty.SetPropInfo(const Value: TrtProperty);
begin
  inherited;
  if EditCtrl=nil then Exit;
  FbtDialog := TSDLButton.Create(Parent);
  FbtDialog.OnClick := EditCtrlDblClick;
  FbtDialog.BorderWidth := 1;
  FbtDialog.Caption := '...';
  with EditCtrl do
  begin
    Width := Width-Height;
    FbtDialog.X := X+Width;
    FbtDialog.Y := Y;
    FbtDialog.Width := Height;
    FbtDialog.Height := Height;
  end;
end;

{ TSDLSetEditForm }

procedure TSDLSetEditForm.ButtonClick(Sender: TSDLComponent;AX,AY: Integer);
var t: string;
    i: Integer;
begin
  inherited;
  if Sender.Tag=2 then Exit;
  t := '[';
  with FListBox do
    for i := 0 to Items.Count-1 do
      if Selected[i] then
        t := t+Items[i]+',';
  if Length(t)=1 then t := '[]'
  else t[Length(t)] := ']';
  FSP.FPropInfo.AsString := t;
end;

constructor TSDLSetEditForm.Create(AParent: TSDLObject);
begin
  inherited;
  Width := 180;
  Height := 225;
  Caption := 'Set Members';
  FListBox := TSDLListBox.Create(Self);
  with FListBox do
  begin
    X := 2;
    Y := 21;
    Width := 176;
    Height := 176;
    MultiSelect := True;
  end;
  with btOK do
  begin
    X := 5;
    Y := 199;
    Width := 80;
    Height := 23;
  end;
  with btCancel do
  begin
    X := 90;
    Y := 199;
    Width := 80;
    Height := 23;
  end;
end;

procedure TSDLSetEditForm.Show;
var i: Integer;
begin
  inherited;
  FListBox.Items.Assign(FSP.FPropInfo.SetNames);
  with FListBox do
    for i := 0 to Items.Count-1 do
      Selected[i] := FSP.FPropInfo.SetHasMember(Items[i]);
end;

{ TSDLSetProperty }

constructor TSDLSetProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.ReadOnly := True;
  FEditCtrl.AutoSelect := True;
  FEditCtrl.OnDblClick := EditCtrlDblClick;
  FEditForm := TSDLSetEditForm.Create(Self);
end;

function TSDLSetProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLSetProperty.RefreshEC;
begin
  FEditCtrl.Text := PropInfo.AsString;
end;

procedure TSDLSetProperty.SetPropInfo(const Value: TrtProperty);
begin
  inherited;
  RefreshEC;
end;

{ TSDLStringsEditForm }

procedure TSDLStringsEditForm.ButtonClick(Sender: TSDLComponent; AX, AY: Integer);
var t: TStrings;
begin
  inherited;
  if Sender.Tag=2 then Exit;
  if FSP.FPropInfo.Kind = tkLString then
    FSP.FPropInfo.AsString := FMemo.Text
  else
  begin
    t := TStrings(FSP.FPropInfo.AsObject);
    if Assigned(t) then
      t.Text := FMemo.Text;
  end;
end;

constructor TSDLStringsEditForm.Create(AParent: TSDLObject);
begin
  inherited;
  Width := 400;
  Height := 300;
  Caption := 'Enter lines';
  FMemo := TSDLMemo.Create(Self);
  with FMemo do
  begin
    X := 2;
    Y := 21;
    Width := 396;
    Height := 251;
  end;
  with btOK do
  begin
    X := 5;
    Y := 274;
    Width := 80;
    Height := 23;
  end;
  with btCancel do
  begin
    X := 90;
    Y := 274;
    Width := 80;
    Height := 23;
  end;
end;

procedure TSDLStringsEditForm.Show;
var t: TStrings;
begin
  inherited;
  if FSP.FPropInfo.Kind = tkLString then
    FMemo.Text := FSP.FPropInfo.AsString
  else
  begin
    t := TStrings(FSP.FPropInfo.AsObject);
    if Assigned(t) then
      FMemo.Text := t.Text;
  end;
end;

{ TSDLStringsProperty }

constructor TSDLStringsProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.Text := '(TStrings)';
  FEditCtrl.ReadOnly := True;
  FEditForm := TSDLStringsEditForm.Create(Self);
end;

function TSDLStringsProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLStringsProperty.RefreshEC;
begin
  if FPropInfo.Kind = tkLString then
    FEditCtrl.Text := FPropInfo.AsString;
end;

{ TSDLNameProperty }

constructor TSDLNameProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.AutoSelect := True;
end;

function TSDLNameProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLNameProperty.EditCtrlExit(Sender: TSDLComponent);
const DELE = ' .;()[]';
      DELS = DELE+'T';
var cet: TStringList;
    t,cn: string;
    p,pt: Integer;
begin
  if (FEditCtrl.Text=FInstance.Name)or not IsValidIdent(FEditCtrl.Text) then
  begin
    FEditCtrl.Text := FInstance.Name;
    Exit;
  end;

  cet := TStringList.Create;
  cet.Text := CodeEditor.Memo.Text;
  if FInstance is TSDLForm then
    cn := 'T'+FEditCtrl.Text
  else
    cn := FInstance.ClassName;
  //check to make sure new name doesn't exist already
  p := IndexOfString(cet,' '+FEditCtrl.Text+':',0);
  if p>0 then
  begin
    FEditCtrl.Text := FInstance.Name;
    Exit;
  end;
  //find declaration of last name and change it
  t := 'T'+TSDLComponent(Gui.DesignForm).Name+' = class(TSDLForm)';
  p := IndexOfString(cet,t,0);
  if p<0 then
  begin
    cet.Free;
    FEditCtrl.Text := FInstance.Name;
    SDLMessageDlg(nil,Gui,[dbOK],'Error!','Can not find "'+t+'" in unit.');
    Exit;
  end;
  t := '    '+FEditCtrl.Text+': '+cn+';';
  if FInstance is TSDLForm then
  begin
    Delete(t,1,2);
    cet[p] := '  '+cn+' = class(TSDLForm)';
    Inc(p);
  end;
  if FInstance.Name='' then
    cet.Insert(p+1,t)
  else
  begin
    p := IndexOfString(cet,FInstance.Name+':',0);
    cet[p] := t;
  end;
  p := IndexOfString(cet,FInstance.Name,p+1);
  while p>0 do
  begin
    cn := cet[p];
    pt := AnsiPos(FInstance.Name,cn);
    if (pt=1)or(AnsiPos(cn[pt-1],DELS)>0) then
    begin
      if (pt+Length(FInstance.Name)>Length(cn))or
         (AnsiPos(cn[pt+Length(FInstance.Name)],DELE)>0) then
      begin
        Delete(cn,pt,Length(FInstance.Name));
        Insert(FEditCtrl.Text,cn,pt);
      end;
    end;
    cet[p] := cn;
    p := IndexOfString(cet,FInstance.Name,p+1);
  end;
  CodeEditor.Memo.Text := cet.Text;
  cet.Free;

  FInstance.Name := FEditCtrl.Text;
  p := ObjInspector.cbControl.Items.IndexOfObject(FInstance);
  ObjInspector.cbControl.Items[p] := FInstance.Name+': '+FInstance.ClassName;
end;

procedure TSDLNameProperty.EditCtrlKeyDown(Sender: TSDLControl;
  var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_RETURN then
  begin
    EditCtrlExit(nil);
    Key := 0;
  end;
end;

procedure TSDLNameProperty.SetPropInfo(const Value: TrtProperty);
var t: TObject;
begin
  t := Value;
  if not (t is TSDLComponent) then Exit;
  EditCtrl.X := Width+2;
  EditCtrl.Y := Y;
  EditCtrl.Width := Width+22;
  EditCtrl.Height := Height;
  EditCtrl.BorderWidth := 1;
  FInstance := TSDLComponent(t);
  FEditCtrl.Text := FInstance.Name;
  if FInstance.Parent.FieldAddress(FInstance.Name)=nil then
  begin
    FEditCtrl.OnExit := EditCtrlExit;
    FEditCtrl.OnKeyDown := EditCtrlKeyDown;
  end
  else
    FEditCtrl.ReadOnly := True;
end;

{ TSDLEventProperty }

constructor TSDLEventProperty.Create(AParent: TSDLObject);
begin
  inherited;
  FEditCtrl := TSDLEdit.Create(AParent);
  FEditCtrl.AutoSelect := True;
end;

function TSDLEventProperty.EditCtrl: TSDLControl;
begin
  Result := FEditCtrl;
end;

procedure TSDLEventProperty.EditCtrlDblClick(Sender: TSDLComponent; AX, AY: Integer);
var t: string;
begin
  if Length(FEditCtrl.Text)>0 then Exit;
  t := FPropInfo.Name;
  Delete(t,1,2);
  FEditCtrl.Text := TSDLComponent(FPropInfo.Instance).Name+t;
  EditCtrlExit(nil);
end;

procedure TSDLEventProperty.EditCtrlExit(Sender: TSDLComponent);
var p,ei: Integer;
    cet: TStringList;
    t: string;
    Prop: TrtProperty;
begin
  FEditCtrl.Text := Trim(FEditCtrl.Text);
  if not FHasProc and (Length(FEditCtrl.Text)=0) then Exit;
  if TSDLComponent(FPropInfo.Instance).Name='' then
  begin
    FEditCtrl.Text := '';
    SDLMessageDlg(nil,Gui,[dbOK],'Error','Component doesn''t have a name.');
    Exit;
  end;

  if FHasProc then ei := GetEventIndex
  else ei := -1;

  if FHasProc and (FEditCtrl.Text=CodeEditor.EventInfo.Names[ei])then Exit;

  cet := TStringList.Create;
  cet.Text := CodeEditor.Memo.Text;
  p := IndexOfString(cet,'procedure '+FEditCtrl.Text,0);
  if (Length(FEditCtrl.Text)>0)and(p>0) then
  begin
    //that procedure already exists
    cet.Free;
    with CodeEditor do
      for p := 0 to EventInfo.Count-1 do
        if EventInfo.Names[p]=FEditCtrl.Text then
        begin
          //If assignable with existing procedure assign it
          t := EventInfo[p];
          Delete(t,1,AnsiPos('=',t));
          Prop := TrtProperty.Create(EventInfo.Objects[p],t);
          if Prop.TypeInfo<>FPropInfo.TypeInfo then Break;
          t := FEditCtrl.Text+'='+FPropInfo.Name;
          if ei<0 then
            EventInfo.AddObject(t,FPropInfo.Instance)
          else
            EventInfo[ei] := t;
          FHasProc := True;
          Exit;
        end;
    if FHasProc then
      FEditCtrl.Text := CodeEditor.EventInfo.Names[ei]
    else FEditCtrl.Text := '';
    Exit;
  end;
  if (ei<0)and (Length(FEditCtrl.Text)>0) then //New procedure
    ei := CodeEditor.AddProcDeclaration(FPropInfo, FEditCtrl.Text, -1)
  else if Length(FEditCtrl.Text)>=0 then  //change name of current procedure or delete it
  begin
    t := CodeEditor.EventInfo.Names[ei];
    CodeEditor.EventInfo.Delete(ei);
    if Length(FEditCtrl.Text)=0 then
      ei := -1 //delete it
    else //Add New
      ei := CodeEditor.AddProcDeclaration(FPropInfo, FEditCtrl.Text,p);
  end;
  cet.Free;
  FHasProc := ei>=0;
end;

procedure TSDLEventProperty.EditCtrlKeyDown(Sender: TSDLControl;
  var Key: Word; Modifier: TSDLMod);
begin
  if Key=SDLK_RETURN then
  begin
    EditCtrlExit(nil);
    Key := 0;
  end;
end;

function TSDLEventProperty.GetEventIndex: Integer;
var p: Integer;
begin
  p := IndexOfString(CodeEditor.EventInfo,FPropInfo.Name,0);
  while (p>=0)and(CodeEditor.EventInfo.Objects[p]<>FPropInfo.Instance) do
    p := IndexOfString(CodeEditor.EventInfo,FPropInfo.Name,p+1);
  if p>=0 then
    if CodeEditor.EventInfo.Objects[p]<>FPropInfo.Instance then p := -1;
  Result := p;
end;

procedure TSDLEventProperty.SetPropInfo(const Value: TrtProperty);
var p: Integer;
begin
  if Assigned(FPropInfo) then Exit;
  inherited;
  FEditCtrl.OnKeyDown := EditCtrlKeyDown;
  FEditCtrl.OnExit := EditCtrlExit;
  FEditCtrl.OnDblClick := EditCtrlDblClick;

  p := GetEventIndex;
  FHasProc := p>=0;
  if FHasProc then
    FEditCtrl.Text := CodeEditor.EventInfo.Names[p];
end;

initialization
  SDLRegisterPropertyEditor(TypeInfo(Integer),nil,'',TSDLIntegerProperty);
  SDLRegisterPropertyEditor(TypeInfo(Word),nil,'',TSDLIntegerProperty);
  SDLRegisterPropertyEditor(TypeInfo(Byte),nil,'',TSDLIntegerProperty);
  SDLRegisterPropertyEditor(TypeInfo(Cardinal),nil,'',TSDLIntegerProperty);
  SDLRegisterPropertyEditor(TypeInfo(Char),nil,'',TSDLCharProperty);
  SDLRegisterPropertyEditor(TypeInfo(string),nil,'',TSDLStringProperty);
  SDLRegisterPropertyEditor(TypeInfo(TFileName),nil,'',TSDLFileNameProperty);
  SDLRegisterPropertyEditor(TypeInfo(Boolean),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TStateImage),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TDrawOption),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TSDLDragMode),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TSDLTextLayout),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TSDLAlignment),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TSDLFontStyles),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TSDLBarKind),nil,'',TSDLEnumProperty);
  SDLRegisterPropertyEditor(TypeInfo(TStateImages),nil,'',TSDLSetProperty);
  SDLRegisterPropertyEditor(TypeInfo(TStringList),nil,'',TSDLStringsProperty);
  SDLRegisterPropertyEditor(TypeInfo(string),TSDLMemo,'Text',TSDLStringsProperty);

finalization
  if Assigned(RegPropEditors) then
  begin
    while RegPropEditors.Count>0 do
    begin
      Dispose(PPropEditorInfo(RegPropEditors.Objects[0]));
      RegPropEditors.Delete(0);
    end;
    RegPropEditors.Free;
  end;

end.
