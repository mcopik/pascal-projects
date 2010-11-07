unit SDLMenuDesigner;

interface

uses SDLLabel, SDLEdit, SDLButton, SDLListBox, SDLComboBox, 
  sdlgui, SDLDraw, SDLForm;

type
  TMenuDesigner = class(TSDLForm)
    lblControl: TSDLLabel;
    ebItem: TSDLEdit;
    btAddItem: TSDLButton;
    btAddPopup: TSDLButton;
    btAddMenu: TSDLButton;
    btClose: TSDLButton;
    lbItems: TSDLListBox;
    cbMenus: TSDLComboBox;
    procedure btAddItemClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btAddMenuClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btAddPopupClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure btCloseClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure cbMenusChange(Sender: TSDLComponent);
    procedure cbMenusClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure ebItemKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
    procedure lbItemsClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure lbItemsDblClick(Sender: TSDLComponent; AX, AY: Integer);
    procedure lbItemsKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
  private
    FAddingCtrl: TSDLComponent;
    { Private declarations }
    procedure Refresh;
    procedure SetAddingCtrl(const Value: TSDLComponent);
  public
    { Public declarations }
    constructor Create(AParent: TSDLObject); override;
    function SetFocus: Boolean; override;
    procedure Show;
    property AddingControl: TSDLComponent read FAddingCtrl write SetAddingCtrl;
  end;

var
  MenuDesigner: TMenuDesigner;

implementation

uses Inspector, SysUtils, Main, SDLMenus, sdl, SDLCodeEditor;

procedure TMenuDesigner.btAddItemClick(Sender: TSDLComponent; AX, AY: Integer);
var tm: TSDLMenuBase;
    p: Integer;
begin
  if (ebItem.Text='')or(cbMenus.ItemIndex=-1) then Exit;
  with cbMenus do
    tm := TSDLMenuBase(Items.Objects[ItemIndex]);
  p := tm.Add(ebItem.Text,nil);
  tm.Items[p].Name := ObjInspector.GetValidNameFrom('mi'+ebItem.Text,False);
  if CodeEditor.AddControlDeclaration(tm.Items[p])<0 then
  begin
    tm.Items[p].Free;
    Exit;
  end;
  if tm is TSDLMenu then
    TSDLMenu(tm).UpdateItems;
  lbItems.ItemIndex := lbItems.Items.AddObject(ebItem.Text,tm.Objects[p]);
  lbItemsClick(nil,0,0);
  ebItem.Text := '';
end;

procedure TMenuDesigner.btAddMenuClick(Sender: TSDLComponent; AX, AY: Integer);
var m: TSDLMenu;
begin
  m := TSDLMenu.Create(TSDLObject(Gui.DesignForm));
  m.Name := ObjInspector.GetValidNameFrom('Menu',False);
  if CodeEditor.AddControlDeclaration(m)<0 then
  begin
    m.Free;
    Exit;
  end;
  ObjInspector.SelectedControl := m;
  with ObjInspector do
    cbMenus.ItemIndex := cbMenus.Items.AddObject(cbControl.Text,SelectedControl);
end;

procedure TMenuDesigner.btAddPopupClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if not Assigned(FAddingCtrl) then Exit;
  ObjInspector.SelectedControl := TSDLPopupMenu.Create(FAddingCtrl);
  with ObjInspector do
    cbMenus.ItemIndex := cbMenus.Items.AddObject(cbControl.Text,SelectedControl);
end;

procedure TMenuDesigner.btCloseClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  LoseFocus;
  Visible := False;
end;

procedure TMenuDesigner.cbMenusChange(Sender: TSDLComponent);

  procedure AddlbItems(Menu: TSDLMenuBase;l: Integer);
  var i: Integer;
  begin
    for i := 0 to Menu.Count-1 do
      with Menu.Objects[i] as TSDLMenuItem do
      begin
        lbItems.Items.AddObject(StringOfChar(' ',l)+Caption,Menu.Objects[i]);
        if Assigned(PopupMenu) then
          AddlbItems(TSDLMenuBase(PopupMenu),l+2);
      end;
  end;

var tm: TSDLMenuBase;
begin
  lbItems.Items.Clear;
  with cbMenus do
    if ItemIndex>=0 then
      tm := TSDLMenuBase(Items.Objects[ItemIndex])
    else Exit;
  ObjInspector.SelectedControl := tm;
  if Assigned(tm) then
    AddlbItems(tm,0);
end;

procedure TMenuDesigner.cbMenusClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  if cbMenus.ItemIndex<0 then Exit;
  with cbMenus do
    ObjInspector.SelectedControl := TSDLComponent(Items.Objects[ItemIndex]);
end;

constructor TMenuDesigner.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('../Data/MenuDesigner.xfm');
  Visible := False;
end;

procedure TMenuDesigner.ebItemKeyDown(Sender: TSDLControl; var Key: Word; Modifier: Cardinal);
begin
  if Key<>SDLK_RETURN then Exit;
  btAddItemClick(nil,0,0);
  Key := 0;
end;

procedure TMenuDesigner.lbItemsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  with lbItems do
    if ItemIndex>=0 then
      ObjInspector.SelectedControl := TSDLComponent(Items.Objects[ItemIndex]);
end;

procedure TMenuDesigner.lbItemsDblClick(Sender: TSDLComponent; AX, AY: Integer);
var tc: TSDLCOmponent;
begin
  if lbItems.ItemIndex>=0 then
    with ObjInspector do
      if Assigned(SelectedControl.PopupMenu) then
      begin
        SelectedControl := SelectedControl.PopupMenu;
        cbMenus.ItemIndex := cbMenus.Items.IndexOf(cbControl.Text);
      end
      else
      begin
        with lbItems do
          tc := TSDLComponent(Items.Objects[ItemIndex]);
        ObjInspector.SelectedControl := TSDLPopupMenu.Create(tc);
        with ObjInspector do
          cbMenus.ItemIndex := cbMenus.Items.AddObject(cbControl.Text,SelectedControl);
      end;
end;

procedure TMenuDesigner.lbItemsKeyDown(Sender: TSDLControl;
          var Key: Word; Modifier: Cardinal);
var selmenu: TObject;
    tpm: TSDLObject;
    p: Integer;
begin
  if (Key<>SDLK_DELETE)or(lbItems.ItemIndex<0) then Exit;
  with cbMenus do
  begin
    selmenu := Items.Objects[ItemIndex];
    p := lbItems.ItemIndex-1;
    tpm := ObjInspector.SelectedControl.Parent;
    if (tpm is TSDLPopupMenu) and (tpm.Count=1) then
      ObjInspector.SelectedControl := TSDLComponent(tpm);
    ObjInspector.DelSelControl;
    Refresh;
    ItemIndex := Items.IndexOfObject(selmenu);
    if (ItemIndex>=0)and(Items.Objects[ItemIndex] is TSDLMenu)then
      TSDLMenu(Items.Objects[ItemIndex]).UpdateItems;
    if p>=0 then
    begin
      lbItems.ItemIndex := p;
      lbItemsClick(nil,0,0);
    end;
  end;
  Key := 0;
end;

procedure TMenuDesigner.Refresh;
var i: Integer;
begin
  cbMenus.Items.Clear;
  with ObjInspector.cbControl do
    for i := 0 to Items.Count-1 do
      if (Items.Objects[i] is TSDLMenuBase) then
        cbMenus.Items.AddObject(Items[i],Items.Objects[i])
end;

procedure TMenuDesigner.SetAddingCtrl(const Value: TSDLComponent);
begin
  FAddingCtrl := Value;
  if Assigned(FAddingCtrl) then
    lblControl.Caption := 'Adding PopupMenu to '+
                          FAddingCtrl.Name+': '+FAddingCtrl.ClassName
  else
    lblControl.Caption := 'nil';
end;

function TMenuDesigner.SetFocus: Boolean;
var tc: TObject;
begin
  Result := inherited SetFocus;
  tc := nil;
  with cbMenus do
  begin
    if ItemIndex>=0 then
      tc := Items.Objects[ItemIndex];
    Refresh;
    if Assigned(tc) then ItemIndex := Items.IndexOfObject(tc);
  end;
  if ObjInspector.cbControl.Items.IndexOfObject(FAddingCtrl)<0 then
    AddingControl := nil;
end;

procedure TMenuDesigner.Show;
begin
  Visible := True;
  SetFocus;
end;

end.







