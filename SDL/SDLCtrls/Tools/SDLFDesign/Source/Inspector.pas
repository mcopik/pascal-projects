unit Inspector;

interface

uses sdl, sdlgui, SDLForm, SDLDraw, SDLButton, SDLComboBox, clRTTI;

type
  TInsPanel = class(TSDLPanel)
  protected
    procedure DoDraw; override;
  end;

  TObjInspector = class(TSDLForm)
    cbControl: TSDLComboBox;
    btProps: TSDLButton;
    btEvents: TSDLButton;
    pnProps: TInsPanel;
    pnEvents: TInsPanel;
    procedure cbControlChange(Sender: TSDLComponent);
    procedure btPropsClick(Sender: TSDLComponent;AX,AY: Integer);
    procedure btEventsClick(Sender: TSDLComponent;AX,AY: Integer);
  private
    FSC: TSDLComponent;
    rtWrapper: TrtWrapper;
    procedure SetSC(const Value: TSDLComponent);
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure DelSelControl;
    function GetValidNameFrom(const AName: string; MustHaveNo: Boolean): string;
    property SelectedControl: TSDLComponent read FSC write SetSC;
  end;

var ObjInspector : TObjInspector;

implementation

uses SysUtils, TypInfo, SDLFont, SDLDesignEditors, SDLLabel;

{ TInsPanel }

procedure TInsPanel.DoDraw;
var cd,l: Cardinal;
    tr: TSDL_Rect;
    i: Integer;
begin
  inherited;
  cd := LightColor(FocusColor,-40);
  i := 17;
  tr := BoundsRect;
  InflateRect(tr,-BorderWidth,-BorderWidth);
  l := tr.w-ScrollBar.Width;
  while i<tr.h do
  begin
    SDLScreen.DrawOrthoLine(nil,True,tr.x,tr.y+i,l,cd);
    Inc(i,20);
  end;
  SDLScreen.DrawOrthoLine(nil,False,tr.x+86,tr.y,tr.h,cd);
end;

{ TObjInspector }

procedure TObjInspector.btEventsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  pnProps.Visible := False;
  pnEvents.Visible := True;
end;

procedure TObjInspector.btPropsClick(Sender: TSDLComponent; AX, AY: Integer);
begin
  pnEvents.Visible := False;
  pnProps.Visible := True;
end;

procedure TObjInspector.cbControlChange(Sender: TSDLComponent);
begin
  with cbControl do
    if ItemIndex>=0 then
      SetSC(TSDLComponent(Items.Objects[ItemIndex]))
    else
      SetSC(nil);
end;

constructor TObjInspector.Create(AParent: TSDLObject);
begin
  inherited;
  LoadCompFromFile('../Data/ObjInspector.xfm');
end;

procedure TObjInspector.DelSelControl;

  procedure DeleteControl(tc: TSDLComponent);
  var i: Integer;
  begin
    if Assigned(tc.PopupMenu) then DeleteControl(tc.PopupMenu);
    for i := 0 to tc.Count-1 do
      if tc.Objects[i] is TSDLComponent then
        DeleteControl(TSDLComponent(tc.Objects[i]));
    i := cbControl.Items.IndexOfObject(tc);
    if i>=0 then
      cbControl.Items.Delete(i);
  end;
var tc: TSDLComponent;
begin
  tc := FSC;
  SelectedControl := TSDLForm(GUI.DesignForm);
  DeleteControl(tc);
  tc.Free;
end;

destructor TObjInspector.Destroy;
begin
  if Assigned(rtWrapper) then
    rtWrapper.Free;
  inherited;
end;

function TObjInspector.GetValidNameFrom(const AName: string; MustHaveNo: Boolean): string;
var p: Integer;
    c: string;
begin
  c := cbControl.Items.Text;
  if not MustHaveNo and (AnsiPos(AName,c)<=0) then
  begin
    Result := AName;
    Exit;
  end;
  p := 1;
  while AnsiPos(AName+IntToStr(p),c)>0 do Inc(p);
  Result := AName+IntToStr(p);
end;

procedure TObjInspector.SetSC(const Value: TSDLComponent);

  procedure AddTocbControl(tc: TSDLComponent);
  var i: Integer;
  begin
    if tc.Designing and (cbControl.Items.IndexOfObject(tc)<0) then
      cbControl.Items.AddObject(tc.Name+': '+tc.ClassName,tc);
    for i := 0 to tc.Count-1 do
      if tc.Objects[i] is TSDLComponent then
        AddTocbControl(TSDLComponent(tc.Objects[i]));
    if Assigned(tc.PopupMenu) then AddTocbControl(tc.PopupMenu);
  end;

var i,c1,c2: Integer;
begin
  if FSC = Value then Exit;
  if Assigned(Value) and not Value.Designing then
    if Value.Parent is TSDLComponent then
      FSC := TSDLComponent(Value.Parent)
    else
      FSC := nil
  else
    FSC := Value;
  if Assigned(rtWrapper) then
  begin
    rtWrapper.Free;
    rtWrapper := nil;
  end;

  pnProps.ScrollBar.Position := 0;
  pnEvents.ScrollBar.Position := 0;
  while pnProps.Count>1 do
    pnProps.Objects[1].Free;
  while pnEvents.Count>1 do
    pnEvents.Objects[1].Free;

  if Assigned(FSC) then
  begin
    if FSC is TSDLPanel then
      TSDLPanel(FSC).RefreshScroll;
    AddTocbControl(FSC);
    cbControl.ItemIndex := cbControl.Items.IndexOfObject(FSC);
    rtWrapper := TrtWrapper.Create(FSC);
    c1 := 1;
    c2 := CreatePropEditor(TrtProperty(FSC),pnProps,1); //Name property editor
    for i := 0 to rtWrapper.Count-1 do
      if rtWrapper.Items[i].Kind=tkMethod then
        c1 := CreatePropEditor(rtWrapper.Items[i],pnEvents,c1)
      else
        c2 := CreatePropEditor(rtWrapper.Items[i],pnProps,c2);
    pnProps.RefreshScroll;
    pnEvents.RefreshScroll;
  end;
end;

initialization
  SDLRegisterClasses([TInsPanel]);

end.
