unit sdlgui;

{*********************************************************************
                sdlgui v1.0b -  03.10.2004.
             
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
Description: TSDLGUI, TSDLMouse, TSDLComponent, TSDLControl, TSDLStdControl.

If you have any suggestions or you made some modifications
please inform me.
**********************************************************************}

interface

uses sdl, sdlutils, SDLDraw, SDLFont, Classes;

type
  TSDLCursor = record
    Image : TSDLImage;
    HotSpotX : Byte;
    HotSpotY : Byte;
  end;
  TSDLBarKind = (sbHorizontal, sbVertical);
  TStateImage = (siFocused,siDown,siDisabled,siNone);
  TStateImages = set of TStateImage;
  TDrawOption = (doNormal,doCenter,doTile,doStretch);
  TSDLDragMode = (sdmManual,sdmAutomatic);

  // Forward declarations
  TSDLGui = class;
  TSDLImageObject = class;
  TSDLComponent = class;
  TSDLControl = class;
  TSDLObjectClass = class of TSDLComponent;

  // Events

  TSDLNotifyEvent = procedure(Sender: TSDLComponent) of object;
  TSDLMouseEvent = procedure(Sender: TSDLComponent;Button: Integer;
                            Modifier: TSDLMod;AX,AY: Word) of object;
  TSDLMouseClickEvent = procedure(Sender: TSDLComponent;AX,AY: Integer) of object;
  TSDLMouseMoveEvent = procedure(Sender: TSDLComponent;Modifier: TSDLMod;
                                AX,AY : Integer) of object;
  TSDLMouseWheelEvent = procedure(Sender: TSDLComponent;Dir: Integer;
                       Modifier: TSDLMod;AX,AY : Word) of object;
  TSDLKeyEvent = procedure(Sender: TSDLControl; var Key: Word;
                          Modifier: TSDLMod) of object;
  TSDLKeyPressEvent = procedure (Sender: TSDLControl; var Key: Char) of object;
  TSDLDrawItemEvent = procedure(Sender: TSDLControl;const ItemNo: Integer;
                      Rect: TSDL_Rect; State: Integer) of object;

  TSDLObject = class(TPersistent)
  private
    FGUI : TSDLGui;
    FParent: TSDLObject;
    FObjects: TList;
    FControlList : TList;
    FActiveControl : TSDLControl;
    FControlParent: TSDLControl;
    FZ : Integer;
    FWidth : Integer;
    FHeight : Integer;
    procedure Add(AObject: TSDLObject);
    procedure AddControl(Control: TSDLControl);
    procedure Remove(AObject: TSDLObject);
    procedure Draw;
    procedure Animate(MoveCount: Integer);
    function GetObject(Index: Integer): TSDLImageObject;
    function GetCount: Integer;
    function GetControlCount: Integer;
    function GetControl(Index: Integer): TSDLControl;
  protected
    procedure SetHeight(const Value: Integer); virtual;
    procedure SetWidth(const Value: Integer); virtual;
    procedure SetZ(const Value: Integer); virtual;
  public
    constructor Create(AParent: TSDLObject); virtual;
    destructor Destroy; override;
    procedure Clear;
    function IndexOfObject(AObject: TSDLImageObject): Integer;
    function IndexOfControl(Control: TSDLControl): Integer;
    property Count: Integer read GetCount;
    property ControlCount: Integer read GetControlCount;
    property GUI: TSDLGui read FGui;
    property Objects[Index: Integer]: TSDLImageObject read GetObject;
    property Controls[Index: Integer]: TSDLControl read GetControl;
    property ActiveControl: TSDLControl read FActiveControl write FActiveControl;
    property Parent: TSDLObject read FParent;
    property ControlParent: TSDLControl read FControlParent;
  published
    property Z: Integer read FZ write SetZ;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
  end;

  TSDLImageObject = class(TSDLObject)
  private
    FX : Integer;
    FY : Integer;
    FVisible : Boolean;
    FImage: TSDLImage;
    FDead : Boolean;
    FDrawOption: TDrawOption;
    FAnimLooped: Boolean;
    FAnimSpeed: Double;
    FAnimPos: Double;
    FAnimCount: Integer;
    FAnimStart: Integer;
    FImageIndex: Integer;
    procedure Draw;
    procedure Animate(MoveCount: Integer);
    function GetWorldX: Integer;
    function GetWorldY: Integer;
    procedure SetImageIndex(const Value: Integer);
  protected
    procedure SetX(const Value: Integer); virtual;
    procedure SetY(const Value: Integer); virtual;
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetImage(Value: TSDLImage); virtual;
    procedure DoDraw; virtual;
    procedure DoAnimate(MoveCount: Integer); virtual;
    function TestCollision: Boolean; virtual;
    function GetBoundsRect: TSDL_Rect; virtual;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure Kill;
    property WorldX: Integer read GetWorldX;
    property WorldY: Integer read GetWorldY;
    property BoundsRect: TSDL_Rect read GetBoundsRect;
    property Image: TSDLImage read FImage write SetImage;
    property Dead: Boolean read FDead;
    property AnimCount: Integer read FAnimCount write FAnimCount;
    property AnimLooped: Boolean read FAnimLooped write FAnimLooped;
    property AnimPos: Double read FAnimPos write FAnimPos;
    property AnimSpeed: Double read FAnimSpeed write FAnimSpeed;
    property AnimStart: Integer read FAnimStart write FAnimStart;
  published
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property DrawOption: TDrawOption read FDrawOption write FDrawOption default doStretch;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
  end;

  TSDLComponent = class(TSDLImageObject)
  private
    FBorderWidth: Byte;
    FEnabled: Boolean;
    FFont: TSDLFont;
    FPopupMenu: TSDLComponent;
    FPushed : Boolean;
    FTag: Integer;
    FDescription: string;
    FHint: string;
    FCursor: TSDLCursor;
    FOnDblClick: TSDLMouseClickEvent;
    FOnClick: TSDLMouseClickEvent;
    FOnMouseDown: TSDLMouseEvent;
    FOnMouseUp: TSDLMouseEvent;
    FOnMouseMove: TSDLMouseMoveEvent;
    FOnMouseWheel: TSDLMouseWheelEvent;
    FOnDestroy: TSDLNotifyEvent;
    FDragMode: TSDLDragMode;
    FDesigning: Boolean;
    FDraging : Boolean;
    FOldPos : TPoint;
    FName: string;
    procedure SetName(const Value: string);
    function GetEnabled: Boolean;
  protected
    FLoading: Boolean;
    procedure SetBorderWidth(const Value: Byte); virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetFont(const Value: TSDLFont); virtual;
    procedure MouseMove(Modifier: TSDLMod;AX,AY : Word); virtual;
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); virtual;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); virtual;
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY : Integer); virtual;
    procedure Click(AX,AY: Integer); virtual;
    procedure DblClick(AX,AY: Integer); virtual;
    procedure Loaded; virtual;
    procedure StartLoading; virtual;
  public
    FLockedCoords: Boolean; //used in Form Designer to destinguish components that can not be moved.
    constructor Create(AParent: TSDLObject); override;
    procedure BeforeDestruction; override;
    procedure HidePopupMenu; virtual;
    procedure Popup(AX,AY: Integer); virtual; //only used by TPopupMenu
    procedure LoadFromStrings(SList: TStrings;var Level: Integer);
    procedure LoadCompFromFile(const FileName: string);
    procedure SaveToStrings(SList: TStrings; Level: Byte);
    procedure SaveCompToFile(const FileName: string);
    property Pushed: Boolean read FPushed write FPushed;
    property Font: TSDLFont read FFont write SetFont;
    property Cursor : TSDLCursor read FCursor write FCursor;
    property Designing: Boolean read FDesigning write FDesigning;
    property Name: string read FName write SetName;
    //Use TPopupMenu.Create(SomeComponent) to assign this value
    property PopupMenu: TSDLComponent read FPopupMenu write FPopupMenu;
  published
    property BorderWidth: Byte read FBorderWidth write SetBorderWidth;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Description: string read FDescription write FDescription;
    property DragMode: TSDLDragMode read FDragMode write FDragMode;
    property Hint: string read FHint write FHint;
    property Tag : Integer read FTag write FTag default 0;
    property OnDestroy: TSDLNotifyEvent read FOnDestroy write FOnDestroy;
    property OnMouseMove: TSDLMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TSDLMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TSDLMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnClick: TSDLMouseClickEvent read FOnClick write FOnClick;
    property OnDblClick: TSDLMouseClickEvent read FOnDblClick write FOnDblClick;
    property OnMouseWheel: TSDLMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
  end;

  TSDLControl = class(TSDLComponent)
  private
    FTabOrder: Integer;
    FInFocus: Boolean;
    FOnKeyDown: TSDLKeyEvent;
    FOnKeyUp: TSDLKeyEvent;
    FOnKeyPress: TSDLKeyPressEvent;
    FOnExit: TSDLNotifyEvent;
    FOnEnter: TSDLNotifyEvent;
    procedure SetTabOrder(const Value: Integer);
  protected
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetVisible(const Value: Boolean); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY: Integer); override;
    procedure KeyUp(var Key: Word; Modifier: TSDLMod); virtual;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod); virtual;
    procedure KeyPress(var Key: Char); virtual;
    function LoseFocus: Boolean; virtual;
  public
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    function SetFocus: Boolean; virtual;
    property HasFocus: Boolean read FInFocus;
  published
    property TabOrder: Integer read FTabOrder write SetTabOrder;
    property OnKeyUp: TSDLKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown: TSDLKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TSDLKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnEnter: TSDLNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TSDLNotifyEvent read FOnExit write FOnExit;
  end;

  TSDLStdControl = class(TSDLControl)
  private
    FFocusColor: Cardinal;
    FColor: Cardinal;
    FDisabledColor: Cardinal;
    FAlwaysUp: Boolean;
    FAlwaysDown: Boolean;
    FStateImages: TStateImages;
    procedure SetAlwaysDown(const Value: Boolean);
    procedure SetAlwaysUp(const Value: Boolean);
  protected
    procedure DrawNormal;
    procedure DrawFocus;
    procedure DrawDown;
    procedure DrawDisabled;
    procedure DoDraw; override;
    procedure SetImage(Value: TSDLImage); override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY : Integer); override;
    function LoseFocus: Boolean; override;
  public
    constructor Create(AParent: TSDLObject); override;
    function SetFocus: Boolean; override;
  published
    property Color: Cardinal read FColor write FColor;
    property FocusColor: Cardinal read FFocusColor write FFocusColor;
    property DisabledColor: Cardinal read FDisabledColor write FDisabledColor;
    property AlwaysDown: Boolean read FAlwaysDown write SetAlwaysDown;
    property AlwaysUp: Boolean read FAlwaysUp write SetAlwaysUp;
    property StateImages : TStateImages read FStateImages write FStateImages;
  end;

  TSDLMouse = class(TSDLImageObject)
  private
    FOldClickTime: Cardinal;
    FDownControl : TSDLComponent;
    FOverControl : TSDLComponent;
    FConstCursor : Boolean;
    FOldTime: Cardinal;
    FHotSpotY: Byte;
    FHotSpotX: Byte;
    FDefaultCursor: TSDLCursor;
    procedure MouseMove(Modifier: TSDLMod;AX,AY: Integer);
    procedure MouseDown(Button: Integer;Modifier: TSDLMod;AX,AY: Integer);
    procedure MouseUp(Button: Integer;Modifier: TSDLMod;AX,AY: Integer);
    procedure Click(AX,AY: Integer);
    procedure MouseWheel(Dir: Integer;Modifier: TSDLMod;AX,AY: Integer);
    function  GetControl: TSDLComponent;
    procedure SetConstCursor(const Value: Boolean);
  protected
    procedure SetImage(Value: TSDLImage); override;
    procedure DoAnimate(MoveCount: Integer); override;
  public
    procedure SetCursor(const Value: TSDLCursor);
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    property ConstCursor: Boolean read FConstCursor write SetConstCursor;
    property HotSpotX: Byte read FHotSpotX write FHotSpotX;
    property HotSpotY: Byte read FHotSpotY write FHotSpotY;
    property DefaultCursor: TSDLCursor read FDefaultCursor write FDefaultCursor;
    property DownControl: TSDLComponent read FDownControl;
    property OverControl: TSDLComponent read FOverControl;
  end;

  TSDLGui = class(TSDLObject)
  private
    FOldTime: Cardinal;
    FAllCount: Integer;
    FDeadList: TList;
    FSDLMouse : TSDLMouse;
    FPopupMenu: TSDLComponent;
    FHint: string;
    FHintCoord: TPoint;
    FMethodRoot: TSDLComponent;
    procedure KeyDown(var Key: Word; Modifier: TSDLMod);
  public
    HintColor: Cardinal;
    ShowHintAfter: Cardinal;
    ShowHintFor: Cardinal;
    HintFont: TSDLFont;
    DesignForm: TObject;
    SetSelControl: TSDLNotifyEvent;
    DragEnabled: Boolean;
    ShowHints: Boolean;
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    procedure Kill;
    procedure ProcessEvent(const Event: TSDL_Event);
    procedure Update;
    procedure SendQuitSignal;
    property AllCount: Integer read FAllCount;
    property SDLMouse: TSDLMouse read FSDLMouse;
    property PopupMenu: TSDLComponent read FPopupMenu write FPopupMenu;
  end;

procedure SDLRegisterClasses(ClassArray: array of TSDLObjectClass);

const
  //Consts used to get mouse button out of Modifier in MouseMove
  KMOD_MBLEFT = 4;
  KMOD_MBMIDDLE = 8;
  KMOD_MBRIGHT = 16;

  //Consts used for State parameter of TSDLDrawItemEvent method
  //If you need anything else add it and inform me
  stSelected = 1;
  stFocused = 2;
  stChecked = 4;
  stDefault = 8;

var RegClasses : TStringList;

implementation

uses SysUtils, clRTTI, TypInfo;

procedure SDLRegisterClasses(ClassArray: array of TSDLObjectClass);
var i : Integer;
begin
  if RegClasses=nil then
    RegClasses := TStringList.Create;
  for i := Low(ClassArray) to High(ClassArray) do
    if RegClasses.IndexOf(ClassArray[i].ClassName)<0 then
      RegClasses.AddObject(ClassArray[i].ClassName,TObject(ClassArray[i]));
end;

{ TSDLObject }

procedure TSDLObject.Add(AObject: TSDLObject);
var L, H, I, C: Integer;
begin
  if FObjects=nil then
    FObjects := TList.Create;

  L := 0;
  H := FObjects.Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := TSDLObject(FObjects[I]).Z-AObject.Z;
    if C < 0 then L := I + 1 else
      H := I - 1;
  end;
  FObjects.Insert(L, AObject);

  if AObject is TSDLControl then
    AddControl(TSDLControl(AObject));
end;

procedure TSDLObject.AddControl(Control: TSDLControl);
var L, H, I, C: Integer;
begin
  if FControlList=nil then
    FControlList := TList.Create;

  L := 0;
  H := FControlList.Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := TSDLControl(FControlList[I]).FTabOrder-Control.FTabOrder;
    if C < 0 then L := I + 1 else
      H := I - 1;
  end;
  FControlList.Insert(L, Control);
end;

procedure TSDLObject.Animate(MoveCount: Integer);
var i: Integer;
begin
  if FObjects<>nil then
    for i:=0 to FObjects.Count-1 do
      TSDLImageObject(FObjects[i]).Animate(MoveCount);
end;

procedure TSDLObject.Clear;
begin
  while Count>0 do
    Objects[0].Free;
end;

constructor TSDLObject.Create(AParent: TSDLObject);
begin
  FParent := AParent;
  if FParent<>nil then
  begin
    if FParent is TSDLImageObject then
      FZ := FParent.Count
    else FZ := MaxInt-10;

    FParent.Add(Self);

    if FParent is TSDLGui then
      FGui := TSDLGui(FParent)
    else
    begin
      FGui := FParent.FGui;
      if FParent is TSDLControl then
        FControlParent := TSDLControl(FParent)
      else
        FControlParent := FParent.FControlParent;
    end;
    Inc(FGui.FAllCount);
  end;
end;

destructor TSDLObject.Destroy;
begin
  Clear;
  if FParent<>nil then
  begin
    Dec(FGui.FAllCount);
    FParent.Remove(Self);
    FGui.FDeadList.Remove(Self);
  end;
  FObjects.Free;
  FControlList.Free;
  inherited;
end;

procedure TSDLObject.Draw;
var i: Integer;
begin
  if Assigned(FObjects) then
    for i:=0 to FObjects.Count-1 do
      TSDLImageObject(FObjects[i]).Draw;
end;

function TSDLObject.GetControl(Index: Integer): TSDLControl;
begin
  if FControlList<>nil then
    Result := FControlList[Index]
  else
    Result := nil;
end;

function TSDLObject.GetControlCount: Integer;
begin
   if FControlList<>nil then
    Result := FControlList.Count
  else
    Result := 0;
end;

function TSDLObject.GetCount: Integer;
begin
  if FObjects<>nil then
    Result := FObjects.Count
  else
    Result := 0;
end;

function TSDLObject.GetObject(Index: Integer): TSDLImageObject;
begin
  if FObjects<>nil then
    Result := FObjects[Index]
  else
    Result := nil;
end;

function TSDLObject.IndexOfControl(Control: TSDLControl): Integer;
begin
  Result := FControlList.IndexOf(Control);
end;

function TSDLObject.IndexOfObject(AObject: TSDLImageObject): Integer;
begin
  Result := FObjects.IndexOf(AObject);
end;

procedure TSDLObject.Remove(AObject: TSDLObject);
begin
  FObjects.Remove(AObject);
  if AObject is TSDLControl then
    FControlList.Remove(AObject);
  if FObjects.Count=0 then
  begin
    FObjects.Free;
    FObjects := nil;
    FControlList.Free;
    FControlList := nil;
  end;
end;

procedure TSDLObject.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TSDLObject.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

procedure TSDLObject.SetZ(const Value: Integer);
begin
  if FZ<>Value then
  begin
    FZ := Value;
    if FParent<>nil then
    begin
      FParent.Remove(Self);
      FParent.Add(Self);
    end;
  end;
end;

{ TSDLImageObject }

procedure TSDLImageObject.Animate(MoveCount: Integer);
begin
  DoAnimate(MoveCount);
  inherited;
end;

constructor TSDLImageObject.Create(AParent: TSDLObject);
begin
  inherited;
  FVisible := True;
  FDrawOption := doStretch;
  FImageIndex := -1;
end;

procedure TSDLImageObject.Kill;
begin
  if Assigned(FGui)and(not FDead) then
  begin
    FDead := True;
    FGui.FDeadList.Add(Self);
  end;
end;

destructor TSDLImageObject.Destroy;
begin
  with FGui.FSDLMouse do
  begin
    if FDownControl=Self then
      FDownControl := nil;
    if FOverControl=Self then
      FOverControl := nil;
  end;
  inherited;
end;

procedure TSDLImageObject.DoAnimate(MoveCount: Integer);
  function Mod2(i: Double; i2: Integer): Double;
  begin
    if i2=0 then
      Result := i
    else
    begin
      Result := i-Trunc(i/i2)*i2;
      if Result<0 then
        Result := i2+Result;
    end;
  end;
begin
  AnimPos := AnimPos + AnimSpeed*MoveCount;

  if AnimLooped then
  begin
    if AnimCount>0 then
      AnimPos := Mod2(AnimPos, AnimCount)
    else
      AnimPos := 0;
  end else
  begin
    if AnimPos>=AnimCount then
    begin
      AnimPos := AnimCount-1;
      AnimSpeed := 0;
    end;
    if AnimPos<0 then
    begin
      AnimPos := 0;
      AnimSpeed := 0;
    end;
  end;
end;

procedure TSDLImageObject.DoDraw;
var IIndex,px,py: Integer;
    r,s : TSDL_Rect;
begin
  if FImage=nil then Exit;

  r := BoundsRect;
  IIndex := AnimStart+Trunc(AnimPos);
  if FDrawOption<>doStretch then
  begin
    s := FImage.PatternRects[IIndex];
    if r.w<s.w then InflateRect(s,(r.w-s.w)div 2,0);
    if r.h<s.h then InflateRect(s,0,(r.h-s.h)div 2);
  end;

  case FDrawOption of
  doNormal: FImage.DrawRect(SDLScreen.Surface,r.x,r.y,s);
  doCenter:
    begin
      OffsetRect(r,(r.w-s.w)div 2,(r.h-s.h)div 2);
      FImage.DrawRect(SDLScreen.Surface,r.x,r.y,s);
    end;
  doTile:
    begin
      SDLScreen.SetClipRect(nil,@r);
      py := r.y;
      repeat
        px := r.x;
        repeat
          FImage.DrawRect(SDLScreen.Surface,px,py,s);
          Inc(px,s.w);
        until px>r.x+r.w;
        Inc(py,s.h);
      until py>r.y+r.h;
      SDLScreen.SetClipRect(nil,nil);
    end;
  else
    FImage.StretchDraw(SDLScreen.Surface,r,IIndex);
  end;
end;

procedure TSDLImageObject.Draw;
var t: Boolean;
begin
  t := (Self is TSDLComponent)and TSDLComponent(Self).FDesigning;
  if Visible or t then
    if Parent=Gui then
    begin
      if OverlapRect(SDLScreen.SurfaceRect, BoundsRect) then
      begin
        DoDraw;
        inherited;
      end;
    end
    else if RectInRect(BoundsRect,TSDLImageObject(Parent).BoundsRect) then
    begin
      DoDraw;
      inherited;
    end;
end;

function TSDLImageObject.GetBoundsRect: TSDL_Rect;
begin
  Result := SDLRect(WorldX, WorldY, Width, Height);
end;

function TSDLImageObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TSDLImageObject.GetWorldX: Integer;
begin
  if Assigned(FParent)and(FParent is TSDLImageObject) then
    Result := TSDLImageObject(FParent).WorldX+FX
  else
    Result := FX;
end;

function TSDLImageObject.GetWorldY: Integer;
begin
  if Assigned(FParent)and(FParent is TSDLImageObject) then
    Result := TSDLImageObject(FParent).WorldY+FY
  else
    Result := FY;
end;

procedure TSDLImageObject.SetImage(Value: TSDLImage);
begin
  if FImage=Value then Exit;

  FImage := Value;
  if FImage=nil then Exit;

  AnimCount := FImage.PatternCount;
  if (AnimCount>1)and(AnimSpeed=0) then
    AnimSpeed := 3/1000;
  AnimPos := 0;
  AnimLooped := True;
end;

procedure TSDLImageObject.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TSDLImageObject.SetX(const Value: Integer);
begin
  FX := Value;
end;

procedure TSDLImageObject.SetY(const Value: Integer);
begin
  FY := Value;
end;

function TSDLImageObject.TestCollision: Boolean;
begin
  Result := True;
end;

procedure TSDLImageObject.SetImageIndex(const Value: Integer);
var ot: TSDLObject;
    pi: ^TSDLImages;
begin
  if Assigned(Image) and (Value = FImageIndex) then Exit;
  FImageIndex := Value;
  Image := nil;
  if FImageIndex<0 then Exit;
  ot := Self;
  pi := nil;
  while (pi=nil)and(ot is TSDLComponent) do
  begin
    pi := ot.FieldAddress('RImages');
    ot := ot.Parent;
  end;
  if Assigned(pi) and Assigned(pi^) then
    if FImageIndex<pi^.Count then
      Image := pi^.Items[FImageIndex];
end;

{ TSDLComponent }

procedure TSDLComponent.BeforeDestruction;
begin
  inherited;
  if Gui.FHint=Hint then Gui.FHint := '';
  if Assigned(OnDestroy) then OnDestroy(Self);
end;

procedure TSDLComponent.Click(AX, AY: Integer);
begin
  if Assigned(FControlParent) and not FControlParent.FInFocus then Exit;

  if PointInRect(SDLPoint(AX,AY),BoundsRect) then
    if Assigned(OnClick) then OnClick(Self,AX,AY);
end;

constructor TSDLComponent.Create(AParent: TSDLObject);
begin
  inherited;
  Font := GlobalFont;
  FEnabled := True;
  FBorderWidth := 2;
  if Parent is TSDLComponent then
    Designing := TSDLComponent(Parent).Designing;
end;

procedure TSDLComponent.DblClick(AX, AY: Integer);
begin
  if Assigned(OnDblClick) then OnDblClick(Self,AX,AY);
end;

function TSDLComponent.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSDLComponent.HidePopupMenu;
var r: Boolean;
  procedure Check(It: TSDLImageObject);
  var i: Integer;
  begin
    for i := 0 to It.Count-1 do
    begin
      r := (FGui.FSDLMouse.FOverControl=It.Objects[i]);
      if r then Exit;
      if It.Objects[i].Count>0 then
        Check(It.Objects[i]);
    end;
  end;
begin
  if FGui.PopupMenu=nil then Exit;
  r := (FGui.FSDLMouse.FOverControl=FGui.PopupMenu);
  if not r then
    Check(FGui.PopupMenu);
  if not r then FGui.FPopupMenu := nil;
end;

procedure TSDLComponent.Loaded;
begin
  FLoading := False;
end;

procedure TSDLComponent.LoadCompFromFile(const FileName: string);
var SList: TStringList;
    l: Integer;
begin
  l := 0;
  SList := TStringList.Create;
  SList.LoadFromFile(FileName);
  LoadFromStrings(SList,l);
  SList.Free;
end;

procedure TSDLComponent.LoadFromStrings(SList: TStrings;var Level: Integer);

  procedure LoadStrings(Strings: TStrings);
  var t: string;
  begin
    Strings.Clear;
    t := SList[Level];
    if t[Length(t)]=')' then Exit;
    Strings.BeginUpdate;
    Inc(Level);
    t := Trim(SList[Level]);
    while t[Length(t)]<>')' do
    begin
      Strings.Add(AnsiDequotedStr(t,''''));
      Inc(Level);
      t := Trim(SList[Level]);
    end;
    Delete(t,Length(t),1);
    Strings.Add(AnsiDequotedStr(t,''''));
    Strings.EndUpdate;
  end;

  procedure SkipToEnd;
  var i: Integer;
      t: string;
  begin
    i := 0;
    Inc(Level);
    t := Trim(SList[Level]);
    while (t<>'end')or(i>0) do
    begin
      if AnsiPos('object',t)>0 then Inc(i);
      if t='end' then Dec(i);
      Inc(Level);
      t := Trim(SList[Level]);
    end;
  end;

var rtWrapper: TrtWrapper;
    Prop: TrtProperty;
    t,pname,pvalue: string;
    p: Integer;
    tc1,tc2: TSDLObjectClass;
    tm: TMethod;
    delRoot: Boolean;
    fa: ^TSDLComponent;
    tsl: TStringList;
begin
  StartLoading;
  delRoot := not Assigned(Gui.FMethodRoot);
  if delRoot then Gui.FMethodRoot := Self;

  rtWrapper := TrtWrapper.Create(Self);

  t := Trim(SList[Level]);
  Delete(t,1,7); //Deletes object_
  FName := Copy(t,1,AnsiPos(':',t)-1);
  fa := Gui.FMethodRoot.FieldAddress(FName);
  if Assigned(fa) then
    fa^ := Self;

  Inc(Level);
  t := Trim(SList[Level]);

  tsl := TStringList.Create;
  while (t<>'end')and (AnsiPos('object',t)=0) do
  begin
    pname := Copy(t,1,AnsiPos(' = ',t)-1);
    pvalue := Copy(t,Length(pname)+4,Length(t));
    Prop := rtWrapper.ItemsByName[pname];
    if Assigned(Prop)then
      case Prop.Kind of
      tkEnumeration, tkSet: Prop.AsString := pvalue;
      tkLString:
        if pvalue[1]='(' then
        begin
          LoadStrings(tsl);
          Prop.AsString := tsl.Text;
        end
        else
        begin
          if pvalue = '''''' then      //two quotes = empty string
            pvalue := ''
          else
            pvalue := AnsiDequotedStr(pvalue,'''');
          Prop.AsString := pvalue;
        end;
      tkFloat: Prop.AsFloat := StrToFloat(pvalue);
      tkChar, tkInteger: Prop.AsInteger := StrToIntDef(pvalue,0);
      tkMethod:
        if Assigned(Gui.DesignForm)and(Gui.DesignForm is TStringList) then
          with Gui.DesignForm as TStringList do
            AddObject(pvalue+'='+pname,Self)
        else
        begin
          tm.Code := Gui.FMethodRoot.MethodAddress(pvalue);
          tm.Data := Gui.FMethodRoot;
          if Assigned(tm.Code) then
            Prop.AsMethod := tm;
        end;
      tkClass:
        if Prop.PropClassType.InheritsFrom(TStrings) then
        begin
          if Prop.AsObject=nil then
            Prop.AsObject := Prop.PropClassType.Create;
          LoadStrings(TStrings(Prop.AsObject));
        end;
      end;
    Inc(Level);
    t := Trim(SList[Level]);
  end;
  tsl.Free;

  if t <> 'end' then
  begin
    p := RegClasses.IndexOf('TSDLPopupMenu');
    tc2 := nil;
    if p>=0 then
      tc2 := TSDLObjectClass(RegClasses.Objects[p]);
    p := RegClasses.IndexOf(Copy(t,AnsiPos(':',t)+2,Length(t)));
    if p<0 then
    begin
      SkipToEnd;
      Inc(Level);
      t := Trim(SList[Level]);
    end
    else
    begin
      tc1 := TSDLObjectClass(RegClasses.Objects[p]);
      if tc1.InheritsFrom(tc2) then
      begin
        with tc1.Create(Self) do
          LoadFromStrings(SList,Level);
        Inc(Level);
        t := Trim(SList[Level]);
      end;
    end;

    while t<>'end' do
    begin
      Delete(t,1,7); //Deletes object_
      pname := Copy(t,1,AnsiPos(':',t)-1);
      pvalue := Copy(t,AnsiPos(':',t)+2,Length(t));
      p := RegClasses.IndexOf(pvalue);
      if p<0 then SkipToEnd
      else
      begin
        tc1 := TSDLObjectClass(RegClasses.Objects[p]);
        fa := FieldAddress(pname);
        if fa=nil then fa := Gui.FMethodRoot.FieldAddress(pname);
        if Assigned(fa) and Assigned(fa^) then
          fa^.LoadFromStrings(SList,Level)
        else with tc1.Create(Self) do
          LoadFromStrings(SList,Level);
      end;
      Inc(Level);
      t := Trim(SList[Level]);
    end;
  end;  //if t<>'end'

  rtWrapper.Free;
  if delRoot then Gui.FMethodRoot := nil;
  Loaded;
end;

procedure TSDLComponent.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  if Assigned(FControlParent) then
    with FControlParent do
      if not FInFocus then
        if not SetFocus then Exit;

  FPushed := Button=1;
  FOldPos := SDLPoint(AX,AY);
  FDraging := FPushed and ((FDragMode=sdmAutomatic)or
              (FDesigning and not FLockedCoords));

  if Assigned(OnMouseDown) then OnMouseDown(Self,Button,Modifier,AX,AY);
end;

procedure TSDLComponent.MouseMove(Modifier: TSDLMod; AX, AY: Word);
begin
  if FDraging and Gui.DragEnabled then
  begin
    X := X+(AX-FOldPos.X);
    Y := Y+(AY-FOldPos.Y);
    FOldPos.X := AX;
    FOldPos.Y := AY;
  end;
  if Assigned(OnMouseMove) then OnMouseMove(Self,Modifier,AX,AY);
end;

procedure TSDLComponent.MouseUp(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  FDraging := False;
  if Assigned(FControlParent) and not FControlParent.FInFocus then Exit;

  FPushed := False;

  if PointInRect(SDLPoint(AX,AY),BoundsRect) then
  begin
    if Assigned(OnMouseUp) then OnMouseUp(Self,Button,Modifier,AX,AY);
    if (Button=3)and Assigned(FPopupMenu) then FPopupMenu.Popup(AX,AY);
  end;
end;

procedure TSDLComponent.MouseWheel(Dir: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  if Assigned(OnMouseWheel) then OnMouseWheel(Self,Dir,Modifier,AX,AY);
end;

procedure TSDLComponent.Popup(AX, AY: Integer);
begin
  // Only PopupMenu uses this but this controls needs to call it on
  // right click
end;

procedure TSDLComponent.SaveCompToFile(const FileName: string);
var SList: TStringList;
begin
  SList := TStringList.Create;
  SaveToStrings(SList,0);
  SList.SaveToFile(FileName);
  SList.Free;
end;

procedure TSDLComponent.SaveToStrings(SList: TStrings; Level: Byte);

  procedure SaveStringList(Strings: TStrings);
  var t: string;
      i : Integer;
  begin
    if Strings.Count=0 then
    begin
      SList[SList.Count-1] := SList[SList.Count-1]+')';
      Exit;
    end;
    t := StringOfChar(' ',2*(Level+2));
    for i := 0 to Strings.Count-2 do
      SList.Add(t+AnsiQuotedStr(Strings[i],''''));
    SList.Add(t+AnsiQuotedStr(Strings[Strings.Count-1],'''')+')');
  end;

var rtWrapper: TrtWrapper;
    Prop: TrtProperty;
    i: Integer;
    t: string;
    tm: TMethod;
    delRoot: Boolean;
    tsl: TStringList;
begin
  delRoot := not Assigned(Gui.FMethodRoot);
  if not delRoot then
    if Gui.FMethodRoot.Designing and not Designing then Exit;
  if delRoot then Gui.FMethodRoot := Self;

  rtWrapper := TrtWrapper.Create(Self);

  t := StringOfChar(' ',2*Level);
  t := t+'object '+FName+': '+rtWrapper.ObjClass.ClassName;
  i := SList.Add(t);

  if Assigned(Gui.DesignForm)and (Gui.DesignForm is TStringList) then
    with Gui.DesignForm as TStringList do
      AddObject(IntToStr(i),Self);

  tsl := TStringList.Create;
  for i := 0 to rtWrapper.Count-1 do
  begin
    t := '';
    Prop := rtWrapper.Items[i];

    if Prop.ReadOnly or not Prop.IsStored then Continue;

    t := StringOfChar(' ',(Level+1)*2)+Prop.Name+' = ';
    case Prop.Kind of
    tkEnumeration, tkSet: t := t+Prop.AsString;
    tkLString:
      begin
        if Length(Prop.AsString)>0 then
          if AnsiPos(#10,Prop.AsString)>0 then
          begin
            SList.Add(t+'(');
            tsl.Text := Prop.AsString;
            SaveStringList(tsl);
          end
          else
            SList.Add(t+AnsiQuotedStr(Prop.AsString,''''));
        Continue;
      end;
    tkFloat: t := t+FloatToStr(Prop.AsFloat);
    tkChar, tkInteger: t := t+IntToStr(Prop.AsInteger);
    tkMethod:
      begin
        tm := Prop.AsMethod;
        if Assigned(tm.Code) then
          t := t+Gui.FMethodRoot.MethodName(tm.Code)
        else Continue;
      end;
    tkClass: if (Prop.AsObject<>nil)and
                (Prop.PropClassType.InheritsFrom(TStrings)) then
      begin
        SList.Add(t+'(');
        SaveStringList(TStrings(Prop.AsObject));
        Continue;
      end;
    end;
    SList.Add(t);
  end;
  tsl.Free;

  if Assigned(PopupMenu) then
    PopupMenu.SaveToStrings(SList,Level+1);

  for i := 0 to Count-1 do
    if Objects[i] is TSDLComponent then
      TSDLComponent(Objects[i]).SaveToStrings(SList,Level+1);

  SList.Add(StringOfChar(' ',2*Level)+'end');

  rtWrapper.Free;
  if delRoot then Gui.FMethodRoot := nil;
end;

procedure TSDLComponent.SetBorderWidth(const Value: Byte);
begin
  FBorderWidth := Value;
end;

procedure TSDLComponent.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSDLComponent.SetFont(const Value: TSDLFont);
begin
  FFont := Value;
end;

procedure TSDLComponent.SetName(const Value: string);
var pc: ^TSDLComponent;
    ot: TSDLObject;
begin
  FName := Value;
  ot := Parent;
  pc := nil;
  while (pc=nil)and(ot is TSDLComponent) do
  begin
    pc := ot.FieldAddress(FName);
    ot := ot.Parent;
  end;
  if Assigned(pc) then
    pc^ := Self;
end;

procedure TSDLComponent.StartLoading;
begin
  FLoading := True;
end;

{ TSDLControl }

constructor TSDLControl.Create(AParent: TSDLObject);
begin
  FTabOrder := AParent.ControlCount;
  inherited;
end;

destructor TSDLControl.Destroy;
begin
  if FParent.FActiveControl=Self then
    FParent.FActiveControl:=nil;
  inherited;
end;

procedure TSDLControl.KeyDown(var Key: Word; Modifier: TSDLMod);
var Tab : Integer;
begin
  Tab := -1;
  if Assigned(FActiveControl) then
  begin
    FActiveControl.KeyDown(Key,Modifier);
    if Key=0 then Exit;
    if Assigned(FActiveControl) then
      Tab := FActiveControl.FTabOrder;
  end;

  if Assigned(OnKeyDown) then OnKeyDown(Self,Key,Modifier);

  if (ControlCount<2)or(KMOD_CTRL and Modifier>0) then Exit;

  repeat
  case Key of
  SDLK_TAB :
    if KMOD_Shift and Modifier>0 then
    begin
      Dec(Tab);
      if Tab<0 then Tab := ControlCount-1;
    end
    else
      Tab := (Tab+1) mod ControlCount;
  SDLK_RIGHT,SDLK_DOWN: Tab := (Tab+1)mod ControlCount;
  SDLK_LEFT,SDLK_UP   :
    begin
      Dec(Tab);
      if Tab<0 then Tab := ControlCount-1;
    end
  else
    Tab := -1;
  end;

  if Tab<0 then Break;
  until Controls[Tab].SetFocus;
  if Tab>=0 then
    Key := 0;
end;

procedure TSDLControl.KeyPress(var Key: Char);
begin
  if Assigned(FActiveControl) then
  begin
    FActiveControl.KeyPress(Key);
    if Key=#0 then Exit;
  end;
  if Assigned(OnKeyPress) then OnKeyPress(Self,Key);
end;

procedure TSDLControl.KeyUp(var Key: Word; Modifier: TSDLMod);
begin
  if Assigned(FActiveControl) then
  begin
    FActiveControl.KeyUp(Key,Modifier);
    if Key=0 then Exit;
  end;
  if Assigned(OnKeyUp) then OnKeyUp(Self,Key,Modifier);
end;

function TSDLControl.LoseFocus: Boolean;
begin
  Result := True;
  if not FInFocus then Exit;
  FInFocus := False;
  if Assigned(OnExit) then OnExit(Self);
  if Parent.ActiveControl=Self then Parent.ActiveControl := nil;
  if Assigned(FActiveControl) then
    FActiveControl.LoseFocus;
end;

procedure TSDLControl.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  if Assigned(FControlParent) and not FControlParent.FInFocus then
    if not FControlParent.SetFocus then Exit;

  if not FInFocus then SetFocus;
  inherited;
end;

procedure TSDLControl.SetEnabled(const Value: Boolean);
begin
  inherited;
  if not (Enabled or FDesigning) then
  begin
    LoseFocus;
    with FGui.FSDLMouse do
    begin
      if FDownControl=Self then
        FDownControl := nil;
      if FOverControl=Self then
        FOverControl := nil;
    end;
  end;
end;

function TSDLControl.SetFocus: Boolean;
begin
  Result := False;
  if not(Visible and Enabled or FDesigning) then Exit;
  if Assigned(FControlParent) then
    with FControlParent do
    begin
      if not FInFocus then
        if not SetFocus then Exit;
      if Assigned(FActiveControl) then
        if not FActiveControl.LoseFocus then Exit;
      FActiveControl := Self;
    end
  else
    with FGUI do
    begin
      if Assigned(FActiveControl) then
        if not FActiveControl.LoseFocus then Exit;
      FActiveControl := Self;
    end;
  FInFocus := True;
  Result := True;
  if Assigned(OnEnter) then OnEnter(Self);
  if Assigned(FActiveControl) then
    FActiveControl.SetFocus
  else if ControlCount>0 then
    Controls[0].SetFocus;
end;

procedure TSDLControl.SetTabOrder(const Value: Integer);
begin
  FTabOrder := Value;
  FParent.Remove(Self);
  FParent.Add(Self);
end;

procedure TSDLControl.SetVisible(const Value: Boolean);
begin
  inherited;
  if not (Visible or FDesigning) then
  begin
    LoseFocus;
    with FGui.FSDLMouse do
    begin
      if FDownControl=Self then
        FDownControl := nil;
      if FOverControl=Self then
        FOverControl := nil;
    end;
  end;
end;

{ TSDLStdControl }

constructor TSDLStdControl.Create(AParent: TSDLObject);
begin
  inherited;
  FFocusColor := $B4C3DC;
  FColor := LightColor(FFocusColor,-30);
  FDisabledColor := $B4B4B4;
end;

procedure TSDLStdControl.DoDraw;
var c: Cardinal;
    t: TSDL_Rect;
begin
  inherited;

  if Image=nil then
  begin
    if HasFocus then c := FFocusColor
    else if not Enabled then c := FDisabledColor
    else c := FColor;
    t := BoundsRect;
    SDLScreen.Draw3DControl(SDLScreen.Surface,t,c,BorderWidth,
                          (Pushed or FAlwaysDown) and not FAlwaysUp);
  end;
end;

procedure TSDLStdControl.DrawDisabled;
begin
  if siDisabled in StateImages then
  begin
    FAnimStart := FAnimCount*3;
    if not(siFocused in StateImages) then
      Dec(FAnimStart,FAnimCount);
    if not(siDown in StateImages) then
      FAnimStart := FAnimCount;
  end
  else FAnimStart := 0;
end;

procedure TSDLStdControl.DrawDown;
begin
  if siDown in StateImages then
    if siFocused in StateImages then
      AnimStart := AnimCount shl 1
    else
      AnimStart := AnimCount
  else if siFocused in StateImages then
    AnimStart := AnimCount
  else
    AnimStart := 0;
end;

procedure TSDLStdControl.DrawFocus;
begin
  if siFocused in StateImages then
    AnimStart := AnimCount
  else
    AnimStart := 0;
end;

procedure TSDLStdControl.DrawNormal;
begin
  AnimStart := 0;
end;

function TSDLStdControl.LoseFocus: Boolean;
begin
  Result := inherited LoseFocus;
  DrawNormal;
  FPushed := False;
end;

procedure TSDLStdControl.MouseDown(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;

  if FInFocus and (Button = 1) then
  begin
    DrawDown;
    FPushed := True;
  end;
end;

procedure TSDLStdControl.MouseUp(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  inherited;
  if FInFocus then
  begin
    DrawFocus;
    FPushed := False;
  end;
end;

procedure TSDLStdControl.SetAlwaysDown(const Value: Boolean);
begin
  if FAlwaysDown=Value then Exit;
  FAlwaysDown := Value;
  if Value then
    FAlwaysUp := False;
end;

procedure TSDLStdControl.SetAlwaysUp(const Value: Boolean);
begin
  if FAlwaysUp=Value then Exit;
  FAlwaysUp := Value;
  if Value then
    FAlwaysDown := False;
end;

procedure TSDLStdControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled=Value then Exit;
  inherited;
  if Enabled then DrawNormal
  else DrawDisabled;
  FPushed := False;
end;

function TSDLStdControl.SetFocus: Boolean;
begin
  Result := inherited SetFocus;
  if Result then DrawFocus;
end;

procedure TSDLStdControl.SetImage(Value: TSDLImage);
var RowCount: Byte;
begin
  inherited;

  if not Assigned(Image) then Exit;
  if Image.PatternHeight>0 then
  begin
    RowCount := Image.Height div Image.PatternHeight;
    AnimCount := AnimCount div RowCount;
    case RowCount of
    1: StateImages := [siNone];
    2: StateImages := [siFocused];
    3: StateImages := [siFocused,siDown];
    else
      StateImages := [siFocused,siDown,siDisabled];
    end;
  end;
end;

{ TSDLMouse }

procedure TSDLMouse.Click(AX, AY: Integer);
var t: Cardinal;
    tc: TSDLComponent;
    ot: TSDLObject;
begin
  if Assigned(FDownControl) then
  begin
    tc := FDownControl;
    FDownControl := nil;
    if Assigned(Gui.DesignForm) then
    begin
      ot := tc;
      while Assigned(ot) and (ot<>Gui.DesignForm) do
        ot := ot.Parent;
      if Assigned(ot) then
        Gui.SetSelControl(tc);
    end;
    tc.Click(AX,AY);
    t := SDL_GetTicks;
    if t-FOldClickTime<500 then
    begin
      tc.DblClick(AX,AY);
      FOldClickTime := t-501; 
    end
    else FOldClickTime := t;
  end;
end;

constructor TSDLMouse.Create(AParent: TSDLObject);
begin
  inherited;
  Z := MaxInt;
end;

destructor TSDLMouse.Destroy;
begin
  inherited;
  SDL_ShowCursor(SDL_ENABLE);
end;

procedure TSDLMouse.DoAnimate(MoveCount: Integer);
var c: Cardinal;
begin
  inherited;
  c := SDL_GetTicks;

  if Assigned(FOverControl) and not FOverControl.FDesigning then
    if c-FOldTime>Gui.ShowHintAfter+Gui.ShowHintFor then
      Gui.FHint := ''
    else if (c-FOldTime>Gui.ShowHintAfter)and(Gui.FHint<>FOverControl.Hint) then
    begin
      Gui.FHint := FOverControl.Hint;
      Gui.FHintCoord.x := X-20;
      Gui.FHintCoord.y := Y+20;
    end;
end;

function TSDLMouse.GetControl: TSDLComponent;
var HotSpot: TPoint;
    TheEnd : Boolean;
  procedure ControlCollision(Control: TSDLImageObject);
  var i : Integer;
  begin
    if not (Control is TSDLComponent) then Exit;
    with Control as TSDLComponent do
      if not(Enabled and Visible or FDesigning) then Exit;

    if PointInRect(HotSpot,Control.BoundsRect)then
    begin
      if Control.TestCollision then
      begin
        Result := TSDLComponent(Control);
        TheEnd := False;

        i := Result.Count;
        if i>0 then
          repeat
            Dec(i);
            ControlCollision(Result.Objects[i]);
          until TheEnd or(i<1);
      end;
      TheEnd := True;
    end;
  end;

var i: Integer;
begin
  Result := nil;
  HotSpot := SDLPoint(X+HotSpotX,Y+HotSpotY);
  TheEnd := False;

  i := Gui.Count;
  repeat
    Dec(i);
    ControlCollision(Gui.Objects[i]);
  until TheEnd or(i<1);
end;

procedure TSDLMouse.MouseDown(Button: Integer; Modifier: TSDLMod; AX, AY: Integer);
var tc: TSDLComponent;
begin
  if Assigned(FDownControl) then Exit;
  tc := Gui.FPopupMenu;
  if Assigned(FOverControl) then
  begin
    if Button=1 then FDownControl := FOverControl;
    FOverControl.MouseDown(Button,Modifier,AX,AY);
  end;
  Gui.PopupMenu := tc;
  if Assigned(Gui.FPopupMenu)then
    Gui.FPopupMenu.HidePopupMenu;
end;

procedure TSDLMouse.MouseMove(Modifier: TSDLMod; AX, AY: Integer);
var tc: TSDLComponent;
    t: Cardinal;
begin
  t := SDL_GetTicks;
  if t>500 then
    if (X<>AX)or(Y<>AY)then FOldClickTime:=t-501;
  X := AX;
  Y := AY;

  if Assigned(FDownControl) then
  begin
    FDownControl.MouseMove(Modifier,AX,AY);
    Exit;
  end;

  if Visible then
    tc := GetControl
  else
    tc := nil;

  if FOverControl<>tc then
  begin
    if Assigned(FOverControl) then
      FOverControl.MouseMove(Modifier,AX,AY);

    Gui.FHint := '';
    if Assigned(Tc) then
    begin
      FOverControl := tc;
      FOldTime := t;             //Used for hints
      if not FConstCursor then
        SetCursor(tc.Cursor)
      else
        SetCursor(DefaultCursor);
    end
    else
    begin
      FOverControl := nil;
      SetCursor(DefaultCursor);
    end;
  end;

  if Assigned(FOverControl) then
    FOverControl.MouseMove(Modifier,AX,AY);
end;

procedure TSDLMouse.MouseUp(Button: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  if Assigned(FDownControl) then
  begin
    if Button=1 then   //if right pressed while left is also pressed
      FDownControl.MouseUp(Button,Modifier,AX,AY);
  end
  else if Assigned(FOverControl) then
    FOverControl.MouseUp(Button,Modifier,AX,AY);
end;

procedure TSDLMouse.MouseWheel(Dir: Integer; Modifier: TSDLMod; AX,
  AY: Integer);
begin
  if Assigned(Gui.FPopupMenu) then
    Gui.FPopupMenu.MouseWheel(Dir,Modifier,AX,AY)
  else if Assigned(FOverControl) then
    FOverControl.MouseWheel(Dir,Modifier,AX,AY);
end;

procedure TSDLMouse.SetConstCursor(const Value: Boolean);
begin
  if FConstCursor=Value then Exit;

  FConstCursor := Value;
  if FConstCursor then
    SetCursor(DefaultCursor)
  else
    if Assigned(FOverControl)then
      SetCursor(FOverControl.Cursor);
end;

procedure TSDLMouse.SetCursor(const Value: TSDLCursor);
begin
  if not Assigned(Value.Image) then Exit;

  SetImage(Value.Image);
  HotSpotX := Value.HotSpotX;
  HotSpotY := Value.HotSpotY;
end;

procedure TSDLMouse.SetImage(Value: TSDLImage);
begin
  inherited;
  if not Assigned(DefaultCursor.Image)then
  begin
    FDefaultCursor.Image := Value;
    SDL_ShowCursor(SDL_DISABLE);
  end;
end;

{ TSDLGui }

constructor TSDLGui.Create(AParent: TSDLObject);
begin
  inherited;
  DragEnabled := True;
  FDeadList := TList.Create;
  FSDLMouse := TSDLMouse.Create(Self);
  FOldTime := SDL_GetTicks;
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,SDL_DEFAULT_REPEAT_INTERVAL);
  HintColor := $FFFF80;
  ShowHintAfter := 500;
  ShowHintFor := 2000;
  ShowHints := True;
  Width := SDLScreen.SurfaceRect.w;
  Height := SDLScreen.SurfaceRect.h;
end;

procedure TSDLGui.Kill;
var i : Integer;
begin
  for i := 0 to FDeadList.Count-1 do
    TSDLObject(FDeadList.Items[0]).Free;
end;

destructor TSDLGui.Destroy;
begin
  inherited;
  FDeadList.Free;
  SDL_EnableUnicode(0);
  SDL_EnableKeyRepeat(0,0);
end;

procedure TSDLGui.KeyDown(var Key: Word; Modifier: TSDLMod);
var Tab : Integer;
    t: Boolean;
    c: Char;
begin
  if Key=SDLK_KP_ENTER then Key := SDLK_RETURN;
  Tab := -1;
  if Assigned(FActiveControl) then
  begin
    FActiveControl.KeyDown(Key,Modifier);
    if Key=0 then Exit;
    if Assigned(FActiveControl) then
      Tab := FActiveControl.FTabOrder;
  end;

  if ControlCount=0 then Exit;

  case Key of
  SDLK_TAB :
    if KMOD_Shift and Modifier>0 then
    begin
      Dec(Tab);
      if Tab<0 then Tab := ControlCount-1;
    end
    else
      Tab := (Tab+1) mod ControlCount;
  SDLK_RIGHT,SDLK_DOWN: Tab := (Tab+1)mod ControlCount;
  SDLK_LEFT,SDLK_UP   :
    begin
      Dec(Tab);
      if Tab<0 then Tab := ControlCount-1;
    end
  else
    Tab := -1;
  end;

  t := Tab<>0;
  if Tab>=0 then
    while (Tab<ControlCount) and not Controls[Tab].SetFocus do
    begin
      Inc(Tab);
      if t and (Tab=ControlCount) then
      begin
        Tab := 0;
        t := False
      end;
    end
  else if (Key>=SDLK_KP0)and(Key<=SDLK_KP9) then
    if Assigned(FActiveControl)and not FActiveControl.FDesigning then
    begin
      c := Char(Key-SDLK_KP0+Ord('0'));
      FActiveControl.KeyPress(c);
    end;
end;

procedure TSDLGui.ProcessEvent(const Event: TSDL_Event);
var Modifier: TSDLMod;
    Button: Cardinal;
    s : Word;
    c : Char;
begin
  if Event.type_ in [4,5,6] then //Mouse events
    Modifier := SDL_GetModState
  else
  begin
    Modifier := 0;
    if Event.key.keysym.sym>$FFFF then Exit;
    s := Event.key.keysym.sym;
  end;
  case Event.type_ of
  SDL_MOUSEMOTION:
    with Event.motion do
    begin
      if state>0 then
      begin
        s := state;
        if s=4 then s:=3;
        Button := 1 shl (s+1);
        Modifier := Modifier or Button;
      end;
      FSDLMouse.MouseMove(Modifier,x,y);
    end;
  SDL_MOUSEBUTTONDOWN:
    with Event.button, FSDLMouse do
      if button<4 then
        MouseDown(button,Modifier,x,y)
      else
        if button=5 then
          MouseWheel(1,Modifier,x,y)
        else MouseWheel(-1,Modifier,x,y);
  SDL_MOUSEBUTTONUP:
    with Event.button do
    begin
      FSDLMouse.MouseUp(button,Modifier,x,y);
      if button=1 then FSDLMouse.Click(x,y);
    end;
  SDL_KEYDOWN:
    with Event.key.keysym do
    begin
      KeyDown(s,modifier);
      c := Char(unicode and $FF);
      if Assigned(FActiveControl)and not FActiveControl.FDesigning and
         (unicode<$80)and not(unicode in [0,9,27]) then  //Esc and Backspace
        FActiveControl.KeyPress(c);
    end;
  SDL_KEYUP:
    with Event.key.keysym do
      if Assigned(FActiveControl)and not FActiveControl.FDesigning then
        FActiveControl.KeyUp(s,Modifier);
  end;
end;

procedure TSDLGui.Update;
var nt: Cardinal;
    size: TSDL_Rect;
begin
  Draw;
  if Assigned(HintFont)and(FHint<>'')and ShowHints then
  begin
    size := HintFont.TextExtent(FHint);
    InflateRect(size,4,0);
    OffsetRect(size,FHintCoord.x,FHintCoord.y);
    if size.x+size.w>=SDLScreen.SurfaceRect.w then
      size.x := SDLScreen.SurfaceRect.w-size.w-1;
    if size.y+size.h>=SDLScreen.SurfaceRect.h then
      size.y := SDLScreen.SurfaceRect.h-size.h-1;
    SDLScreen.FillRect(SDLScreen.Surface,size,HintColor);
    SDLScreen.DrawFrame(SDLScreen.Surface,size,0);
    HintFont.TextOut(SDLScreen.Surface,size.x+2,size.y,FHint);
  end;
  nt := SDL_GetTicks;
  Animate(nt-FOldTime);
  FOldTime := nt;
  Kill;
end;

procedure TSDLGui.SendQuitSignal;
var qe: TSDL_Event;
begin
  FillChar(qe,SizeOf(qe),0);
  qe.type_ := SDL_QUITEV;
  SDL_PushEvent(@qe);
end;

initialization

finalization
  if Assigned(RegClasses) then
    RegClasses.Free;

end.
