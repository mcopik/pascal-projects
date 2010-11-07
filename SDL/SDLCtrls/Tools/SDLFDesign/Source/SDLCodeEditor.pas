unit SDLCodeEditor;

interface

uses sdl, sdlgui, SDLForm, SDLDraw, SDLMemo, Classes, clRTTI;

{
  EventInfo[i]:
    String: Unique procedure name = Property Name
            \_Name of procedure you assign to an event
    Object: Instance
}

type
  TSDLCodeEditor = class(TSDLForm)
  private
    FMemo: TSDLMemo;
  public
    EventInfo: TStringList;
    AddProcImplementation: Boolean;
    constructor Create(AParent: TSDLObject); override;
    destructor Destroy; override;
    function AddControlDeclaration(AControl: TSDLComponent): Integer;
    function AddProcDeclaration(AProp: TrtProperty; const Value: string;
             P: Integer): Integer;
    property Memo: TSDLMemo read FMemo;
  end;

function IndexOfString(sl: TStringList;const S: string;si: Integer): Integer;

var
  CodeEditor: TSDLCodeEditor;

implementation

uses SysUtils, TypInfo, SDLDialogs;

function IndexOfString(sl: TStringList;const S: string;si: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  if si>=sl.Count then Exit;

  for i := si to sl.Count-1 do
    if AnsiPos(S,sl[i])>0 then
    begin
      Result := i;
      Exit;
    end;
end;

{ TSDLCodeEditor }

function TSDLCodeEditor.AddControlDeclaration(AControl: TSDLComponent): Integer;
var cet: TStringList;
    p: Integer;
    t: string;
begin
  Result := -1;
  cet := TStringList.Create;
  cet.Text := Memo.Text;
  t := 'T'+TSDLComponent(Gui.DesignForm).Name+' = class(TSDLForm)';
  p := IndexOfString(cet,t,0)+1;
  if p<1 then
  begin
    cet.Free;
    SDLMessageDlg(nil,Gui,[dbOK],'Error!','Can not find "'+t+'" in unit.');
    Exit;
  end;
  cet.Insert(p,'    '+AControl.Name+': '+AControl.ClassName+';');
  p := IndexOfString(cet,'uses',0);
  t := GetTypeData(PTypeInfo(AControl.ClassInfo)).UnitName;
  if (AnsiPos(t,cet[p]+cet[p+1])<=0) then
    cet[p] := 'uses '+t+', '+Copy(cet[p],6,Length(cet[p]));
  Memo.Text := cet.Text;
  cet.Free;
  Result := 1;
end;

function TSDLCodeEditor.AddProcDeclaration(AProp: TrtProperty;
  const Value: string; P: Integer): Integer;
var t: string;
    cet: TStringList;
    i: Integer;
begin
  t := Value+'='+AProp.Name;
  Result := EventInfo.AddObject(t,AProp.Instance);
  cet := TStringList.Create;
  cet.Text := Memo.Text;
  if p<0 then p := cet.IndexOf('  private');
  if p<0 then p := cet.IndexOf('  public');
  if p<0 then p := cet.IndexOf('  end;');
  if p>0 then
  begin
    t :=  '    procedure '+Value+'(';
    with AProp.MethodData do
      for i := 0 to ParamCount-1 do
      begin
        if pfVar in ParamList[i].Flags then
          t := t+'var '
        else if pfConst in ParamList[i].Flags then
          t := t+'const ';
        t := t+ParamList[i].ParamName;
        if (i<ParamCount-1)and(ParamList[i+1].TypeName=ParamList[i].TypeName)then
          t := t+', '
        else
          t := t+': '+ParamList[i].TypeName+'; ';
      end;
    t[Length(t)-1] := ')';
    t[Length(t)] := ';';
    cet.Insert(p,t);
    p := cet.IndexOf('end.');
    if (p>0)and AddProcImplementation then
    begin
    Delete(t,1,Ansipos('(',t)-1);
    for i := 0 to Gui.ControlCount-1 do
      if Gui.Controls[i].ClassName='TDesignForm' then
      begin
        t := 'procedure T'+Gui.Controls[i].Name+'.'+Value+t;
        Break;
      end;
    cet.Insert(p,t);
    cet.Insert(p+1,'begin');
    cet.Insert(p+2,'  ');
    cet.Insert(p+3,'end;');
    cet.Insert(p+4,'');
    end;
    Memo.Text := cet.Text;
  end;
  cet.Free;
end;

constructor TSDLCodeEditor.Create(AParent: TSDLObject);
begin
  inherited;
  X := 225;
  Y := 90;
  Width := SDLScreen.SurfaceRect.w-240;
  Height := 470;
  Caption := 'Code viewer';
  FMemo := TSDLMemo.Create(Self);
  with FMemo do
  begin
    X := 2;
    Y := 21;
    Height := 445;
    Width := Self.Width-4;
    ReadOnly := True;
    Color := $EEEEEE;
    FocusColor := $EEEEEE;
  end;
  EventInfo := TStringList.Create;
end;

destructor TSDLCodeEditor.Destroy;
begin
  EventInfo.Free;
  inherited;
end;

end.
