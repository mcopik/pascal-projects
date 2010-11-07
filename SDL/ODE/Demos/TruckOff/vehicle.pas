unit vehicle;
{******************************************************************}
{                                                                  }
{  $Id: vehicle.pas,v 1.5 2004/04/07 22:07:29 savage Exp $         }
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the Ed Jones' ODE TruckOff Demo              }
{                                                                  }
{ Portions created by Ed Jones <ed.jones@oracle.com>,  are         }
{ Copyright(C) 2002-2004 Ed Jones.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : vehicle.pas                        }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright(C) 2002-2004 Dominique Louis.                          }
{                                                                  }
{  $Log: vehicle.pas,v $
{  Revision 1.5  2004/04/07 22:07:29  savage
{  Changes required to accommodate odeimport.pas changes.
{
{  Revision 1.4  2004/03/31 09:57:24  savage
{  Change wheel colour and upward force when F is pressed.
{
{  Revision 1.3  2004/03/28 14:51:58  savage
{  Fixes
{
{  Revision 1.2  2004/03/10 11:15:01  savage
{  Added more force to be able to flip the car, tried to fix the DrawText, but to no avail.
{
{  Revision 1.1  2004/03/08 21:29:15  savage
{  ODE TruckOff Demo
{                                                                  }
{                                                                  }
{******************************************************************}

interface

uses
  gl,
  glu,
  odeimport;

type
  TDriveTrain = class
  private
    redline, rev_limit, steering_lock : GLFloat;
    throttle_response, brake_response, steer_speed : GLFloat;
    revs, steering_angle : GLFloat;
  protected
    //
  public
    constructor Create; overload;
    constructor Create(
      fRevLimit : GLFloat;
      fSteeringLock : GLFloat;
      fThrottleResponse : GLFloat;
      fBrakeResponse : GLFloat;
      fSteerSpeed : GLFloat
      ); overload;
    destructor Destroy; override;
    procedure HandleBinaryControls(
      bThrottle : boolean;
      bBrake : boolean;
      bHandBrake : boolean;
      bLeft : boolean;
      bRight : boolean
      );
    procedure SetParameters(
      fRedLine : GLFloat;
      fRevLimit : GLFloat;
      fSteeringLock : GLFloat;
      fThrottleResponse : GLFloat;
      fBrakeResponse : GLFloat;
      fSteerSpeed : GLFloat
      );
    function GetRevs : GLFloat;
    function GetRevLimit : GLFloat;
    function GetRedLine : GLFloat;
    function GetSteer : GLFloat;
  published

  end;

  TWheel = class
  private
    width, radius, brake, power, mass, steer, erp, cfm : GLFloat;
    red, green, blue : GLFloat;
    handbrake : boolean;
    vehicle : pointer;
    world : PdxWorld;
    body : PdxBody;
    joint : TdJointID;
    sphere : PdxGeom;
    procedure DoDefaultInit;
  protected
    //
  public
    constructor Create( idWorld : PdxWorld;
      fWidth : GLFloat;
      fRadius : GLFloat;
      fBrake : GLFloat;
      fPower : GLFloat;
      fMass : GLFloat;
      fSteer : GLFloat;
      fERP : GLFloat;
      fCFM : GLFloat;
      bHandbrake : boolean ); overload;
    constructor Create( const w : TWheel ); overload;
    destructor Destroy; override;
    procedure Render( var qobj : GLUQuadricObj; bColor : boolean );
    procedure SetColor( fRed, fGreen, fBlue : GLFloat );
    procedure JoinTo( vBody : PdxBody; x, y, z : GLFloat );
    function GetGeomID : PdxGeom;
    function GetBodyID : PdxBody;
    function GetJointID : TdJointID;
    procedure SetGroupRef( v : Pointer );
    function GetPower : GLFloat;
    function GetSteer : GLFloat;
  published

  end;

  TVehicle = class
  private
    width, height, slength, mass : GLFloat;
    red, green, blue : GLFloat;
    gear : GLFloat;
    fw_x, fw_y, fw_z : GLFloat;
    fwgl : TdVector3;
    engine : TDriveTrain;
    wheels : array of TWheel;
    world : PdxWorld;
    space : PdxSpace;
    body : PdxBody;
    box: PdxGeom;
    group : PdxSpace;
    fifth_wheel : TdJointID;
  protected

  public
    constructor Create( idWorld : PdxWorld; idSpace : PdxSpace; fWidth, fHeight
      : GLFloat; fLength : GLFloat; fMass : GLFloat );
    destructor Destroy; override;
    procedure AddWheel( wheelW : TWheel; x, y, z : GLFloat );
    procedure SetPosition( x, y, z : GLFloat );
    procedure GetPosition( var pos : PdVector3 );
    procedure Render( qobj : PGLUquadricObj; bColor : boolean );
    procedure SetColor( fRed, fGreen, fBlue : GLFloat );
    function GetGroup : PdxSpace;
    function GetBodyID : PdxBody;
    procedure HandleBinaryControls( bThrottle, bBrake, bHandBrake, bLeft,
      bRight, bReverse : boolean );
    function GetRevsFraction : GLFloat;
    function GetRedLineFraction : GLFloat;
    procedure CreateFifthWheel( x, y, z : GLFloat );
    procedure SetHookPoint( x, y, z : GLFloat );
    procedure HookUp( var trailer : TVehicle );
    function GetFifthWheelID : TdJointID;
    procedure GetFifthWheelPosition( var f : PdVector3 );
    function GetGearChar : string;
    function IsTowing : boolean;
    procedure UnHook;
  published

  end;

implementation

uses
  gfxutils,
  sysutils,
  logger;

{ TDriveTrain }

constructor TDriveTrain.Create;
begin
  inherited;
  redline := 4.5;
  rev_limit := 5.0;
  steering_lock := 0.75;
  throttle_response := 0.05;
  brake_response := 0.1;
  steer_speed := 0.05;
  revs := 0.0;
  steering_angle := 0.0;
end;

constructor TDriveTrain.Create( fRevLimit, fSteeringLock, fThrottleResponse,
  fBrakeResponse, fSteerSpeed : GLFloat );
begin
  inherited Create;
  redline := fRevLimit;
  rev_limit := fSteeringLock;
  steering_lock := 0.75;
  throttle_response := fThrottleResponse;
  brake_response := fBrakeResponse;
  steer_speed := fSteerSpeed;
  revs := 0.0;
  steering_angle := 0.0;
end;

destructor TDriveTrain.Destroy;
begin

  inherited;
end;

function TDriveTrain.GetRedLine : GLFloat;
begin
  result := redline;
end;

function TDriveTrain.GetRevLimit : GLFloat;
begin
  result := rev_limit;
end;

function TDriveTrain.GetRevs : GLFloat;
begin
  result := revs;
end;

function TDriveTrain.GetSteer : GLFloat;
begin
  result := steering_angle;
end;

procedure TDriveTrain.HandleBinaryControls( bThrottle, bBrake, bHandBrake,
  bLeft, bRight : boolean );
begin
  if ( bThrottle ) then
  begin
    if ( revs > rev_limit ) then
      revs := rev_limit - ( throttle_response * 5.0 )
    else
      revs := revs + throttle_response;
  end;

  if ( bBrake ) then
  begin
    revs := revs - brake_response;
    if ( revs < 0.0 ) then
      revs := 0.0;
  end;

  if ( bHandBrake ) then
  begin
    revs := 0.0;
  end;

  if ( ( not bRight ) and ( not bLeft ) ) then
  begin
    if ( steering_angle > 0.0 ) then
    begin
      steering_angle := steering_angle - steer_speed;
      if ( steering_angle < 0.0 ) then
        steering_angle := 0.0;
    end
    else
    begin
      steering_angle := steering_angle + steer_speed;
      if ( steering_angle > 0.0 ) then
        steering_angle := 0.0;
    end;
  end
  else
  begin
    if ( bRight ) then
    begin
      steering_angle := steering_angle + steer_speed;
      if ( steering_angle > steering_lock ) then
        steering_angle := steering_lock;
    end;

    if ( bLeft ) then
    begin
      steering_angle := steering_angle - steer_speed;
      if ( steering_angle < -steering_lock ) then
        steering_angle := -steering_lock;
    end;
  end;
end;

procedure TDriveTrain.SetParameters( fRedLine, fRevLimit, fSteeringLock,
  fThrottleResponse, fBrakeResponse, fSteerSpeed : GLFloat );
begin
  redline := fRedLine;
  rev_limit := fRevLimit;
  steering_lock := fSteeringLock;
  throttle_response := fThrottleResponse;
  brake_response := fBrakeResponse;
  steer_speed := fSteerSpeed;
end;

{ TWheel }

constructor TWheel.Create( idWorld : PdxWorld; fWidth, fRadius, fBrake,
  fPower, fMass, fSteer, fERP, fCFM : GLFloat; bHandbrake : boolean );
begin
  inherited Create;
  world := idWorld;

  width := fWidth;
  radius := fRadius;
  brake := fBrake;
  power := fPower;
  mass := fMass;
  steer := fSteer;
  erp := fERP;
  cfm := fCFM;
  handbrake := bHandbrake;

  red := 0.0;
  green := 0.0;
  blue := 0.0;

  DoDefaultInit;
end;

constructor TWheel.Create( const w : TWheel );
var
  b0, b1 : PdxBody;
  a, b : PdVector3;
begin
  inherited Create;
  world := w.world;

  width := w.width;
  radius := w.radius;
  brake := w.brake;
  power := w.power;
  mass := w.mass;
  steer := w.steer;
  erp := w.erp;
  cfm := w.cfm;
  handbrake := w.handbrake;
  vehicle := w.vehicle;

  red := w.red;
  green := w.green;
  blue := w.blue;

  DoDefaultInit;

  b0 := dJointGetBody( w.joint, 0 );
  b1 := dJointGetBody( w.joint, 1 );

  if ( b1 <> nil ) then
  begin
    a := dBodyGetPosition( b0 );
    b := dBodyGetPosition( b1 );

    if ( vehicle <> nil ) then
    begin
      dSpaceAdd( TVehicle( vehicle ).GetGroup, GetGeomID );
    end;

    JoinTo( b0, b[ 0 ] - a[ 0 ], b[ 1 ] - a[ 1 ], b[ 2 ] - a[ 2 ] );
  end;
end;

destructor TWheel.Destroy;
begin
  if ( vehicle <> nil ) then
  begin
    dSpaceRemove( TVehicle( vehicle ).GetGroup, GetGeomID );
  end;

  dJointDestroy( joint );
  dGeomDestroy( sphere );

  dBodyDestroy( body );
  inherited;
end;

procedure TWheel.DoDefaultInit;
var
  m : TdMass;
  q : TdQuaternion;
begin
  body := dBodyCreate( world );

  dQFromAxisAndAngle( q, 0, 0, 1, PI * 0.5 );
  dBodySetQuaternion( body, q );
  dMassSetSphere( m, 1, radius );
  dMassAdjust( m, mass );
  dBodySetMass( body, @m );

  sphere := dCreateSphere( nil, radius );

  dGeomSetBody( sphere, body );
end;

function TWheel.GetBodyID : PdxBody;
begin
  result := body;
end;

function TWheel.GetGeomID : PdxGeom;
begin
  result := sphere;
end;

function TWheel.GetJointID : TdJointID;
begin
  result := joint;
end;

function TWheel.GetPower : GLFloat;
begin
  result := power;
end;

function TWheel.GetSteer : GLFloat;
begin
  result := steer;
end;

procedure TWheel.JoinTo( vBody : PdxBody; x, y, z : GLFloat );
begin
  dBodySetPosition( body, x, y, z );

  joint := dJointCreateHinge2( world, 0 );

  dJointAttach( joint, vBody, body );
  dJointSetHinge2Anchor( joint, x, y, z );

  dJointSetHinge2Axis1( joint, 0, 1, 0 );
  dJointSetHinge2Axis2( joint, 0, 0, 1 );

  // set joint suspension
  dJointSetHinge2Param( joint, dParamSuspensionERP, erp );
  dJointSetHinge2Param( joint, dParamSuspensionCFM, cfm );

  // set stops to make sure wheels always stay in alignment
  if ( steer = 0.0 ) then
  begin
    dJointSetHinge2Param( joint, dParamLoStop, 0 );
    dJointSetHinge2Param( joint, dParamHiStop, 0 );
  end;
end;

procedure TWheel.Render( var qobj : GLUquadricObj; bColor : boolean );
begin
  if ( bColor ) then
    guSetColor( red, green, blue );
  guSetTransform( dBodyGetPosition( body ), dBodyGetRotation( body ) );
  guDrawCylinder( qobj, width, radius );
  guUnsetTransform;
end;

procedure TWheel.SetColor( fRed, fGreen, fBlue : GLFloat );
begin
  red := fRed;
  green := fGreen;
  blue := fBlue;
end;

procedure TWheel.SetGroupRef( v : Pointer );
begin
  vehicle := v;
end;

{ TVehicle }

procedure TVehicle.AddWheel( wheelW : TWheel; x, y, z : GLFloat );
begin
  wheelW.JoinTo( body, x, y, z );
  wheelW.SetGroupRef( self );
  dSpaceAdd( group, wheelW.GetGeomID );
  SetLength( wheels, Length( wheels ) + 1 );
  wheels[ High( wheels ) ] := wheelW;
  //wheels.push_back( wheelW );
end;

constructor TVehicle.Create( idWorld : PdxWorld; idSpace : PdxSpace; fWidth,
  fHeight, fLength, fMass : GLFloat );
var
  m : TdMass;
begin
  inherited Create;

  engine := TDriveTrain.Create;
  space := idSpace;
  world := idWorld;

  width := fWidth;
  height := fHeight;
  slength := fLength;
  mass := fMass;
  gear := 1.0;

  red := 1.0;
  green := 1.0;
  blue := 1.0;

  body := dBodyCreate( world );

  fifth_wheel := 0;

  dMassSetBox( m, 1, slength, height, width );
  dMassAdjust( m, mass );

  dBodySetMass( body, @m );

  box := dCreateBox( nil, slength, height, width );

  dGeomSetBody( box, body );

  group := dSimpleSpaceCreate( space );
  dSpaceAdd( group, box );
end;

procedure TVehicle.CreateFifthWheel( x, y, z : GLFloat );
begin
  SetHookPoint( x, y, z );

  fifth_wheel := dJointCreateBall( world, 0 );
end;

destructor TVehicle.Destroy;
var
  i : Cardinal;
begin
  engine.Free;

  //wheels.clear;
  for i := Low( wheels ) to High( wheels ) do
  begin
    wheels[ i ].Free;
  end;
  SetLength( wheels, 0 );

  if ( fifth_wheel <> 0 ) then
    dJointDestroy( fifth_wheel );

  dGeomDestroy( box );
  dSpaceDestroy( group );

  dBodyDestroy( body );
  inherited;
end;

function TVehicle.GetBodyID : PdxBody;
begin
  result := body;
end;

function TVehicle.GetFifthWheelID : TdJointID;
begin
  result := fifth_wheel;
end;

procedure TVehicle.GetFifthWheelPosition( var f : PdVector3 );
begin
  f := @fwgl;
end;

function TVehicle.GetGearChar : string;
begin
  if ( gear < 0.0 ) then
    result := 'R'
  else if ( gear > 1.0 ) then
    result := '2'
  else
    result := '1';
end;

function TVehicle.GetGroup : PdxSpace;
begin
  result := group;
end;

procedure TVehicle.GetPosition( var pos : PdVector3 );
begin
  pos := dGeomGetPosition( box );
end;

function TVehicle.GetRedLineFraction : GLFloat;
begin
  result := engine.GetRevs / engine.GetRedLine;
end;

function TVehicle.GetRevsFraction : GLFloat;
begin
  result := engine.GetRevs / engine.GetRevLimit;
end;

procedure TVehicle.HandleBinaryControls( bThrottle, bBrake, bHandBrake,
  bLeft, bRight, bReverse : boolean );
var
  revs, steer, p, s : GLFloat;
  i : Cardinal;
  j : TdJointID;
  v : GLFloat;
begin
  engine.HandleBinaryControls( bThrottle, bBrake, bHandBrake, bLeft, bRight );

  revs := engine.GetRevs;
  steer := engine.GetSteer;

  if ( bReverse ) then
  begin
    if ( revs = 0.0 ) then
    begin
      if ( gear > 0.0 ) then
        gear := -0.5
      else
        gear := 1.0;
    end
    else
    begin
      if ( gear > 1.0 ) then
        gear := 1.0
      else if ( gear > 0.0 ) then
        gear := 1.5;
    end;
  end;

  for i := 0 to Length( wheels ) - 1 do
  begin
    p := wheels[ i ].GetPower;
    s := wheels[ i ].GetSteer;
    j := wheels[ i ].GetJointID;

    if ( p <> 0.0 ) then
    begin
      dJointSetHinge2Param( j, dParamVel2, ( p * revs ) * gear );
      dJointSetHinge2Param( j, dParamFMax2, 0.1 );
    end;

    if ( s <> 0.0 ) then
    begin
      v := ( s * steer ) - dJointGetHinge2Angle1( j );
      if ( v > 0.1 ) then
        v := 0.1;
      if ( v < -0.1 ) then
        v := -0.1;
      v := v * 10.0;
      dJointSetHinge2Param( j, dParamVel, v );
      dJointSetHinge2Param( j, dParamFMax, 0.2 );
      dJointSetHinge2Param( j, dParamLoStop, -0.75 );
      dJointSetHinge2Param( j, dParamHiStop, 0.75 );
      dJointSetHinge2Param( j, dParamFudgeFactor, 0.1 );
    end;
  end;
end;

procedure TVehicle.HookUp( var trailer : TVehicle );
var
  wpos : PdVector3;
  pos : PdVector3;
begin
  if ( ( fifth_wheel <> 0 ) and ( trailer.GetFifthWheelID = 0 ) ) then
  begin
    trailer.GetFifthWheelPosition( wpos );
    trailer.GetPosition( pos );

    trailer.SetPosition(
      pos[ 0 ] - ( wpos[ 0 ] - fwgl[ 0 ] ),
      pos[ 1 ] - ( wpos[ 1 ] - fwgl[ 1 ] ),
      pos[ 2 ] - ( wpos[ 2 ] - fwgl[ 2 ] )
      );

    dJointAttach( fifth_wheel, GetBodyID, trailer.GetBodyID );
    //dJointSetBallAnchor( fifth_wheel, fw_x, fw_y, fw_z );
    dJointSetBallAnchor( fifth_wheel, fwgl[ 0 ], fwgl[ 1 ], fwgl[ 2 ] );
  end;
end;

function TVehicle.IsTowing : boolean;
begin
  if ( dJointGetBody( fifth_wheel, 1 ) = nil ) then
    result := false
  else
    result := true;
end;

procedure TVehicle.Render( qobj : PGLUquadricObj; bColor : boolean );
var
  ss : TdVector3;
  i : integer;
begin
  if ( bColor ) then
    glColor3f( red, green, blue );
  dGeomBoxGetLengths( box, ss );

  guSetTransform( dGeomGetPosition( box ), dGeomGetRotation( box ) );
  guDrawBox( ss );
  dBodyGetRelPointPos( GetBodyID, fw_x, fw_y, fw_z, fwgl );
  guUnsetTransform;

  for i := 0 to Length( wheels ) - 1 do
  begin
    wheels[ i ].Render( qobj^, bColor );
  end;
end;

procedure TVehicle.SetColor( fRed, fGreen, fBlue : GLFloat );
begin
  red := fRed;
  green := fGreen;
  blue := fBlue;
end;

procedure TVehicle.SetHookPoint( x, y, z : GLFloat );
begin
  if ( fifth_wheel <> 0 ) then
    dJointDestroy( fifth_wheel );
  fifth_wheel := 0;

  fw_x := x;
  fw_y := y;
  fw_z := z;

  {guSetTransform(dGeomGetPosition(box),dGeomGetRotation(box));
  guTransformPoint(fw_x,fw_y,fw_z,fwgl);
  guUnsetTransform;}

  dBodyGetRelPointPos( GetBodyID, fw_x, fw_y, fw_z, fwgl );
end;

procedure TVehicle.SetPosition( x, y, z : GLFloat );
var
  a, b : PdVector3;
  d : PdxBody;
  i : Cardinal;
begin
  a := dBodyGetPosition( body );
  for i := 0 to Length( wheels ) - 1 do
  begin
    d := wheels[ i ].GetBodyID;
    b := dBodyGetPosition( d );
    dBodySetPosition( d, b[ 0 ] + ( x - a[ 0 ] ), b[ 1 ] + ( y - a[ 1 ] ), b[ 2 ]
      + ( z - a[ 2 ] ) );
  end;
  dBodySetPosition( body, x, y, z );
end;

procedure TVehicle.UnHook;
begin
  if ( IsTowing ) then
    dJointAttach( fifth_wheel, nil, nil );
end;

end.

 