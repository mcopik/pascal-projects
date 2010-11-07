// =============================================================================
//
//  SDLNewtonVehicleDemo.pas
//   Simple demo to show off newton's specialized vehicle container that can
//   be used to easily create vehicles.
//
// =============================================================================
//
//   Copyright © 2005/06 by Sascha Willems (www.delphigl.de)
//   Newton Game Dynamics © 2003-2006 by Julio Jerez (www.newtondynamics.com)
//
// =============================================================================
//
//   Contents of this file are subject to the Mozilla Public Licencse 1.1
//   which can be obtained here : http://opensource.org/licenses/mozilla1.1.php
//
// =============================================================================

program SDLNewtonVehicleDemo;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$APPTYPE GUI}
 {$UNITPATH ../common}
{$ENDIF}

uses
  sdl,
  newton,
  gl,
  glu,
  glext,
  glPrimitives in '..\common\glPrimitives.pas',
  glMatrixHelper in '..\common\glMatrixHelper.pas';

type
  TNewtonVehicle = class;

  // TNewtonTire ===============================================================
  //  Baseclass for vehicle tires
  TNewtonTire = class
    ID        : Integer;
    Torque    : Single;
    Brake     : Single;
    GraphBody : TNewtonPrimitive;
    constructor Create(pVehicle : TNewtonVehicle; pMatrix : TMatrix4f; pWidth, pRadius : Single);
    procedure SetTirePhysics(const Vehicle : PNewtonJoint; TireID : Integer); virtual;
   end;

  // TNewtonTire ===============================================================
  //  Tire that can steer
  TNewtonSteeringTire = class(TNewtonTire)
    SteerAngle : Single;
    procedure SetTirePhysics(const Vehicle : PNewtonJoint; TireID : Integer); override;
   end;

  // TNewtonVehicle ============================================================
  TNewtonVehicle = class
    Tire      : array of TNewtonTire;
    Joint     : PNewtonJoint;
    Body      : PNewtonBody;
    GraphBody : TNewtonPrimitive;
    procedure AddTire(pMatrix : TMatrix4f; pWidth, pRadius : Single);
    procedure AddSteeringTire(pMatrix : TMatrix4f; pWidth, pRadius : Single);
    procedure SetSteeringAngle(pSteeringAngle : Single);
    procedure SetTorque(pTorque : Single);
    procedure SetBrake(pBrake : Single);
    procedure CreateCar;
    procedure Flip;
    procedure ProcessInput;
    constructor Create(pMatrix : TMatrix4f);
   end;

const
 VehicleMass =  100;
 Gravity     = -9.8;

var
  // Newton
  NewtonWorld   : PNewtonWorld;
  Vehicle       : TNewtonVehicle;

  // Misc
  ShowDebug     : Boolean;
  SceneRotation : TVector3f;
  SceneZoom     : Single = -35;
  KeyState      : TKeyStateArr;

  // Physics timing
  AccTimeSlice  : Single;
  TimeLastFrame : Cardinal;

  // SDL
  Surface       : PSDL_Surface;

// *****************************************************************************
//  Newton callbacks
// *****************************************************************************

// =============================================================================
//  SetTransform
// =============================================================================
//  Global callback for transformation of the vehicle
// =============================================================================
procedure PSetTransform(const Body : PNewtonBody; const Matrix : PFloat); cdecl;
var
 TmpVehicle : TNewtonVehicle;
begin
// Get vehicle from the body's user data
TmpVehicle := TNewtonVehicle(NewtonBodyGetUserData(Body));
// Update matrix of chassis' graphical body
Move(Matrix^, TmpVehicle.GraphBody.Matrix[0,0], SizeOf(TMatrix4f));
end;

// =============================================================================
//  PTireUpdate
// =============================================================================
//  Global callback for updating the vehicle's tires
// =============================================================================
procedure PTireUpdate(const Vehicle : PNewtonJoint); cdecl;
var
 TireID  : Integer;
 TmpTire : TNewtonTire;
begin
// Iterate through all tires and update them
TireID := Integer(NewtonVehicleGetFirstTireID(Vehicle));
if TireID <> 0 then
 repeat
 TmpTire := NewtonVehicleGetTireUserData(Vehicle, Pointer(TireID));
 TmpTire.SetTirePhysics(Vehicle, TmpTire.ID);
 TireID  := Integer(NewtonVehicleGetNextTireID(Vehicle, Pointer(TireID)));
 until TireID = 0;
end;

// =============================================================================
//  PhysicsApplyGravityForce
// =============================================================================
procedure PhysicsApplyGravityForce(const Body : PNewtonBody); cdecl;
var
 mass  : Single;
 Ixx   : Single;
 Iyy   : Single;
 Izz   : Single;
 Force : TVector3f;
begin
NewtonBodyGetMassMatrix(Body, @mass, @Ixx, @Iyy, @Izz);
Force := V3(0, Mass * Gravity, 0);
NewtonBodySetForce(Body, @Force[0]);
end;

// =============================================================================
//  Debug_ShowGeometryCollision
// =============================================================================
//  Callback that shows collision geometry of a body
// =============================================================================
procedure Debug_ShowGeometryCollision(const Body : PNewtonBody; VertexCount : Integer; const FaceArray : PFloat; FaceId : int); cdecl;
var
 i     : Integer;
 v0,v1 : array[0..2] of Single;
 vA    : array of Single;
begin
if VertexCount = 0 then
 exit;
SetLength(vA, VertexCount*3);
Move(FaceArray^, vA[0], VertexCount*3*SizeOf(Single));
v0[0] := vA[(VertexCount-1)*3];
v0[1] := vA[(VertexCount-1)*3+1];
v0[2] := vA[(VertexCount-1)*3+2];
for i := 0 to VertexCount-1 do
 begin
 v1[0] := vA[i*3];
 v1[1] := vA[i*3+1];
 v1[2] := vA[i*3+2];
 glVertex3f(v0[0], v0[1], v0[2]);
 glVertex3f(v1[0], v1[1], v1[2]);
 v0 := v1;
 end;
end;

// =============================================================================
//  Debug_ShowBodyCollision
// =============================================================================
//  Show collision geometry for all bodies in the current scene
// =============================================================================
procedure Debug_ShowBodyCollision(const Body : PNewtonBody); cdecl;
begin
NewtonBodyForEachPolygonDo(Body, Debug_ShowGeometryCollision);
end;

// =============================================================================
//  PhysicsSetTransform
// =============================================================================
//  Callback that's used to update the matrix of the graphic primitives
// =============================================================================
procedure PhysicsSetTransform(const Body : PNewtonBody; const Matrix : PFloat); cdecl;
var
 Primitive : TNewtonPrimitive;
begin
if not Assigned(NewtonBodyGetUserData(Body)) then
 exit;
// Get the primitive by the user data assigned with this body
Primitive := TNewtonPrimitive(NewtonBodyGetUserData(Body));
// Now update it's matrix
Move(Matrix^, Primitive.Matrix, SizeOf(TMatrix4f));
end;

// *****************************************************************************
//  TNewtonTire
// *****************************************************************************

// =============================================================================
//  TNewtonTire.Create
// =============================================================================
constructor TNewtonTire.Create(pVehicle : TNewtonVehicle; pMatrix : TMatrix4f; pWidth, pRadius : Single);
var
 tireMass             : Single;
 tireSuspensionShock  : Single;
 tireSuspensionSpring : Single;
 tireSuspensionLength : Single;
 tirePin              : TVector3f;
begin
// Mass should be ~Mass of Vehicle/20
tireMass             := VehicleMass/20;
// Suspension shock and spring should not be altered
tireSuspensionLength := 0.3;
tireSuspensionSpring := (200 * 9.8) / 0.3;
tireSuspensionShock  := 2 * Sqrt(tireSuspensionSpring);
// Length of suspension should not be too low compared to the vehicle
// the tire will spin around the lateral axis of the same tire space
TirePin := V3(0, 0, 1);
// Now add the tire to the vehicle joint and retrive it's internal ID (for later use)
ID      := Integer(NewtonVehicleAddTire(pVehicle.Joint, @PMatrix[0,0], @TirePin[0], tireMass, pWidth, pRadius,
	      	                              tireSuspensionShock, tireSuspensionSpring, tireSuspensionLength, self, 0));
// Set the maximum rate (angular) at which the wheel can rotate before loosing traction
NewtonVehicleSetTireMaxLongitudinalSlideSpeed(pVehicle.Joint, Pointer(ID), 500);
// Create a graphic primitive for this tire
GraphBody := PrimitiveManager.SpawnChamferCylinder(pRadius, pWidth, pMatrix);
end;

// =============================================================================
//  TNewtonTire.SetTirePhysics
// =============================================================================
//  This is used to update the tire's properties in the proper callback
// =============================================================================
procedure TNewtonTire.SetTirePhysics(const Vehicle : PNewtonJoint; TireID : Integer);
var
 Accel : Single;
 Omega : Single;
begin
// Get tire's angular velocity (angular velocity = omega)
Omega := NewtonVehicleGetTireOmega(Vehicle, Pointer(TireID));
// Add some viscuos damp to the tire torque (this prevents out of control spin)
NewtonVehicleSetTireTorque(Vehicle, Pointer(TireID), Torque-0.1*Omega);
// Braking
if Brake > 0 then
 begin
 // Get acceleration that's needed to stop the wheel
 Accel := NewtonVehicleTireCalculateMaxBrakeAcceleration(Vehicle, Pointer(TireID));
 // Brake the wheel
 NewtonVehicleTireSetBrakeAcceleration(Vehicle, Pointer(TireID), Accel, 225);
 end;
// Reset brake
Brake := 0;
// Update graphics body
NewtonVehicleGetTireMatrix(Vehicle, Pointer(ID), @GraphBody.Matrix[0,0]);
GraphBody.Rotation := V3(0,90,0);
end;

// *****************************************************************************
//  TNewtonSteeringTire
// *****************************************************************************

// =============================================================================
//  TNewtonSteeringTire.SetTirePhysics
// =============================================================================
//  This is used to update the tire's properties in the proper callback.
// =============================================================================
procedure TNewtonSteeringTire.SetTirePhysics(const Vehicle : PNewtonJoint; TireID : Integer);
var
 CurrSteerAngle : Single;
begin
// Update steering angle
CurrSteerAngle := NewtonVehicleGetTireSteerAngle(Vehicle, Pointer(TireID));
NewtonVehicleSetTireSteerAngle(Vehicle, Pointer(TireID), CurrSteerAngle+(SteerAngle-CurrSteerAngle) * 0.25);
// Call inherited SetTirePhysics for the rest of the work
inherited;
end;

// *****************************************************************************
//  TNewtonVehicle
// *****************************************************************************

// =============================================================================
//  TNewtonVehicle.Create
// =============================================================================
constructor TNewtonVehicle.Create(pMatrix : TMatrix4f);
var
 Mass     : Single;
 Ixx      : Single;
 Iyy      : Single;
 Izz      : Single;
 Collider : PNewtonCollision;
 UpDir    : TVector3f;
 BoxSize  : TVector3f;
begin
// =============================================================================
// first we make a normal rigid body
// =============================================================================
// Create graphics primitive
GraphBody := PrimitiveManager.SpawnBox(V3(5, 2, 3), pMatrix);
// Get the vehicle physics dimensions
Mass      := VehicleMass/2;
BoxSize   := V3(5, 2, 3);
Ixx := Mass * (BoxSize[1] * BoxSize[1] + BoxSize[2] * BoxSize[2]) / 12;
Iyy := Mass * (BoxSize[0] * BoxSize[0] + BoxSize[2] * BoxSize[2]) / 12;
Izz := Mass * (BoxSize[0] * BoxSize[0] + BoxSize[1] * BoxSize[1]) / 12;
// In this demo, we use a normal box for the chassis of the car. You'd normally load a
// 3D model and use it's convex hull (NewtonCreateConvexHull) for the chassis collision
Collider := NewtonCreateBox(NewtonWorld, BoxSize[0],BoxSize[1],BoxSize[2], nil);
Body     := NewtonCreateBody(NewtonWorld, Collider);
// Release the Collider
NewtonReleaseCollision(NewtonWorld, Collider);
// Set user data (so that we can directly access the class in callbacks)
NewtonBodySetUserData(Body, self);
// Set transform callback
NewtonBodySetTransformCallback(Body, PSetTransform);
// Set the force and torque call back funtion
NewtonBodySetForceAndTorqueCallback(Body, PhysicsApplyGravityForce);
// Set mass matrix
NewtonBodySetMassMatrix(Body, Mass, Ixx, Iyy, Izz);
// Set the matrix for the rigid body
NewtonBodySetMatrix(Body, @pMatrix[0,0]);
// =============================================================================
//  second we need to add a vehicle joint to the body
// =============================================================================
UpDir := V3(pMatrix[0,1], pMatrix[1,1], pMatrix[2,1]);
Joint := NewtonConstraintCreateVehicle(NewtonWorld, @UpDir[0], Body);
// Set the callback for the tires
NewtonVehicleSetTireCallback(Joint, @PTireUpdate);
end;

// =============================================================================
//  TNewtonVehicle.CreateCar
// =============================================================================
//  Creates a normal 4-wheeled car based on this vehicle joint.
// =============================================================================
procedure TNewtonVehicle.CreateCar;
var
 TireMatrix : TMatrix4f;
begin
Matrix_SetIdentity(TireMatrix);
// First we create two static wheels (back)
Matrix_SetTransform(TireMatrix, V3( 2, -1,  2)); AddTire(TireMatrix, 0.8, 1);
Matrix_SetTransform(TireMatrix, V3( 2, -1, -2)); AddTire(TireMatrix, 0.8, 1);
// And now two steering tires (front)
Matrix_SetTransform(TireMatrix, V3(-2, -1, -2)); AddSteeringTire(TireMatrix, 0.8, 1);
Matrix_SetTransform(TireMatrix, V3(-2, -1,  2)); AddSteeringTire(TireMatrix, 0.8, 1);
end;

// =============================================================================
//  TNewtonVehicle.Flip
// =============================================================================
//  Flips the car back to correct orientation if it bended over
// =============================================================================
procedure TNewtonVehicle.Flip;
var
 tmpV : TVector3f;
 tmpM : TMatrix4f;
begin
tmpV := V3(0,0,0);
// Stop all car motions
NewtonVehicleReset(Vehicle.Joint);
NewtonBodySetVelocity(Vehicle.Body, @tmpV[0]);
NewtonBodySetOmega(Vehicle.Body, @tmpV[0]);
NewtonBodySetTorque(Vehicle.Body, @tmpV[0]);
NewtonBodyGetMatrix(Vehicle.Body, @tmpM[0,0]);
tmpV := V3(tmpM[3,0], tmpM[3,1]+5, tmpM[3,2]);
Matrix_SetIdentity(tmpM);
Matrix_SetTransform(tmpM, tmpV);
tmpM[1,1] := 1;
NewtonBodySetMatrix(Vehicle.Body, @tmpM[0,0]);
Vehicle.SetTorque(0);
end;

// =============================================================================
//  TNewtonVehicle.SetSteeringAngle
// =============================================================================
//  Process all keys that are used to steer/accelerate the car.
// =============================================================================
procedure TNewtonVehicle.ProcessInput;
begin
if KeyState[SDLK_LEFT] = 1 then
 Vehicle.SetSteeringAngle( 30 * PI/180);
if KeyState[SDLK_RIGHT] = 1 then
 Vehicle.SetSteeringAngle(-30 * PI/180);
if KeyState[SDLK_UP] = 1 then
 Vehicle.SetTorque(250);
if KeyState[SDLK_DOWN] = 1 then
 Vehicle.SetTorque(-250);
if KeyState[SDLK_SPACE] = 1 then
 Vehicle.SetBrake(1);
// Reset car's steering and torque if no key is pressed
if (KeyState[SDLK_LEFT] = 0) and (KeyState[SDLK_RIGHT] = 0) then
 SetSteeringAngle(0);
if (KeyState[SDLK_UP] = 0) and (KeyState[SDLK_DOWN] = 0) then
 SetTorque(0);
end;

// =============================================================================
//  TNewtonVehicle.SetSteeringAngle
// =============================================================================
procedure TNewtonVehicle.SetSteeringAngle(pSteeringAngle : Single);
var
 i : Integer;
begin
if Length(Tire) > 0 then
 for i := 0 to High(Tire) do
  if Tire[i] is TNewtonSteeringTire then
   (Tire[i] as TNewtonSteeringTire).SteerAngle := pSteeringAngle;
end;

// =============================================================================
//  TNewtonVehicle.SetTorque
// =============================================================================
//  Apply torque to the wheels. In this demo, the car is front-drive, so we
//  only apply torque to the two (front) steering wheels
// =============================================================================
procedure TNewtonVehicle.SetTorque(pTorque : Single);
var
 i : Integer;
begin
if Length(Tire) > 0 then
 for i := 0 to High(Tire) do
  if Tire[i] is TNewtonSteeringTire then
   (Tire[i] as TNewtonSteeringTire).Torque := pTorque;
end;

// =============================================================================
//  TNewtonVehicle.SetBrake
// =============================================================================
procedure TNewtonVehicle.SetBrake(pBrake : Single);
var
 i : Integer;
begin
if Length(Tire) > 0 then
 for i := 0 to High(Tire) do
  Tire[i].Brake := pBrake;
end;

// =============================================================================
//  TNewtonVehicle.AddTire
// =============================================================================
procedure TNewtonVehicle.AddTire(pMatrix : TMatrix4f; pWidth, pRadius : Single);
begin
SetLength(Tire, Length(Tire)+1);
Tire[High(Tire)] := TNewtonTire.Create(self, pMatrix, pWidth, pRadius);
end;

// =============================================================================
//  TNewtonVehicle.AddSteeringTire
// =============================================================================
procedure TNewtonVehicle.AddSteeringTire(pMatrix : TMatrix4f; pWidth, pRadius : Single);
begin
SetLength(Tire, Length(Tire)+1);
Tire[High(Tire)] := TNewtonSteeringTire.Create(self, pMatrix, pWidth, pRadius);
end;

// *****************************************************************************
//  Misc. procedures
// *****************************************************************************

// =============================================================================
//  InitNewton
// =============================================================================
procedure InitNewton;
var
 FloorBody     : PNewtonBody;
 TmpBody       : PNewtonBody;
 FloorCollider : PNewtonCollision;
 Collider      : PNewtonCollision;
 M             : TMatrix4f;
 TireMatrix    : TMatrix4f;
 i             : Integer;
 TmpSize       : Single;
begin
Randomize;
// Create newton world
NewtonWorld := NewtonCreate(nil, nil);
NewtonSetMinimumFrameRate(NewtonWorld, 150);
// Create a floor
FloorCollider := NewtonCreateBox(NewtonWorld, 400, 1, 400, nil);
FloorBody     := NewtonCreateBody(NewtonWorld, FloorCollider);
Matrix_SetIdentity(M);
Matrix_SetTransform(M, V3(0,-1,0));
NewtonBodySetMatrix(FloorBody, @M[0,0]);
NewtonReleaseCollision(NewtonWorld, FloorCollider);
NewtonBodySetUserData(FloorBody, PrimitiveManager.SpawnBox(V3(400,1,400), M));
// Build our vehicle
Matrix_SetIdentity(TireMatrix);
Matrix_SetTransform(TireMatrix, V3(0, 5, 0));
Vehicle := TNewtonVehicle.Create(TireMatrix);
Vehicle.CreateCar;
NewtonBodySetAutoFreeze(Vehicle.Body, 0);
TmpBody  := nil;
Collider := nil;
// Spawn some random primitives to interact with
for i := 0 to 29 do
 begin
 TmpSize := 1+Random*2;
 Matrix_SetIdentity(M);
 Matrix_SetTransform(M, V3(Random(40)-Random(40), 10+Random(20), Random(40)-Random(40)));
 case Random(2) of
  0 : begin
      Collider := NewtonCreateBox(NewtonWorld, TmpSize, TmpSize, TmpSize, nil);
      TmpBody  := NewtonCreateBody(NewtonWorld, Collider);
      NewtonBodySetUserData(TmpBody, PrimitiveManager.SpawnBox(V3(TmpSize,TmpSize,TmpSize), M));
      end;
  1 : begin
      Collider := NewtonCreateSphere(NewtonWorld, TmpSize, TmpSize, TmpSize, nil);
      TmpBody  := NewtonCreateBody(NewtonWorld, Collider);
      NewtonBodySetUserData(TmpBody, PrimitiveManager.SpawnSphere(TmpSize, M));
      end;
 end;
 NewtonBodySetMatrix(TmpBody, @M[0,0]);
 NewtonBodySetMassMatrix(TmpBody, 10, 10, 10, 10);
 NewtonBodySetForceAndTorqueCallBack(TmpBody, PhysicsApplyGravityForce);
 NewtonBodySetTransformCallBack(TmpBody, PhysicsSetTransform);
 end;
if Assigned(Collider) then
 NewtonReleaseCollision(NewtonWorld, Collider);
end;

// =============================================================================
//  InitGL
// =============================================================================
procedure InitGL;
begin
glEnable(GL_DEPTH_TEST);
glDepthFunc(GL_LEQUAL);
glClearColor(0, 0, 0.3, 0);
glEnable(GL_LIGHTING);
glEnable(GL_LIGHT0);
glEnable(GL_COLOR_MATERIAL);
end;

// =============================================================================
//  Render
// =============================================================================
procedure Render;
begin
Vehicle.ProcessInput;

AccTimeSlice  := AccTimeSlice + (SDL_GetTicks-TimeLastFrame);
TimeLastFrame := SDL_GetTicks;

glMatrixMode(GL_MODELVIEW);
glLoadIdentity;
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

glTranslatef(0, 0, SceneZoom);
glRotatef(SceneRotation[0], 1,0,0);
glRotatef(SceneRotation[1], 0,1,0);
glRotatef(SceneRotation[2], 0,0,1);

// Render objects
PrimitiveManager.Render;

// Render debug output
if ShowDebug then
 begin
 glBegin(GL_LINES);
  NewtonWorldForEachBodyDo(NewtonWorld, @Debug_ShowBodyCollision);
 glEnd;
 end;

SDL_GL_SwapBuffers;

// Correct timing is crucial for physics calculations if they should run the same
// speed, no matter what FPS. So we use a method called "accumulative timeslicing"
// which will give us the same results across all framerates
while AccTimeSlice > 12 do
 begin
 NewtonUpdate(NewtonWorld, (12 / 1000));
 AccTimeSlice := AccTimeSlice - 12;
 end;
end;

// =============================================================================
//  TerminateApplication
// =============================================================================
procedure TerminateApplication;
begin
Vehicle.Free;
PrimitiveManager.Free;
// Destroy the Newton world
NewtonDestroy(NewtonWorld);
SDL_QUIT;
Halt(0);
end;

// *****************************************************************************
//  Event handlers
// *****************************************************************************

// =============================================================================
//  HandleKeyPress
// =============================================================================
procedure HandleKeyPress(KeySym : PSDL_KeySym);
begin
case KeySym.Sym of
 SDLK_ESCAPE : TerminateApplication;
 SDLK_D      : ShowDebug := not ShowDebug;
 SDLK_F      : Vehicle.Flip;
end;
KeyState[KeySym.Sym] := 1;
end;

// =============================================================================
//  ResizeWindow
// =============================================================================
function ResizeWindow(Width : Integer; Height : Integer) : Boolean;
begin
PrimitiveManager.ClearListIDs;
if Height = 0 then
 Height := 1;
glViewport(0, 0, Width, Height);
glMatrixMode(GL_PROJECTION);
glLoadIdentity;
gluPerspective(60, width/height, 1, 100);
glMatrixMode(GL_MODELVIEW);
glLoadIdentity;
Result := true;
end;

// =============================================================================
//  Main
// =============================================================================
var
 VideoFlags  : UInt32;
 VideoInfo   : PSDL_VideoInfo;
 Event       : TSDL_Event;
 Done        : Boolean;
begin
// Initialize SDL
if SDL_Init(SDL_INIT_VIDEO) < 0 then
 begin
 TerminateApplication;
 end;
// Get video information from SDL
VideoInfo := SDL_GetVideoInfo;
// Set flags to use OpenGL, double buffer and hardware
VideoFlags := SDL_OPENGL or SDL_DOUBLEBUF OR SDL_HWPALETTE or SDL_RESIZABLE;
// Check if surface can be stored on hardware memory
if VideoInfo.hw_available <> 0 then
 VideoFlags := VideoFlags or SDL_HWSURFACE
else
 VideoFlags := VideoFlags or SDL_SWSURFACE;
// Checks hardware blits
if VideoInfo.blit_hw <> 0 then
 VideoFlags := VideoFlags or SDL_HWACCEL;
// Set the OpenGL Attributes
SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
// Window title
SDL_WM_SetCaption('Newton - Simple Vehicle Demo (SDL) - 2005/06 by Sascha Willems', nil);
// Create SDL surface
Surface := SDL_SetVideoMode(640, 480, 32, Videoflags);
if not Assigned(Surface) then
 begin
 TerminateApplication;
 end;

InitGL;
PrimitiveManager := TNewtonPrimitiveManager.Create;
SceneRotation    := V3(45, 45, 0);
ReSizeWindow(640, 480);
InitNewton;
TimeLastFrame := SDL_GetTicks;
// Main loop
Done := False;
while not Done do
 begin
 while SDL_PollEvent(@Event) = 1 do
  begin
  case event.type_ of
   SDL_QUITEV      : Done := true;
   SDL_KEYDOWN     : HandleKeyPress(@Event.Key.KeySym);
   SDL_KEYUP       : KeyState[Event.Key.KeySym.Sym] := 0;
   SDL_VIDEORESIZE : begin
                     Surface := SDL_SetVideoMode(event.resize.w, event.resize.h, 32, videoflags );
                     if surface = nil then
                      begin
                      TerminateApplication;
                      end;
                     ReSizeWindow(event.resize.w, event.resize.h);
                     InitGL;
                     end;
  end;
  end;
 Render;
 end;
TerminateApplication;
end.
