Newton Game Dynamics Delphi-Headertranslation                           
 Current SDK version 1.52
                                                                        
Copyright (c) 2004,05,06 Sascha Willems                                 
                         Jon Walton                                     
                         Dominique Louis                                
        Initial Author : S.Spasov (Sury)                                   

About :
==============================================================================================
The file "NewtonImport.pas" contains the current translation of the header for the
Newton Game Dynamics Physics SDK (http://www.newtongamedynamics.com) and should work with
Delphi and Free Pascal. Other Pascal languages might work but were not tested
(except for VP, which was tested, but VP is discontinued).


Where to get :
==============================================================================================
The current version of this header can always be downloaded at http://newton.delphigl.de
It's usually udpated when the SDK itself is updated, but there may also be some out-of-order
updates in case there are errors/problems with the header translation.


History :
==============================================================================================
  Conversion started at 13.08.2004 by Sury                                     

  Conversion completed at 13.08.2004 by Sury                                   

  Non-Delphi compiler support added on 17.08.2004 by Dominique Louis           
  Newton 1.3 support added on 22.11.2004 by Jon Walton                         

  Legend : (- changes, + added, x removed)                                     
  Fixes on 22/23/24/25.11.2004 by Sascha Willems                               
   - NewtonCollisionIterator                    : cdecl was missing                  
   - NewtonCreateSphere                   	: Fixed parameters                   
   - NewtonVehicleTireIsAirBorne          	: Corrected spelling error           
   - NewtonVehicleTireLostSideGrip        	: Corrected spelling error           
   - NewtonVehicleTireLostTraction        	: Corrected spelling error           
   - NewtonRagDollAddBone                 	: Fixed parameters                   
   - NewtonWorldCollide                   	: Added missing "const"s for params  
   - NewtonWorldRayFilterCallback         	: Fixed parameters and return value  
   - NewtonWorldRayCast                   	: Fixed parameters (removed normal)  
   - NewtonBodySetContinuousCollisionMode 	: Corrected spelling error           
   - NewtonConstraintCreateUniversal      	: Corrected spelling error           
   - NewtonJointSetStiffness              	: Corrected spelling error           
   + NewtonGetTimeStep                    	: Function added                     

 Fixes on 27.11.2004 by Sascha Willems                                         
   - NewtonGetBuoyancyPlane                     : globalSpaceMatrix changed to PFloat

 Updated to SDK 1.31 on 09.01.2005 by Sascha Willems                           
   x NewtonUpVectorCallBack                     : Removed (no longer in SDK)         
   x NewtonUpVectorSetUserCallback        	: Removed (no longer in SDK)         
   - NewtonConstraintCreateUserJoint      	: Changed parameters to new SDK      
   x NewtonUserJointSetUserCallback       	: Removed (no longer in SDK)         
   + NewtonUserJointAddLinearRow;         	: Function added                     
   + NewtonUserJointAddAngularRow         	: Function added                     
   + NewtonUserJointSetRowMinimunFriction 	: Function added                     
   + NewtonUserJointSetRowMaximunFriction 	: Function added                     
   + NewtonUserJointSetRowAcceleration    	: Function added                     
   + NewtonUserJointSetRowStiffness       	: Function added                     
   + NewtonSetSolverModel                 	: Function added                     
   + NewtonSetFrictionModel               	: Function added                     
   + NewtonUserJointGetRowForce           	: Function addes                     
   + NewtonAddBodyImpulse                 	: Declaration fixed                  

 * Changes on 03.04.2005 by Sascha Willems                                     
   - Symbol NEWTON_DOUBLE_PRECISION             : Define this when you want to use   
                                            	  Newton with the double precision   
                                           	  dll                                

 * Changes on 13.04.2005 by Sascha Willems                                     
  - NewtonAllocMemory                           : Fixed declaration. Was declared    
                                            	  as procedure but should have been  
                                            	  a function returning a pointer.    
                                            	  Thx to Tux for pointing it out.    

 Updated to SDK 1.5 on 02.01.2006 by Sascha Willems
   x NewtonWorldCollide                         : Removed (no longer in SDK)   
   + NewtonMaterialSetContactNormalAcceleration : Function added               
   + NewtonMaterialSetContactNormalDirection    : Function added               
   + NewtonCollisionPointDistance               : Function added               
   + NewtonCollisionClosestPoint                : Function added               
   + NewtonCollisionCollide                     : Function added               
   - NewtonBodyCoriolisForcesMode               : Corrected spelling (FPC)     
   - NewtonRagDollGetRootBone                   : Commented out (not in DLL)   
   x NewtonBodyGetTotalVolume                   : Removed (renamed in SDK)     
   + NewtonConvexCollisionCalculateVolume       : Function added
   + NewtonConvexCollisionCalculateInertial...  : Function added               
   + NewtonMaterialSetContinuousCollisionMode   : Function added               
   - NewtonUserJointSetRowMinimunFriction       : Corrected spelling           
   - NewtonUserJointSetRowMaximunFriction       : Corrected spelling           
   + NewtonCollisionCollideContinue             : Function added               
   + NewtonBodySetCentreOfMass                  : Function added               
   + NewtonBodyGetCentreOfMass                  : Function added               
   + NewtonUserJointSetRowSpringDamperAcce...   : Function added               
   x NewtonVehicleBalanceTires                  : Removed (no longer in SDK)   
   - NewtonGetBuoyancyPlane                     : Changed parameters to new SDK
   + NewtonSetPlatformArchitecture              : Function added               
   + NewtonCollisionMakeUnique                  : Function added               
   - NewtonVehicle*                             : Changed parameters to new SDK
   + NewtonUserJointAddGeneralRow               : Function added

  Updated to SDK 1.52 on 13.03.2006 by Sascha Willems
   + NewtonWorldForEachBodyInAABBDo             : Function added
   - NewtonCreateConvexHull                     : Added consts to pointer params     

  Updated to SDK 1.53 on 26.05.2006 by Sascha Willems
   - NewtonWorldRayCast			        : Changed parameters to new SDK

  Changes on 28.06.2006 by Sascha Willems 
   + NewtonBodyGetForceAndTorqueCallback	: Function added