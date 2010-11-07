program JEDISDLFading;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Alphablending Fade Example              }
{       Conversion of an SDL tutorial in C                         }
{                                                                  }
{ The original files were : main.c, fading.c                       }
{ Original author was:                                             }
{    Patrick Kooman <patrick@2dgame-tutorial.com>                  }
{ Original source from:                                            }
{    http://www.2dgame-tutorial.com/sdl/fading.htm                 }
{                                                                  }
{ The original Pascal code is : fadingexample.dpr, faderoutine.pas }
{ The initial developer of the Pascal code is :                    }
{ Stephan Schonberg <sschonbe@fgcu.edu>                            }
{                                                                  }
{ Portions created by Stephan Schonberg are                        }
{ Copyright (C) 2003 Stephan Schonberg.                            }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ <Contributer Name> ( contributer@sp.sw )                         }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{   Demonstrates a method of fade in / fade out /cross fade by     }
{   using alphablending of an image and a black surface.           }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   SDL logger, SDL Image libraries                                }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This tutorial code is a nice, simple demonstration of using    }
{   alphablending in SDL to fade an image--but it's certainly not  }
{   ideal, and there are plenty of opportunities for improvement.  }
{                                                                  }
{   The method employed for fade in/out is to blit a black surface }
{   of varying alpha levels onto the image which is being faded.   }
{   For cross-fading, the method is similar, but simply uses two   }
{   images instead of one image and black.                         }
{                                                                  }
{   In a 'real world' application, it's likely that better event   }
{   handling would be required, and it would be nice to have       }
{   multiple images fading at once, or fades of images of various  }
{   sizes and locations (instead of full-screen at 0,0).  This     }
{   code could be easily extended to allow for other types of      }
{   fades, for example, fade-to-white.                             }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   March 19 2003 - SS : Initial translation.  Added better error  }
{                        handling via Logger.  Various other minor }
{                        changes: reformatted code, added window   }
{                        title, created new demo images, switched  }
{                        to SDL_Image to allow for .PNG support.   }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  logger,
  sdl_image,
  sdl,
  faderoutine;

const
  // Display dimemsions
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  BPP = 16;

var
  //Surfaces
  p_surf_screen: PSDL_Surface;
  p_surf_img1: PSDL_Surface;
  p_surf_img2: PSDL_Surface;
  p_temp_surf: PSDL_Surface;

begin
  // Initialize SDL
  if (SDL_Init (SDL_INIT_VIDEO) < 0) then
  begin
    //couldn't initialize, so log error and quit
    Log.LogError(Format('Couldn''t initialize SDL : %s', [SDL_GetError]), 'Main');
    SDL_QUIT;
    Halt(0);
  end;

  //make the compiler happy
  p_surf_img1:= Nil;
  p_surf_img2:= Nil;
  p_surf_screen:= Nil;
  p_temp_surf:= Nil;

  // Setup up the display - try hardware support first
  if (SDL_VideoModeOK (SCREEN_WIDTH, SCREEN_HEIGHT, BPP, SDL_HWSURFACE) = BPP) then
    begin
    // Hardware mode
    p_surf_screen := SDL_SetVideoMode (SCREEN_WIDTH, SCREEN_HEIGHT, BPP, SDL_HWSURFACE);
    if (p_surf_screen = Nil) then
      begin
        Log.LogError(Format('Couldn''t set SDL video mode: %s', [SDL_GetError]), 'Main');
        SDL_QUIT;
        Halt(0); //error
      end;
    end
  else
    begin
    // Software mode
    p_surf_screen := SDL_SetVideoMode (SCREEN_WIDTH, SCREEN_HEIGHT, BPP, SDL_SWSURFACE);
    if (p_surf_screen = Nil) then
      begin
        Log.LogError(Format('Couldn''t set SDL video mode: %s', [SDL_GetError]), 'Main');
        SDL_QUIT;
        Halt(0); //error
      end;
  end;

  // Load the images
  p_temp_surf := IMG_Load ('images/img1.png');
  if p_temp_surf <> nil then
    p_surf_img1 := SDL_DisplayFormat (p_temp_surf)
  else
  begin
    Log.LogError( Format( 'Couldn''t load image file: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    SDL_QUIT;
    Halt(0);
  end;

  p_temp_surf := IMG_Load('images/img2.png');
  if p_temp_surf <> nil then
    p_surf_img2 := SDL_DisplayFormat (p_temp_surf)
  else
  begin
    Log.LogError( Format( 'Couldn''t load image file: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    SDL_QUIT;
    Halt(0);
  end;

  // BEGIN THE FADE DEMONSTRATION

  // Display the first image
  SDL_BlitSurface (p_surf_img1, Nil, p_surf_screen, Nil);
  SDL_Flip (p_surf_screen);
  SDL_WM_SetCaption('Welcome to the JEDI-SDL Fade Demo', nil);
  SDL_Delay (4000);   // Pause

  // 2-second Fade out of image 1
  SDL_WM_SetCaption('Fading out...', nil);
  fade (p_surf_screen, p_surf_img1, 2, TRUE);
  SDL_WM_SetCaption('Fading out...complete', nil);
  SDL_Delay (1000);   // Pause

  // 2-second Fade in of image 1
  SDL_WM_SetCaption('Fading in...', nil);
  fade (p_surf_screen, p_surf_img1, 2, FALSE);
  SDL_WM_SetCaption('Fading in...complete', nil);
  SDL_Delay (2000);   // Pause

  // 2-second Cross-fade from image 1 to image 2
  SDL_WM_SetCaption('Cross-fading...', nil);
  crossFade (p_surf_screen, p_surf_img1, p_surf_img2, 2) ;
  SDL_WM_SetCaption('Cross-fading...complete', nil);
  SDL_Delay (2000) ;  // 2 second delay

  // 1-second Fade image 2 out
  SDL_WM_SetCaption('', nil);
  fade (p_surf_screen, p_surf_img2, 1, TRUE);
  SDL_Delay (500) ;   // 1/2 second delay

  //The End, tidy up
  SDL_FreeSurface (p_surf_img1);
  SDL_FreeSurface (p_surf_img2);
  SDL_FreeSurface (p_temp_surf);
  SDL_FreeSurface (p_surf_screen);

  SDL_QUIT;
  exit;
end.
