unit faderoutine;
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
{ See fadingexample.dpr for notes and history.                     }
{                                                                  }
{******************************************************************}

interface
uses
 sysutils,
 logger,
 SDL;

  procedure fade (p_surf_screen: PSDL_Surface; p_surf_img: PSDL_Surface;
  ui_seconds: Uint32; FadeOut: boolean);

  procedure crossFade (p_surf_screen: PSDL_Surface; p_surf_img1: PSDL_Surface;
  p_surf_img2: PSDL_Surface; ui_seconds:Uint32);

implementation

{******************************************************************}
// Fade a given surface in or out, by copying it to the screen and
// alphablending a black layer over it.
procedure fade (p_surf_screen: PSDL_Surface; p_surf_img: PSDL_Surface;
  ui_seconds: Uint32; FadeOut: boolean);

  var
  p_surf_black: PSDL_Surface; // the black surface
  ui_flags: Uint32; // flag to pass when creating a surface

  // The following are used to calculate the steps
  // to make a fade in the given time:
  ui_old_time, ui_curr_time, ui_time_ms: Uint32;
  f_alpha: real;

begin
  ui_flags := SDL_SRCALPHA;

  // Create the black surface in the format of the given screen
  if ((p_surf_screen.flags and SDL_HWSURFACE) = 1) then
    ui_flags := (ui_flags or SDL_HWSURFACE)
  else
    ui_flags := (ui_flags or SDL_SWSURFACE);

  // Create the surface
  p_surf_black := SDL_CreateRGBSurface (
        ui_flags,
        p_surf_screen.w,
        p_surf_screen.h,
        p_surf_screen.format.BitsPerPixel,
        p_surf_screen.format.Rmask,
        p_surf_screen.format.Gmask,
        p_surf_screen.format.Bmask,
        p_surf_screen.format.Amask);

   if (p_surf_black = Nil) then
    // Couldn't create surface - display error & quit
  begin
    Log.LogError( Format( 'Couldn''t create surface: %s', [ SDL_GetError ] ),
      'Fade' );
    SDL_QUIT;
    Halt(0);
  end;

  // Fill the Surface with black
  SDL_FillRect (p_surf_black, Nil, SDL_MapRGB (p_surf_screen.format, 0, 0, 0));

  // Ok, we are now ready for the fade. These are the steps (looped):
  // 1. Draw p_surf_img onto p_surf_screen, just an ordinary blit.
  // 2. Decrease or increase (depends on fading in or out) the alpha value,
  //    based on the elapsed time since the previous loop-iteration.
  // 3. Draw p_surf_black onto p_surf_screen in the current alpha value.

  ui_old_time := SDL_GetTicks();
  ui_curr_time := ui_old_time;

  // Convert the given time in seconds into miliseconds.
  ui_time_ms := ui_seconds * 1000;

  if (FadeOut) then
  begin
    f_alpha := 0.0;
    // Loop until the alpha value exceeds 255 (this is the maximum alpha value)
    while (f_alpha < 255.0) do
      begin
      // Draw the image onto the screen
      SDL_BlitSurface (p_surf_img, Nil, p_surf_screen, Nil);
      // Draw the black surface onto the screen
      SDL_SetAlpha (p_surf_black, SDL_SRCALPHA, Uint8(trunc(f_alpha)) );
      SDL_BlitSurface (p_surf_black, Nil, p_surf_screen, Nil);
      // Update the timer variables
      ui_old_time := ui_curr_time;
      ui_curr_time := SDL_GetTicks ();

      // Flip the screen Surface
      SDL_Flip (p_surf_screen);
      // Calculate the next alpha value
      f_alpha := f_alpha + 255 * ((ui_curr_time - ui_old_time) / ui_time_ms);
    end;

    //After fade time has elapsed, make sure we reached final alpha value
    SDL_SetAlpha (p_surf_black, SDL_SRCALPHA, 255 );
    SDL_BlitSurface (p_surf_black, Nil, p_surf_screen, Nil);
    SDL_Flip (p_surf_screen);

  end // end Fade out
  else // Fade in
    begin
    f_alpha := 255.0;
    // Loop until the alpha value exceeds 255
    while (f_alpha > 0.0) do
      begin
        // Draw the image onto the screen
        SDL_BlitSurface (p_surf_img, Nil, p_surf_screen, Nil);
        // Draw the black surface onto the screen
        SDL_SetAlpha (p_surf_black, SDL_SRCALPHA, Uint8(trunc(f_alpha)));
        SDL_BlitSurface (p_surf_black, Nil, p_surf_screen, Nil);
        // Update the timer variables
        ui_old_time := ui_curr_time;
        ui_curr_time := SDL_GetTicks();
        // Flip the screen Surface
        SDL_Flip (p_surf_screen);
        // Calculate the next alpha value
        f_alpha := f_alpha - 255 * ((ui_curr_time - ui_old_time) / ui_time_ms);
      end;

    //After fade time has elapsed, make sure we reached final alpha value
    SDL_BlitSurface (p_surf_img, Nil, p_surf_screen, Nil);
    SDL_Flip (p_surf_screen);
    end;

  // Free the black Surface
  SDL_FreeSurface (p_surf_black)
end; //end procedure

{******************************************************************}
// Cross-Fades the given surfaces onto the given screen within the given time
procedure crossFade (p_surf_screen: PSDL_Surface; p_surf_img1: PSDL_Surface;
  p_surf_img2: PSDL_Surface; ui_seconds:Uint32);

var
  // The following are used to calculate the steps
  // to make a fade in the given time:
  ui_old_time, ui_curr_time, ui_time_ms: Uint32;
  f_alpha: real;

  // These are the steps to perform a cross-fade (looped):
  // 1. Draw p_surf_img1 onto p_surf_screen, just an ordinary blit.
  // 2. Increase the alpha value, based on the elapsed time since the
  //    previous loop-iteration.
  // 3. Draw p_surf_img2 onto p_surf_screen in the current alpha value.

begin;
  ui_old_time := SDL_GetTicks();
  ui_curr_time := ui_old_time;

  // Convert the given time in seconds into miliseconds.
  ui_time_ms := ui_seconds * 1000;
  f_alpha := 0.0;

  // Loop until the alpha value exceeds 255 (this is the maximum alpha value)
  while (f_alpha < 255.0) do
    begin
    // Draw the image onto the screen
    SDL_BlitSurface (p_surf_img1, Nil, p_surf_screen, Nil);
    // Fade the black surface onto the screen
    SDL_SetAlpha (p_surf_img2, SDL_SRCALPHA, Uint8(trunc(f_alpha)) );
    SDL_BlitSurface (p_surf_img2, Nil, p_surf_screen, Nil);
    // Update the timer variables
    ui_old_time := ui_curr_time;
    ui_curr_time := SDL_GetTicks ();
    // Flip the screen Surface
    SDL_Flip (p_surf_screen);
    // Calculate the next alpha value
    f_alpha := f_alpha + 255 * ((ui_curr_time - ui_old_time) / ui_time_ms);
    end;

    //After fade time has elapsed, make sure we reached final alpha value
    SDL_BlitSurface (p_surf_img2, Nil, p_surf_screen, Nil);
    SDL_Flip (p_surf_screen);
  end;
end.
