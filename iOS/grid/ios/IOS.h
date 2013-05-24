/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2013 Carl Joachim Svenn
 *
 *   This file is part of grid.
 *
 *   grid is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   grid is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with grid.  If not, see <http://www.gnu.org/licenses/>.
*/
#import <OpenGLES/ES2/gl.h>
#import <QuartzCore/QuartzCore.h>
#import "IOSScreen.h"
#import "IOSKeys.h"
#import "IOSTick.h"
#import "debug.h"

// assert:
//   alignment = 4
//   sizeof = 4 + 4 + 4
//   sizeof screen_multisample = 4, offset = 0
//   sizeof screen_orientations = 4,  offset = 4
//   sizeof sound_rate = 4,  offset = 8
//   sizeof keys_acclgyro_rate = 4, offset = 12
typedef struct
{
    uint screen_multisample;
    uint screen_orientations;
    uint screen_rate;
    uint sound_rate;
    float keys_acclgyro_rate;
    
} IOSInit;


extern IOSInit theIOSInit;


// type of callback into Haskell
typedef void (*HaskellCall)();

extern HaskellCall haskell_begin;

extern HaskellCall haskell_iterate;



void ios_init(IOSInit* );

void ios_main(HaskellCall , HaskellCall );

uint ios_fileStaticData(const char* , char* , uint );

uint ios_fileDynamicData(const char* , char* , uint );

uint ios_fileUser(const char* , char* , uint );
