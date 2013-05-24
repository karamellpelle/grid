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
#define IOS_ORIENTATION_PORTRAIT            0x00000001
#define IOS_ORIENTATION_PORTRAIT_FLIPPED    0x00000002
#define IOS_ORIENTATION_LANDSCAPE_LEFT      0x00000004
#define IOS_ORIENTATION_LANDSCAPE_RIGHT     0x00000008


extern EAGLContext* theEAGLContext;

extern const uint ios_screen_rate_default;


void ios_initScreen();

BOOL ios_shouldAutorotateToInterfaceOrientation(UIInterfaceOrientation question);


void ios_screenSize(uint* , uint* );

uint ios_screenFBO();

void ios_screenSetRate(uint );

