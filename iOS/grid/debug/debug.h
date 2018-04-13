/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2018 karamellpelle@hotmail.com
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
#ifndef DEBUG_H
#define DEBUG_H
#import <QuartzCore/QuartzCore.h>
#import <UIKit/UIKit.h>

void debug_dump_image(const char*, uint8_t* , uint , uint );

void debug_dump_bundle();

void debug_filltexback(const char* tag);

void debug_fill_bytes(uint8_t* , size_t );

#endif
