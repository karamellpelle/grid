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

void ios_initKeys();


void ios_touchesBegan(NSSet* touches, UIEvent* event);

void ios_touchesMoved(NSSet* touches, UIEvent* event);

void ios_touchesEnded(NSSet* touches, UIEvent* event);

void ios_touchesCancelled(NSSet* touches, UIEvent* event);


void ios_keysBegin();

void ios_keysEnd();


uint ios_keysTouchHandlePointTouched(float* , float* );

uint ios_keysTouchHandlePointReleased(float* , float* );

uint ios_keysTouchHandlePointDrag(double* ticks, float* x, float* y, float* x1, float* y1);

uint ios_keysTouchHandlePointDrop(double* ticks, float* x, float* y, float* x1, float* y1);  




uint ios_keysTouchHandleCircleTouched(float* , float* , float* );

uint ios_keysTouchHandleCircleReleased(float* , float* , float* );

uint ios_keysTouchHandleCircleDrag(double* ticks, float* x, float* y, float* d, float* x1, float* y1, float* d1);

uint ios_keysTouchHandleCircleDrop(double* ticks, float* x, float* y, float* d, float* x1, float* y1, float* d1);



uint ios_keysTouchHandleButtonA(float* x, float* y);

uint ios_keysTouchHandleButtonB(float* x, float* y);



void ios_keysAccel(float* x, float *y, float* z);

void ios_keysGyro(float* x, float* y, float* z);

//void ios_keysAccelRelative(float* x, float *y, float* z); // using CMDeviceMotion

//void ios_keysGyroRelative(Mat3* );    // using CMDeviceMotion




