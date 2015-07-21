/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2013-2015 Carl Joachim Svenn
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
#include <stdint.h> 
//#include <math.h>
#include <GL/glew.h>    // must be done before GLFW!!
#include <GLFW/glfw3.h> // FIXME: build option?
#include <AL/al.h>

// expose native GL
// FIXME: set this based on Platform and context type!!
//        see http://www.glfw.org/docs/latest/group__native.html
#define GLFW_EXPOSE_NATIVE_X11
#define GLFW_EXPOSE_NATIVE_GLX // or EGL??
//#include <GLFW/glfw3native.h>

////////////////////////////////////////////////////////////////////////////////
// types

typedef uint32_t uint;

////////////////////////////////////////////////////////////////////////////////
// run MEnv
//

// assert:
//   alignment = 4
//   sizeof = 4 + 4
//   sizeof screen_multisample = 4, offset = 0
//   sizeof screen_fullscreen = 4,  offset = 4
typedef struct
{
    uint32_t screen_multisample;
    uint32_t screen_fullscreen;
    
} GLFWInit;


extern GLFWInit theGLFWInit;


// type of callback into Haskell
typedef void (*HaskellCall)();

extern GLFWInit theGLFWInit;


// type of callback into Haskell
typedef void (*HaskellCall)();

extern HaskellCall haskell_begin;

extern HaskellCall haskell_iterate;

void glfw_init(GLFWInit* );

void ios_main(HaskellCall , HaskellCall );


////////////////////////////////////////////////////////////////////////////////
// Screen
//

uint glfw_screenFBO();

void glfw_screenSize(uint* wth, uint* hth);

void glfw_screenSetRate(uint rate);

////////////////////////////////////////////////////////////////////////////////
// OpenGL
//

uint glfw_loadTexPreMult(GLenum , const char*, GLuint* , GLuint* , GLenum );


////////////////////////////////////////////////////////////////////////////////
// OpenAL
//

uint glfw_loadBuf(ALuint , const char* );

////////////////////////////////////////////////////////////////////////////////
// Keys
//

void glfw_keysClear();

uint glfw_keysTouchHandlePointTouched(float* x, float* y);

uint glfw_keysTouchHandlePointVector(double* ticks, float* x, float* y, float* x1, float* y1);

uint glfw_keysTouchHandlePointVector(double* ticks, float* x, float* y, float* x1, float* y1);

uint glfw_keysTouchHandleCircleTouched(float* x, float* y, float* r);

uint glfw_keysTouchHandleCircleReleased(float* x, float* y, float* r);

uint glfw_keysTouchHandleCircleDrag(double* ticks, float* x0, float* y0, float* r0, float* x1, float* y1, float* r1);

uint glfw_keysTouchHandleCircleDrop(double* ticks, float* x0, float* y0, float* r0, float* x1, float* y1, float* r1);

uint glfw_keysTouchHandleButtonA(float* x0, float* y0);

uint glfw_keysTouchHandleButtonB(float* x0, float* y0);

uint glfw_keysAcclGyro(float* ax, float* ay, float* az, float* gx, float* gy, float* gz);


////////////////////////////////////////////////////////////////////////////////
// Tick
//

double glfw_tickGet();
void glfw_tickSet(double t);

double glfw_tickClockAGet();
void glfw_tickClockASet(double t);
void glfw_tickClockASetSpeed(double t);
double glfw_tickClockAGetSpeed();

double glfw_tickClockBGet();
void glfw_tickClockBSet(double t);
void glfw_tickClockBSetSpeed(double t);
double glfw_tickClockBGetSpeed();

double glfw_tickClockCGet();
void glfw_tickClockCSet(double t);
void glfw_tickClockCSetSpeed(double t);
double glfw_tickClockCGetSpeed();

double glfw_tickClockDGet();
void glfw_tickClockDSet(double t);
void glfw_tickClockDSetSpeed(double t);
double glfw_tickClockDGetSpeed();

double glfw_tickClockEGet();
void glfw_tickClockESet(double t);
void glfw_tickClockESetSpeed(double t);
double glfw_tickClockEGetSpeed();

double glfw_tickClockFGet();
void glfw_tickClockFSet(double t);
void glfw_tickClockFSetSpeed(double t);
double glfw_tickClockFGetSpeed();


