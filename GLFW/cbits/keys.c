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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "glfw_foreign.h"
#include "helpers.h"

#define GLFW_KEYS_MAX_FINGERS    2
#define PEAK2_RADIUS_VEL         1.0
#define PEAK2_RADIUS_POS_MIN     0.0
#define PEAK2_RADIUS_POS_MAX     1.0
#define PEAK2_RADIUS_POS_TOUCHED 0.5
#define PEAK2_RADIUS_SCROLL_STEP 0.02

#define KEYBOARD_KEY_UP    GLFW_KEY_W
#define KEYBOARD_KEY_DOWN  GLFW_KEY_S
#define KEYBOARD_KEY_LEFT  GLFW_KEY_A
#define KEYBOARD_KEY_RIGHT GLFW_KEY_D
#define KEYBOARD_KEY_A     GLFW_KEY_SPACE
#define KEYBOARD_KEY_B     GLFW_KEY_ENTER

// https://stackoverflow.com/questions/3437404/min-and-max-in-c
#define MAX(X, Y) (((X) < (Y)) ? (Y) : (X))

typedef double NSTimeInterval;
typedef int UITouchPtr;

GLFWcursor* g_cursor = 0;

// is buttonB => (buttonB_ticks <= touching ticks)
static const NSTimeInterval buttonB_ticks = 1.0;

typedef struct
{
    double x;
    double y;
} CGPoint;

typedef struct
{
    double width;
    double height;
} CGSize;

typedef struct
{
    UITouchPtr name;     // GLFW_MOUSE_BUTTON_LEFT, GLFW_MOUSE_BUTTON_RIGHT, GLFW_MOUSE_BUTTON_MIDDLE
    CGPoint pos;
} Finger;


static bool a_released = false; 
static bool b_released = false; 
static double arrows_tick = 0.0;
static bool up = false;
static bool down = false;
static bool left = false;
static bool right = false;
static double arrows_x1 = 0.0;
static double arrows_y1 = 0.0;
static bool arrows_released = false; 
static inline bool is_arrows() { return up || down || left || right; }
                                   
// locals                          
static Finger fingers[ GLFW_KEYS_MAX_FINGERS ];

static size_t fingers_ix = 0;

static uint   fingers_peak = 0;

static double tick;

static CGSize screen_size;

static float  x_scale;

static float  y_scale;


void mouse_button_callback(GLFWwindow* window, int button, int action, int mods);
void scroll_callback(GLFWwindow* window, double xoffset, double yoffset);
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
void cursor_position_callback(GLFWwindow* window, double xpos, double ypos);

void glfw_initKeys()
{
    // set cursor
    glfwSetInputMode( g_window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
    g_cursor = glfwCreateStandardCursor(GLFW_HAND_CURSOR);
   
    // set mouse callbacks
    glfwSetCursorPosCallback( g_window, cursor_position_callback);
    glfwSetMouseButtonCallback( g_window, mouse_button_callback);
    glfwSetScrollCallback( g_window, scroll_callback);

    // set keyboard callback
    glfwSetKeyCallback( g_window, key_callback);


}






// peak1 - one finger sequence
// state
NSTimeInterval  peak1_touched_tick;
CGPoint         peak1_touched_pos;
NSTimeInterval  peak1_touching_tick;
CGPoint         peak1_touching_pos;
bool            peak1_buttonB_handled;
// events
bool            peak1_touched;
bool            peak1_released;
bool            peak1_buttonA;
bool            peak1_buttonB;


void peak1_clear()
{
    peak1_touched = false;
    peak1_released = false;
    peak1_buttonA = false;
    peak1_buttonB = false;
}


void peak1_begin()
{
    peak1_touched = true;
    peak1_touched_tick = tick;
    peak1_touched_pos.x = fingers[ 0 ].pos.x * x_scale;
    peak1_touched_pos.y = fingers[ 0 ].pos.y * y_scale;
}


void peak1_update(uint n_fingers)
{
    peak1_touching_tick = tick;
    peak1_touching_pos.x = fingers[0].pos.x * x_scale;
    peak1_touching_pos.y = fingers[0].pos.y * y_scale;
    
    if ( !peak1_buttonB_handled && 
        peak1_touched_tick + buttonB_ticks <= peak1_touching_tick )
    {
        peak1_buttonB = true;
        peak1_buttonB_handled = true;
    }
}


void peak1_end(uint peak)
{
    // ending peak1
    if ( peak == 1 )
    {
        peak1_released = true;
        if ( peak1_touching_tick < peak1_touched_tick + buttonB_ticks )
        {
            peak1_buttonA = true;
        }
    }
    
    peak1_buttonB_handled = false;
}


// peak2 - two finger sequence
// state
NSTimeInterval  peak2_touched_tick;
CGPoint         peak2_touched_pos;
float           peak2_touched_radius;
NSTimeInterval  peak2_touching_tick;
CGPoint         peak2_touching_pos;
float           peak2_touching_radius;
Vel             peak2_radius_vel;
// events
bool            peak2_touched;
bool            peak2_released;


void peak2_clear()
{
    peak2_touched = false;
    peak2_released = false;
}


void peak2_begin()
{
    peak2_touched_tick = tick;
    
    CGPoint a;
    a.x = fingers[ 0 ].pos.x * x_scale;
    a.y = fingers[ 0 ].pos.y * y_scale;
    CGPoint b;
    b.x = fingers[ 1 ].pos.x * x_scale;
    b.y = fingers[ 1 ].pos.y * y_scale;
    
    peak2_touched_pos.x = 0.5f * a.x + 0.5f * b.x;
    peak2_touched_pos.y = 0.5f * a.y + 0.5f * b.y;

    //peak2_touched_radius = 0.5f * sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
    // clear radius, let scroll wheel update this
    peak2_radius_vel.v = PEAK2_RADIUS_VEL;
    peak2_radius_vel.p = PEAK2_RADIUS_POS_TOUCHED;
    peak2_touched_radius = peak2_radius_vel.p;

        //printf( "touched: peak2_touched_radius: %+6.2f\n", peak2_touched_radius );

    peak2_touched = true;
}


void peak2_update(uint n_fingers)
{
    peak2_touching_tick = tick;
    
    if ( n_fingers == 2 )
    {
        CGPoint a;
        a.x = fingers[ 0 ].pos.x * x_scale;
        a.y = fingers[ 0 ].pos.y * y_scale;
        CGPoint b;
        b.x = fingers[ 1 ].pos.x * x_scale;
        b.y = fingers[ 1 ].pos.y * y_scale;
        
        peak2_touching_pos.x = 0.5f * a.x + 0.5f * b.x;
        peak2_touching_pos.y = 0.5f * a.y + 0.5f * b.y;

        //peak2_touching_radius = 0.5f * sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));    
        // NOTE: scrolling only happens in discrete steps, hence we only need to step in the callback
        // instead, emulate radius by scroll wheel
        peak2_touching_radius = peak2_radius_vel.p;
        //printf( "touched: peak2_touching_radius: %+6.8f\n", peak2_touching_radius );
    }
    
}


void peak2_end(uint peak)
{
    if ( peak == 2 )
    {
        peak2_released = true;
    }
}


// fingers_peak                 : highest number of fingers down during touching.
//                                X finger sequence is active iff X is the current peak.
//                                (0 < X) 
// peakX_clear()                : before each iteration
// peakX_begin()                : X number of fingers begins touching
// peakX_update(uint n_fingers) : update the active X finger sequence, n_fingers is number
//                                of current touching fingers (n_fingers <= X).
//                                (only current peak is updated) 
// peakX_end(uint peak)         : all fingers released, 'peak' was the peak during touching
void glfw_keysBegin()
{
    tick = glfw_tickGet();

    int width, height;
    glfwGetWindowSize( g_window, &width, &height);
    screen_size.width = width;
    screen_size.height = height;
    float scale = 1.0 / (float) MAX(screen_size.width, screen_size.height);
    x_scale = scale;
    y_scale = scale;
    
    peak1_clear();
    peak2_clear();
    

    // touching on screen in progress
    if ( 0 < fingers_ix )
    {
        // is a new finger added?
        if ( fingers_peak < fingers_ix )
        {
            fingers_peak = fingers_ix;
            switch ( fingers_peak )
            {
                case 1:  peak1_begin(); break;
                case 2:  peak2_begin(); break;
            }
        }
        switch ( fingers_peak )
        {
            case 1: peak1_update( fingers_ix ); break;
            case 2: peak2_update( fingers_ix ); break;
        }   
    }
    
    // no touching
    else
    {
        if ( 1 <= fingers_peak )
        {
            peak1_end( fingers_peak );
        }
        if ( 2 <= fingers_peak )
        {
            peak2_end( fingers_peak );
        }
        
        fingers_peak = 0;
    }
}


void glfw_keysEnd()
{
    arrows_released = false; 
    a_released = false; 
    b_released = false; 
}


void glfw_keysClear()
{
    fingers_ix = 0;
    // peakX_end shall not be called:
    fingers_peak = 0;
}



// functions

uint glfw_keysTouchHandlePointTouched(float* x, float* y)
{
    *x = peak1_touched_pos.x;
    *y = peak1_touched_pos.y;
    return peak1_touched;
}


uint glfw_keysTouchHandlePointReleased(float* x, float* y)
{
    *x = peak1_touching_pos.x;
    *y = peak1_touching_pos.y;
    return peak1_released;
}


uint glfw_keysTouchHandlePointDrag(double* ticks, float* x0, float* y0, float* x1, float* y1)
{
    if ( fingers_peak == 1 )
    {
        *ticks = peak1_touching_tick - peak1_touched_tick;
        *x0 = peak1_touched_pos.x;
        *y0 = peak1_touched_pos.y;
        *x1 = peak1_touching_pos.x;
        *y1 = peak1_touching_pos.y;
        
        return true;
    }
    // new chance: keyboard key
    if ( fingers_peak == 0 )
    {

        // is there a key down?
        if ( is_arrows() )
        {
            *ticks = tick - arrows_tick;
            // drag direction
            *x0 = 0.5;
            *y0 = 0.5;
            *x1 = arrows_x1;
            *y1 = arrows_y1;

            return true;
        }
    }
    return false;
}


uint glfw_keysTouchHandlePointDrop(double* ticks, float* x0, float* y0, float* x1, float* y1)
{
    if ( peak1_released )
    { 
        *ticks = peak1_touching_tick - peak1_touched_tick;
        *x0 = peak1_touched_pos.x;
        *y0 = peak1_touched_pos.y;
        *x1 = peak1_touching_pos.x;
        *y1 = peak1_touching_pos.y;
        return true;
    }

    // new chance: keyboard key
    if ( arrows_released )
    {
      printf("arrows released\n");

        //*ticks = peak1_touching_tick - peak1_touched_tick;
        *ticks = tick - arrows_tick;
        *x0 = 0.5f; // FIXME: is this center?? or 0.0?
        *y0 = 0.5f;
        *x1 = arrows_x1;
        *y1 = arrows_y1;
        

      printf("x1: %f\n", arrows_x1 ); 
      printf("y1: %f\n", arrows_y1 ); 

        arrows_released = false;

        return true;
    }

    return false;
    
    
}


uint glfw_keysTouchHandleCircleTouched(float* x, float* y, float* radius)
{
    *x = peak2_touched_pos.x;
    *y = peak2_touched_pos.y;
    *radius = peak2_touched_radius;
    
    return peak2_touched;
}


uint glfw_keysTouchHandleCircleReleased(float* x, float* y, float* radius)
{
    *x = peak2_touching_pos.x;
    *y = peak2_touching_pos.y;
    *radius = peak2_touching_radius;
    
    return peak2_released;
}


uint glfw_keysTouchHandleCircleDrag(double* ticks, float* x0, float* y0, float* radius0,
                                                      float* x1, float* y1, float* radius1)
{
    if ( fingers_peak == 2 )
    {

        *ticks = peak2_touching_tick - peak2_touched_tick;
        *x0 = peak2_touched_pos.x;
        *y0 = peak2_touched_pos.y;
        *radius0 = peak2_touched_radius;
        *x1 = peak2_touching_pos.x;
        *y1 = peak2_touching_pos.y;
        *radius1 = peak2_touching_radius;

        //printf( "radius1 %+6.2f\n", *radius1 );
        //printf( "   drag: Vel.v %+6.2f Vel.p %+6.2f, touchingradius: %+6.2f \n", peak2_radius_vel.v, peak2_radius_vel.p, *radius1 );
        
        return true;
    }
    return false;
}


uint glfw_keysTouchHandleCircleDrop(double* ticks, float* x0, float* y0, float* radius0, 
                                                      float* x1, float* y1, float* radius1)
{
    if ( peak2_released )
    {
        *ticks = peak2_touching_tick - peak2_touched_tick;
        *x0 = peak2_touched_pos.x;
        *y0 = peak2_touched_pos.y;
        *radius0 = peak2_touched_radius;
        *x1 = peak2_touching_pos.x;
        *y1 = peak2_touching_pos.y;
        *radius1 = peak2_touching_radius;
        
        return true;
    }
    return false;
}


// button A is a short touch
uint glfw_keysTouchHandleButtonA(float* x, float* y)
{
    if ( peak1_buttonA )
    {
        *x = peak1_touched_pos.x;
        *y = peak1_touched_pos.y;
        
        return true;
    }
    // new chance: keyboard key
    if ( a_released )
    {
        *x = 0.5f;
        *y = 0.5f;
        return true;
        
    }
    return false;
}


// button B is a long hold
uint glfw_keysTouchHandleButtonB(float* x, float* y)
{
    if ( peak1_buttonB )
    {
        *x = peak1_touched_pos.x;
        *y = peak1_touched_pos.y;
        
        return true;
    }
    // new chance: keyboard key
    if ( b_released )
    {
        *x = 0.5f;
        *y = 0.5f;
        return true;
        
    }
    return false;
}

uint glfw_keysAcclGyro(float* ax, float* ay, float* az, float* gx, float* gy, float* gz)
{
    *ax = 0.0;
    *ay = 0.0;
    *az = 0.0;
    *gx = 0.0;
    *gy = 0.0;
    *gz = 0.0;
    return 0;
}

void cursor_position_callback(GLFWwindow* window, double xpos, double ypos)
{

    // emulating 'void ios_touchesMoved(NSSet* touches, UIEvent* event)'
    // note that all fingers (here: mouse buttons) share the same position,
    // naturally

    // update position
    for (uint i = 0; i != fingers_ix; ++i)
    {
        
        fingers[ i ].pos.x = xpos;
        fingers[ i ].pos.y = ypos;
    }

}


void mouse_button_callback(GLFWwindow* window, int button, int action, int mods)
{
    // buttons:
    // GLFW_MOUSE_BUTTON_LEFT   
    // GLFW_MOUSE_BUTTON_RIGHT
    // GLFW_MOUSE_BUTTON_MIDDLE  
    // ... GLFW_MOUSE_BUTTON_8
    //
    // actions:
    // GLFW_PRESS
    // GLFW_RELEASE
    //
    // mods:
    // GLFW_MOD_SHIFT   : If this bit is set one or more Shift keys were held down. More...
    // GLFW_MOD_CONTROL : If this bit is set one or more Control keys were held down. More...
    // GLFW_MOD_ALT     : If this bit is set one or more Alt keys were held down. More...
    // GLFW_MOD_SUPER   : If this bit is set one or more Super k

    // emulating 'void ios_touchesBegan(NSSet* touches, UIEvent* event)'
    if ( action == GLFW_PRESS )
    {
        if ( fingers_ix != GLFW_KEYS_MAX_FINGERS )
        {
            fingers[ fingers_ix ].name = button;
            double x; double y;
            glfwGetCursorPos( window, &x, &y ); 
            fingers[ fingers_ix ].pos.x = x;
            fingers[ fingers_ix ].pos.y = y;
            ++fingers_ix;
        }
    }
    // emulating 'void ios_touchesEnded(NSSet* touches, UIEvent* event)'
    else
    {
        // erase finger
        size_t i = 0;
        while ( i != fingers_ix )
        {
            if ( fingers[ i ].name == button )
            {
                // erase, pack fingers down
                size_t j = i;
                while ( j + 1 != fingers_ix )
                {
                    fingers[ j ] = fingers[ j + 1 ];
                    ++j;
                }
                
                --fingers_ix;
                break;
            }
            
            ++i;
        }

    }

    // ignoring 'void ios_touchesCancelled(NSSet* touches, UIEvent* event)'

}   

void scroll_callback(GLFWwindow* window, double xoffset, double yoffset)
{
    //printf( "mouse scroll: %+6.2f %+6.2f\n", xoffset, yoffset );
    // scroll sets the velocity
    vel_clamp_step( &peak2_radius_vel, PEAK2_RADIUS_POS_MIN, PEAK2_RADIUS_POS_MAX, (yoffset) * PEAK2_RADIUS_SCROLL_STEP );

    peak2_touching_radius = peak2_radius_vel.p;

    //printf( "mouse scroll: peak2_touching_radius %+6.2f\n", peak2_touching_radius );
}

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    // key:
    // http://www.glfw.org/docs/latest/group__keys.html
    //
    // scancode:
    // platform specific but constant
    //
    // action:
    // GLFW_PRESS
    // GLFW_RELEASE
    // GLFW_REPEAT (only here, not for glfwGetKey())
    //
    // 
    if ( action == GLFW_PRESS )
    {
        bool was_arrows = is_arrows();

        // set direction
        if ( key == KEYBOARD_KEY_UP ) up = true;
        if ( key == KEYBOARD_KEY_DOWN ) down = true;
        if ( key == KEYBOARD_KEY_LEFT ) left = true;
        if ( key == KEYBOARD_KEY_RIGHT ) right = true;

        arrows_x1 = 0.5 + (left ? -2.0 : 0.0) + (right ? 2.0 : 0.0);
        arrows_y1 = 0.5 + (up ? -2.0 : 0.0) + (down ? 2.0 : 0.0);

        // was this the first arrow key down?
        if ( !was_arrows && is_arrows() )
        {
            arrows_tick = tick;
        }

    }

    if ( action == GLFW_RELEASE )
    {
        if ( key == KEYBOARD_KEY_A )      a_released = true;
        if ( key == KEYBOARD_KEY_B )      b_released = true;

        bool was_arrows = is_arrows();

        if ( key == KEYBOARD_KEY_UP ) up = false;
        if ( key == KEYBOARD_KEY_DOWN ) down = false;
        if ( key == KEYBOARD_KEY_LEFT ) left = false;
        if ( key == KEYBOARD_KEY_RIGHT ) right = false;

        if ( was_arrows && !is_arrows() )
        {
            arrows_released = true;
        }
    }
}
