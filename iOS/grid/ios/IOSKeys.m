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
#import "IOSKeys.h"
#import "IOSTick.h"
#import "IOSView.h"
#import "IOS.h"
#import <CoreMotion/CoreMotion.h>

#define IOS_KEYS_MAX_FINGERS 2


// is buttonB => (buttonB_ticks <= touching ticks)
static const NSTimeInterval buttonB_ticks = 1.0;


typedef struct
{
    UITouch* name;
    CGPoint pos;
} Finger;


// locals
static Finger fingers[ IOS_KEYS_MAX_FINGERS ];

static size_t fingers_ix = 0;

static uint   fingers_peak = 0;

static double tick;

static CGSize screen_size;

static float  x_scale;

static float  y_scale;

static CMMotionManager* motion_manager = nil;




void ios_initKeys()
{
    if ( theIOSInit.keys_acclgyro_rate != 0.0 )
    {
        motion_manager = [[CMMotionManager alloc] init];
        
        // we want samples from accelerometer
        //motion_manager.accelerometerUpdateInterval = init object;
        //[motion_manager startAccelerometerUpdates];
        
        // we want samples from gyro
        // motion_manager.gyroUpdateInterval = init object;
        //[motion_manager startGyroUpdates];
        
        // we want processed samples
        [motion_manager setDeviceMotionUpdateInterval:theIOSInit.keys_acclgyro_rate];
        [motion_manager startDeviceMotionUpdates];
    }

}


void ios_touchesBegan(NSSet* touches, UIEvent* event)
{
    for (UITouch* touch in touches) 
    {
        if ( fingers_ix != IOS_KEYS_MAX_FINGERS )
        {
            fingers[ fingers_ix ].name = touch;
            fingers[ fingers_ix ].pos = [touch locationInView:(UIView*)theIOSView];
            ++fingers_ix;
        }
    }

}


void ios_touchesMoved(NSSet* touches, UIEvent* event)
{
    for (UITouch* touch in touches)
    {        
        // update position
        size_t i = 0;
        while ( i != fingers_ix )
        {
            if ( fingers[ i ].name == touch )
            {
                fingers[ i ].pos = [touch locationInView:(UIView*)theIOSView];
                break;
            }
            
            ++i;
        }
    }
}


void ios_touchesEnded(NSSet* touches, UIEvent* event)
{
    for (UITouch* touch in touches)
    {
        // erase finger
        size_t i = 0;
        while ( i != fingers_ix )
        {
            if ( fingers[ i ].name == touch )
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
}


void ios_touchesCancelled(NSSet* touches, UIEvent* event)
{
    fingers_ix = 0;
    // reset finger_peak or not? (no peakX_end or peakX_end)
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
    peak2_touched_radius = 0.5f * sqrtf((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
    
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
        peak2_touching_radius = 0.5f * sqrtf((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));    
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
void ios_keysBegin()
{
    tick = ios_tickGet();
    screen_size = [theIOSView bounds].size;
    float scale = 1.0 / (float) MAX(screen_size.width, screen_size.height);
    x_scale = scale;
    y_scale = scale;
    
    peak1_clear();
    peak2_clear();
    
    // touching
    if ( 0 < fingers_ix )
    {
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


void ios_keysEnd()
{
    
}


void ios_keysClear()
{
    fingers_ix = 0;
    // peakX_end shall not be called:
    fingers_peak = 0;
}



// functions

uint ios_keysTouchHandlePointTouched(float* x, float* y)
{
    *x = peak1_touched_pos.x;
    *y = peak1_touched_pos.y;
    return peak1_touched;
}


uint ios_keysTouchHandlePointReleased(float* x, float* y)
{
    *x = peak1_touching_pos.x;
    *y = peak1_touching_pos.y;
    return peak1_released;
}


uint ios_keysTouchHandlePointDrag(double* ticks, float* x0, float* y0, float* x1, float* y1)
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
    return false;
}


uint ios_keysTouchHandlePointDrop(double* ticks, float* x0, float* y0, float* x1, float* y1)
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
    return false;
    
    
}


uint ios_keysTouchHandleCircleTouched(float* x, float* y, float* radius)
{
    *x = peak2_touched_pos.x;
    *y = peak2_touched_pos.y;
    *radius = peak2_touched_radius;
    
    return peak2_touched;
}


uint ios_keysTouchHandleCircleReleased(float* x, float* y, float* radius)
{
    *x = peak2_touching_pos.x;
    *y = peak2_touching_pos.y;
    *radius = peak2_touching_radius;
    
    return peak2_released;
}


uint ios_keysTouchHandleCircleDrag(double* ticks, float* x0, float* y0, float* radius0,
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
        
        return true;
    }
    return false;
}


uint ios_keysTouchHandleCircleDrop(double* ticks, float* x0, float* y0, float* radius0, 
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


uint ios_keysTouchHandleButtonA(float* x, float* y)
{
    if ( peak1_buttonA )
    {
        *x = peak1_touched_pos.x;
        *y = peak1_touched_pos.y;
        
        return true;
    }
    return false;
}


uint ios_keysTouchHandleButtonB(float* x, float* y)
{
    if ( peak1_buttonB )
    {
        *x = peak1_touched_pos.x;
        *y = peak1_touched_pos.y;
        
        return true;
    }
    return false;
}


void ios_keysAcclGyro(float* ax, float* ay, float* az, float* gx, float* gy, float* gz)
{
    CMDeviceMotion* data = [motion_manager deviceMotion];
    CMAcceleration accl = [data userAcceleration];
    CMRotationRate gyro = [data rotationRate];
    *ax = (float)accl.x,
    *ay = (float)accl.y;
    *az = (float)accl.z;
    *gx = (float)gyro.x;
    *gy = (float)gyro.y;
    *gz = (float)gyro.z;
}

/*
void ios_keysAccel(float* x, float *y, float* z)
{
    CMAccelerometerData* data = [motion_manager accelerometerData];
    *x = data.acceleration.x;
    *y = data.acceleration.y;
    *z = data.acceleration.z;
}


void ios_keysGyro(float* x, float* y, float* z)
{
    CMGyroData* data = [motion_manager gyroData];
    *x = data.rotationRate.x;
    *y = data.rotationRate.y;
    *z = data.rotationRate.z;
}
*/


