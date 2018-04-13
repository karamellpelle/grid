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
#include "glfw_foreign.h"

// minimum clock update limit
// this should be non-zero, in order to prevent no clock
// update on fast systems!
// fixme: use small but non-zero value
#define CLOCK_UPDATE_DT 0.0


typedef struct
{
    bool used;
    double time;
    double speed;
    
} ClockT;


// locals
static double tick_ref;
static double clock_tick;
static double clock_tick_prev;
static ClockT clock_a;
static ClockT clock_b;
static ClockT clock_c;
static ClockT clock_d;
static ClockT clock_e;
static ClockT clock_f;



// fixme: inline!
void update_clock(double dt, ClockT* clock)
{
    if ( clock->used )
    {
        clock->time = clock->time + (clock->speed) * dt;
    }
}



void glfw_initTick()
{
    tick_ref = glfwGetTime();
    
    // setup clocks
    clock_tick_prev = 0.0;
    clock_tick = 0.0;
    
    clock_a.time = 0.0;
    clock_a.speed = 1.0;
    clock_a.used = false;
    clock_b.time = 0.0;
    clock_b.speed = 1.0;
    clock_b.used = false;
    clock_c.time = 0.0;
    clock_c.speed = 1.0;
    clock_c.used = false;
    clock_d.time = 0.0;
    clock_d.speed = 1.0;
    clock_d.used = false;
    clock_e.time = 0.0;
    clock_e.speed = 1.0;
    clock_e.used = false;
    clock_f.time = 0.0;
    clock_f.speed = 1.0;
    clock_f.used = false;
    
    // setup sampling
    // (not implemented!)
}



void glfw_tickBegin()
{
    double tick = glfw_tickGet();
    double dt = tick - clock_tick_prev;
    
    if ( CLOCK_UPDATE_DT <= dt )
    {
        update_clock( dt, &clock_a );
        update_clock( dt, &clock_b );
        update_clock( dt, &clock_c );
        update_clock( dt, &clock_d );
        update_clock( dt, &clock_f );
        clock_a.used = false;
        clock_b.used = false;
        clock_c.used = false;
        clock_d.used = false;
        clock_e.used = false;
        clock_f.used = false;

        
        clock_tick_prev = tick;
    }

    clock_tick = tick;
    // sampling = samp;
            

}


void glfw_tickEnd()
{
    // fixme!
}


double glfw_tickGet()
{
    return glfwGetTime() - tick_ref;
}

void glfw_tickSet(double t)
{
    tick_ref = glfwGetTime() - t;
}


double glfw_tickClockAGet()
{
    clock_a.used = true;
    return clock_a.time + clock_a.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockASet(double t)
{
    clock_a.time = t - (clock_a.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockAGetSpeed()
{
    return clock_a.speed;
}

void glfw_tickClockASetSpeed(double a)
{
    clock_a.speed = a;
}

double glfw_tickClockBGet()
{
    clock_b.used = true;
    return clock_b.time + clock_b.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockBSet(double t)
{
    clock_b.time = t - (clock_b.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockBGetSpeed()
{
    return clock_b.speed;
}

void glfw_tickClockBSetSpeed(double a)
{
    clock_b.speed = a;
}

double glfw_tickClockCGet()
{
    clock_c.used = true;
    return clock_c.time + clock_c.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockCSet(double t)
{
    clock_c.time = t - (clock_c.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockCGetSpeed()
{
    return clock_c.speed;
}

void glfw_tickClockCSetSpeed(double a)
{
    clock_c.speed = a;
}

double glfw_tickClockDGet()
{
    clock_d.used = true;
    return clock_d.time + clock_d.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockDSet(double t)
{
    clock_d.time = t - (clock_d.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockDGetSpeed()
{
    return clock_d.speed;
}

void glfw_tickClockDSetSpeed(double a)
{
    clock_d.speed = a;
}

double glfw_tickClockEGet()
{
    clock_e.used = true;
    return clock_e.time + clock_e.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockESet(double t)
{
    clock_e.time = t - (clock_e.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockEGetSpeed()
{
    return clock_e.speed;
}

void glfw_tickClockESetSpeed(double a)
{
    clock_e.speed = a;
}

double glfw_tickClockFGet()
{
    clock_f.used = true;
    return clock_f.time + clock_f.speed * (clock_tick - clock_tick_prev);
}

void glfw_tickClockFSet(double t)
{
    clock_f.time = t - (clock_f.speed) * (clock_tick - clock_tick_prev);
}

double glfw_tickClockFGetSpeed()
{
    return clock_f.speed;
}

void glfw_tickClockFSetSpeed(double a)
{
    clock_f.speed = a;
}

#if 0
double glfw_tickClockAGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockASet(double t)
{

}

void glfw_tickClockASetSpeed(double t)
{

}

double glfw_tickClockAGetSpeed()
{
    return 1.0;
}

double glfw_tickClockBGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockBSet(double t)
{

}

void glfw_tickClockBSetSpeed(double t)
{

}

double glfw_tickClockBGetSpeed()
{
    return 1.0;
}

double glfw_tickClockCGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockCSet(double t)
{

}

void glfw_tickClockCSetSpeed(double t)
{

}

double glfw_tickClockCGetSpeed()
{
    return 1.0;
}
double glfw_tickClockDGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockDSet(double t)
{

}

void glfw_tickClockDSetSpeed(double t)
{

}

double glfw_tickClockDGetSpeed()
{
    return 1.0;
}
double glfw_tickClockEGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockESet(double t)
{

}

void glfw_tickClockESetSpeed(double t)
{

}

double glfw_tickClockEGetSpeed()
{
    return 1.0;
}
double glfw_tickClockFGet()
{
    //return 0.0;
    return glfwGetTime();
}

void glfw_tickClockFSet(double t)
{
    glfwSetTime( t );
}

void glfw_tickClockFSetSpeed(double t)
{

}

double glfw_tickClockFGetSpeed()
{
    return 1.0;
}
#endif
