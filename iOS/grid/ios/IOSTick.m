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
#import "IOSTick.h"

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



void ios_initTick()
{
    tick_ref = [NSDate timeIntervalSinceReferenceDate];
    
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



void ios_tickBegin()
{
    double tick = ios_tickGet();
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


void ios_tickEnd()
{
    // fixme!
}


double ios_tickGet()
{
    return [NSDate timeIntervalSinceReferenceDate] - tick_ref;
}


void ios_tickSet(double t)
{
    tick_ref = [NSDate timeIntervalSinceReferenceDate] - t;
}



double ios_tickClockAGet()
{
    clock_a.used = true;
    return clock_a.time + clock_a.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockASet(double t)
{
    clock_a.time = t - (clock_a.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockAGetSpeed()
{
    return clock_a.speed;
}

void ios_tickClockASetSpeed(double a)
{
    clock_a.speed = a;
}

double ios_tickClockBGet()
{
    clock_b.used = true;
    return clock_b.time + clock_b.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockBSet(double t)
{
    clock_b.time = t - (clock_b.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockBGetSpeed()
{
    return clock_b.speed;
}

void ios_tickClockBSetSpeed(double a)
{
    clock_b.speed = a;
}

double ios_tickClockCGet()
{
    clock_c.used = true;
    return clock_c.time + clock_c.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockCSet(double t)
{
    clock_c.time = t - (clock_c.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockCGetSpeed()
{
    return clock_c.speed;
}

void ios_tickClockCSetSpeed(double a)
{
    clock_c.speed = a;
}

double ios_tickClockDGet()
{
    clock_d.used = true;
    return clock_d.time + clock_d.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockDSet(double t)
{
    clock_d.time = t - (clock_d.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockDGetSpeed()
{
    return clock_d.speed;
}

void ios_tickClockDSetSpeed(double a)
{
    clock_d.speed = a;
}

double ios_tickClockEGet()
{
    clock_e.used = true;
    return clock_e.time + clock_e.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockESet(double t)
{
    clock_e.time = t - (clock_e.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockEGetSpeed()
{
    return clock_e.speed;
}

void ios_tickClockESetSpeed(double a)
{
    clock_e.speed = a;
}

double ios_tickClockFGet()
{
    clock_f.used = true;
    return clock_f.time + clock_f.speed * (clock_tick - clock_tick_prev);
}

void ios_tickClockFSet(double t)
{
    clock_f.time = t - (clock_f.speed) * (clock_tick - clock_tick_prev);
}

double ios_tickClockFGetSpeed()
{
    return clock_f.speed;
}

void ios_tickClockFSetSpeed(double a)
{
    clock_f.speed = a;
}

