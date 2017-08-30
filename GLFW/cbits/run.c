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
#include "glfw_foreign.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

// the canonical init object (configuration)
//
GLFWInit theGLFWInit;

// globals
GLFWwindow* g_window = 0;

static double tick_           = 0.0;
static uint frame_count_      = 0;
static uint frame_fps_count_  = 0;
static double frame_fps_tick_ = 0.0;
static double frame_fps_      = 0.0;


static void glfw_error_callback(int error, const char* str)
{
    printf( "GLFW error (code %d): %s\n", error, str );
}

void glfw_init(GLFWInit* init)
{
    theGLFWInit = *init;
    if ( init == 0 )
    {
        printf("cbits: no 'init' in object to glfw_init()\n");
    }
    else
    {
        printf("* * * C B I T S * * *\n\n"
               "screen_multisample:  %u\n"
               "screen_fullscreen:   %s\n",
               init->screen_multisample,
               init->screen_fullscreen == 0 ? "false" : "true" );

        glfwSetErrorCallback( glfw_error_callback );

        if ( !glfwInit() )
        {
            printf( "cbits: glfwInit() failed!\n" );
        }

        GLFWmonitor* monitor = 0;

        // set all hints to defaults
        // http://www.glfw.org/docs/latest/window.html#window_hints
        glfwDefaultWindowHints();

        // we want ES2.0
        glfwWindowHint( GLFW_CLIENT_API, GLFW_OPENGL_ES_API );
        glfwWindowHint( GLFW_CONTEXT_VERSION_MAJOR, 2 );

        //glfwWindowHint( GLFW_DECORATED, decorate );               

        // size
        uint wth = 640;
        uint hth = 480;
        monitor = init->screen_fullscreen ? glfwGetPrimaryMonitor() : 0;

        // multisamples
        uint samples = 0;
        glfwWindowHint( GLFW_SAMPLES, samples ); 

        // create OpenGL (context and window)
        g_window = glfwCreateWindow( wth, hth, "https://github.com/karamellpelle/grid", monitor, 0 );

        // set GL context as g_window
        glfwMakeContextCurrent( g_window );
        if ( !g_window )
        {
            printf( "cbits: could not create window\n" );
        }

        // we now have a context, init GLEW
        GLenum err = glewInit();
        if ( err != GLEW_OK )
        {
            printf( "cbits: could not init GLEW (%s)\n", glewGetErrorString( err ) );
        }

    }
}

void glfw_deinit()
{
    glfwDestroyWindow( g_window ); 
    g_window = 0;

    // frame
    frame_count_ = 0;
    frame_fps_ = 0.0;

    ////////////////////////////////////////////////////////////////////////////////
    // tick
    tick_ = 0;

    glfwTerminate();

}


void glfw_frame_begin()
{
    tick_ = glfwGetTime();

    // compute frames per second
    static const double update_delta = 0.4; // update interval in seconds
    double delta = tick_ - frame_fps_tick_;
    if ( update_delta <= delta )
    {
        frame_fps_ = (frame_count_ - frame_fps_count_) / delta;

        frame_fps_tick_ = tick_; 
        frame_fps_count_ = frame_count_;
    }

    // populate events
    glfwPollEvents();

}

void glfw_frame_end()
{
    glfwSwapBuffers( g_window );

    ++frame_count_;

}
