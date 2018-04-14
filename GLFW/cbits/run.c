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
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define DEBUG_GLGET

extern void glfw_initTick();
//extern void glfw_deinitTick();
extern void glfw_tickBegin();
extern void glfw_tickEnd();
extern void glfw_initKeys();
//extern void glfw_deinitKeys();
extern void glfw_keysBegin();
extern void glfw_keysEnd();
extern void glfw_initSound();
extern void glfw_deinitSound();


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
            return; // FIXME: error return code back to Haskell
        }

        GLFWmonitor* monitor = 0;

        // set all hints to defaults
        // http://www.glfw.org/docs/latest/window.html#window_hints
        glfwDefaultWindowHints();

        // we want ES2.0
        glfwWindowHint( GLFW_CLIENT_API, GLFW_OPENGL_ES_API );
        glfwWindowHint( GLFW_CONTEXT_VERSION_MAJOR, 2 );

        // we want decorated window (unless fullscreen)
        //glfwWindowHint( GLFW_DECORATED, decorate );               
        glfwWindowHint( GLFW_DECORATED, true );               

        // size
        uint wth = 640;
        uint hth = 480;
        monitor = init->screen_fullscreen ? glfwGetPrimaryMonitor() : 0;

        // multisamples
        uint samples = 0;
        glfwWindowHint( GLFW_SAMPLES, samples ); 

        // create OpenGL (context and window)
        g_window = glfwCreateWindow( wth, hth, "https://github.com/karamellpelle/grid", monitor, 0 );
        if ( !g_window )
        {
            printf( "cbits: could not create window\n" );
            return; // FIXME: error return code back to Haskell
        }

        // set current GL context to g_window's context
        glfwMakeContextCurrent( g_window );

        // we now have a context, init GLEW
        //GLenum err = glewInit();
        //if ( err != GLEW_OK )
        //{
        //    printf( "cbits: could not init GLEW (%s)\n", glewGetErrorString( err ) );
        //}
    
        printf("glfwMakeContextCurrent: glGetError(): %s\n", gl_error_string( glGetError() ) );

        ////////////////////////////////////////////////////////////////////////////////
        // TODO: get FBO of screen. I assume this to be 0, i.e. not bound, 
        // see https://www.khronos.org/opengl/wiki/GLAPI/glBindFramebuffer
        // use this value for glfw_screenFBO()


        ////////////////////////////////////////////////////////////////////////////////
        // glGet
        // 
        //  * https://www.khronos.org/opengl/wiki/GLAPI/glGet#Framebuffers
#ifdef DEBUG_GLGET
        GLint num;
        glGetIntegerv( GL_MAX_RENDERBUFFER_SIZE, &num );        // (GLint, at least 16384, see glFramebufferRenderbuffer) The maximum supported width and height for renderbuffers.
        printf( "GL_MAX_RENDERBUFFER_SIZE:          %i\n", num );
        glGetIntegerv( GL_MAX_COLOR_ATTACHMENTS, &num );        // (integer, at least 8) Maximum number of framebuffer attachment points for color buffers.
        printf( "GL_MAX_COLOR_ATTACHMENTS:          %i\n", num );
        glGetIntegerv( GL_MAX_COLOR_TEXTURE_SAMPLES, &num );    // (integer, at least 1) The maximum number of samples for all color formats in a multisample texture.
        printf( "GL_MAX_COLOR_TEXTURE_SAMPLES:      %i\n", num );
        glGetIntegerv( GL_MAX_DEPTH_TEXTURE_SAMPLES, &num );    // (integer, at least 1) The maximum number of samples in a multisample depth or depth-stencil texture.
        printf( "GL_MAX_DEPTH_TEXTURE_SAMPLES:      %i\n", num );
        glGetIntegerv( GL_MAX_DRAW_BUFFERS, &num );             // (integer, at least 8, see glDrawBuffers) The maximum number of simultaneous outputs that may be written in a fragment shader.
        printf( "GL_MAX_DRAW_BUFFERS:               %i\n", num );
        glGetIntegerv( GL_MAX_DUAL_SOURCE_DRAW_BUFFERS, &num ); // (integer, at least 1, see glBlendFunc and glBlendFuncSeparate) The maximum number of active draw buffers when using dual-source blending.
        printf( "GL_MAX_DUAL_SOURCE_DRAW_BUFFERS:   %i\n", num );
        glGetIntegerv( GL_MAX_FRAMEBUFFER_HEIGHT, &num );       // (integer, at least 16384, see glFramebufferParameter) The maximum height for a framebuffer that has no attachments.
        printf( "GL_MAX_FRAMEBUFFER_HEIGHT:         %i\n", num );
        glGetIntegerv( GL_MAX_FRAMEBUFFER_LAYERS, &num );       // (integer, at least 2048, see glFramebufferParameter) The maximum number of layers for a framebuffer that has no attachments.
        printf( "GL_MAX_FRAMEBUFFER_LAYERS:         %i\n", num );
        glGetIntegerv( GL_MAX_FRAMEBUFFER_SAMPLES, &num );      // (integer, at least 4, see glFramebufferParameter) The maximum samples in a framebuffer that has no attachments.
        printf( "GL_MAX_FRAMEBUFFER_SAMPLES:        %i\n", num );
        glGetIntegerv( GL_MAX_FRAMEBUFFER_WIDTH, &num );        // (integer, at least 16384, see glFramebufferParameter) The maximum width for a framebuffer that has no attachments.
        printf( "GL_MAX_FRAMEBUFFER_WIDTH:          %i\n", num );
        glGetIntegerv( GL_MAX_INTEGER_SAMPLES, &num );          // (integer, at least 1) The maximum number of samples supported in integer format multisample buffers.
        printf( "GL_MAX_INTEGER_SAMPLES:            %i\n", num );
        glGetIntegerv( GL_MAX_SAMPLES, &num );                  // (integer, at least 4) 
        printf( "GL_MAX_SAMPLES:                    %i\n", num );
        glGetIntegerv( GL_MAX_TEXTURE_BUFFER_SIZE, &num );      // (GLint, at least 65536) The maximum number of texels allowed in the texel array of a texture buffer object. 
        printf( "GL_MAX_TEXTURE_BUFFER_SIZE:        %i\n", num );
#endif
        ////////////////////////////////////////////////////////////////////////////////
        //
        //
        printf("glfw_init() -> glGetError(): %s\n", gl_error_string( glGetError() ) );

        // init tick
        glfw_initTick(); 

        // init keys
        glfw_initKeys();

        // init keys
        glfw_initSound();
    }
}

void glfw_deinit()
{
    //glfwDestroyCursor( g_cursor );
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

    // pre iteration
    glfw_tickBegin();
    glfw_keysBegin();
}

void glfw_frame_end()
{
    // post iteration
    glfw_keysEnd();
    glfw_tickEnd();


    glfwSwapBuffers( g_window );

    ++frame_count_;

}
