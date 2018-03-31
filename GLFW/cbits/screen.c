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


uint glfw_screenFBO()
{
    // TODO: retrieve variable in glfw_init()
    // https://www.khronos.org/opengl/wiki/GLAPI/glGet#Framebuffers
    return 0;
}

void glfw_screenSize(uint* wth, uint* hth)
{
    // we must use the window's framebuffer size, not window size!
    int width, height;
    glfwGetFramebufferSize( g_window, &width, &height );

    *wth = width;
    *hth = height;

}

void glfw_screenSetRate(uint rate)
{

}

