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
#import "IOS.h"
#import "IOSScreen.h"
#import "IOSView.h"
#import "IOSAppDelegate.h"


EAGLContext* theEAGLContext = nil;

const uint ios_screen_rate_default = 1;



BOOL ios_shouldAutorotateToInterfaceOrientation(UIInterfaceOrientation question)
{
    uint bits = theIOSInit.screen_orientations;
    
    switch (question)
    {
        case UIInterfaceOrientationPortrait:
            return ( bits & IOS_ORIENTATION_PORTRAIT ) != 0;
            
        case UIInterfaceOrientationPortraitUpsideDown:
            return ( bits & IOS_ORIENTATION_PORTRAIT_FLIPPED ) != 0;
            
        case UIInterfaceOrientationLandscapeLeft:
            return ( bits & IOS_ORIENTATION_LANDSCAPE_LEFT ) != 0;
            
        case UIInterfaceOrientationLandscapeRight:
            return ( bits & IOS_ORIENTATION_LANDSCAPE_RIGHT ) != 0;
            
    }
    return NO;
}


void ios_initScreen()
{

    // create OpenGL ES 2.0 context for this thread
    theEAGLContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    if ( !theEAGLContext || ![EAGLContext setCurrentContext:theEAGLContext] )
    {
        NSLog(@"ios_initScreen: could not create OpenGL context!");
    }

    // screen_rate == 0 is special, but handled by ios_screenSetRate
    ios_screenSetRate( theIOSInit.screen_rate );

    // force OpenGL/IOSView FBO to be present!
    [[IOSView alloc] init];
    
    
    
}


void ios_screenSize(uint* wth, uint* hth)
{
    *wth = (uint) theIOSView->wth_;
    *hth = (uint) theIOSView->hth_;
}


uint ios_screenFBO()
{
    return theIOSView->multisample_framebuffer_;
    
}


void ios_screenSetRate(uint rate)
{
    printf("screenSetRate %u\n", rate);
    
    // if 0, reset screen rate to value defined by 'theIOSInit'
    uint r;
    if (rate == 0)
    {
        r = theIOSInit.screen_rate == 0 ? ios_screen_rate_default : theIOSInit.screen_rate;
    }
    else
    {
        r = rate;
    }
    [theIOSAppDelegate setHaskellRate:r];

}

