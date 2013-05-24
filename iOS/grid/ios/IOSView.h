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

#import <UIKit/UIKit.h>
#import <QuartzCore/QuartzCore.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>



@interface IOSView : UIView
{
@public
    CAEAGLLayer* layer_;
    
    GLint wth_;
    GLint hth_;
    GLuint resolve_framebuffer_;
    GLuint resolve_renderbuffer_;
    GLuint multisample_framebuffer_;
    GLuint multisample_renderbuffer_;
    GLuint multisample_depthstencilbuffer_;
    
}

-(void) iterateIOSView;

@end


extern IOSView* theIOSView;
