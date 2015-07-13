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


// loading a 2D texture of type 'target' into current bound texture.
// colors in the textures will be represented by premultiplicated alpha.
// this is good for textures used as images, but not for textures used as 
// data (4-tuples). however, it is really hard to get the png data from files,
// since iOS think it is smart, and want to prevent freedom :(
//
// for discussion of premultiplied alpha, see
// * http://home.comcast.net/~tom_forsyth/blog.wiki.html#[[Premultiplied%20alpha]]
// * http://keithp.com/~keithp/porterduff/p253-porter.pdf
// * http://stackoverflow.com/questions/13336241/loading-4-channel-texture-data-in-ios
// valid targets includes
//      TEXTURE_2D
//      TEXTURE_CUBE_MAP_POSITIVE_X
//      TEXTURE_CUBE_MAP_POSITIVE_Y
//      TEXTURE_CUBE_MAP_POSITIVE_Z
//      TEXTURE_CUBE_MAP_NEGATIVE_X
//      TEXTURE_CUBE_MAP_NEGATIVE_Y
//      TEXTURE_CUBE_MAP_NEGATIVE_Z
// valid internal formats includes (fixme: verify this list and implement)
//      ALPHA
//      RGB
//      RGBA
//      LUMINANCE
//      LUMINANCE_ALPHA
//      GL_DEPTH_COMPONENT
//      BGRA_EXT (BGRA8888)
//      RED_EXT
//      RG_EXT
//      R8_EXT
//      RG8_EXT
//      RGB565
//      RGBA5551
//      RGBA4444
uint glfw_loadTexPreMult(GLenum target, const char* path, uint* wth, uint* hth, GLenum intfmt)
{
    // (assuming tex is bound)
/* 
    UIImage *image = [[UIImage alloc] initWithContentsOfFile: [NSString stringWithUTF8String:path]];
    if (image == nil)
    {
        printf("ios_loadTexPreMult: could not load path %s\n", path);
        return false;
    }
    
    CGImageRef image_ref = image.CGImage;
    *wth = CGImageGetWidth( image_ref );
    *hth = CGImageGetHeight( image_ref );
    
    // create bitmap context
    uint image_size = (*wth) * (*hth) * 4 * sizeof(GLubyte);
    GLubyte* image_data = (GLubyte*) malloc( image_size );
    CGContextRef context = 
        CGBitmapContextCreate( image_data, *wth, *hth, 
                               8, 4 * (*wth), CGImageGetColorSpace(image_ref), 
                               kCGImageAlphaPremultipliedLast 
                               //kCGImageAlphaLast is not implemented for this context :(
                              );
    // setup context
    CGContextSetShouldAntialias( context, false );
    CGContextSetBlendMode( context, kCGBlendModeCopy );    
    CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, *hth) );
    
    // draw image to context
    CGContextDrawImage( context, CGRectMake( 0, 0, *wth, *hth ), image_ref );
    CGContextRelease( context );

    // load into bound texture object
    glTexImage2D( target, 0, intfmt, *wth, *hth, 0, GL_RGBA, GL_UNSIGNED_BYTE, image_data );
    
    free(image_data);
    [image release];
*/
    return true;

}
