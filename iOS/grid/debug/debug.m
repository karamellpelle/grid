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
#import "debug.h"
#import <stdint.h>

#import "IOS.h"

void debug_dump_bundle()
{
    NSString* path = [[NSBundle mainBundle] bundlePath];
    
    // Enumerators are recursive
    NSDirectoryEnumerator *enumerator = [[[NSFileManager defaultManager] enumeratorAtPath:path] retain];

    NSString *filePath;

    while ((filePath = [enumerator nextObject]) != nil){
        printf("%s\n", [filePath UTF8String]);
        
    }

    [enumerator release];

}


void debug_dump_image(const char* name, uint8_t* data, uint wth, uint hth)
{
    printf("\ndump of %s. wth is %u, hth is %u:\n", name, wth, hth);
    uint i = 0;
    while (i != hth)
    {
        int j = 0;
        while (j != wth)
        {
            size_t ix = (i * wth + j) * 4;
            printf("(%#4x %#4x %#4x %#4x) ", data[ix], data[ix + 1],
                    data[ix + 2], data[ix + 3]);
            ++j;
        }
        printf("\n");
        ++i;
    }

}

void debug_filltexback(const char* tag)
{    
    NSString* ns_name = [NSString stringWithFormat:@"data/guis/zero/filltex_back.png"];
    
    const char* path =
            [[[NSBundle mainBundle] pathForResource:ns_name ofType:nil] UTF8String];

    UIImage *image = [[UIImage alloc] initWithContentsOfFile: [NSString stringWithUTF8String:path]];
    if (image == nil)
    {
        NSLog(@"ios_loadTexPreMult: could not load path %s", path);
        return;
    }
    
    CGImageRef image_ref = image.CGImage;

    uint wth = CGImageGetWidth( image_ref );
    uint hth = CGImageGetHeight( image_ref );
    
    uint image_size = (wth) * (hth) * 4 * sizeof(GLubyte);
    GLubyte* image_data = (GLubyte*) malloc( image_size );
    debug_fill_bytes(image_data, image_size);
    
    // all textures are represented by premultiplied alpha.
    // for discussion of premultiplied alpha, see
    // * http://home.comcast.net/~tom_forsyth/blog.wiki.html#[[Premultiplied%20alpha]]
    // * http://keithp.com/~keithp/porterduff/p253-porter.pdf
    // this is good for textures used as images, but not for textures used as 
    // data (4-tuples)! (use libpng for data textures (?))
    CGContextRef context = 
        CGBitmapContextCreate( image_data, wth, hth, 
                               8, 4 * (wth), CGImageGetColorSpace(image_ref), 
                               kCGImageAlphaPremultipliedLast 
                               //kCGImageAlphaLast is not implemented!
                              );

    CGContextConcatCTM(context, CGAffineTransformMake(1, 0, 0, -1, 0, hth) );
    CGContextDrawImage( context, CGRectMake( 0, 0, wth, hth ), image_ref );
    CGContextRelease( context );

    debug_dump_image( tag, image_data, wth, hth );

    //glTexImage2D( target, 0, intfmt, wth, hth, 0, GL_RGBA, GL_UNSIGNED_BYTE, image_data );
    
    free(image_data);
    [image release];

}
/*
void null_f() {}

int main(int argc, char** argv)
{
    debug_filltexback("main(argc, argv)");
    ios_main( null_f, null_f);
    return 0;
}
*/
void debug_fill_bytes(uint8_t* buf, size_t size)
{
    uint8_t value = 0;
    for (size_t i = 0; i != size; ++i)
    {
        buf[i] = value++;
    }
}
