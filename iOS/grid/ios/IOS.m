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
#import "IOS.h"
#import "IOSSound.h"
#import "IOSAppDelegate.h"
#import "IOSViewController.h"
#import "IOSView.h"

#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
#import <AudioToolbox/AudioToolbox.h>
#import <stdio.h>
#import <OpenAL/al.h>


// globals
IOSInit theIOSInit;

HaskellCall haskell_begin;

HaskellCall haskell_iterate;


void ios_init(IOSInit* init)
{
    theIOSInit = *init;
    if ( init == 0 )
    {
        printf("no 'init' in ios_init\n");
    }
    else
    {
        printf("screen_multisample:  %u\n"
               "screen_orientations: %u\n"
               "screen_rate:         %u\n"
               "sound_rate:          %u\n"
               "keys_acclgyro_rate:  %f\n", init->screen_multisample,
                                            init->screen_orientations,
                                            init->screen_rate,
                                            init->sound_rate,
                                            init->keys_acclgyro_rate);
    }
}



void ios_main(HaskellCall begin, HaskellCall iterate)
{
    NSLog(@"ios_main");
    
    haskell_begin = begin;
    haskell_iterate = iterate;

    //debug_dump_bundle();
    
    // now run UIApplicationMain. 
    // this function calls back into Haskell, and typically never returns.
    UIApplicationMain(0, 0, nil, @"IOSAppDelegate" );
    
}


// path to read-only file (in main NSBundle) (for application)
uint ios_fileStaticData(const char* name, char* dst, uint len)
{    
    
    // fixme: should ns_path be released? (fuck objective-c)
    NSString* ns_name = [NSString stringWithFormat:@"%s%s", "data/", name];
    
    const char* path =
            [[[NSBundle mainBundle] pathForResource:ns_name ofType:nil] UTF8String];
    
    if ( path )
    {
        // copy into dst
        size_t i = 0;
        while ( i != len )
        {
            dst[i] = path[i]; 
            if ( path[i] == '\0' )
            {
                return true;
            }

            ++i;
        }
        
        NSLog(@"ios_fileStaticData: overflow (dst length is: %u)", len);
        return false;
    }
    else
    {
        return false;
    }
    
    
}


// path to read-write file or directory (for application)
uint ios_fileDynamicData(const char* name, char* dst, uint len)
{    
    NSFileManager* file_mgr = [NSFileManager defaultManager];
    
    NSArray* urls = [file_mgr URLsForDirectory:NSApplicationSupportDirectory
                              inDomains:NSUserDomainMask];
    if ( [urls count] != 0 )
    {
        NSURL* app_support_dir = [urls objectAtIndex:0];
        NSString* ns_name = [NSString stringWithUTF8String:name];
        NSURL* path_url = [app_support_dir URLByAppendingPathComponent:ns_name];
        const char* path = [[path_url path] UTF8String];

        // copy into dst
        size_t i = 0;
        while ( i != len )
        {
            dst[i] = path[i]; 
            if ( path[i] == '\0' )
            {
                return true;
            }
            
            ++i;
        }
        
        NSLog(@"ios_fileDynamicData: overflow (dst length is: %u)", len);
        return false;
        
    }
    else
    {
        NSLog(@"ios_fileDynamicData: no Application Support/");
        return false;
    }
}


// path to read-write file or directory (for user)
uint ios_fileUser(const char* name, char* dst, uint len)
{    
    NSFileManager* file_mgr = [NSFileManager defaultManager];
    
    NSArray* urls = [file_mgr URLsForDirectory:NSDocumentDirectory
                                     inDomains:NSUserDomainMask];
    if ( [urls count] != 0 )
    {
        NSURL* app_support_dir = [urls objectAtIndex:0];
        NSString* ns_name = [NSString stringWithUTF8String:name];
        NSURL* path_url = [app_support_dir URLByAppendingPathComponent:ns_name];
        const char* path = [[path_url path] UTF8String];
        
        // copy into dst
        size_t i = 0;
        while ( i != len )
        {
            dst[i] = path[i]; 
            if ( path[i] == '\0' )
            {
                return true;
            }
            
            ++i;
        }
        
        NSLog(@"ios_fileUser: overflow (dst length is: %u)", len);
        return false;
        
    }
    else
    {
        NSLog(@"ios_fileUser: no Documents/");
        return false;
    }
    
}


// path to read-write file or directory (for tmp)
uint ios_fileTmp(const char* name, char* dst, uint len)
{   
    NSString* ns_dir = NSTemporaryDirectory();
    if (ns_dir)
    {
        NSString* ns_name = [NSString stringWithUTF8String:name];
        NSString* ns_path = [NSString stringWithFormat:@"%@/%@", ns_dir, ns_name];
        
        const char* path = [ns_path UTF8String];
        
        // copy into dst
        size_t i = 0;
        while ( i != len )
        {
            dst[i] = path[i]; 
            if ( path[i] == '\0' )
            {
                return true;
            }
            
            ++i;
        }        
        
        NSLog(@"ios_fileTmp: overflow (dst length is: %u)", len);
        return false;
    }
    else
    {
        NSLog(@"ios_fileTmp: no tmp/");
        return false;        
    }
}


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
uint ios_loadTexPreMult(GLenum target, const char* path, uint* wth, uint* hth, GLenum intfmt)
{
    // (assuming tex is bound)
    
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
    return true;
}


// load sound file into buffer.
// supported file formats, according to Audio Converter Services:
//   iLBC
//   IMA/ADPCM
//   PCM
//   muLaw, aLaw
//   AAC
//   ALAC
//   MP3
uint ios_loadBuf(ALuint buf, const char* path)
{
    // open file as a ExtAudioFileRef
    CFURLRef cfurl = 
        (CFURLRef)[[NSURL fileURLWithPath:[NSString stringWithUTF8String:path]] retain];
    ExtAudioFileRef ext_ref;
    if ( ExtAudioFileOpenURL( cfurl, &ext_ref ) )
    {
        printf("ios_loadBuffer: could not open file\n");
        CFRelease( cfurl );
        return false;
    }
    CFRelease( cfurl );
    
    // buffer data is: 
    // PCM, 16 bit native endian (i.e. little endian), mono, ios_sample_rate rate
    AudioStreamBasicDescription out_dsc;
    out_dsc.mSampleRate = (Float64)theIOSInit.sound_rate;
    out_dsc.mChannelsPerFrame = 1;
    out_dsc.mFormatID = kAudioFormatLinearPCM;
    out_dsc.mBytesPerPacket = 2;
    out_dsc.mFramesPerPacket = 1;
    out_dsc.mBytesPerFrame = 2;
    out_dsc.mBitsPerChannel = 16;
    out_dsc.mFormatFlags = kAudioFormatFlagsNativeEndian |
                           kAudioFormatFlagIsPacked      |
                           kAudioFormatFlagIsSignedInteger;
    ExtAudioFileSetProperty( ext_ref, kExtAudioFileProperty_ClientDataFormat,
                             sizeof(out_dsc), &out_dsc );

    // extract buffer data
    SInt64 num_frames_sint64;
    UInt32 num_frames_sint64_size = sizeof(num_frames_sint64);
    ExtAudioFileGetProperty( ext_ref, kExtAudioFileProperty_FileLengthFrames, 
                             &num_frames_sint64_size, &num_frames_sint64 );
    size_t data_size = num_frames_sint64 * out_dsc.mBytesPerFrame;
    void* data = malloc( data_size );
    AudioBufferList buffer_list;
    buffer_list.mNumberBuffers = 1;
    buffer_list.mBuffers[0].mDataByteSize = data_size;
		buffer_list.mBuffers[0].mNumberChannels = out_dsc.mChannelsPerFrame;
		buffer_list.mBuffers[0].mData = data;
    UInt32 num_frames_uint32 = num_frames_sint64;
    if ( ExtAudioFileRead( ext_ref, &num_frames_uint32, &buffer_list) )
    {
        printf("ios_loadBuffer: could not extract buffer data.\n");
        free( data );
        return false;
    }
    
    // populate AL buffer
    alBufferData( buf, AL_FORMAT_MONO16, data, data_size, theIOSInit.sound_rate );
    free( data );
    return true;

}
