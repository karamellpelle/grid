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



// load sound file into buffer.
// supported file formats, according to Audio Converter Services:
//   iLBC
//   IMA/ADPCM
//   PCM
//   muLaw, aLaw
//   AAC
//   ALAC
//   MP3
uint glfw_loadBuf(ALuint buf, const char* path)
{
/*
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
*/
    return true;

}
