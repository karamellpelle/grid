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
#import <AudioToolbox/AudioToolbox.h>
#import <OpenAL/al.h>
#import <OpenAL/alc.h>


ALCcontext* theALCcontext = 0;
ALCdevice* theALCdevice = 0;







static void ios_interrupt_f(void* data, UInt32 state)
{
    switch (state)
    {
        // begin interruption
        case kAudioSessionBeginInterruption:
            alcSuspendContext( theALCcontext );
            alcMakeContextCurrent( 0 );
            
        break;
        
        // end interruption
        case kAudioSessionEndInterruption:
            if ( AudioSessionSetActive( true ) ) 
            {
                printf("ios_interrupt_f: could not activate AudioSession!\n");
            }
            alcMakeContextCurrent( theALCcontext );
            alcProcessContext( theALCcontext );
            
        break;
            
    }
}


// set up Sound.
// see documentation "Audio Session Cookbook"
// fixme: use settings from theIOSSoundInit
void ios_initSound()
{
    printf("ios_initSound\n");
    // init AudioSession 
		if ( AudioSessionInitialize(NULL, NULL, ios_interrupt_f, 0) )
    {
        printf("ios_initSound: could not initialize AudioSession!\n");
    }
    
    // set category for AudioSession
    UInt32 cat = kAudioSessionCategory_SoloAmbientSound;
    AudioSessionSetProperty( kAudioSessionProperty_AudioCategory, sizeof(UInt32), &cat );

    // activate AudioSession
    if ( AudioSessionSetActive( true ) )
    {
        printf("ios_initSound: could not activate AudioSession!\n");
    }
    
    // init OpenAL
    theALCdevice = alcOpenDevice( 0 );
    theALCcontext = alcCreateContext( theALCdevice, 0 );
    alcMakeContextCurrent( theALCcontext );
    
    // fixme: set mixer rate.
    // alcMacOSXMixerOutputRate ?
    // kAudioSessionProperty_PreferredHardwareSampleRate ?
    typedef ALvoid AL_APIENTRY (*alcMacOSXMixerOutputRateProcPtr)
                               (const ALdouble value);
    alcMacOSXMixerOutputRateProcPtr proc_ptr = 
            alcGetProcAddress(NULL, (const ALCchar*) "alcMacOSXMixerOutputRate");
    if ( proc_ptr )
    {
        proc_ptr( (ALdouble)theIOSInit.sound_rate );
    }


}



