-- grid is a game written in Haskell
-- Copyright (C) 2013 Carl Joachim Svenn
-- 
-- This file is part of grid.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenAL
  (
    module OpenAL.Values,
    module OpenAL.Types,

    module Foreign.Storable,
    module Foreign.Ptr,
    module Foreign.C,
    module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Array,
    module Data.Word,
    module Data.Bits,
    
    alGetString,
    alGetBooleanv,
    alGetIntegerv, 
    alGetFloatv, 
    alGetDoublev, 
    alGetBoolean, 
    alGetInteger, 
    alGetFloat, 
    alGetDouble, 
    alEnable, 
    alGetError,
    alDisable,
    alIsEnabled,
    alIsExtensionPresent, 
    alGetProcAddress, 
    alGetEnumValue, 
    alListenerf, 
    alListener3f, 
    alListenerfv, 
    alListeneri, 
    alListener3i, 
    alListeneriv, 
    alGetListenerf, 
    alGetListener3f, 
    alGetListenerfv, 
    alGetListeneri, 
    alGetListener3i, 
    alGetListeneriv, 
    alGenSources, 
    alDeleteSources, 
    alIsSource, 
    alSourcef, 
    alSource3f, 
    alSourcefv,
    alSourcei, 
    alGetSourcefv, 
    alSource3i, 
    alSourceiv, 
    alGetSourcef, 
    alGetSource3f, 
    alGetSourcei, 
    alGetSource3i, 
    alGetSourceiv, 
    alSourcePlayv, 
    alSourceStopv, 
    alSourceRewindv, 
    alSourcePausev, 
    alSourcePlay, 
    alSourceStop, 
    alSourceRewind, 
    alSourcePause, 
    alSourceQueueBuffers, 
    alGenBuffers, 
    alSourceUnqueueBuffers, 
    alDeleteBuffers, 
    alIsBuffer, 
    alBufferData, 
    alBufferf, 
    alBuffer3f, 
    alBufferfv, 
    alBufferi, 
    alBuffer3i,
    alBufferiv, 
    alGetBufferf, 
    alGetBuffer3f, 
    alGetBufferfv, 
    alGetBufferi, 
    alGetBuffer3i, 
    alGetBufferiv, 
    alDopplerFactor, 
    alDopplerVelocity, 
    alSpeedOfSound, 
    alDistanceModel, 

  ) where



import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Word
import Data.Bits

import OpenAL.Values
import OpenAL.Types


    

-- | const ALchar* alGetString( ALenum param );
foreign import ccall unsafe "alGetString" alGetString :: ALenum -> IO (Ptr ALchar) 

-- | void alGetBooleanv( ALenum param, ALboolean* data );
foreign import ccall unsafe "alGetBooleanv" alGetBooleanv 
    :: ALenum -> Ptr ALboolean -> IO ()

-- | void alGetIntegerv( ALenum param, ALint* data );
foreign import ccall unsafe "alGetIntegerv" alGetIntegerv 
    :: ALenum -> Ptr ALint -> IO ()

-- | void alGetFloatv( ALenum param, ALfloat* data );
foreign import ccall unsafe "alGetFloatv" alGetFloatv 
    :: ALenum -> Ptr ALfloat -> IO ()

-- | void alGetDoublev( ALenum param, ALdouble* data );
foreign import ccall unsafe "alGetDoublev" alGetDoublev 
    :: ALenum -> Ptr ALdouble -> IO ()

-- | ALboolean alGetBoolean( ALenum param );
foreign import ccall unsafe "alGetBoolean" alGetBoolean 
    :: ALenum -> IO ALboolean 

-- | ALint alGetInteger( ALenum param );
foreign import ccall unsafe "alGetInteger" alGetInteger 
    :: ALenum -> IO ALint

-- | ALfloat alGetFloat( ALenum param );
foreign import ccall unsafe "alGetFloat" alGetFloat 
    :: ALenum -> IO ALfloat

-- | ALdouble alGetDouble( ALenum param );
foreign import ccall unsafe "alGetDouble" alGetDouble 
    :: ALenum -> IO ALdouble

-- | alEnable( ALenum capability );
foreign import ccall unsafe "alEnable" alEnable :: ALenum -> IO () 

-- | ALenum alGetError( void );
foreign import ccall unsafe "alGetError" alGetError :: IO ALenum

-- | alDisable( ALenum capability ); 
foreign import ccall unsafe "alDisable" alDisable :: ALenum -> IO () 

-- | ALboolean alIsEnabled( ALenum capability ); 
foreign import ccall unsafe "alIsEnabled" alIsEnabled :: ALenum -> IO ALboolean 

-- | ALboolean alIsExtensionPresent( const ALchar* extname );
foreign import ccall unsafe "alIsExtensionPresent" alIsExtensionPresent 
    :: Ptr ALchar -> IO ALboolean

-- | void* alGetProcAddress( const ALchar* fname );
foreign import ccall unsafe "alGetProcAddress" alGetProcAddress 
    :: Ptr ALchar -> IO (Ptr ())

-- | ALenum alGetEnumValue( const ALchar* ename );
foreign import ccall unsafe "alGetEnumValue" alGetEnumValue 
    :: Ptr ALchar -> IO ALenum

-- | alListenerf( ALenum param, ALfloat value );
foreign import ccall unsafe "alListenerf" alListenerf 
    :: ALenum -> ALfloat -> IO ()

-- | alListener3f( ALenum param, ALfloat value1, ALfloat value2, ALfloat value3 );
foreign import ccall unsafe "alListener3f" alListener3f 
    :: ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()

-- | alListenerfv( ALenum param, const ALfloat* values ); 
foreign import ccall unsafe "alListenerfv" alListenerfv 
    :: ALenum -> Ptr ALfloat -> IO ()

-- | alListeneri( ALenum param, ALint value );
foreign import ccall unsafe "alListeneri" alListeneri 
    :: ALenum -> ALint -> IO ()

-- | alListener3i( ALenum param, ALint value1, ALint value2, ALint value3 );
foreign import ccall unsafe "alListener3i" alListener3i 
    :: ALenum -> ALint -> ALint -> ALint -> IO ()

-- | alListeneriv( ALenum param, const ALint* values );
foreign import ccall unsafe "alListeneriv" alListeneriv 
    :: ALenum -> Ptr ALint -> IO ()

-- | alGetListenerf( ALenum param, ALfloat* value );
foreign import ccall unsafe "alGetListenerf" alGetListenerf 
    :: ALenum -> Ptr ALfloat -> IO ()

-- | alGetListener3f( ALenum param, ALfloat *value1, ALfloat *value2, ALfloat *value3 );
foreign import ccall unsafe "alGetListener3f" alGetListener3f 
    :: ALenum -> Ptr ALfloat -> Ptr ALfloat -> Ptr ALfloat -> IO ()

-- | alGetListenerfv( ALenum param, ALfloat* values );
foreign import ccall unsafe "alGetListenerfv" alGetListenerfv 
    :: ALenum -> Ptr ALfloat -> IO ()

-- | alGetListeneri( ALenum param, ALint* value );
foreign import ccall unsafe "alGetListeneri" alGetListeneri 
    :: ALenum -> Ptr ALint -> IO ()

-- | alGetListener3i( ALenum param, ALint *value1, ALint *value2, ALint *value3 );
foreign import ccall unsafe "alGetListener3i" alGetListener3i 
    :: ALenum -> Ptr ALint -> Ptr ALint -> Ptr ALint -> IO ()

-- | alGetListeneriv( ALenum param, ALint* values );
foreign import ccall unsafe "alGetListeneriv" alGetListeneriv 
    :: ALenum -> Ptr ALint -> IO ()

-- | alGenSources( ALsizei n, ALuint* sources ); 
foreign import ccall unsafe "alGenSources" alGenSources 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | alDeleteSources( ALsizei n, const ALuint* sources );
foreign import ccall unsafe "alDeleteSources" alDeleteSources 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | ALboolean alIsSource( ALuint sid ); 
foreign import ccall unsafe "alIsSource" alIsSource 
    :: ALuint -> IO ALboolean

-- | alSourcef( ALuint sid, ALenum param, ALfloat value ); 
foreign import ccall unsafe "alSourcef" alSourcef 
    :: ALuint -> ALenum -> ALfloat -> IO ()

-- | alSource3f( ALuint sid, ALenum param, ALfloat value1, ALfloat value2, ALfloat value3 );
foreign import ccall unsafe "alSource3f" alSource3f 
    :: ALuint -> ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()

-- | alSourcefv( ALuint sid, ALenum param, const ALfloat* values ); 
foreign import ccall unsafe "alSourcefv" alSourcefv
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alSourcei( ALuint sid, ALenum param, ALint value ); 
foreign import ccall unsafe "alSourcei" alSourcei 
    :: ALuint -> ALenum -> ALint -> IO ()

-- | alGetSourcefv( ALuint sid, ALenum param, ALfloat* values );
foreign import ccall unsafe "alGetSourcefv" alGetSourcefv 
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alSource3i( ALuint sid, ALenum param, ALint value1, ALint value2, ALint value3 );
foreign import ccall unsafe "alSource3i" alSource3i 
    :: ALuint -> ALenum -> ALint -> ALint -> ALint -> IO ()

-- | alSourceiv( ALuint sid, ALenum param, const ALint* values );
foreign import ccall unsafe "alSourceiv" alSourceiv 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alGetSourcef( ALuint sid, ALenum param, ALfloat* value );
foreign import ccall unsafe "alGetSourcef" alGetSourcef 
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alGetSource3f( ALuint sid, ALenum param, ALfloat* value1, ALfloat* value2, ALfloat* value3);
foreign import ccall unsafe "alGetSource3f" alGetSource3f 
    :: ALuint -> ALenum -> Ptr ALfloat -> Ptr ALfloat -> Ptr ALfloat -> IO ()

-- | alGetSourcei( ALuint sid,  ALenum param, ALint* value );
foreign import ccall unsafe "alGetSourcei" alGetSourcei 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alGetSource3i( ALuint sid, ALenum param, ALint* value1, ALint* value2, ALint* value3);
foreign import ccall unsafe "alGetSource3i" alGetSource3i 
    :: ALuint -> ALenum -> Ptr ALint -> Ptr ALint -> Ptr ALint -> IO ()

-- | alGetSourceiv( ALuint sid,  ALenum param, ALint* values );
foreign import ccall unsafe "alGetSourceiv" alGetSourceiv 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alSourcePlayv( ALsizei ns, const ALuint *sids );
foreign import ccall unsafe "alSourcePlayv" alSourcePlayv 
    ::  ALsizei -> Ptr ALuint -> IO ()

-- | alSourceStopv( ALsizei ns, const ALuint *sids );
foreign import ccall unsafe "alSourceStopv" alSourceStopv 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | alSourceRewindv( ALsizei ns, const ALuint *sids );
foreign import ccall unsafe "alSourceRewindv" alSourceRewindv 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | alSourcePausev( ALsizei ns, const ALuint *sids );
foreign import ccall unsafe "alSourcePausev" alSourcePausev 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | alSourcePlay( ALuint sid );
foreign import ccall unsafe "alSourcePlay" alSourcePlay 
    :: ALuint -> IO ()

-- | alSourceStop( ALuint sid );
foreign import ccall unsafe "alSourceStop" alSourceStop 
    :: ALuint -> IO ()

-- | alSourceRewind( ALuint sid );
foreign import ccall unsafe "alSourceRewind" alSourceRewind 
    :: ALuint -> IO ()

-- | alSourcePause( ALuint sid );
foreign import ccall unsafe "alSourcePause" alSourcePause 
    :: ALuint -> IO ()

-- | alSourceQueueBuffers( ALuint sid, ALsizei numEntries, const ALuint *bids );
foreign import ccall unsafe "alSourceQueueBuffers" alSourceQueueBuffers 
    :: ALuint -> ALsizei -> Ptr ALuint -> IO ()

-- | alGenBuffers( ALsizei n, ALuint* buffers );
foreign import ccall unsafe "alGenBuffers" alGenBuffers 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | alSourceUnqueueBuffers( ALuint sid, ALsizei numEntries, ALuint *bids );
foreign import ccall unsafe "alSourceUnqueueBuffers" alSourceUnqueueBuffers 
    :: ALuint -> ALsizei -> Ptr ALuint -> IO ()

-- | alDeleteBuffers( ALsizei n, const ALuint* buffers );
foreign import ccall unsafe "alDeleteBuffers" alDeleteBuffers 
    :: ALsizei -> Ptr ALuint -> IO ()

-- | ALboolean alIsBuffer( ALuint bid );
foreign import ccall unsafe "alIsBuffer" alIsBuffer 
    :: ALuint -> IO ALboolean

-- | alBufferData( ALuint bid, ALenum format, const ALvoid* data, ALsizei size, ALsizei freq );
foreign import ccall unsafe "alBufferData" alBufferData 
    :: ALuint -> ALenum -> Ptr ALvoid -> ALsizei -> ALsizei -> IO ()

-- | alBufferf( ALuint bid, ALenum param, ALfloat value );
foreign import ccall unsafe "alBufferf" alBufferf 
    :: ALuint -> ALenum -> ALfloat -> IO ()

-- | alBuffer3f( ALuint bid, ALenum param, ALfloat value1, ALfloat value2, ALfloat value3 );
foreign import ccall unsafe "alBuffer3f" alBuffer3f 
    :: ALuint -> ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()

-- | alBufferfv( ALuint bid, ALenum param, const ALfloat* values );
foreign import ccall unsafe "alBufferfv" alBufferfv 
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alBufferi( ALuint bid, ALenum param, ALint value );
foreign import ccall unsafe "alBufferi" alBufferi 
    :: ALuint -> ALenum -> ALint -> IO ()

-- | alBuffer3i( ALuint bid, ALenum param, ALint value1, ALint value2, ALint value3 );
foreign import ccall unsafe "alBuffer3i" alBuffer3i
    :: ALuint -> ALenum -> ALint -> ALint -> ALint -> IO ()

-- | alBufferiv( ALuint bid, ALenum param, const ALint* values );
foreign import ccall unsafe "alBufferiv" alBufferiv 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alGetBufferf( ALuint bid, ALenum param, ALfloat* value );
foreign import ccall unsafe "alGetBufferf" alGetBufferf 
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alGetBuffer3f( ALuint bid, ALenum param, ALfloat* value1, ALfloat* value2, ALfloat* value3);
foreign import ccall unsafe "alGetBuffer3f" alGetBuffer3f 
    :: ALuint -> ALenum -> Ptr ALfloat -> Ptr ALfloat -> Ptr ALfloat -> IO ()

-- | alGetBufferfv( ALuint bid, ALenum param, ALfloat* values );
foreign import ccall unsafe "alGetBufferfv" alGetBufferfv 
    :: ALuint -> ALenum -> Ptr ALfloat -> IO ()

-- | alGetBufferi( ALuint bid, ALenum param, ALint* value );
foreign import ccall unsafe "alGetBufferi" alGetBufferi 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alGetBuffer3i( ALuint bid, ALenum param, ALint* value1, ALint* value2, ALint* value3);
foreign import ccall unsafe "alGetBuffer3i" alGetBuffer3i 
    :: ALuint -> ALenum -> Ptr ALint -> Ptr ALint -> Ptr ALint -> IO ()

-- | alGetBufferiv( ALuint bid, ALenum param, ALint* values );
foreign import ccall unsafe "alGetBufferiv" alGetBufferiv 
    :: ALuint -> ALenum -> Ptr ALint -> IO ()

-- | alDopplerFactor( ALfloat value );
foreign import ccall unsafe "alDopplerFactor" alDopplerFactor 
    :: ALfloat -> IO ()

-- | alDopplerVelocity( ALfloat value );
foreign import ccall unsafe "alDopplerVelocity" alDopplerVelocity 
    :: ALfloat -> IO ()

-- | alSpeedOfSound( ALfloat value );
foreign import ccall unsafe "alSpeedOfSound" alSpeedOfSound 
    :: ALfloat -> IO ()

-- | alDistanceModel( ALenum distanceModel );
foreign import ccall unsafe "alDistanceModel" alDistanceModel 
    :: ALenum -> IO ()

