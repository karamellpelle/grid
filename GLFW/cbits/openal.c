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
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include "glfw_foreign.h"

#define DEBUG_LOADBUF

static mpg123_handle* mpg123 = 0;

void glfw_initSound()
{

    // see https://github.com/kcat/alure/blob/36844e1584408683999f38298fa99a12740619ac/src/decoders/mpg123.cpp#L128

    if ( mpg123_init() != MPG123_OK )
    {
        printf( "ERROR: (mpg123) could not initialize!\n" );
        return;
    }
    mpg123 = mpg123_new( 0, 0 );    
    if ( !mpg123 )
    {
        printf( "ERROR: (mpg123) could not create handle!\n" );
        return;
    }
    
    printf( "mpg123 handle created.\n" );
}

void glfw_deinitSound()
{
    mpg123_delete( mpg123 );
    printf( "mpg123 deleted.\n" );
}


// load mp3 file into buffer using mpg123
uint glfw_loadBuf(ALuint buf, const char* path)
{
#ifdef DEBUG_LOADBUF
    printf( "loading audio '%s'...\n", path );
#endif

    // open file
    if ( mpg123_open( mpg123, path ) != MPG123_OK )
    {
        printf( "ERROR: (mpg123) could not open file '%s'.\n", path );
        return 0;
    }
    int enc, chancount;
    long srate;
    if( mpg123_getformat( mpg123, &srate, &chancount, &enc ) != MPG123_OK )
    {
        printf( "ERROR: (mpg123) could not read format of file '%s'.\n", path );
        return 0;
    }
    if(srate < 1 || INT_MAX <= srate )
    {
        printf( "ERROR: (mpg123) sample rate %i of file '%s' is not valid.\n", srate, path );
        return 0;
    }
#ifdef DEBUG_LOADBUF
    printf( "   -> file format: sample rate %i, channels %i, enc %i \n", srate, chancount, enc );
#endif


    // set mp3 decoders output format
    // FIXME: can the format be changed during read? 
    // see https://www.mpg123.de/api/group__mpg123__output.shtml
    if( mpg123_format_none( mpg123 ) != MPG123_OK )
    {
        printf( "ERROR: (mpg123) could not clear output format for file '%s'.\n", path );
        return 0;
    }
    chancount = 1; // we want mono channel
    if ( mpg123_format( mpg123, srate, chancount, MPG123_ENC_SIGNED_16 ) != MPG123_OK )
    {
        printf( "ERROR: (mpg123) could create output format (rate %i, channels %i, MPG123_ENC_SIGNED_16) for file '%s'.\n", srate, chancount, path );
        return 0;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // decoder is Oscar Mike, let's send data to OpenAL buffer

    size_t samples = mpg123_length( mpg123 );

    size_t size = samples * 1 * 2; // samples * channels * int16_t
    uint8_t* data = (uint8_t*)( malloc( size ) );
   
#ifdef DEBUG_LOADBUF
    printf( "   -> allocated %d bytes for data\n", size );
#endif

    size_t total = 0;
    while( total < size )
    {
        size_t got = 0;
        int ret = mpg123_read( mpg123, data + total, size - total, &got );
        if ( (ret != MPG123_OK && ret != MPG123_DONE) || got == 0)
            break;

        total += got;
        if( ret == MPG123_DONE )
            break;
    }


    // populate AL buffer
    alBufferData( buf, AL_FORMAT_MONO16, data, size, srate );

    // free data
    free( data );

    // close current file
    mpg123_close( mpg123 );

#ifdef DEBUG_LOADBUF
    printf( "   -> OK!\n", size );
#endif

    // buffer loaded
    return 1;

}
