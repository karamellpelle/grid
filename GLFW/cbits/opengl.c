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
#include <stdio.h>
#include <stdlib.h>
#include <png.h>
#include <assert.h>

// print debug info from glfw_loadTexPreMult?
//#define DEBUG_LOADTEXPREMULT


// loading a 2D texture of type 'target' into current bound texture.
// colors in the textures will be represented by premultiplicated alpha.
// this is good for textures used as images, but not for textures used as 
// data (4-tuples). 
// only premultiplied png images is because iOS forces us into this, 
// and this application is originally buildt for GRID_PLATFORM_IOS
//
// TODO: premultiply the alpha values here too
// TODO: verify 'path' is a valid png file
//
// for discussion of premultiplied alpha, see
// * http://home.comcast.net/~tom_forsyth/blog.wiki.html#[[Premultiplied%20alpha]]
// * http://keithp.com/~keithp/porterduff/p253-porter.pdf
// * http://stackoverflow.com/questions/13336241/loading-4-channel-texture-data-in-ios
//
// this code is based upon
// * https://gist.github.com/niw/5963798
// * https://blog.nobel-joergensen.com/2010/11/07/loading-a-png-as-texture-in-opengl-using-libpng/
// * http://www.libpng.org/pub/png/libpng-manual.txt
//
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
uint glfw_loadTexPreMult(GLenum target, const char* path, GLuint* wth, GLuint* hth, GLenum intfmt)
{
    // (assuming tex is bound)
#if DEBUG_LOADTEXPREMULT
    printf("glfw_loadTexPreMult: loading file %s\n", path);
#endif

    FILE *fp = fopen( path, "rb");

    // make sure file is a png file
    if (!fp)
    {
#if DEBUG_LOADTEXPREMULT
       printf("glfw_loadTexPreMult: Error: no fp\n\n");
#endif
        return false;
    }

// we assume this is a valid png file
/*
// how many bytes in file to check that this is in fact an PNG
#define SIGNATURE_READS 8
    if (fread(header, 1, SIGNATURE_READS, fp) != SIGNATURE_READS)
    {
        fclose( fp );
        return false;
    }
    bool is_png = !png_sig_cmp(header, 0, SIGNATURE_READS);
    if (!is_png)
    {
        fclose( fp );
        return false;
    }
*/
    // create data structs
    extern void user_warning_fn(png_structp png_ptr, png_const_charp warning_msg);
    png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, user_warning_fn ); // NULL for the error reports
    if(!png)
    {
#if DEBUG_LOADTEXPREMULT
        printf("glfw_loadTexPreMult: Error: could not create read struct\n\n");
#endif
        return false;
    }
    png_infop info = png_create_info_struct(png);
    if(!info)
    { 
#if DEBUG_LOADTEXPREMULT
        printf("glfw_loadTexPreMult: Error: could not create info struct\n\n");
#endif
        png_destroy_read_struct(&png, (png_infopp)NULL, (png_infopp)NULL); 
        return false; 
    }

    // setup file IO
    if ( setjmp( png_jmpbuf( png ) ) )
    {
#if DEBUG_LOADTEXPREMULT
        printf("glfw_loadTexPreMult: Error: could not setjmp\n\n");
#endif
       png_destroy_read_struct(&png, &info, NULL );
       fclose(fp);
       return false;
    }
    png_init_io(png, fp);


// we assume this is a valid png file
/*
    png_set_sig_bytes(png_ptr, SIGNATURE_READS);
*/

    // read PNG info
    png_read_info(png, info);
    int width      = png_get_image_width(png, info);
    int height     = png_get_image_height(png, info);
    png_byte color_type = png_get_color_type(png, info);
    png_byte bit_depth  = png_get_bit_depth(png, info);

    // remember to write these
    *wth = width;
    *hth = height;
    
#if DEBUG_LOADTEXPREMULT
        printf("glfw_loadTexPreMult: \n");
        printf("    width:      %u\n", width);
        printf("    height:     %u\n", height );
        printf("    color_type: 0x%02x\n", color_type );
        printf("    bit_depth:  0x%02x\n", bit_depth );
#endif
    ////////////////////////////////////////////////////////////////////////////////
    // convert any format into RGBA with 1 byte per channel

    if(bit_depth == 16)
      png_set_strip_16(png);
    if(color_type == PNG_COLOR_TYPE_PALETTE)
      png_set_palette_to_rgb(png);
    // PNG_COLOR_TYPE_GRAY_ALPHA is always 8 or 16bit depth.
    if(color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
      png_set_expand_gray_1_2_4_to_8(png);
    if(png_get_valid(png, info, PNG_INFO_tRNS))
      png_set_tRNS_to_alpha(png);
    // These color_type don't have an alpha channel then fill it with 0xff.
    if(color_type == PNG_COLOR_TYPE_RGB ||
       color_type == PNG_COLOR_TYPE_GRAY ||
       color_type == PNG_COLOR_TYPE_PALETTE)
      png_set_filler(png, 0xFF, PNG_FILLER_AFTER);
    if(color_type == PNG_COLOR_TYPE_GRAY ||
       color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
      png_set_gray_to_rgb(png);

    
    ////////////////////////////////////////////////////////////////////////////////
    // now get pointer to data

    // read new info
    png_read_update_info(png, info);

    // create data to hold all RGBA values
    size_t row_bytes = png_get_rowbytes( png, info );
#if DEBUG_LOADTEXPREMULT
    printf("glfw_loadTexPreMult: allocating %u bytes for each row.\n", row_bytes);
#endif
    png_bytep data = (png_bytep)( malloc( row_bytes * height ) );

    // point PNG rows into 'data'
    // NOTE: PNG is ordered top to bottom, OpenGL textures bottom to top
    png_bytepp row_pointers = malloc( sizeof(png_bytep) * height );
    for (uint i = 0; i != height; ++i)
    {
        size_t j = (height - (i + 1));
        row_pointers[ j ] = data + i * row_bytes;
    }
 
    // now read PNG image RGBA  rows into 'data'. remember each channel is 1 byte.
    png_read_image(png, row_pointers);

    // and now we have to premultiply all - what a job!
    for (size_t i = 0; i != width * height; ++i)
    {
        // RGBA
        size_t j = i * 4;
        uint8_t* r = (data + j) + 0;
        uint8_t* g = (data + j) + 1;
        uint8_t* b = (data + j) + 2;
        uint8_t* a = (data + j) + 3;

        *r = (uint8_t)( ((uint16_t)( *r ) * (uint16_t)( *a )) >> 8 );
        *g = (uint8_t)( ((uint16_t)( *g ) * (uint16_t)( *a )) >> 8 );
        *b = (uint8_t)( ((uint16_t)( *b ) * (uint16_t)( *a )) >> 8 );
    }


    // read into bound texture object
#if DEBUG_LOADTEXPREMULT
    glGetError(); // clear error
#endif
    glTexImage2D( target, 0, intfmt, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data );
#if DEBUG_LOADTEXPREMULT
    printf("glfw_loadTexPreMult: glGetError(): %s\n", gl_error_string( glGetError() ) );
#endif


    // clean up
    free( row_pointers );
    free( data );
    png_destroy_read_struct( &png, &info, NULL);
    fclose(fp);
#if DEBUG_LOADTEXPREMULT
    printf("\n");
#endif

    return true;
}


char const* gl_error_string(GLenum const err)
{
  switch (err)
  {
    // opengl 2 errors (8)
    case GL_NO_ERROR:
      return "GL_NO_ERROR";

    case GL_INVALID_ENUM:
      return "GL_INVALID_ENUM";

    case GL_INVALID_VALUE:
      return "GL_INVALID_VALUE";

    case GL_INVALID_OPERATION:
      return "GL_INVALID_OPERATION";

    case GL_STACK_OVERFLOW:
      return "GL_STACK_OVERFLOW";

    case GL_STACK_UNDERFLOW:
      return "GL_STACK_UNDERFLOW";

    case GL_OUT_OF_MEMORY:
      return "GL_OUT_OF_MEMORY";

    case GL_TABLE_TOO_LARGE:
      return "GL_TABLE_TOO_LARGE";

    // opengl 3 errors (1)
    case GL_INVALID_FRAMEBUFFER_OPERATION:
      return "GL_INVALID_FRAMEBUFFER_OPERATION";

    // gles 2, 3 and gl 4 error are handled by the switch above
    default:
      assert(!"unknown error");
      return "";
  }
}

void user_warning_fn(png_structp png_ptr, png_const_charp warning_msg)
{
#if DEBUG_LOADTEXPREMULT
    printf("glfw_loadTexPreMult: PNG warning: %s\n", warning_msg );
#endif
}
