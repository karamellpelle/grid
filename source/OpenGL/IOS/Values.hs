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
module OpenGL.IOS.Values where

import OpenGL.IOS.Types


gl_DEPTH_BUFFER_BIT :: GLbitfield
gl_DEPTH_BUFFER_BIT =             0x00000100

gl_STENCIL_BUFFER_BIT :: GLbitfield
gl_STENCIL_BUFFER_BIT =           0x00000400

gl_COLOR_BUFFER_BIT :: GLbitfield
gl_COLOR_BUFFER_BIT =             0x00004000

gl_FALSE :: GLboolean
gl_FALSE =                        0

gl_TRUE :: GLboolean
gl_TRUE =                         1

gl_POINTS :: GLenum
gl_POINTS =                       0x0000

gl_LINES :: GLenum
gl_LINES =                        0x0001

gl_LINE_LOOP :: GLenum
gl_LINE_LOOP =                    0x0002

gl_LINE_STRIP :: GLenum
gl_LINE_STRIP =                   0x0003

gl_TRIANGLES :: GLenum
gl_TRIANGLES =                    0x0004

gl_TRIANGLE_STRIP :: GLenum
gl_TRIANGLE_STRIP =               0x0005

gl_TRIANGLE_FAN :: GLenum
gl_TRIANGLE_FAN =                 0x0006

gl_ZERO :: GLenum
gl_ZERO =                         0

gl_ONE :: GLenum
gl_ONE =                          1

gl_SRC_COLOR :: GLenum
gl_SRC_COLOR =                    0x0300

gl_ONE_MINUS_SRC_COLOR :: GLenum
gl_ONE_MINUS_SRC_COLOR =          0x0301

gl_SRC_ALPHA :: GLenum
gl_SRC_ALPHA =                    0x0302

gl_ONE_MINUS_SRC_ALPHA :: GLenum
gl_ONE_MINUS_SRC_ALPHA =          0x0303

gl_DST_ALPHA :: GLenum
gl_DST_ALPHA =                    0x0304

gl_ONE_MINUS_DST_ALPHA :: GLenum
gl_ONE_MINUS_DST_ALPHA =          0x0305

gl_DST_COLOR :: GLenum
gl_DST_COLOR =                    0x0306

gl_ONE_MINUS_DST_COLOR :: GLenum
gl_ONE_MINUS_DST_COLOR =          0x0307

gl_SRC_ALPHA_SATURATE :: GLenum
gl_SRC_ALPHA_SATURATE =           0x0308

gl_FUNC_ADD :: GLenum
gl_FUNC_ADD =                     0x8006

gl_BLEND_EQUATION :: GLenum
gl_BLEND_EQUATION =               0x8009

gl_BLEND_EQUATION_ALPHA :: GLenum
gl_BLEND_EQUATION_ALPHA =         0x883D

gl_FUNC_SUBTRACT :: GLenum
gl_FUNC_SUBTRACT =                0x800A

gl_FUNC_REVERSE_SUBTRACT :: GLenum
gl_FUNC_REVERSE_SUBTRACT =        0x800B

gl_BLEND_DST_RGB :: GLenum
gl_BLEND_DST_RGB =                0x80C8

gl_BLEND_SRC_RGB :: GLenum
gl_BLEND_SRC_RGB =                0x80C9

gl_BLEND_EQUATION_RGB :: GLenum
gl_BLEND_EQUATION_RGB =           0x8009
 
gl_BLEND_DST_ALPHA :: GLenum
gl_BLEND_DST_ALPHA =              0x80CA

gl_BLEND_SRC_ALPHA :: GLenum
gl_BLEND_SRC_ALPHA =              0x80CB

gl_CONSTANT_COLOR :: GLenum
gl_CONSTANT_COLOR =               0x8001

gl_ONE_MINUS_CONSTANT_COLOR :: GLenum
gl_ONE_MINUS_CONSTANT_COLOR =     0x8002

gl_CONSTANT_ALPHA :: GLenum
gl_CONSTANT_ALPHA =               0x8003

gl_ONE_MINUS_CONSTANT_ALPHA :: GLenum
gl_ONE_MINUS_CONSTANT_ALPHA =     0x8004

gl_BLEND_COLOR :: GLenum
gl_BLEND_COLOR =                  0x8005

gl_ARRAY_BUFFER :: GLenum
gl_ARRAY_BUFFER =                 0x8892

gl_ELEMENT_ARRAY_BUFFER :: GLenum
gl_ELEMENT_ARRAY_BUFFER =         0x8893

gl_ARRAY_BUFFER_BINDING :: GLenum
gl_ARRAY_BUFFER_BINDING =         0x8894

gl_ELEMENT_ARRAY_BUFFER_BINDING :: GLenum
gl_ELEMENT_ARRAY_BUFFER_BINDING = 0x8895

gl_STREAM_DRAW :: GLenum
gl_STREAM_DRAW =                  0x88E0

gl_STATIC_DRAW :: GLenum
gl_STATIC_DRAW =                  0x88E4

gl_DYNAMIC_DRAW :: GLenum
gl_DYNAMIC_DRAW =                 0x88E8

gl_BUFFER_SIZE :: GLenum
gl_BUFFER_SIZE =                  0x8764

gl_BUFFER_USAGE :: GLenum
gl_BUFFER_USAGE =                 0x8765

gl_CURRENT_VERTEX_ATTRIB :: GLenum
gl_CURRENT_VERTEX_ATTRIB =        0x8626

gl_FRONT :: GLenum
gl_FRONT =                        0x0404

gl_BACK :: GLenum
gl_BACK =                         0x0405

gl_FRONT_AND_BACK :: GLenum
gl_FRONT_AND_BACK =               0x0408

gl_TEXTURE_2D :: GLenum
gl_TEXTURE_2D =                   0x0DE1

gl_CULL_FACE :: GLenum
gl_CULL_FACE =                    0x0B44

gl_BLEND :: GLenum
gl_BLEND =                        0x0BE2

gl_DITHER :: GLenum
gl_DITHER =                       0x0BD0

gl_STENCIL_TEST :: GLenum
gl_STENCIL_TEST =                 0x0B90

gl_DEPTH_TEST :: GLenum
gl_DEPTH_TEST =                   0x0B71

gl_SCISSOR_TEST :: GLenum
gl_SCISSOR_TEST =                 0x0C11

gl_POLYGON_OFFSET_FILL :: GLenum
gl_POLYGON_OFFSET_FILL =          0x8037

gl_SAMPLE_ALPHA_TO_COVERAGE :: GLenum
gl_SAMPLE_ALPHA_TO_COVERAGE =     0x809E

gl_SAMPLE_COVERAGE :: GLenum
gl_SAMPLE_COVERAGE =              0x80A0

gl_NO_ERROR :: GLenum
gl_NO_ERROR =                     0

gl_INVALID_ENUM :: GLenum
gl_INVALID_ENUM =                 0x0500

gl_INVALID_VALUE :: GLenum
gl_INVALID_VALUE =                0x0501

gl_INVALID_OPERATION :: GLenum
gl_INVALID_OPERATION =            0x0502

gl_OUT_OF_MEMORY :: GLenum
gl_OUT_OF_MEMORY =                0x0505

gl_CW :: GLenum
gl_CW =                           0x0900

gl_CCW :: GLenum
gl_CCW =                          0x0901

gl_LINE_WIDTH :: GLenum
gl_LINE_WIDTH =                   0x0B21

gl_ALIASED_POINT_SIZE_RANGE :: GLenum
gl_ALIASED_POINT_SIZE_RANGE =     0x846D

gl_ALIASED_LINE_WIDTH_RANGE :: GLenum
gl_ALIASED_LINE_WIDTH_RANGE =     0x846E

gl_CULL_FACE_MODE :: GLenum
gl_CULL_FACE_MODE =               0x0B45

gl_FRONT_FACE :: GLenum
gl_FRONT_FACE =                   0x0B46

gl_DEPTH_RANGE :: GLenum
gl_DEPTH_RANGE =                  0x0B70

gl_DEPTH_WRITEMASK :: GLenum
gl_DEPTH_WRITEMASK =              0x0B72

gl_DEPTH_CLEAR_VALUE :: GLenum
gl_DEPTH_CLEAR_VALUE =            0x0B73

gl_DEPTH_FUNC :: GLenum
gl_DEPTH_FUNC =                   0x0B74

gl_STENCIL_CLEAR_VALUE :: GLenum
gl_STENCIL_CLEAR_VALUE =          0x0B91

gl_STENCIL_FUNC :: GLenum
gl_STENCIL_FUNC =                 0x0B92

gl_STENCIL_FAIL :: GLenum
gl_STENCIL_FAIL =                 0x0B94

gl_STENCIL_PASS_DEPTH_FAIL :: GLenum
gl_STENCIL_PASS_DEPTH_FAIL =      0x0B95

gl_STENCIL_PASS_DEPTH_PASS :: GLenum
gl_STENCIL_PASS_DEPTH_PASS =      0x0B96

gl_STENCIL_REF :: GLenum
gl_STENCIL_REF =                  0x0B97

gl_STENCIL_VALUE_MASK :: GLenum
gl_STENCIL_VALUE_MASK =           0x0B93

gl_STENCIL_WRITEMASK :: GLenum
gl_STENCIL_WRITEMASK =            0x0B98

gl_STENCIL_BACK_FUNC :: GLenum
gl_STENCIL_BACK_FUNC =            0x8800

gl_STENCIL_BACK_FAIL :: GLenum
gl_STENCIL_BACK_FAIL =            0x8801

gl_STENCIL_BACK_PASS_DEPTH_FAIL :: GLenum
gl_STENCIL_BACK_PASS_DEPTH_FAIL = 0x8802

gl_STENCIL_BACK_PASS_DEPTH_PASS :: GLenum
gl_STENCIL_BACK_PASS_DEPTH_PASS = 0x8803

gl_STENCIL_BACK_REF :: GLenum
gl_STENCIL_BACK_REF =             0x8CA3

gl_STENCIL_BACK_VALUE_MASK :: GLenum
gl_STENCIL_BACK_VALUE_MASK =      0x8CA4

gl_STENCIL_BACK_WRITEMASK :: GLenum
gl_STENCIL_BACK_WRITEMASK =       0x8CA5

gl_VIEWPORT :: GLenum
gl_VIEWPORT =                     0x0BA2

gl_SCISSOR_BOX :: GLenum
gl_SCISSOR_BOX =                  0x0C10

gl_COLOR_CLEAR_VALUE :: GLenum
gl_COLOR_CLEAR_VALUE =            0x0C22

gl_COLOR_WRITEMASK :: GLenum
gl_COLOR_WRITEMASK =              0x0C23

gl_UNPACK_ALIGNMENT :: GLenum
gl_UNPACK_ALIGNMENT =             0x0CF5

gl_PACK_ALIGNMENT :: GLenum
gl_PACK_ALIGNMENT =               0x0D05

gl_MAX_TEXTURE_SIZE :: GLenum
gl_MAX_TEXTURE_SIZE =             0x0D33

gl_MAX_VIEWPORT_DIMS :: GLenum
gl_MAX_VIEWPORT_DIMS =            0x0D3A

gl_SUBPIXEL_BITS :: GLenum
gl_SUBPIXEL_BITS =                0x0D50

gl_RED_BITS :: GLenum
gl_RED_BITS =                     0x0D52

gl_GREEN_BITS :: GLenum
gl_GREEN_BITS =                   0x0D53

gl_BLUE_BITS :: GLenum
gl_BLUE_BITS =                    0x0D54

gl_ALPHA_BITS :: GLenum
gl_ALPHA_BITS =                   0x0D55

gl_DEPTH_BITS :: GLenum
gl_DEPTH_BITS =                   0x0D56

gl_STENCIL_BITS :: GLenum
gl_STENCIL_BITS =                 0x0D57

gl_POLYGON_OFFSET_UNITS :: GLenum
gl_POLYGON_OFFSET_UNITS =         0x2A00

gl_POLYGON_OFFSET_FACTOR :: GLenum
gl_POLYGON_OFFSET_FACTOR =        0x8038

gl_TEXTURE_BINDING_2D :: GLenum
gl_TEXTURE_BINDING_2D =           0x8069

gl_SAMPLE_BUFFERS :: GLenum
gl_SAMPLE_BUFFERS =               0x80A8

gl_SAMPLES :: GLenum
gl_SAMPLES =                      0x80A9

gl_SAMPLE_COVERAGE_VALUE :: GLenum
gl_SAMPLE_COVERAGE_VALUE =        0x80AA

gl_SAMPLE_COVERAGE_INVERT :: GLenum
gl_SAMPLE_COVERAGE_INVERT =       0x80AB

gl_NUM_COMPRESSED_TEXTURE_FORMATS :: GLenum
gl_NUM_COMPRESSED_TEXTURE_FORMATS = 0x86A2

gl_COMPRESSED_TEXTURE_FORMATS :: GLenum
gl_COMPRESSED_TEXTURE_FORMATS =   0x86A3

gl_DONT_CARE :: GLenum
gl_DONT_CARE =                    0x1100

gl_FASTEST :: GLenum
gl_FASTEST =                      0x1101

gl_NICEST :: GLenum
gl_NICEST =                       0x1102

gl_GENERATE_MIPMAP_HINT :: GLenum
gl_GENERATE_MIPMAP_HINT =         0x8192

gl_BYTE :: GLenum
gl_BYTE =                         0x1400

gl_UNSIGNED_BYTE :: GLenum
gl_UNSIGNED_BYTE =                0x1401

gl_SHORT :: GLenum
gl_SHORT =                        0x1402

gl_UNSIGNED_SHORT :: GLenum
gl_UNSIGNED_SHORT =               0x1403

gl_INT :: GLenum
gl_INT =                          0x1404

gl_UNSIGNED_INT :: GLenum
gl_UNSIGNED_INT =                 0x1405

gl_FLOAT :: GLenum
gl_FLOAT =                        0x1406

gl_FIXED :: GLenum
gl_FIXED =                        0x140C

gl_DEPTH_COMPONENT :: GLenum
gl_DEPTH_COMPONENT =              0x1902

gl_ALPHA :: GLenum
gl_ALPHA =                        0x1906

gl_RGB :: GLenum
gl_RGB =                          0x1907

gl_RGBA :: GLenum
gl_RGBA =                         0x1908

gl_LUMINANCE :: GLenum
gl_LUMINANCE =                    0x1909

gl_LUMINANCE_ALPHA :: GLenum
gl_LUMINANCE_ALPHA =              0x190A

gl_UNSIGNED_SHORT_4_4_4_4 :: GLenum
gl_UNSIGNED_SHORT_4_4_4_4 =       0x8033

gl_UNSIGNED_SHORT_5_5_5_1 :: GLenum
gl_UNSIGNED_SHORT_5_5_5_1 =       0x8034

gl_UNSIGNED_SHORT_5_6_5 :: GLenum
gl_UNSIGNED_SHORT_5_6_5 =         0x8363

gl_FRAGMENT_SHADER :: GLenum
gl_FRAGMENT_SHADER =              0x8B30

gl_VERTEX_SHADER :: GLenum
gl_VERTEX_SHADER =                0x8B31

gl_SHADER_TYPE :: GLenum
gl_SHADER_TYPE =                  0x8B4F

gl_DELETE_STATUS :: GLenum
gl_DELETE_STATUS =                0x8B80

gl_LINK_STATUS :: GLenum
gl_LINK_STATUS =                  0x8B82

gl_VALIDATE_STATUS :: GLenum
gl_VALIDATE_STATUS =              0x8B83

gl_ATTACHED_SHADERS :: GLenum
gl_ATTACHED_SHADERS =             0x8B85

gl_ACTIVE_UNIFORMS :: GLenum
gl_ACTIVE_UNIFORMS =              0x8B86

gl_ACTIVE_UNIFORM_MAX_LENGTH :: GLenum
gl_ACTIVE_UNIFORM_MAX_LENGTH =    0x8B87

gl_ACTIVE_ATTRIBUTES :: GLenum
gl_ACTIVE_ATTRIBUTES =            0x8B89

gl_ACTIVE_ATTRIBUTE_MAX_LENGTH :: GLenum
gl_ACTIVE_ATTRIBUTE_MAX_LENGTH =  0x8B8A

gl_SHADING_LANGUAGE_VERSION :: GLenum
gl_SHADING_LANGUAGE_VERSION =     0x8B8C

gl_CURRENT_PROGRAM :: GLenum
gl_CURRENT_PROGRAM =              0x8B8D

gl_NEVER :: GLenum
gl_NEVER =                        0x0200

gl_LESS :: GLenum
gl_LESS =                         0x0201

gl_EQUAL :: GLenum
gl_EQUAL =                        0x0202

gl_LEQUAL :: GLenum
gl_LEQUAL =                       0x0203

gl_GREATER :: GLenum
gl_GREATER =                      0x0204

gl_NOTEQUAL :: GLenum
gl_NOTEQUAL =                     0x0205

gl_GEQUAL :: GLenum
gl_GEQUAL =                       0x0206

gl_ALWAYS :: GLenum
gl_ALWAYS =                       0x0207

gl_KEEP :: GLenum
gl_KEEP =                         0x1E00

gl_REPLACE :: GLenum
gl_REPLACE =                      0x1E01

gl_INCR :: GLenum
gl_INCR =                         0x1E02

gl_DECR :: GLenum
gl_DECR =                         0x1E03

gl_INVERT :: GLenum
gl_INVERT =                       0x150A

gl_INCR_WRAP :: GLenum
gl_INCR_WRAP =                    0x8507

gl_DECR_WRAP :: GLenum
gl_DECR_WRAP =                    0x8508

gl_VENDOR :: GLenum
gl_VENDOR =                       0x1F00

gl_RENDERER :: GLenum
gl_RENDERER =                     0x1F01

gl_VERSION :: GLenum
gl_VERSION =                      0x1F02

gl_EXTENSIONS :: GLenum
gl_EXTENSIONS =                   0x1F03

gl_NEAREST :: GLenum
gl_NEAREST =                      0x2600

gl_LINEAR :: GLenum
gl_LINEAR =                       0x2601

gl_NEAREST_MIPMAP_NEAREST :: GLenum
gl_NEAREST_MIPMAP_NEAREST =       0x2700

gl_LINEAR_MIPMAP_NEAREST :: GLenum
gl_LINEAR_MIPMAP_NEAREST =        0x2701

gl_NEAREST_MIPMAP_LINEAR :: GLenum
gl_NEAREST_MIPMAP_LINEAR =        0x2702

gl_LINEAR_MIPMAP_LINEAR :: GLenum
gl_LINEAR_MIPMAP_LINEAR =         0x2703

gl_TEXTURE_MAG_FILTER :: GLenum
gl_TEXTURE_MAG_FILTER =           0x2800

gl_TEXTURE_MIN_FILTER :: GLenum
gl_TEXTURE_MIN_FILTER =           0x2801

gl_TEXTURE_WRAP_S :: GLenum
gl_TEXTURE_WRAP_S =               0x2802

gl_TEXTURE_WRAP_T :: GLenum
gl_TEXTURE_WRAP_T =               0x2803

gl_TEXTURE :: GLenum
gl_TEXTURE =                      0x1702

gl_TEXTURE_CUBE_MAP :: GLenum
gl_TEXTURE_CUBE_MAP =             0x8513

gl_TEXTURE_BINDING_CUBE_MAP :: GLenum
gl_TEXTURE_BINDING_CUBE_MAP =     0x8514

gl_TEXTURE_CUBE_MAP_POSITIVE_X :: GLenum
gl_TEXTURE_CUBE_MAP_POSITIVE_X =  0x8515

gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: GLenum
gl_TEXTURE_CUBE_MAP_NEGATIVE_X =  0x8516

gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: GLenum
gl_TEXTURE_CUBE_MAP_POSITIVE_Y =  0x8517

gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: GLenum
gl_TEXTURE_CUBE_MAP_NEGATIVE_Y =  0x8518

gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: GLenum
gl_TEXTURE_CUBE_MAP_POSITIVE_Z =  0x8519

gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: GLenum
gl_TEXTURE_CUBE_MAP_NEGATIVE_Z =  0x851A

gl_MAX_CUBE_MAP_TEXTURE_SIZE :: GLenum
gl_MAX_CUBE_MAP_TEXTURE_SIZE =    0x851C

gl_TEXTURE0 :: GLenum
gl_TEXTURE0 =                     0x84C0

gl_TEXTURE1 :: GLenum
gl_TEXTURE1 =                     0x84C1

gl_TEXTURE2 :: GLenum
gl_TEXTURE2 =                     0x84C2

gl_TEXTURE3 :: GLenum
gl_TEXTURE3 =                     0x84C3

gl_TEXTURE4 :: GLenum
gl_TEXTURE4 =                     0x84C4

gl_TEXTURE5 :: GLenum
gl_TEXTURE5 =                     0x84C5

gl_TEXTURE6 :: GLenum
gl_TEXTURE6 =                     0x84C6

gl_TEXTURE7 :: GLenum
gl_TEXTURE7 =                     0x84C7

gl_TEXTURE8 :: GLenum
gl_TEXTURE8 =                     0x84C8

gl_TEXTURE9 :: GLenum
gl_TEXTURE9 =                     0x84C9

gl_TEXTURE10 :: GLenum
gl_TEXTURE10 =                    0x84CA

gl_TEXTURE11 :: GLenum
gl_TEXTURE11 =                    0x84CB

gl_TEXTURE12 :: GLenum
gl_TEXTURE12 =                    0x84CC

gl_TEXTURE13 :: GLenum
gl_TEXTURE13 =                    0x84CD

gl_TEXTURE14 :: GLenum
gl_TEXTURE14 =                    0x84CE

gl_TEXTURE15 :: GLenum
gl_TEXTURE15 =                    0x84CF

gl_TEXTURE16 :: GLenum
gl_TEXTURE16 =                    0x84D0

gl_TEXTURE17 :: GLenum
gl_TEXTURE17 =                    0x84D1

gl_TEXTURE18 :: GLenum
gl_TEXTURE18 =                    0x84D2

gl_TEXTURE19 :: GLenum
gl_TEXTURE19 =                    0x84D3

gl_TEXTURE20 :: GLenum
gl_TEXTURE20 =                    0x84D4

gl_TEXTURE21 :: GLenum
gl_TEXTURE21 =                    0x84D5

gl_TEXTURE22 :: GLenum
gl_TEXTURE22 =                    0x84D6

gl_TEXTURE23 :: GLenum
gl_TEXTURE23 =                    0x84D7

gl_TEXTURE24 :: GLenum
gl_TEXTURE24 =                    0x84D8

gl_TEXTURE25 :: GLenum
gl_TEXTURE25 =                    0x84D9

gl_TEXTURE26 :: GLenum
gl_TEXTURE26 =                    0x84DA

gl_TEXTURE27 :: GLenum
gl_TEXTURE27 =                    0x84DB

gl_TEXTURE28 :: GLenum
gl_TEXTURE28 =                    0x84DC

gl_TEXTURE29 :: GLenum
gl_TEXTURE29 =                    0x84DD

gl_TEXTURE30 :: GLenum
gl_TEXTURE30 =                    0x84DE

gl_TEXTURE31 :: GLenum
gl_TEXTURE31 =                    0x84DF

gl_ACTIVE_TEXTURE :: GLenum
gl_ACTIVE_TEXTURE =               0x84E0

gl_REPEAT :: GLenum
gl_REPEAT =                       0x2901

gl_CLAMP_TO_EDGE :: GLenum
gl_CLAMP_TO_EDGE =                0x812F

gl_MIRRORED_REPEAT :: GLenum
gl_MIRRORED_REPEAT =              0x8370

gl_FLOAT_VEC2 :: GLenum
gl_FLOAT_VEC2 =                   0x8B50

gl_FLOAT_VEC3 :: GLenum
gl_FLOAT_VEC3 =                   0x8B51

gl_FLOAT_VEC4 :: GLenum
gl_FLOAT_VEC4 =                   0x8B52

gl_INT_VEC2 :: GLenum
gl_INT_VEC2 =                     0x8B53

gl_INT_VEC3 :: GLenum
gl_INT_VEC3 =                     0x8B54

gl_INT_VEC4 :: GLenum
gl_INT_VEC4 =                     0x8B55

gl_BOOL :: GLenum
gl_BOOL =                         0x8B56

gl_BOOL_VEC2 :: GLenum
gl_BOOL_VEC2 =                    0x8B57

gl_BOOL_VEC3 :: GLenum
gl_BOOL_VEC3 =                    0x8B58

gl_BOOL_VEC4 :: GLenum
gl_BOOL_VEC4 =                    0x8B59

gl_FLOAT_MAT2 :: GLenum
gl_FLOAT_MAT2 =                   0x8B5A

gl_FLOAT_MAT3 :: GLenum
gl_FLOAT_MAT3 =                   0x8B5B

gl_FLOAT_MAT4 :: GLenum
gl_FLOAT_MAT4 =                   0x8B5C

gl_SAMPLER_2D :: GLenum
gl_SAMPLER_2D =                   0x8B5E

gl_SAMPLER_CUBE :: GLenum
gl_SAMPLER_CUBE =                 0x8B60

gl_VERTEX_ATTRIB_ARRAY_ENABLED :: GLenum
gl_VERTEX_ATTRIB_ARRAY_ENABLED =  0x8622

gl_VERTEX_ATTRIB_ARRAY_SIZE :: GLenum
gl_VERTEX_ATTRIB_ARRAY_SIZE =     0x8623

gl_VERTEX_ATTRIB_ARRAY_STRIDE :: GLenum
gl_VERTEX_ATTRIB_ARRAY_STRIDE =   0x8624

gl_VERTEX_ATTRIB_ARRAY_TYPE :: GLenum
gl_VERTEX_ATTRIB_ARRAY_TYPE =     0x8625

gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: GLenum
gl_VERTEX_ATTRIB_ARRAY_NORMALIZED=0x886A

gl_VERTEX_ATTRIB_ARRAY_POINTER :: GLenum
gl_VERTEX_ATTRIB_ARRAY_POINTER =  0x8645

gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: GLenum
gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING=0x889F

gl_MAX_VERTEX_ATTRIBS :: GLsizei
gl_MAX_VERTEX_ATTRIBS =           0x8869

gl_MAX_VERTEX_UNIFORM_VECTORS :: GLsizei
gl_MAX_VERTEX_UNIFORM_VECTORS =   0x8DFB

gl_MAX_VARYING_VECTORS :: GLsizei
gl_MAX_VARYING_VECTORS =          0x8DFC

gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: GLsizei
gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS=0x8B4D

gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: GLsizei
gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS=0x8B4C

gl_MAX_TEXTURE_IMAGE_UNITS :: GLsizei
gl_MAX_TEXTURE_IMAGE_UNITS =      0x8872

gl_MAX_FRAGMENT_UNIFORM_VECTORS :: GLsizei
gl_MAX_FRAGMENT_UNIFORM_VECTORS = 0x8DFD

gl_IMPLEMENTATION_COLOR_READ_TYPE :: GLenum
gl_IMPLEMENTATION_COLOR_READ_TYPE = 0x8B9A

gl_IMPLEMENTATION_COLOR_READ_FORMAT :: GLenum
gl_IMPLEMENTATION_COLOR_READ_FORMAT=0x8B9B

gl_COMPILE_STATUS :: GLenum
gl_COMPILE_STATUS =               0x8B81

gl_INFO_LOG_LENGTH :: GLenum
gl_INFO_LOG_LENGTH =              0x8B84

gl_SHADER_SOURCE_LENGTH :: GLenum
gl_SHADER_SOURCE_LENGTH =         0x8B88

gl_SHADER_COMPILER :: GLenum
gl_SHADER_COMPILER =              0x8DFA

gl_SHADER_BINARY_FORMATS :: GLenum
gl_SHADER_BINARY_FORMATS =        0x8DF8

gl_NUM_SHADER_BINARY_FORMATS :: GLenum
gl_NUM_SHADER_BINARY_FORMATS =    0x8DF9

gl_LOW_FLOAT :: GLenum
gl_LOW_FLOAT =                    0x8DF0

gl_MEDIUM_FLOAT :: GLenum
gl_MEDIUM_FLOAT =                 0x8DF1

gl_HIGH_FLOAT :: GLenum
gl_HIGH_FLOAT =                   0x8DF2

gl_LOW_INT :: GLenum
gl_LOW_INT =                      0x8DF3

gl_MEDIUM_INT :: GLenum
gl_MEDIUM_INT =                   0x8DF4

gl_HIGH_INT :: GLenum
gl_HIGH_INT =                     0x8DF5

gl_FRAMEBUFFER :: GLenum
gl_FRAMEBUFFER =                                    0x8D40

gl_RENDERBUFFER :: GLenum
gl_RENDERBUFFER =                                   0x8D41

gl_RGBA4 :: GLenum
gl_RGBA4 =                                          0x8056

gl_RGB5_A1 :: GLenum
gl_RGB5_A1 =                                        0x8057

gl_RGB565 :: GLenum
gl_RGB565 =                                         0x8D62

gl_DEPTH_COMPONENT16 :: GLenum
gl_DEPTH_COMPONENT16 =                              0x81A5

gl_STENCIL_INDEX :: GLenum
gl_STENCIL_INDEX =                                  0x1901

gl_STENCIL_INDEX8 :: GLenum
gl_STENCIL_INDEX8 =                                 0x8D48

gl_RENDERBUFFER_WIDTH :: GLenum
gl_RENDERBUFFER_WIDTH =                             0x8D42

gl_RENDERBUFFER_HEIGHT :: GLenum
gl_RENDERBUFFER_HEIGHT =                            0x8D43

gl_RENDERBUFFER_INTERNAL_FORMAT :: GLenum
gl_RENDERBUFFER_INTERNAL_FORMAT =                   0x8D44

gl_RENDERBUFFER_RED_SIZE :: GLenum
gl_RENDERBUFFER_RED_SIZE =                          0x8D50

gl_RENDERBUFFER_GREEN_SIZE :: GLenum
gl_RENDERBUFFER_GREEN_SIZE =                        0x8D51

gl_RENDERBUFFER_BLUE_SIZE :: GLenum
gl_RENDERBUFFER_BLUE_SIZE =                         0x8D52

gl_RENDERBUFFER_ALPHA_SIZE :: GLenum
gl_RENDERBUFFER_ALPHA_SIZE =                        0x8D53

gl_RENDERBUFFER_DEPTH_SIZE :: GLenum
gl_RENDERBUFFER_DEPTH_SIZE =                        0x8D54

gl_RENDERBUFFER_STENCIL_SIZE :: GLenum
gl_RENDERBUFFER_STENCIL_SIZE =                      0x8D55

gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: GLenum
gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE =             0x8CD0

gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: GLenum
gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME =             0x8CD1

gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: GLenum
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL =           0x8CD2

gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: GLenum
gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE =   0x8CD3

gl_COLOR_ATTACHMENT0 :: GLenum
gl_COLOR_ATTACHMENT0 =                              0x8CE0

gl_DEPTH_ATTACHMENT :: GLenum
gl_DEPTH_ATTACHMENT =                               0x8D00

gl_STENCIL_ATTACHMENT :: GLenum
gl_STENCIL_ATTACHMENT =                             0x8D20

gl_NONE :: GLenum
gl_NONE =                                           0

gl_FRAMEBUFFER_COMPLETE :: GLenum
gl_FRAMEBUFFER_COMPLETE =                           0x8CD5

gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: GLenum
gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT =              0x8CD6

gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: GLenum
gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT =      0x8CD7

gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: GLenum
gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS =              0x8CD9

gl_FRAMEBUFFER_UNSUPPORTED :: GLenum
gl_FRAMEBUFFER_UNSUPPORTED =                        0x8CDD

gl_FRAMEBUFFER_BINDING :: GLenum
gl_FRAMEBUFFER_BINDING =                            0x8CA6

gl_RENDERBUFFER_BINDING :: GLenum
gl_RENDERBUFFER_BINDING =                           0x8CA7

gl_MAX_RENDERBUFFER_SIZE :: GLenum
gl_MAX_RENDERBUFFER_SIZE =                          0x84E8

gl_INVALID_FRAMEBUFFER_OPERATION :: GLenum
gl_INVALID_FRAMEBUFFER_OPERATION =                  0x0506

