module OpenGL.GLFW.Ext where

import OpenGL.GLFW.Types
import Foreign.Ptr
import Foreign.C.Types

{-

/*------------------------------------------------------------------------*
 * APPLE extension tokens
 *------------------------------------------------------------------------*/
#if GL_APPLE_framebuffer_multisample
#define GL_RENDERBUFFER_SAMPLES_APPLE                           0x8CAB
#define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_APPLE             0x8D56
#define GL_MAX_SAMPLES_APPLE                                    0x8D57
#define GL_READ_FRAMEBUFFER_APPLE                               0x8CA8
#define GL_DRAW_FRAMEBUFFER_APPLE                               0x8CA9
#define GL_DRAW_FRAMEBUFFER_BINDING_APPLE                       0x8CA6
#define GL_READ_FRAMEBUFFER_BINDING_APPLE                       0x8CAA
#endif

#if GL_APPLE_rgb_422
#define GL_RGB_422_APPLE                                        0x8A1F
#define GL_UNSIGNED_SHORT_8_8_APPLE                             0x85BA
#define GL_UNSIGNED_SHORT_8_8_REV_APPLE                         0x85BB
#endif

#if GL_APPLE_texture_format_BGRA8888
#define GL_BGRA_EXT                                             0x80E1
#endif

#if GL_APPLE_texture_format_BGRA8888 || GL_IMG_read_format
#define GL_BGRA                                                 0x80E1
#endif

#if GL_APPLE_texture_max_level
#define GL_TEXTURE_MAX_LEVEL_APPLE                              0x813D
#endif

/*------------------------------------------------------------------------*
 * EXT extension tokens
 *------------------------------------------------------------------------*/
#if GL_EXT_blend_minmax
#define GL_MIN_EXT                                              0x8007
#define GL_MAX_EXT                                              0x8008
#endif

#if GL_EXT_color_buffer_half_float
#define GL_RGBA16F_EXT                                          0x881A
#define GL_RGB16F_EXT                                           0x881B
#define GL_RG16F_EXT                                            0x822F
#define GL_R16F_EXT                                             0x822D
#define GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT            0x8211
#define GL_UNSIGNED_NORMALIZED_EXT                              0x8C17
#endif

#if GL_EXT_debug_label
#define GL_BUFFER_OBJECT_EXT                                    0x9151
#define GL_SHADER_OBJECT_EXT                                    0x8B48
#define GL_PROGRAM_OBJECT_EXT                                   0x8B40
#define GL_QUERY_OBJECT_EXT                                     0x9153
#define GL_VERTEX_ARRAY_OBJECT_EXT                              0x9154
#define GL_PROGRAM_PIPELINE_OBJECT_EXT                          0x8A4F
#endif

#if GL_EXT_discard_framebuffer
#define GL_COLOR_EXT                                            0x1800
#define GL_DEPTH_EXT                                            0x1801
#define GL_STENCIL_EXT                                          0x1802
#endif

#if GL_EXT_occlusion_query_boolean
#define GL_ANY_SAMPLES_PASSED_EXT                               0x8C2F
#define GL_ANY_SAMPLES_PASSED_CONSERVATIVE_EXT                  0x8D6A
#define GL_CURRENT_QUERY_EXT                                    0x8865
#define GL_QUERY_RESULT_EXT                                     0x8866
#define GL_QUERY_RESULT_AVAILABLE_EXT                           0x8867
#endif

#if GL_EXT_read_format_bgra
#define GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT                       0x8365
#define GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT                       0x8366
#define GL_UNSIGNED_SHORT_1_5_5_5_REV                           GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT
#endif

#if GL_EXT_read_format_bgra || GL_IMG_read_format
#define GL_UNSIGNED_SHORT_4_4_4_4_REV                           0x8365
#endif

#if GL_EXT_separate_shader_objects
#define GL_VERTEX_SHADER_BIT_EXT                                0x00000001
#define GL_FRAGMENT_SHADER_BIT_EXT                              0x00000002
#define GL_ALL_SHADER_BITS_EXT                                  0xFFFFFFFF
#define GL_PROGRAM_SEPARABLE_EXT                                0x8258
#define GL_ACTIVE_PROGRAM_EXT                                   0x8259
#define GL_PROGRAM_PIPELINE_BINDING_EXT                         0x825A
#endif

#if GL_EXT_shadow_samplers
#define GL_TEXTURE_COMPARE_MODE_EXT                             0x884C
#define GL_TEXTURE_COMPARE_FUNC_EXT                             0x884D
#define GL_COMPARE_REF_TO_TEXTURE_EXT                           0x884E
#endif

#if GL_EXT_texture_filter_anisotropic
#define GL_TEXTURE_MAX_ANISOTROPY_EXT                           0x84FE
#define GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                       0x84FF
#endif

#if GL_EXT_texture_rg
#define GL_RED_EXT                                              0x1903
#define GL_RG_EXT                                               0x8227
#define GL_R8_EXT                                               0x8229
#define GL_RG8_EXT                                              0x822B
#endif

/*------------------------------------------------------------------------*
 * IMG extension tokens
 *------------------------------------------------------------------------*/
#if GL_IMG_read_format
#define GL_BGRA_IMG                                             0x80E1
#define GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG                       0x8365
#endif

#if GL_IMG_texture_compression_pvrtc
#define GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG                      0x8C00
#define GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG                      0x8C01
#define GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG                     0x8C02
#define GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG                     0x8C03
#endif

/*------------------------------------------------------------------------*
 * OES extension tokens
 *------------------------------------------------------------------------*/
#if GL_OES_depth24
#define GL_DEPTH_COMPONENT24_OES                                0x81A6
#endif
-}

-- GL_OES_packed_depth_stencil
gl_DEPTH_STENCIL_OES :: GLenum
gl_DEPTH_STENCIL_OES = 0x84F9

gl_UNSIGNED_INT_24_8_OES :: GLenum
gl_UNSIGNED_INT_24_8_OES = 0x84FA

gl_DEPTH24_STENCIL8_OES :: GLenum 
gl_DEPTH24_STENCIL8_OES = 0x88F0


{-
#if GL_OES_rgb8_rgba8
#define GL_RGB8_OES                                             0x8051
#define GL_RGBA8_OES                                            0x8058
#endif

#if GL_OES_standard_derivatives
#define GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES                  0x8B8B
#endif

#if GL_OES_texture_half_float
#define GL_HALF_FLOAT_OES                                       0x8D61
#endif

#if GL_OES_vertex_array_object
#define GL_VERTEX_ARRAY_BINDING_OES                             0x85B5
#endif
-}

{-

 *------------------------------------------------------------------------*/
#if GL_APPLE_framebuffer_multisample
GL_API GLvoid glRenderbufferStorageMultisampleAPPLE(GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0);
GL_API GLvoid glResolveMultisampleFramebufferAPPLE(void)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0);
#endif

#if GL_EXT_debug_label
GL_API GLvoid glLabelObjectEXT(GLenum type, GLuint object, GLsizei length, const GLchar *label)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetObjectLabelEXT(GLenum type, GLuint object, GLsizei bufSize, GLsizei *length, GLchar *label)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
#endif

#if GL_EXT_debug_marker
GL_API GLvoid glInsertEventMarkerEXT(GLsizei length, const GLchar *marker)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glPushGroupMarkerEXT(GLsizei length, const GLchar *marker)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glPopGroupMarkerEXT(void)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
#endif

#if GL_EXT_discard_framebuffer
GL_API GLvoid GL_APIENTRY glDiscardFramebufferEXT(GLenum target, GLsizei numAttachments, const GLenum *attachments)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_4_0);
#endif

#if GL_EXT_occlusion_query_boolean
GL_API GLvoid glGenQueriesEXT(GLsizei n, GLuint *ids)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glDeleteQueriesEXT(GLsizei n, const GLuint *ids)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLboolean glIsQueryEXT(GLuint id)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glBeginQueryEXT(GLenum target, GLuint id)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glEndQueryEXT(GLenum target)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetQueryivEXT(GLenum target, GLenum pname, GLint *params)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetQueryObjectivEXT(GLuint id, GLenum pname, GLint *params)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetQueryObjectuivEXT(GLuint id, GLenum pname, GLuint *params)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
#endif

-}

{-
GL_API GLvoid glUseProgramStagesEXT(GLuint pipeline, GLbitfield stages, GLuint program)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glActiveShaderProgramEXT(GLuint pipeline, GLuint program)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLuint glCreateShaderProgramvEXT(GLenum type, GLsizei count, const GLchar **strings)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glBindProgramPipelineEXT(GLuint pipeline)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glDeleteProgramPipelinesEXT(GLsizei n, const GLuint *pipelines)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGenProgramPipelinesEXT(GLsizei n, GLuint *pipelines)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLboolean glIsProgramPipelineEXT(GLuint pipeline)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramParameteriEXT(GLuint program, GLenum pname, GLint value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetProgramPipelineivEXT(GLuint pipeline, GLenum pname, GLint *params)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glValidateProgramPipelineEXT(GLuint pipeline)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glGetProgramPipelineInfoLogEXT(GLuint pipeline, GLsizei bufSize, GLsizei *length, GLchar *infoLog)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);

-}

-- | GLvoid glProgramUniform1iEXT(GLuint program, GLint location, GLint x)
foreign import ccall unsafe "glProgramUniform1iEXT" glProgramUniform1iEXT
    :: GLuint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform2iEXT(GLuint program, GLint location, GLint x, GLint y)  
foreign import ccall unsafe "glProgramUniform2iEXT" glProgramUniform2iEXT
    :: GLuint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform3iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z)  
foreign import ccall unsafe "glProgramUniform3iEXT" glProgramUniform3iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform4iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z, GLint w)
foreign import ccall unsafe "glProgramUniform4iEXT" glProgramUniform4iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform1fEXT(GLuint program, GLint location, GLfloat x)  
foreign import ccall unsafe "glProgramUniform1fEXT" glProgramUniform1fEXT
    :: GLuint -> GLint -> GLfloat -> IO ()

-- | GLvoid glProgramUniform2fEXT(GLuint program, GLint location, GLfloat x, GLfloat y)
foreign import ccall unsafe "glProgramUniform2fEXT" glProgramUniform2fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform3fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z)  
foreign import ccall unsafe "glProgramUniform3fEXT" glProgramUniform3fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform4fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w)  
foreign import ccall unsafe "glProgramUniform4fEXT" glProgramUniform4fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

{-
GL_API GLvoid glProgramUniform1ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform2ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform3ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform4ivEXT(GLuint program, GLint location, GLsizei count, const GLint *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);

GL_API GLvoid glProgramUniform1fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform2fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform3fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniform4fvEXT(GLuint program, GLint location, GLsizei count, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);

GL_API GLvoid glProgramUniformMatrix2fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniformMatrix3fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);
GL_API GLvoid glProgramUniformMatrix4fvEXT(GLuint program, GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)  __OSX_AVAILABLE_STARTING(__MAC_NA,__IPHONE_5_0);


/*------------------------------------------------------------------------*
 * OES extension functions
 *------------------------------------------------------------------------*/
-}

gl_WRITE_ONLY_OES :: GLenum
gl_WRITE_ONLY_OES = 0x88B9

gl_BUFFER_ACCESS_OES :: GLenum 
gl_BUFFER_ACCESS_OES = 0x88BB

gl_BUFFER_MAPPED_OES :: GLenum
gl_BUFFER_MAPPED_OES = 0x88BC

gl_BUFFER_MAP_POINTER_OES :: GLenum
gl_BUFFER_MAP_POINTER_OES = 0x88BD

-- | void glGetBufferPointervOES (GLenum target, GLenum pname, GLvoid **params);
foreign import ccall unsafe "glGetBufferPointervOES" glGetBufferPointervOES
    :: GLenum -> GLenum -> Ptr (Ptr a) -> IO ()

-- | GLvoid* glMapBufferOES (GLenum target, GLenum access);
foreign import ccall unsafe "glMapBufferOES" glMapBufferOES
    :: GLenum -> GLenum -> IO (Ptr GLvoid)

-- | GLboolean glUnmapBufferOES (GLenum target);
foreign import ccall unsafe "glUnmapBufferOES" glUnmapBufferOES
    :: GLenum -> IO GLboolean



-- | GLvoid glBindVertexArrayOES(GLuint array);  
foreign import ccall unsafe "glBindVertexArrayOES" glBindVertexArrayOES
    :: GLuint -> IO ()

-- | GLvoid glDeleteVertexArraysOES(GLsizei n, const GLuint *arrays);  
foreign import ccall unsafe "glDeleteVertexArraysOES" glDeleteVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLvoid glGenVertexArraysOES(GLsizei n, GLuint *arrays);  
foreign import ccall unsafe "glGenVertexArraysOES" glGenVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLboolean glIsVertexArrayOES(GLuint array);
foreign import ccall unsafe "glIsVertexArrayOES" glIsVertexArrayOES
    :: GLuint -> IO GLboolean


