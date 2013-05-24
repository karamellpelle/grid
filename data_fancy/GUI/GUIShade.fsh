// uniforms
uniform sampler2D u_tex;
uniform sampler2D u_filltex;
uniform sampler2D u_stencil;
uniform lowp float u_focus;
uniform lowp float u_alpha;
uniform bool u_use_filltex;
uniform bool u_use_tex;
uniform bool u_use_stencil;
uniform lowp vec3 u_stencil_dim;


// varyings
varying mediump vec2 v_tex_coord;
varying mediump vec2 v_filltex_coord;


void main()
{
    lowp vec4 color = vec4(0, 0, 0, 0);
    
    
    if ( u_use_filltex )
    {
        lowp vec4 color_filltex = texture2D( u_filltex, v_filltex_coord );
        color = color + color_filltex;
    }
    if ( u_use_stencil )
    {
        lowp float stencil = dot( u_stencil_dim, texture2D( u_stencil, v_tex_coord ).xyz );
        color *= stencil;
    }
    if ( u_use_tex )
    {
        // u_tex is premultiplied
        lowp vec4 color_tex = texture2D( u_tex, v_tex_coord );
        color = (1.0 - color_tex.a) * color + color_tex;
    }
    
    // fixme: focus
    //color += 0.5 * u_focus;
    
    color *= u_alpha;

    gl_FragColor = color;


}
