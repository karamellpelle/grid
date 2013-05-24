uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_tex_coord;


void main()
{

    lowp vec4 tex = texture2D( u_tex, v_tex_coord );
    
    tex *= u_alpha;
    
    gl_FragColor = tex;
}

