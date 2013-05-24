uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;

const lowp vec4 ambient = vec4( 0.1, 0.1, 0.1, 0.0 );


void main()
{
    // tex
    lowp vec4 tex = texture2D( u_tex, v_texcoord );
    
    lowp vec4 color = tex;

    // since background has color (0 0 0 1):
    color.a = 1.0;
    
    // light
    color += vec4(0.09, 0.09, 0.09, 0.0);
    color *= vec4( vec3(v_diffuse), 1.0 ) + ambient;
    
    // alpha
    color *= u_alpha;
    
    gl_FragColor = color;
}
