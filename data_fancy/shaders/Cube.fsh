uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform lowp float u_beta;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;

const lowp vec4 ambient = vec4( 0.1, 0.1, 0.1, 0.0 );
//const lowp vec4 grayscaler = vec4( 0.2126, 0.7152, 0.0722, 0.0 );
//const lowp vec4 grayscaler = vec4( 0.2 * 0.25, 0.7 * 0.25, 0.03 * 0.25, 0.0 );
const lowp vec4 grayscaler = vec4( 0.08, 0.08, 0.08, 0.0 );

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
    
    // grayscale representation
    lowp float v = dot( grayscaler, color );
    lowp vec4 grayscale = vec4( v, v, v, color.a );


    // interpolate from colors to grayscale
    //color = mix( color, grayscale, 0.8 );
    color = mix( color, grayscale, u_beta );

    // alpha
    color *= u_alpha;

    gl_FragColor = color;
}
