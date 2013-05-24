uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;
varying lowp float v_alpha;


const lowp vec4 ambient = vec4(0.5, 0.5, 0.5, 0.0);

void main()
{
    lowp vec4 color = u_color;

    // tex
    lowp vec4 tex = texture2D( u_tex, v_texcoord );
    color += tex;
    
    // add alpha
    color += vec4(v_alpha);
    
    // light
    color *= vec4( vec3(v_diffuse), 1.0) + ambient;


    // mult alpha
    color *= u_alpha;

    
    gl_FragColor = color;

}
