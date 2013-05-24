precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;

varying mediump vec2 v_texcoord;
varying mediump float v_diffuse;


void main()
{
    lowp vec4 color = texture2D( u_tex, v_texcoord );
    color *= u_color;

    // light
    color *= vec4( vec3(v_diffuse), 1) + vec4(0.5, 0.5, 0.5, 0.0);
    
    // alpha
    color *= u_alpha;
    
    gl_FragColor = color;
    

}
