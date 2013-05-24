precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform mediump float u_d;
uniform mediump float u_d_inv;
uniform mediump float u_tweak;

varying mediump vec2 v_texcoord;


void main()
{

    //lowp vec2 texcoord = u_d * floor( u_d_inv * v_texcoord );
    //lowp vec4 color = texture2D( u_tex, texcoord );
    lowp vec4 color = texture2D( u_tex, v_texcoord );
    u_d;
    u_d_inv;
    u_tweak;
    
    //color.rgb *= u_tweak * u_tweak;
    
    color.a *= u_alpha;

    gl_FragColor = color;
    //gl_FragColor = vec4(1, 0, 1, 1);
}

