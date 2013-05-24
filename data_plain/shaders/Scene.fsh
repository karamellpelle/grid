precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_texcoord;


void main()
{

    lowp vec4 color = texture2D( u_tex, v_texcoord );
    
    color.a *= u_alpha;

    gl_FragColor = color;
    //gl_FragColor = vec4(1, 0, 1, 1);
}

