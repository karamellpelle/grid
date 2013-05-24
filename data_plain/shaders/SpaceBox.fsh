precision highp float;

uniform samplerCube u_cubetex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;

varying mediump vec3 v_texcoord;


void main()
{
    lowp vec4 color = textureCube( u_cubetex, v_texcoord );
    color *= (u_color + vec4(0.1, 0.1, 0.1, 0.0));
    
    // alpha
    color.a *= u_alpha;

    gl_FragColor = color;
}

