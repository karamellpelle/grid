precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform mediump float u_tweak;

varying mediump vec2 v_texcoord;


void main()
{

    // color0
    lowp vec4 color0 = texture2D( u_tex, v_texcoord );
    
    // color1
    lowp float value = dot( vec3(0.2125, 0.7154, 0.0721), color0.rgb );
    lowp vec4 color1 = vec4(value, value, value, color0.a);
    
    lowp vec4 color = mix(color0, color1, u_tweak);

    
    color.a *= u_alpha;

    gl_FragColor = color;
}

