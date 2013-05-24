precision highp float;

uniform sampler2D u_tex_scene;
uniform sampler2D u_tex_image;
uniform lowp float u_alpha;
uniform mediump float u_tweak;

varying mediump vec2 v_texcoord;


void main()
{

    // color0
    lowp vec4 color0 = texture2D( u_tex_image, v_texcoord );
    
    // color1
    lowp vec4 color1 = texture2D( u_tex_scene, v_texcoord );
    
    lowp vec4 color = mix(color0, color1, u_tweak);

    
    color.a *= u_alpha;

    gl_FragColor = color;
}

