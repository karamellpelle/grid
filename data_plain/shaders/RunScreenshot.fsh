precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_tex_coord;


void main()
{

    
    // background_color is background color of scene (glClearColor)
    // lowp vec4 tex_color = texture2D( u_tex, v_tex_coord );
    // lowp vec4 color = (1.0 - tex_color.a) * background_color.rgb + tex_color.rgb;
    
    // since background_color.rgb is assumed to be (0, 0, 0):
    lowp vec4 color = texture2D( u_tex, v_tex_coord );
    
    // scene is not transparent
    color.a = 1.0;
    
    color *= u_alpha;

    gl_FragColor = color;
}

