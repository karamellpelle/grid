uniform sampler2D u_scene;
uniform sampler2D u_stencil;
uniform lowp vec4 u_fill_color;
uniform lowp vec4 u_x_color;
uniform lowp vec4 u_y_color;
uniform lowp float u_stencil_alpha;
uniform lowp float u_scene_alpha;
uniform lowp float u_x_alpha;
uniform lowp float u_y_alpha;

varying mediump vec2 v_scene_coord;
varying mediump vec2 v_stencil_coord;

void main()
{
    // scene
    lowp vec4 scene = texture2D( u_scene, v_scene_coord );
    lowp vec4 stencil = texture2D( u_stencil, v_stencil_coord );
    
    lowp vec4 x_color = dot( vec3(0.3, 0.4, 0.3), scene.rgb ) * u_x_color;
    lowp vec4 y_color = dot( vec3(0.3, 0.4, 0.3), scene.rgb ) * u_y_color;
    
    x_color = mix( scene, x_color, u_x_alpha );
    y_color = mix( scene, y_color, u_y_alpha );
    
    stencil *= u_stencil_alpha;
    
    lowp vec4 color = (1.0 - (stencil.x + stencil.y)) * u_fill_color +
                      stencil.x * x_color + stencil.y * y_color;

    // mix scene
    color = mix( color, scene, u_scene_alpha );
    

    gl_FragColor = color;
}


/*
uniform sampler2D u_tex_scene;
uniform sampler2D u_tex_image;
uniform lowp float u_alpha;
uniform mediump float u_tweak;

varying mediump vec2 v_tex_scene_coord;
varying mediump vec2 v_tex_image_coord;


// 0.5 -/+ 1/16
const lowp float smooth_min = 0.5 - 1.0 / 20.0;

const lowp float smooth_max = 0.5 + 1.0 / 20.0;


void main()
{

    // color scene
    lowp vec4 color_scene = texture2D( u_tex_scene, v_tex_scene_coord );
    

    lowp float smooth_min1 =  u_tweak * smooth_min;
    
    // image
    lowp vec4 dist = texture2D( u_tex_image, v_tex_image_coord );
    lowp vec4 alpha = smoothstep( smooth_min1, smooth_max, dist);
    
    alpha.b = 1.0;
    alpha.a = 1.0;
    
    //color_scene = alpha.r * vec4(1.0, 0.0, 0.0, 0.0) + alpha.g * vec4(0.0, 1.0, 0.0, 0.0);
    color_scene *= alpha;


    // fixme: remove alpha!
    u_alpha;


    gl_FragColor = color_scene;
}
*/

