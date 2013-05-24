// uniform
uniform sampler2D u_scene;
uniform sampler2D u_texbegin;
uniform lowp float u_scene_alpha;
uniform lowp float u_texbegin_alpha;

// varying
varying mediump vec2 v_scene_coord;
varying mediump vec2 v_texbegin_coord;

const lowp vec4 color_back = vec4(0.0, 0.0, 0.0, 1.0);


void main()
{
    // scene
    lowp vec4 scene = texture2D( u_scene, v_scene_coord );
    
    // texbegin
    lowp vec4 texbegin = texture2D( u_texbegin, v_texbegin_coord );
    
    
    // color_back + scene + texbegin 
    lowp vec4 color = mix( color_back, scene, u_scene_alpha );
    color += u_texbegin_alpha * texbegin;


        

    gl_FragColor = color;

}

