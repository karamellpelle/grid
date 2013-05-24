uniform sampler2D u_scene;
uniform sampler2D u_texa;


varying mediump vec2 v_texa_coord;
varying mediump vec2 v_scene_coord;

void main()
{
    lowp vec4 texa = texture2D( u_texa, v_texa_coord );
    
    mediump vec2 scene_coord = v_scene_coord + texa.z * (texa.x - 0.5, texa.y - 0.5);
    
    // fixme: mod color!
    lowp vec4 color = texture2D( u_scene, scene_coord );
    
    gl_FragColor = color;

}

