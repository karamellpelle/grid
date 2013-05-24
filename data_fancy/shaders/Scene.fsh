uniform sampler2D u_scene;
uniform sampler2D u_texa;
uniform mediump float u_delta_scale;
uniform mediump vec4 u_colorfy;
uniform mediump float u_colorfy_alpha;

varying mediump vec2 v_scene_coord;
varying mediump vec2 v_texa_coord;
varying mediump vec2 v_pos;

void main()
{
    // texa
    lowp vec4 texa = texture2D( u_texa, v_texa_coord );
    
    // find bounded delta
    mediump float bound = min(0.3, dot(v_pos, v_pos));
    mediump vec2 delta = (u_delta_scale * bound) *  (texa.xy - vec2(0.5));
    
    // scene
    lowp vec4 color = texture2D( u_scene, v_scene_coord + delta );
    
    //color += texa; // tmp: show texa
    
    // intensify colors
    color *= u_colorfy_alpha * u_colorfy;
    
    // color 
    gl_FragColor = color;

}

