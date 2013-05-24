uniform vec2 u_texa_tx;
uniform vec2 u_texa_ty;
uniform vec2 u_texa_pos;

attribute vec4 a_pos;

varying vec2 v_scene_coord;
varying vec2 v_texa_coord;
varying vec2 v_pos;

void main()
{
    
    v_scene_coord = 0.5 * (a_pos.xy + vec2(1.0));
    
    v_texa_coord = u_texa_pos + (a_pos.x * u_texa_tx + a_pos.y * u_texa_ty);
    
    v_pos = a_pos.xy;
    
    //vec2 texa_coord = a_pos.x * u_texa_tx + a_pos.y * u_texa_ty;
    //texa_coord += vec2(1.0);
    //texa_coord *= 0.5;
    //texa_coord += u_texa_pos;
    //v_texa_coord = texa_coord;
    
    gl_Position = a_pos;
    
}
