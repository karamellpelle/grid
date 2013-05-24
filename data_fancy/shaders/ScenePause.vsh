uniform vec2 u_tx;
uniform vec2 u_ty;
uniform vec2 u_pos;

attribute vec4 a_pos;

varying vec2 v_texa_coord;
varying vec2 v_scene_coord;


void main()
{
    
    v_texa_coord = u_pos + (a_pos.x * u_tx + a_pos.y * u_ty);
    v_scene_coord = 0.5 * (a_pos.xy + vec2(1.0));
    
    gl_Position = a_pos;
    
}
