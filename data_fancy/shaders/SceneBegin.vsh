uniform vec2 u_texbegin_scale;

attribute vec2 a_pos;

varying vec2 v_scene_coord;
varying vec2 v_texbegin_coord;

void main()
{
    v_scene_coord = 0.5 * (a_pos + vec2(1.0));
    v_texbegin_coord = 0.5 * (u_texbegin_scale * a_pos + vec2(1.0));
    
    gl_Position = vec4(a_pos, 0.0, 1.0);
    
}
