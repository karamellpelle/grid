// attribute
attribute vec2 a_pos;

// uniform
uniform vec2 u_stencil_scale;

// varying
varying vec2 v_scene_coord;
varying vec2 v_stencil_coord;


void main()
{
    v_scene_coord = 0.5 * (a_pos + vec2(1.0));
    v_stencil_coord = 0.5 * (u_stencil_scale * a_pos + vec2(1.0));
    
    gl_Position = vec4(a_pos, 0.0, 1.0);
}