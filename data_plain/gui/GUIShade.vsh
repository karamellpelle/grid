uniform mat4 u_projmodv_matrix;
uniform vec2 u_pos;
uniform vec2 u_scale;
uniform float u_depth;
uniform float u_fill_tex_repeat_scale;

attribute vec2 a_pos;
attribute vec2 a_tex_coord;
attribute vec2 a_stencil_coord;


varying vec2 v_fill_tex_coord;
varying vec2 v_tex_coord;
varying vec2 v_stencil_coord;



void main()
{
    vec2 pos2D = u_pos + u_scale * a_pos;
    
    // fill_tex
    v_fill_tex_coord = u_fill_tex_repeat_scale * pos2D;
    
    // tex
    v_tex_coord = a_tex_coord;
    
    // stencil
    v_stencil_coord = a_stencil_coord;
    
    
    gl_Position = u_projmodv_matrix * vec4(pos2D, u_depth, 1);

}
