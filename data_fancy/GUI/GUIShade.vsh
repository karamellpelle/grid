// uniforms
uniform mat4 u_projmodv_matrix;
uniform vec2 u_pos;
uniform vec2 u_scale;
uniform float u_depth;
uniform float u_filltex_repeat;


// attributes
attribute vec2 a_pos;
attribute vec2 a_tex_coord;


// varyings
varying vec2 v_tex_coord;
varying vec2 v_filltex_coord;


void main()
{
    vec2 pos = u_pos + u_scale * a_pos;
    
    v_tex_coord = a_tex_coord;
    v_filltex_coord = u_filltex_repeat * pos;
    
    gl_Position = u_projmodv_matrix * vec4(pos, u_depth, 1.0);

}
