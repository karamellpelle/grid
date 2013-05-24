

uniform mat4 u_projmodv_matrix;
uniform vec3 u_pos;
uniform vec3 u_plane_x;
uniform vec3 u_plane_y;
uniform vec2 u_translate;
uniform vec2 u_char_size;

attribute vec2 a_pos;
attribute vec2 a_stencil_coord;

varying vec2 v_stencil_coord;


void main()
{
    const vec4 epsilon = vec4( 0, 0, -0.001, 0 );

    vec2 pos2d = u_char_size * (u_translate + a_pos);
    vec3 pos_plane_x = pos2d.x * u_plane_x;
    vec3 pos_plane_y = pos2d.y * u_plane_y;
    
    vec3 pos3d = u_pos + pos_plane_x + pos_plane_y;
    vec4 pos4d = u_projmodv_matrix * vec4(pos3d, 1) + epsilon;
    
    v_stencil_coord = a_stencil_coord;
    
    gl_Position = pos4d;

}