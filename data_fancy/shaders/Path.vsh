uniform mat4 u_projmodv_matrix;
uniform float u_radius;

attribute vec4 a_pos;
attribute vec4 a_antipos;
attribute vec2 a_tex_coord;
attribute vec2 a_dxdy;

varying vec2 v_tex_coord;


void main()
{

    v_tex_coord = a_tex_coord; //(a_dxdy + vec2(1.0, 1.0)) * vec2(1.0, 0.5);
    
    vec4 pos0 = u_projmodv_matrix * a_pos;
    vec4 pos1 = u_projmodv_matrix * a_antipos;
    
    vec2 dx = u_radius * normalize ( pos1.xy / pos1.w - pos0.xy / pos0.w );
    vec2 dy = vec2(-1, 1) * dx.yx;
    
    mat2 dxdy_matrix = mat2(dx, dy);
    
    gl_Position = pos0 + vec4( dxdy_matrix * a_dxdy, 0, 0 );
        
}
