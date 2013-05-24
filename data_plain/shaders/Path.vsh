
uniform mat4 u_projmodv_matrix;
uniform float u_radius;

attribute vec3 a_pos;
attribute vec3 a_antipos;
attribute vec2 a_dxdy;
attribute vec2 a_texcoord;


varying vec2 v_texcoord;


void main()
{

    v_texcoord = a_texcoord;
    
    vec4 pos0 = u_projmodv_matrix * vec4(a_pos, 1);
    vec4 pos1 = u_projmodv_matrix * vec4(a_antipos, 1);
    
    vec2 dx = u_radius * normalize ( pos1.xy / pos1.w - pos0.xy / pos0.w );
    vec2 dy = vec2(-1, 1) * dx.yx;
    
    mat2 dxdy_matrix = mat2(dx, dy);
    
    gl_Position = pos0 + vec4( dxdy_matrix * a_dxdy, 0, 0 ); // fixme: 1
        
}
