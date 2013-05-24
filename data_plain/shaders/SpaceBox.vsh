#extension GL_EXT_separate_shader_objects : enable

uniform mat4 u_projmodv_matrix;
uniform vec4 u_pw;
uniform float u_radius;

attribute vec3 a_pos;

varying vec3 v_texcoord;



void main()
{
    v_texcoord = normalize(a_pos);
    
    // position without translation!
    gl_Position = u_projmodv_matrix * vec4(u_radius * a_pos, 0) + u_pw;
    
}