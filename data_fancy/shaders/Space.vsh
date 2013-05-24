uniform mat4 u_modv_inv_matrix;
uniform vec2 u_scale;
uniform vec2 u_scale_plus;
attribute vec4 a_pos;

varying vec3 v_texcoord;



void main()
{

    vec4 tex_pos = a_pos;
    tex_pos.xy *= u_scale_plus * u_scale;
    // flip vertically (fixme: do this in software or images instead)
    v_texcoord = (u_modv_inv_matrix * tex_pos).xyz * vec3(1.0, -1.0, 1.0);
    
    
    vec4 pos = a_pos;
    pos.z = 1.0;
    
    gl_Position = pos;
}