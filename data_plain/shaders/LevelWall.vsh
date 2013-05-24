uniform mat4 u_projmodv_matrix;
uniform mat3 u_normal_matrix;
uniform vec3 u_pos;
uniform vec3 u_ref_dir;

attribute vec3 a_pos;
attribute vec3 a_normal;
attribute vec2 a_texcoord;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;



void main()
{    
    vec3 normal = u_normal_matrix * a_normal;
    
    v_diffuse = max( dot(u_ref_dir, normal), 0.0 );
    v_texcoord = a_texcoord;

    gl_Position = u_projmodv_matrix * vec4(a_pos, 1.0);
}
