uniform mat4 u_projmodv_matrix;
uniform mat3 u_normal_matrix;
uniform vec3 u_pos;
uniform vec3 u_ref_dir;

attribute vec4 a_pos;
attribute vec3 a_normal;
attribute vec2 a_texcoord;

varying vec2 v_texcoord;
varying float v_diffuse;


void main()
{   
    // pos
    vec4 pos = u_projmodv_matrix * a_pos;
    
    // normal
    vec3 normal = u_normal_matrix * a_normal;

    // light
    //vec3 ref_dir = normalize(-pos.xyz);
    //v_diffuse = max( dot(ref_dir, normal), 0.0 );
    v_diffuse = max( dot(u_ref_dir, normal), 0.0 );
    
    // texcoord
    v_texcoord = a_texcoord;

    gl_Position = pos;
}
