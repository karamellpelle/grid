uniform mat4 u_projmodv_matrix;
uniform mat3 u_normal_matrix;
uniform vec3 u_pos;
uniform vec3 u_ref_dir;
uniform float u_radius;

attribute vec3 a_pos;
//attribute vec3 a_normal;
attribute vec2 a_texcoord;

varying vec2 v_texcoord;
varying float v_diffuse;
varying float v_alpha;


void main()
{

    vec3 normal = u_normal_matrix * a_pos;
    
    // diffuse
    v_diffuse = max( dot(u_ref_dir, normal), 0.0 );
    
    // alpha
    v_alpha = 0.7 * normal.z;
    
    // texcoord
    v_texcoord = 0.5 * (normal.xy + vec2(1.0, 1.0));
    
    // pos
    gl_Position = u_projmodv_matrix * vec4(u_pos + u_radius * a_pos, 1);

}