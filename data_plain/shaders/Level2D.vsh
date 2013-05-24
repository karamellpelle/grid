uniform mat4 u_projmodv_matrix;
uniform mat3 u_normal_matrix;
uniform vec2 u_pos;
uniform vec2 u_scale;

attribute vec4 a_pos;
attribute vec3 a_normal;
attribute vec2 a_stencilcoord;

varying mediump vec2 v_stencilcoord;
varying mediump float v_diffuse;



void main()
{
    u_pos;
    u_scale;
    
    const vec3 light_dir = vec3(1, 0, 0);
  
    // fixme: normal matrix!
    vec3 normal = normalize( u_normal_matrix * a_normal );

    v_diffuse = max( dot(light_dir, normal), 0.0 );
    v_stencilcoord = a_stencilcoord;

    gl_Position = u_projmodv_matrix * a_pos;
}
