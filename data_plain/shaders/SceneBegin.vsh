uniform mat4 u_projmodv_matrix;

attribute vec3 a_pos;
attribute vec2 a_texcoord;


varying vec2 v_texcoord;



void main()
{
    v_texcoord = a_texcoord;
    
    gl_Position = u_projmodv_matrix * vec4(a_pos, 1);
    
}
