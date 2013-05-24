attribute vec2 a_pos;
attribute vec2 a_tex_coord;

varying vec2 v_tex_coord;


void main()
{
    v_tex_coord = a_tex_coord;
    
    gl_Position = vec4(a_pos, 0, 1);
    
}
