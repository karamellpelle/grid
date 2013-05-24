uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;

varying mediump vec2 v_tex_coord;


void main()
{

    lowp vec4 tex = texture2D( u_tex, v_tex_coord );

    lowp vec4 color = tex * u_color + tex;
    color *= u_alpha;
    
    gl_FragColor = color;
}