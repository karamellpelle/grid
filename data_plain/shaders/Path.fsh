
precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;

varying mediump vec2 v_texcoord;


void main()
{

    lowp vec4 tex = texture2D( u_tex, v_texcoord );
/*
    //lowp vec4 color = tex * u_color; // this does not work for some odd reason :(
    lowp vec4 color = u_color;
    color.a *= tex.a;
    
    // alpha
    color.a *= u_alpha;
    
    // we represent transparent colors by premultiplied alpha
    color *= vec4(color.a, color.a, color.a, 1.0);
*/
    vec4 color = tex * u_color + tex;
    color *= u_alpha;
    
    gl_FragColor = color;
}