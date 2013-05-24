precision highp float;

uniform sampler2D u_tex;
uniform sampler2D u_stencil;
uniform vec4 u_ambient;
uniform lowp float u_alpha;

varying mediump vec2 v_stencilcoord;
varying mediump float v_diffuse;


void main()
{
    u_alpha;
    u_tex;

    lowp vec4 color = texture2D( u_stencil, v_stencilcoord );
    
    gl_FragColor = color * (vec4(v_diffuse) + u_ambient);
    
}
