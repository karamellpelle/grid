precision highp float;

uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;


void main()
{
    lowp vec4 color = texture2D( u_tex, v_texcoord );
   
    // light
    color *= vec4( vec3(v_diffuse), u_alpha ) + vec4( 0.6, 0.6, 0.6, 0.0 );
    
    color *= u_alpha;
    
/*
    if ( gl_FrontFacing )
    {
    color = vec4(1, 0, 0, 1);
    }
    else
    {
    color = vec4(0, 0, 1, 1);
    }
*/
    
    gl_FragColor = color;
    

}
