uniform sampler2D u_tex;
uniform lowp float u_alpha;

varying mediump vec2 v_texcoord;
varying lowp float v_diffuse;

const lowp vec4 ambient = vec4(0.4, 0.4, 0.4, 0.0);

void main()
{
    lowp vec4 color = texture2D( u_tex, v_texcoord );
   
    // light
    color *= vec4( vec3(v_diffuse), 1.0 ) + ambient;
    
    color *= u_alpha;

    
    gl_FragColor = color;
    

}
