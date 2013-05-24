uniform samplerCube u_tex;
uniform lowp float u_alpha;
uniform lowp vec4 u_color;
uniform lowp vec4 u_dot;
uniform lowp float u_intensity;
uniform lowp float u_colorfy;

varying mediump vec3 v_texcoord;


void main()
{
    lowp vec4 tex = textureCube( u_tex, v_texcoord );
    
    lowp float value = u_intensity * dot(u_dot, tex);
    
    lowp vec4 color = mix( tex, value * u_color, u_colorfy );
    
    // alpha
    color *= u_alpha;

    gl_FragColor = color;
    //gl_FragColor = (u_alpha * value) * u_color;
}

