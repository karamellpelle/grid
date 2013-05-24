precision highp float;

uniform lowp float u_alpha;

uniform sampler2D u_fill_tex;
uniform sampler2D u_tex;
uniform sampler2D u_stencil;

uniform bool u_use_fill_tex;
uniform bool u_use_tex;
uniform bool u_use_stencil;
uniform float u_tweak; // lowp?

varying mediump vec2 v_fill_tex_coord;
varying mediump vec2 v_tex_coord;
varying mediump vec2 v_stencil_coord;


// widgets are typicall drawn with FillTex around. if we do not want non-rectangular widgets to be filled up, 
// then do not use FillTex.

void main()
{
    // all colors are represented with premultiplied alpha!
    
    const float soft_a = 0.3;
    const float soft_edge_min = 0.5 - soft_a;
    const float soft_edge_max = 0.5;
  
    lowp vec4 color = vec4(0, 0, 0, 0);

    if ( u_use_fill_tex )
    {
        color = texture2D( u_fill_tex, v_fill_tex_coord );
    }
    if ( u_use_tex )
    {
        vec4 tex_color = texture2D( u_tex, v_tex_coord );
        vec3 rgb = tex_color.rgb + (1.0 - tex_color.a) * color.rgb; // tex_color uses premultiplied alpha
        color = vec4(rgb, tex_color.a); // fixme: correct alpha value
    }
    if ( u_use_stencil )
    {
        float dist = texture2D( u_stencil, v_stencil_coord ).a;
        //color *= smoothstep(soft_edge_min, soft_edge_max, dist); 
        color *= dist;
    }
    
    // tweak is (currently) modifying alpha
    // alpha + tweak (premultiply is ok)
    color *= u_alpha * (1.0 - u_tweak);
    
    gl_FragColor = color;
}
