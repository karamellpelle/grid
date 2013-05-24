
precision highp float;

uniform lowp float u_alpha;
uniform sampler2D u_stencil;
uniform lowp vec4 u_color;

varying mediump vec2 v_stencil_coord;


void main()
{

    const float soft_a = 0.3;
    const float soft_edge_min = 0.5 - soft_a;
    const float soft_edge_max = 0.5;
    const float outline_wth = 0.4;
    const float outline_a = 0.3;  // 0.2
    const float outline_b = 0.2;  // 0.1
    
    const float outline_min_value0 = soft_edge_min - outline_wth * outline_a;
    const float outline_min_value1 = soft_edge_max + outline_wth * outline_b;
    const float outline_max_value0 = soft_edge_min - outline_wth * outline_b;
    const float outline_max_value1 = soft_edge_max + outline_wth * outline_a;
    
    
    lowp vec4 tex = texture2D( u_stencil, v_stencil_coord );
    
    // 0 iff distance < 0.5
    // 1 iff 0.5 <= distance
    float distance = tex.a;
    
    lowp vec4 color = u_color;
/*
    if ( u_color_back_enabled )
    {
        if ( outline_min_value0 <= distance && 
             distance < outline_max_value1     )
        {
            float alpha;
            
            if ( distance < outline_min_value1 )
            {
                alpha = smoothstep( outline_min_value0, outline_min_value1, distance );
            }
            else
            {
                alpha = 1.0 - smoothstep( outline_max_value0, outline_max_value1, distance );
                
            }
            color = mix( u_color_front, u_color_back, alpha );
        }
    }
*/
    
    // hard-egdes:
    //color.a *= float(0.5 <= dist);
    
    // soft-edges:
    color.a *= smoothstep( soft_edge_min, soft_edge_max, distance ); 
    
    // alpha 
    color.a *= u_alpha;

    // we represent transparent colors by premultiplied alpha
    color *= vec4(color.a, color.a, color.a, 1.0);
    
    // no discard, since this is not efficient
    //if ( color.a == 0.0 )
    //    discard;
    
    gl_FragColor = color;
}