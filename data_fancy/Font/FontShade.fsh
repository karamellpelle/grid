precision lowp float;

uniform lowp vec4 u_color;
uniform sampler2D u_stencil;
uniform lowp float u_alpha;

varying mediump vec2 v_stencil_coord;


// 0.5 -/+ 1/16
const lowp float smooth_min = 0.5 - 1.0 / 20.0;

const lowp float smooth_max = 0.5 + 1.0 / 20.0;


void main()
{
    lowp float dist = texture2D(u_stencil, v_stencil_coord).r;
    lowp float alpha = smoothstep( smooth_min, smooth_max, dist);
    vec4 color = (u_alpha * alpha) * u_color;

    gl_FragColor = color;
}

