#version 300 es

// This is a fragment shader. If you've opened this file first, please
// open and read lambert.vert.glsl before reading on.
// Unlike the vertex shader, the fragment shader actually does compute
// the shading of geometry. For every pixel in your program's output
// screen, the fragment shader is run for every bit of geometry that
// particular pixel overlaps. By implicitly interpolating the position
// data passed into the fragment shader by the vertex shader, the fragment shader
// can compute what color to apply to its pixel based on things like vertex
// position, light position, and vertex color.
precision highp float;

uniform vec4 u_Color; // The color with which to render this instance of geometry.
uniform highp float u_Time;

// These are the interpolated values out of the rasterizer, so you can't know
// their specific values without knowing the vertices that contributed to them

in vec4 fs_Pos;
in vec4 fs_Nor;
in vec4 fs_LightVec;
in vec4 fs_Col;

out vec4 out_Col; // This is the final output color that you will see on your
                  // screen for the pixel that is currently being processed.

// Pseudorandom output modified from https://stackoverflow.com/questions/4200224/random-noise-functions-for-glsl
// Outputs red, green, or blue, based on which value is the largest
vec3 rand(vec3 co){
    float a = fract(sin(dot(co, vec3(12.9898, 78.233, 34.252))) * 43758.5453);
    float b = fract(sin(dot(co, vec3(78.233, 34.252, 12.9898))) * 43758.5453);
    float c = fract(sin(dot(co, vec3(34.252, 78.233, 12.9898))) * 43758.5453);
    if (a > b && a > c) {
        return vec3(1.0, 0.0, 0.0);
    } else if (b > a && b > c) {
        return vec3(0.0, 1.0, 0.0);
    } else if (c > b && c > a) {
        return vec3(0.0, 0.0, 1.0);
    }
    return vec3(a, b, c);
}

// Taken from cis460 sky shader (not sure where it came from originally)
vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}
vec4 taylorInvSqrt(vec4 r){return 1.79284291400159 - 0.85373472095314 * r;}

float snoise(vec3 v){
    const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
    const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
    vec3 i  = floor(v + dot(v, C.yyy) );
    vec3 x0 =   v - i + dot(i, C.xxx) ;

    // Other corners
    vec3 g = step(x0.yzx, x0.xyz);
    vec3 l = 1.0 - g;
    vec3 i1 = min( g.xyz, l.zxy );
    vec3 i2 = max( g.xyz, l.zxy );

    //  x0 = x0 - 0. + 0.0 * C
    vec3 x1 = x0 - i1 + 1.0 * C.xxx;
    vec3 x2 = x0 - i2 + 2.0 * C.xxx;
    vec3 x3 = x0 - 1. + 3.0 * C.xxx;

    // Permutations
    i = mod(i, 289.0 );
    vec4 p = permute( permute( permute(
                                   i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
                               + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
                      + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients
    // ( N*N points uniformly over a square, mapped onto an octahedron.)
    float n_ = 1.0/7.0; // N=7
    vec3  ns = n_ * D.wyz - D.xzx;

    vec4 j = p - 49.0 * floor(p * ns.z *ns.z);  //  mod(p,N*N)

    vec4 x_ = floor(j * ns.z);
    vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

    vec4 x = x_ *ns.x + ns.yyyy;
    vec4 y = y_ *ns.x + ns.yyyy;
    vec4 h = 1.0 - abs(x) - abs(y);

    vec4 b0 = vec4( x.xy, y.xy );
    vec4 b1 = vec4( x.zw, y.zw );

    vec4 s0 = floor(b0)*2.0 + 1.0;
    vec4 s1 = floor(b1)*2.0 + 1.0;
    vec4 sh = -step(h, vec4(0.0));

    vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
    vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

    vec3 p0 = vec3(a0.xy,h.x);
    vec3 p1 = vec3(a0.zw,h.y);
    vec3 p2 = vec3(a1.xy,h.z);
    vec3 p3 = vec3(a1.zw,h.w);

    //Normalise gradients
    vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
    p0 *= norm.x;
    p1 *= norm.y;
    p2 *= norm.z;
    p3 *= norm.w;

    // Mix final noise value
    vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
    m = m * m;
    return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                  dot(p2,x2), dot(p3,x3) ) );
}

// Self-written referencing noise 2021 slide deck. https://cis566-procedural-graphics.github.io/noise-2021.pdf
float fbm(float nOctaves, vec3 pos) {
    float total = 0.;
    float persistence = 1.f / 2.f;

    for (float i = 0.f; i < nOctaves; ++i) {
        float frequency = pow(2.f, i);
        float amplitude = pow(persistence, i);

        total += amplitude * snoise(pos * frequency);
    }
    return total;
}

void main()
{
    // Material base color (before shading)
    vec4 diffuseColor = u_Color;

    // Helps to determine scaling factor for rate the colors change
    float rampUp = .5 / tan(u_Time * .02);

    // To keep track of when the cube is formed vs moving
    float flash = sin(u_Time * .02);
    
    // random red, blue, or green based on position
    vec3 flashColor = rand(vec3(fs_Pos));

    if (flash < 0.0 ) { // Color when the sides of the box are "still"
        // Flashes through very high octave fbm
        float stopFlash = fbm(50.0, vec3((fs_Pos.xy * u_Time), fs_Pos.z));
        out_Col = vec4(vec3(stopFlash) * flashColor, 1.0);
    }
    else { // Color when the sides of the box are moving
        // Modify noise in relation to movement of object
        float fbmBase = fbm(50.0, vec3(fs_Pos) * rampUp);
        vec3  fbmRemap = vec3(smoothstep(-1., 1., fbmBase));

        // Modify base color in relation to movement of object
        diffuseColor *= (1.0 - smoothstep(0., 1., flash));
        diffuseColor =  mix(diffuseColor, vec4(flashColor, 1.0), 1. - smoothstep(0., 1., flash));
        out_Col = vec4(fbmRemap, 1.0) * diffuseColor;
    }
}