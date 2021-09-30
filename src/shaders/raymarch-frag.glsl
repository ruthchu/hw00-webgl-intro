#version 300 es

precision highp float;

uniform vec4 u_Color; // The color with which to render this instance of geometry.
uniform highp float u_Time;
uniform mat4 u_ViewProj;    // The matrix that defines the camera's transformation.
uniform float u_Terrain;
uniform float u_Cloud;

in vec4 fs_Pos;
in vec4 fs_Nor;
in vec4 fs_LightVec;
in vec4 fs_Col;

in vec4 old_Pos;

out vec4 out_Col; // This is the final output color that you will see on your
                  // screen for the pixel that is currently being processed.

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

float bias(float time, float bias)
{
  return (time / ((((1.0/bias) - 2.0)*(1.0 - time))+1.0));
}

float gain(float time, float gain)
{
  if (time < 0.5) {
    return bias(time * 2.0,gain)/2.0;
  }
  else {
    return bias(time * 2.0 - 1.0,1.0 - gain)/2.0 + 0.5;
  }
} 

float easeInOutCustom(float x) {
    return -(4.0 *pow((x - .5),2.0)) + 1.0;
}

void main()
{
    float baseNoise = (fbm(7.0, old_Pos.xyz * u_Terrain) + 1.338) / 2.638;
    baseNoise = gain(baseNoise, .8);
    float lowBound = u_Cloud - .35;
    float highBound = u_Cloud - .12;
    if (baseNoise > lowBound && baseNoise < highBound) {
        float modTime = sin(u_Time * .0007);
        float cloudNoise = (fbm(20.0, vec3(fs_Pos.x + modTime, fs_Pos.y, fs_Pos.z + modTime) * 5.0) + 1.338) / 2.638;
        float remapped = ((baseNoise - lowBound) / .23);
        cloudNoise = mix(0.0, cloudNoise, easeInOutCustom(remapped));
        out_Col = vec4(vec3(1.0), cloudNoise);
    }
    else {
        out_Col = vec4(0.0);
    }
}