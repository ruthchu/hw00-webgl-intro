#version 300 es

precision highp float;

uniform vec4 u_Color; // The color with which to render this instance of geometry.
uniform highp float u_Time;
uniform mat4 u_ViewProj;    // The matrix that defines the camera's transformation.

in vec4 fs_Pos;
in vec4 fs_Nor;
in vec4 fs_LightVec;
in vec4 fs_Col;

out vec4 out_Col; // This is the final output color that you will see on your
                  // screen for the pixel that is currently being processed.

void main()
{
    // Material base color (before shading)
    vec4 diffuseColor = u_Color;
    if (fs_Pos.y > .5) {
        out_Col = vec4(vec3(diffuseColor), 0.0);
    }
}