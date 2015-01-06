//
// Copyright : (c) T.Mishima 2014
// License : Apache-2.0
//
#version 130
in vec3 VertexPosition;
in vec3 VertexNormal;
in vec2 VertexCoord;

out vec3 Normal;
out vec3 Position;
out vec2 Coord;

out vec4 ShadowCoord;

uniform   mat4 mMatrix;
uniform   mat4 mvpMatrix;
//uniform   mat4 tMatrix;

void main(void){
    Coord       = VertexCoord;
    Position    = (mMatrix * vec4(VertexPosition, 1.0)).xyz;
    Normal      = VertexNormal;
    //ShadowCoord = tMatrix * vec4(Position, 1.0);
    //ShadowCoord = tMatrix * vec4(VertexPosition, 1.0);
    gl_Position = mvpMatrix * vec4(VertexPosition, 1.0);
}

