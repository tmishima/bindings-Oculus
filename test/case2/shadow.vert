//
// Copyright : (c) T.Mishima 2014
// License : Apache-2.0
//
#version 130
in vec3 VertexPosition;

uniform mat4 mvpMatrix;

void main() {
  gl_Position = mvpMatrix * vec4(VertexPosition,1.0);
}

