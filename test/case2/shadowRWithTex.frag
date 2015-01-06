//
// Copyright : (c) T.Mishima 2014
// License : Apache-2.0
//
#version 130

in vec3 Normal;
in vec3 Position;
in vec2 Coord;
//in vec4 ShadowCoord;

out vec4 FragColor;

uniform sampler2D TexMap;
//uniform sampler2D ShadowMap;
uniform vec3 LightPosition = vec3 (0,200,-200);
uniform float MaterialShininess = 0.5;
/*
vec3 phongModelDiffAndSpec()
{
    vec3 n = Normal;
    if( !gl_FrontFacing ) n = -n;
    vec3 s = normalize(vec3(LightPosition) - Position);
    vec3 v = normalize(-Position.xyz);
    vec3 r = reflect( -s, n );
    float sDotN = max( dot(s,n), 0.0 );
    vec3 diffuse = vec3(0.8,0.8,0.8) * sDotN; // Light.Intensity * Material.Kd * sDotN;
    vec3 spec = vec3(0.0);
    if( sDotN > 0.0 )
        spec =  vec3(0.8,0.8,0.8) *    // Light.Intensity * Material.Ks *
            pow( max( dot(r,v), 0.0 ), MaterialShininess );

    return clamp (diffuse + spec, vec3(0.0,0.0,0.0), vec3(0.8,0.8,0.8));
}
*/
void main()
{	
  //vec4 shadowCoordinateWdivide = ShadowCoord / ShadowCoord.w ;
  vec3 ambient = vec3 (0.1,0.1,0.1);
  //vec3 diffAndSpec = phongModelDiffAndSpec();
  vec4 texColor = texture( TexMap, Coord); 
  // Used to lower moire pattern and self-shadowing
  //shadowCoordinateWdivide.z += 0.00005;

  //float distanceFromLight = texture (ShadowMap,shadowCoordinateWdivide.st).z;
  //float shadow = 1.0;
  //if (ShadowCoord.w > 0.0)
  //  shadow = distanceFromLight < shadowCoordinateWdivide.z ? 0.1 : 1.0 ;

  //FragColor = vec4(diffAndSpec * shadow + ambient, 1.0);
  //FragColor = vec4(vec3(1.0,1.0,1.0) * shadow, 1.0);
  //FragColor = vec4(texColor.rgb * diffAndSpec * shadow + ambient, 1.0);
  FragColor = vec4(texColor.rgb + ambient, 1.0);
}


