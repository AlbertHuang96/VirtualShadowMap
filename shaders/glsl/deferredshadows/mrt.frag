#version 450

//#extension GL_EXT_debug_printf : enable

//layout (set = 0, binding = 1) uniform sampler2D samplerColor;
//layout (set = 0, binding = 2) uniform sampler2D samplerNormalMap;

layout (set = 2, binding = 0) uniform sampler2D samplerColor;
//layout (set = 2, binding = 1) uniform sampler2D samplerNormalMap;

//layout (set = 1, binding = 0) uniform sampler2D samplerColor;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec2 inUV;
layout (location = 2) in vec3 inColor;
layout (location = 3) in vec3 inWorldPos;
layout (location = 4) in vec3 inTangent;

layout (location = 0) out vec4 outPosition;
layout (location = 1) out vec4 outNormal;
layout (location = 2) out vec4 outAlbedo;
layout (location = 3) out float outDepth;

void main() 
{
	outPosition = vec4(inWorldPos, 1.0);

	// Calculate normal in tangent space
	vec3 N = normalize(inNormal);
	
	//vec3 T = normalize(inTangent);
	
	//vec3 B = cross(N, T);
	//mat3 TBN = mat3(T, B, N);
	//vec3 sampled = texture(samplerNormalMap, inUV).xyz;
	//debugPrintfEXT("sampled.x = %f, sampled.y = %f, sampled.z = %f \n", sampled.x, sampled.y, sampled.z);
	outNormal = vec4(N, 1.0);
	//vec3 tnorm = TBN * normalize(texture(samplerNormalMap, inUV).xyz * 2.0 - vec3(1.0));
	//outNormal = vec4(tnorm, 1.0);


	outAlbedo = texture(samplerColor, inUV);

	outDepth = gl_FragCoord.z;
}