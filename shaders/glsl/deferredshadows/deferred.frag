#version 450

#extension GL_EXT_debug_printf : enable

layout (set = 0, binding = 1) uniform sampler2D samplerposition;
layout (set = 0, binding = 2) uniform sampler2D samplerNormal;
layout (set = 0, binding = 3) uniform sampler2D samplerAlbedo;
//layout (set = 0, binding = 5) uniform sampler2DArray samplerShadowMap;

#define TILE_COUNT 16

layout (set = 1, binding = 0) buffer VirtualTileTable
{
	int count;
	int id[TILE_COUNT * TILE_COUNT];
} table;

//layout (set = 1, binding = 2, rgba8ui) uniform uimage2D PhysicalImage;
layout (set = 1, binding = 2, r32ui) uniform uimage2D PhysicalImage;

// layout (set = 0, binding = 5, r32ui) uniform uimage2D PhysicalImage;
// layout (set = 0, binding = 6) buffer VirtualTileTable
// {
// 	int count;
// 	int id[TILE_COUNT * TILE_COUNT];
// } table;

layout (location = 0) in vec2 inUV;

layout (location = 0) out vec4 outFragColor;

#define PHYSICAL_TILE_COUNT 8
#define TILE_SIZE 128

#define LIGHT_COUNT 1
#define SHADOW_FACTOR 0.25
#define AMBIENT_LIGHT 0.1
#define USE_PCF

struct Light 
{
	vec4 position;
	vec4 target;
	vec4 color;
	mat4 viewMatrix;
};

layout (set = 0, binding = 4) uniform UBO 
{
	vec4 viewPos;
	Light lights[LIGHT_COUNT];
	int useShadows;
	int debugDisplayTarget;
} ubo;

layout (set = 1, binding = 3) readonly buffer TileTable
{
	uint offsetInfo[TILE_COUNT * TILE_COUNT];
} tileOffsetTable;

float textureProj(vec4 P, float layer, vec2 offset)
{
	float shadow = 1.0;
	vec4 shadowCoord = P / P.w;
	shadowCoord.st = shadowCoord.st * 0.5 + 0.5;
	
	if (shadowCoord.z > -1.0 && shadowCoord.z < 1.0) 
	{
		float dist = 0.0f; //texture(samplerShadowMap, vec3(shadowCoord.st + offset, layer)).r;
		if (shadowCoord.w > 0.0 && dist < shadowCoord.z) 
		{
			shadow = SHADOW_FACTOR;
		}
	}
	return shadow;
}

float filterPCF(vec4 sc, float layer)
{
	// ivec2 texDim = textureSize(samplerShadowMap, 0).xy;
	// float scale = 1.5;
	// float dx = scale * 1.0 / float(texDim.x);
	// float dy = scale * 1.0 / float(texDim.y);

	// float shadowFactor = 0.0;
	// int count = 0;
	// int range = 1;
	
	// for (int x = -range; x <= range; x++)
	// {
	// 	for (int y = -range; y <= range; y++)
	// 	{
	// 		shadowFactor += textureProj(sc, layer, vec2(dx*x, dy*y));
	// 		count++;
	// 	}
	
	// }
	// return shadowFactor / count;
	return 0.0;
}

vec3 shadow(vec3 fragcolor, vec3 fragpos) {
	for(int i = 0; i < LIGHT_COUNT; ++i)
	{
		vec4 shadowClip	= ubo.lights[i].viewMatrix * vec4(fragpos, 1.0);
		//debugPrintfEXT("fragpos.x = %f and fragpos.y = %f and fragpos.z = %f\n", fragpos.x, fragpos.y, fragpos.z);
		//debugPrintfEXT("shadowClip.z = %f and shadowClip.w = %f\n", shadowClip.z, shadowClip.w);

		float shadowFactor = 1.0f;
		//#ifdef USE_PCF
		//	shadowFactor= filterPCF(shadowClip, i);
		//#else
		//	shadowFactor = textureProj(shadowClip, i, vec2(0.0));
		//#endif
		vec4 shadowCoord = shadowClip / shadowClip.w;
		//debugPrintfEXT("shadowCoord.z = %f\n", shadowCoord.z);

	    shadowCoord.st = shadowCoord.st * 0.5 + 0.5;

		// clip range check
		bool bShaodwInClip = shadowCoord.w > 0.0f && 
        (shadowCoord.x >= -1.0f && shadowCoord.x <= 1.0f) &&
        (shadowCoord.y >= -1.0f && shadowCoord.y <= 1.0f) &&
        (shadowCoord.z >= 0.0f && shadowCoord.z <= 1.0f);
    	if(!bShaodwInClip)
    	{
        	return fragcolor;
     	}
	
		if (bShaodwInClip) 
		{
			
			//float / int
			//int indexX = int(x / TILE_SIZE);
			//int indexY = int(y / TILE_SIZE);
			//int index2 = indexY * TILE_COUNT + indexX;

			float indexTmp = floor(shadowCoord.y * TILE_COUNT) * TILE_COUNT + floor(shadowCoord.x * TILE_COUNT);
    		//debugPrintfEXT("indexTmp = %f and shadowCoord.x = %f and shadowCoord.y = %f and floor(shadowCoord.y * TILE_COUNT) = %f\n", indexTmp, shadowCoord.x, shadowCoord.y, floor(shadowCoord.y * TILE_COUNT));
    		//GLSL casting a float to an int automatically floors it (at least in any implementation I've ever seen)
    		int index = int(indexTmp);
    		//debugPrintfEXT("index = %d\n", index);
    		// == or >=?
    		if (index >= TILE_COUNT * TILE_COUNT)
    		{
        		// uvShadow.y == 1.0f
        		return fragcolor;
    		}

    		int targetID = table.id[index];
    		//debugPrintfEXT("index = %d and targetID = %d\n", index, targetID);

    		int offsetX = targetID % PHYSICAL_TILE_COUNT;
    		int offsetY = targetID / PHYSICAL_TILE_COUNT;

    		int imageOffsetX = offsetX * TILE_SIZE;
    		int imageOffsetY = offsetY * TILE_SIZE;

			// need to modify
			// need a lookup indirection texture
			//int tileOffsetX = int(gl_FragCoord.x / TILE_SIZE);
    		//int tileOffsetY = int(gl_FragCoord.y / TILE_SIZE);

			// heres problem its tile offset 
			//uint offset = tileOffsetTable.offsetInfo[index];
			//offset = 65535
			//uint tileOffsetXTmp = (offset >> 24) & 0xff;
			//uint tileOffsetYTmp = (offset >> 16) & 0xff;


			//float floatTileOffsetX = uintBitsToFloat(tileOffsetXTmp);
			//float floatTileOffsetY = uintBitsToFloat(tileOffsetYTmp);
			//int tileOffsetX = int(floatTileOffsetX * TILE_SIZE);
			//int tileOffsetY = int(floatTileOffsetY * TILE_SIZE);

			// highp int/uint  uint -> int
			//int tileOffsetX = (int(offset) >> 8) & 0xff;
			//int tileOffsetY = int(offset) & 0xff;

			float x = shadowCoord.x * TILE_COUNT * TILE_SIZE;
			float y = shadowCoord.y * TILE_COUNT * TILE_SIZE;
			//debugPrintfEXT("x = %f and y = %f\n", x, y);
			//debugPrintfEXT("x = %f and y = %f and x / TILE_SIZE = %d and y / TILE_SIZE = %d\n", x, y, int(x) / (TILE_SIZE), int(y) / (TILE_SIZE));
			int tileOffsetX = int(x) - TILE_SIZE * (int(x) / (TILE_SIZE));
			int tileOffsetY = int(y) - TILE_SIZE * (int(y) / (TILE_SIZE));
			//debugPrintfEXT("x2 = %d and y2 = %d\n", x2, y2);
			//int tileOffsetX = int(x - x / float(TILE_SIZE));
			//int tileOffsetY = int(y - y / float(TILE_SIZE));
			//debugPrintfEXT("tileOffsetX = %d and tileOffsetY = %d\n", tileOffsetX, tileOffsetY);

    		ivec2 P = ivec2(imageOffsetX + tileOffsetX, imageOffsetY + tileOffsetY);
			debugPrintfEXT("P.x = %d and P.y = %d\n", P.x, P.y);

			uvec4 shadowDepthVec4 = imageLoad(PhysicalImage, P);

			float shadowDepth = uintBitsToFloat(shadowDepthVec4.x);
			//debugPrintfEXT("shadowDepth = %f and shadowCoord.z = %f\n", shadowDepth, shadowCoord.z);

			// reverse depth in shadowmap
			if (shadowDepth > shadowCoord.z)
			{
				//debugPrintfEXT("shadowDepth = %f and shadowCoord.z = %f\n", shadowDepth, shadowCoord.z);
				shadowFactor = SHADOW_FACTOR;
			}
		}

		fragcolor *= shadowFactor;
		
	}
	return fragcolor;
}

void main() 
{
	// rasterize area 0-128?
	//debugPrintfEXT("inUV.x = %f and inUV.y = %f\n", inUV.x, inUV.y);
	// Get G-Buffer values
	vec3 fragPos = texture(samplerposition, inUV).rgb;
	//debugPrintfEXT("fragPos.x = %f and fragPos.y = %f and fragPos.z = %f\n", fragPos.x, fragPos.y, fragPos.z);
	vec3 normal = texture(samplerNormal, inUV).rgb;
	vec4 albedo = texture(samplerAlbedo, inUV);

	// Debug display
	if (ubo.debugDisplayTarget > 0) {
		switch (ubo.debugDisplayTarget) {
			case 1: 
				//outFragColor.rgb = shadow(vec3(1.0), fragPos).rgb;
				vec4 shadowClip	= ubo.lights[0].viewMatrix * vec4(fragPos, 1.0);
				outFragColor.rgb = vec3(shadowClip.xyz);
				//outFragColor.rgb = vec3(shadowClip.z); //all white if shadowClip.z > 1
				break;
			case 2: 
				outFragColor.rgb = fragPos;
				break;
			case 3: 
				outFragColor.rgb = normal;
				break;
			case 4: 
				outFragColor.rgb = albedo.rgb;
				break;
			case 5: 
				outFragColor.rgb = albedo.aaa;
				break;
		}		
		outFragColor.a = 1.0;
		return;
	}

	// Ambient part
	vec3 fragcolor  = albedo.rgb * AMBIENT_LIGHT;

	vec3 N = normalize(normal);
		
	for(int i = 0; i < LIGHT_COUNT; ++i)
	{
		// Vector to light
		vec3 L = ubo.lights[i].position.xyz - fragPos;
		// Distance from light to fragment position
		float dist = length(L);
		L = normalize(L);

		// Viewer to fragment
		vec3 V = ubo.viewPos.xyz - fragPos;
		V = normalize(V);

		float lightCosInnerAngle = cos(radians(15.0));
		float lightCosOuterAngle = cos(radians(25.0));
		float lightRange = 100.0;

		// Direction vector from source to target
		vec3 dir = normalize(ubo.lights[i].position.xyz - ubo.lights[i].target.xyz);

		// Dual cone spot light with smooth transition between inner and outer angle
		float cosDir = dot(L, dir);
		float spotEffect = smoothstep(lightCosOuterAngle, lightCosInnerAngle, cosDir);
		float heightAttenuation = smoothstep(lightRange, 0.0f, dist);

		// Diffuse lighting
		float NdotL = max(0.1, dot(N, L));
		vec3 diff = vec3(NdotL);

		// Specular lighting
		vec3 R = reflect(-L, N);
		float NdotR = max(0.0, dot(R, V));
		vec3 spec = vec3(pow(NdotR, 16.0) * albedo.a * 2.5);

		fragcolor += diff * albedo.rgb;
		//fragcolor += vec3((diff + spec) * spotEffect * heightAttenuation) * ubo.lights[i].color.rgb * albedo.rgb;
	}    	

	// Shadow calculations in a separate pass
	if (ubo.useShadows > 0)
	{
		fragcolor = shadow(fragcolor, fragPos);
	}

	outFragColor = vec4(fragcolor, 1.0);
}