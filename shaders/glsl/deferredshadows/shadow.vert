#version 450

//#extension GL_EXT_debug_printf : enable

#define LIGHT_COUNT 1
#define TILE_COUNT 16

layout (location = 0) in vec4 inPos;

//layout (location = 0) out int outInstanceIndex;
layout (location = 0) out vec2 outShadowUV;

layout (set = 0, binding = 0) uniform UBO 
{
	mat4 proj[LIGHT_COUNT];
	mat4 modelView[LIGHT_COUNT];
	vec4 instancePos[LIGHT_COUNT];
} ubo;

layout (set = 1, binding = 0) buffer VirtualTileTable
{
	int count;
	int id[TILE_COUNT * TILE_COUNT];
} table;

struct UsedVirtualTileProj
{
	mat4 virtualTileProj;
};

layout (set = 1, binding = 1) buffer UsedVirtualTileMatrices
{
	UsedVirtualTileProj matrices[];
};

layout(push_constant) uniform PushConsts 
{
	int index;
} pushConsts;

//layout (binding = 3) uniform sampler2D samplerDepth;

void main()
{
    //debugPrintfEXT("inPos.x = %f, inPos.y = %f, inPos.z = %f \n", inPos.x, inPos.y, inPos.z);
	vec4 shaodwClipPos = ubo.proj[0] * ubo.modelView[0] * vec4(inPos.xyz, 1.0f);
    shaodwClipPos.xyz = shaodwClipPos.xyz / shaodwClipPos.w;

    bool bShaodwInClip = shaodwClipPos.w > 0.0f && 
        (shaodwClipPos.x >= -1.0f && shaodwClipPos.x <= 1.0f) &&
        (shaodwClipPos.y >= -1.0f && shaodwClipPos.y <= 1.0f) &&
        (shaodwClipPos.z >=  0.0f && shaodwClipPos.z <= 1.0f);
    if(!bShaodwInClip)
    {
        return;
     }

    vec2 uvShadow = 0.5f * (shaodwClipPos.xy + 1.0f);
    //debugPrintfEXT("uvShadow.x = %f and uvShadow.y = %f\n", uvShadow.x, uvShadow.y);
    //[0-1] floats
    outShadowUV = uvShadow;

    // need to modify
	float indexTmp = floor(uvShadow.y * TILE_COUNT) * TILE_COUNT + floor(uvShadow.x * TILE_COUNT);
    
    //GLSL casting a float to an int automatically floors it (at least in any implementation I've ever seen)
    int index = int(indexTmp);
    if (index >= TILE_COUNT * TILE_COUNT)
    {
        // uvShadow.y == 1.0f
        return;
    }

    int targetID = table.id[index];
    //debugPrintfEXT("targetID = %d\n", targetID);
    // print out targetID

//shadow.vert:64: '=' : 
// cannot convert from 'layout( column_major std430) temp structure{layout( column_major) global highp 4X4 matrix of float virtualTileProj}' 
//to ' temp highp 4X4 matrix of float'
    mat4 tileProjMatrix = matrices[targetID].virtualTileProj;
    //gl_Position = matrices[targetID] * ubo.modelView[0] * tmpPos;

    //mat4 tileProjMatrixPushConsts = matrices[pushConsts.index].virtualTileProj;

    // mat4 == operator?
    // if (tileProjMatrix == tileProjMatrixPushConsts)
    // {
    //      debugPrintfEXT("tileProjMatrix == tileProjMatrixPushConsts\n");
    // }
    // else
    // {
    //     debugPrintfEXT("tileProjMatrix != tileProjMatrixPushConsts\n");
    // }

	//outInstanceIndex = gl_InstanceIndex;
	//vec4 instancedPos = ubo.instancePos[inInstanceIndex[0]];
    vec4 instancedPos = ubo.instancePos[0];
	vec4 tmpPos = inPos + instancedPos;

    // ubo.modelView[0] view here needs to be changed?
    gl_Position = tileProjMatrix * ubo.modelView[0] * tmpPos;
	//l_Position = ubo.proj[0] * ubo.modelView[0] * tmpPos;
}