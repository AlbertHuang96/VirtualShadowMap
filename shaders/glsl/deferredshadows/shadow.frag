#version 450

#extension GL_EXT_debug_printf : enable

#define PHYSICAL_TILE_COUNT 8
#define TILE_COUNT 16
#define TILE_SIZE 128

//Type mismatch on location 0.0: 'ptr to output sint32' vs 'ptr to input vec2 of float32'
layout (location = 0) in vec2 inShadowUV;

layout (set = 1, binding = 0) buffer VirtualTileTable
{
	int count;
	int id[TILE_COUNT * TILE_COUNT];
} table;

//layout (set = 1, binding = 2, rgba8ui) uniform uimage2D PhysicalImage;
layout (set = 1, binding = 2, r32ui) uniform uimage2D PhysicalImage;

void main()
{
	// gl_FragDepth = gl_FragCoord.z;
    //ivec2 P2 = ivec2(gl_FragCoord.xy);
    //imageAtomicMax(PhysicalImage, P2, 250);
    //return;

    float indexTmp = floor(inShadowUV.y * TILE_COUNT) * TILE_COUNT + floor(inShadowUV.x * TILE_COUNT);
    
    //GLSL casting a float to an int automatically floors it (at least in any implementation I've ever seen)
    int index = int(indexTmp);
    //debugPrintfEXT("index = %d\n", index);
    // == or >=?
    if (index >= TILE_COUNT * TILE_COUNT)
    {
        // uvShadow.y == 1.0f
        return;
    }

    int targetID = table.id[index];
    debugPrintfEXT("index = %d and targetID = %d\n", index, targetID);

    int offsetX = targetID % PHYSICAL_TILE_COUNT;
    int offsetY = targetID / PHYSICAL_TILE_COUNT;

    int imageOffsetX = offsetX * TILE_SIZE;
    int imageOffsetY = offsetY * TILE_SIZE;

    //gl_FragCoord.x * TILE_SIZE
    int tileOffsetX = int(gl_FragCoord.x);
    int tileOffsetY = int(gl_FragCoord.y);

    ivec2 P = ivec2(imageOffsetX + tileOffsetX, imageOffsetY + tileOffsetY);
    //debugPrintfEXT("P.x = %d P.y = %d\n", P.x, P.y);
// print out P
// print out fragDepth
    uint fragDepth = floatBitsToUint(gl_FragCoord.z);
    //debugPrintfEXT("fragDepth = %d\n", fragDepth);

    //result of PhysicalImage is all zero !
    // init value of PhysicalImage? imageAtomicMin(0, depth) = 0

    imageAtomicMax(PhysicalImage, P, fragDepth);
    //imageAtomicMax(PhysicalImage, P, 250);
    //imageAtomicMin(PhysicalImage, P, fragDepth);
    // Shader requires fragmentStoresAndAtomics but is not enabled on the device

}


