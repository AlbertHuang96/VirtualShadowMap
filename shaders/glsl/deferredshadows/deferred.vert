#version 450

//#extension GL_EXT_debug_printf : enable

layout (location = 0) out vec2 outUV;

void main() 
{
	outUV = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
	//debugPrintfEXT("outUV.x = %f and outUV.y = %f\n", outUV.x, outUV.y);
	gl_Position = vec4(outUV * 2.0f - 1.0f, 0.0f, 1.0f);
}