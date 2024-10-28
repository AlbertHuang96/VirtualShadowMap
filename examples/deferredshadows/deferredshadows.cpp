/*
* Vulkan Example - Deferred shading with shadows from multiple light sources using geometry shader instancing
*
* This sample adds dynamic shadows (using shadow maps) to a deferred rendering setup
* 
* Copyright (C) 2016-2023 by Sascha Willems - www.saschawillems.de
*
* This code is licensed under the MIT license (MIT) (http://opensource.org/licenses/MIT)
*/

#include "vulkanexamplebase.h"
#include "VulkanFrameBuffer.hpp"
#include "VulkanglTFModel.h"
#include <set>
#include <algorithm>

// Must match the LIGHT_COUNT define in the shadow and deferred shaders
#define LIGHT_COUNT 1

// VSM settings
//  the whole virtual shadow map size will be about 16k*16k (w * h = 128*128 * 128*128) (not now
// 
// tile num 16*16 = 256
#define TILE_COUNT 16
// total num of tile will be 16*16
// 
// tile dim
#define TILE_DIM 128
// one tile size will be 128*128

// physical tile num = 8 * 8
// physical image size = 8 * 8 * (128 * 128)
#define PHYSICAL_TILE_COUNT 8

// frustum culling setting
#define NUM_OF_NODES_PER_TILE 20

#define GBUFFER_DEPTH_DIM 512

class VulkanExample : public VulkanExampleBase
{
public:
	int32_t debugDisplayTarget = 0;
	bool enableShadows = true;

	// Virtual shadow map
	bool enableDynamicLighting = false;
	bool enableVirtualShadowMap = true;
	bool enablePerTileFrustumCulling = false;
	bool enableMultiViewExtension = false;

	// Keep depth range as small as possible
	// for better shadow map precision
	float zNear = 0.1f;
	float zFar = 64.0f;

	// reverse depth
	//1. reverse depth for deferred shading; the mrt depth buffer
	//2. reverse depth for shadow map

	//lightFOV 
	float lightFOV = 100.0f;

	
	// Depth bias (and slope) are used to avoid shadowing artifacts
	float depthBiasConstant = 1.25f;
	float depthBiasSlope = 1.75f;

	/*struct {
		struct {
			vks::Texture2D colorMap;
			vks::Texture2D normalMap;
		} model;
		struct {
			vks::Texture2D colorMap;
			vks::Texture2D normalMap;
		} background;
	} textures;*/

	float sceneRadius = 0;
	/*struct {
		vkglTF::Model model;
		vkglTF::Model background;
	} models;*/

	vkglTF::Model scene;

	struct UniformDataOffscreen {
		glm::mat4 projection;
		glm::mat4 model;
		glm::mat4 view;
		glm::vec4 instancePos[3];
		int layer{ 0 };
	} uniformDataOffscreen;

	// This UBO stores the shadow matrices for all of the light sources
	// The matrices are indexed using geometry shader instancing
	// The instancePos is used to place the models using instanced draws
	struct UniformDataShadows {
		// split the origin mvp into modelView and proj
		glm::mat4 proj[LIGHT_COUNT];
		glm::mat4 modelView[LIGHT_COUNT];
		glm::vec4 instancePos[3];
	} uniformDataShadows;

	struct Light {
		glm::vec4 position;
		glm::vec4 target;
		glm::vec4 color;
		glm::mat4 viewMatrix;
	};

	struct UniformDataComposition {
		glm::vec4 viewPos;
		Light lights[LIGHT_COUNT];
		uint32_t useShadows = 1;
		int32_t debugDisplayTarget = 0;
	} uniformDataComposition;

	// alignas(16)
	struct UniformDataCompute0 {
		/*float sceneRadius;
		alignas(16) glm::mat4 camInvViewProj;*/
		float sceneRadius;
		glm::mat4 camInvViewProj;
	} uniformDataComputeScene;

	struct {
		vks::Buffer ubSceneComputeShadowView;
		vks::Buffer ubSceneCompute;
		vks::Buffer offscreen;
		vks::Buffer composition;
		vks::Buffer shadowGeometryShader;
	} uniformBuffers;

	struct {
		VkPipeline deferred{ VK_NULL_HANDLE };
		VkPipeline offscreen{ VK_NULL_HANDLE };
		VkPipeline shadowpass{ VK_NULL_HANDLE };
	} pipelines;
	VkPipelineLayout pipelineLayout{ VK_NULL_HANDLE };

	struct {
		//VkDescriptorSet model{ VK_NULL_HANDLE };
		VkDescriptorSet offscreen{ VK_NULL_HANDLE };
		VkDescriptorSet shadow{ VK_NULL_HANDLE };
		VkDescriptorSet virtualShadowMap{ VK_NULL_HANDLE };
		VkDescriptorSet composition{ VK_NULL_HANDLE };
	} descriptorSets;

	VkDescriptorSetLayout descriptorSetLayout{ VK_NULL_HANDLE };
	VkDescriptorSetLayout descriptorSetLayoutRenderShadowMap{ VK_NULL_HANDLE };

	// ---------------------------VSM-------------------------------
	// VSM ssbo
	struct
	{
		int atomicCounter;
	} usedVirtualTileCounter;
	vks::Buffer usedVirualTileCountBuffer;

	vks::Buffer virtualTileFlagBuffer;
	vks::Buffer virtualTileTableBuffer;
	vks::Buffer tileOffsetTableBuffer;

	vks::Buffer usedVirtualTileMatrixBuffer;

	vks::Buffer nodesAABBBuffer;
	vks::Buffer visibleNodesIndexBuffer;

	struct
	{
		int atomicCounter;
	} visibleNodesCounter;
	vks::Buffer visibleNodesCountBuffer;

	std::vector<uint32_t> culledNodeIndexes;

	//std::vector<std::vector<int>> nodeIndicesPerTiles;
	std::vector<std::set<int>> nodeIndicesPerTiles;


	// VSM physical image
	vks::Texture2D textureComputeTarget;

	//TODO layout flag 1 bit, id 8 bit (0-255)
	// 
	// ssbo member struct
	// layout of the struct
	// std430
	struct VirtualTile 
	{
		// glm::ivec2/4
		int flag;
		//int id;
	};

	struct VirtualTileTable
	{
		int count;
		int id[TILE_COUNT * TILE_COUNT];
	} virtualTileTable;

	struct UsedVirtualTileViewProj
	{
		glm::mat4 virtualTileViewProj;
	};

	struct TileOffset
	{
		uint32_t tileOffset;
	} tileOffset;

	struct {
		VkQueue queue;								// Separate queue for compute commands (queue family may differ from the one used for graphics)
		VkCommandPool commandPool;					// Use a separate command pool (queue family may differ from the one used for graphics)
		VkCommandBuffer commandBuffer;				// Command buffer storing the dispatch commands and barriers
		VkFence fence;								// Synchronization fence to avoid rewriting compute CB if still in use
		VkSemaphore semaphore;						//  ---- here to sync for shadow pass and compute pass
		VkDescriptorSetLayout descriptorSetLayout;	// Compute shader binding layout
		VkDescriptorSet descriptorSet;				// Compute shader bindings
		VkPipelineLayout pipelineLayout;			// Layout of the compute pipeline
		VkPipeline pipeline;						// Compute pipeline for updating particle positions
	} computeMarkUsedVirtualTiles;

	struct {
		VkQueue queue;								// Separate queue for compute commands (queue family may differ from the one used for graphics)
		VkCommandPool commandPool;					// Use a separate command pool (queue family may differ from the one used for graphics)
		VkCommandBuffer commandBuffer;				// Command buffer storing the dispatch commands and barriers
		VkFence fence;								// Synchronization fence to avoid rewriting compute CB if still in use
		VkSemaphore semaphore;						// Used as a wait semaphore for graphics submission
		VkDescriptorSetLayout descriptorSetLayout;	// Compute shader binding layout
		VkDescriptorSet descriptorSet;				// Compute shader bindings
		VkPipelineLayout pipelineLayout;			// Layout of the compute pipeline
		VkPipeline pipeline;						// Compute pipeline for updating particle positions
	} computePreparePhysicalTiles;

	std::vector<uint32_t> asyncQueueFamilyIndices;
	uint32_t graphicsQueueFamilyIndex = 0;
	struct {
		uint32_t queueFamilyIndex;					// Used to check if compute and graphics queue families differ and require additional barriers
		// be aware to sync between queues
		VkQueue queue;								// Separate queue for compute commands (queue family may differ from the one used for graphics)
		VkCommandPool commandPool;					// Use a separate command pool (queue family may differ from the one used for graphics)
		VkCommandBuffer commandBuffer;				// Command buffer storing the dispatch commands and barriers
		VkFence fence;								// Synchronization fence to avoid rewriting compute CB if still in use
		VkSemaphore semaphore;						// Used as a wait semaphore for graphics submission
		VkDescriptorSetLayout descriptorSetLayout;	// Compute shader binding layout
		VkDescriptorSet descriptorSet;				// Compute shader bindings
		VkPipelineLayout pipelineLayout;			// Layout of the compute pipeline
		VkPipeline pipeline;						// Compute pipeline for updating particle positions
	} computePerTileFrustumCulling;
	
	// -------------------VSM--------------------------

	struct {
		// Framebuffer resources for the deferred pass
		vks::Framebuffer *deferred;
		// Framebuffer resources for the shadow pass
		vks::Framebuffer *shadow;
	} frameBuffers{};

	VkCommandBuffer offScreenCmdBuffer{ VK_NULL_HANDLE };
	// Semaphore used to synchronize between offscreen and final scene rendering
	VkSemaphore offscreenSemaphore{ VK_NULL_HANDLE };

	VkCommandBuffer shadowmapCmdBuffer{ VK_NULL_HANDLE };
	// Semaphore used to synchronize between offscreen and final scene rendering
	VkSemaphore shadowmapSemaphore{ VK_NULL_HANDLE };

	VulkanExample() : VulkanExampleBase()
	{
		title = "Deferred shading with shadows";
		camera.type = Camera::CameraType::firstperson;
#if defined(__ANDROID__)
		camera.movementSpeed = 2.5f;
#else
		camera.movementSpeed = 5.0f;
		camera.rotationSpeed = 0.25f;
#endif
		camera.position = { 2.15f, 0.3f, -8.75f };
		camera.setRotation(glm::vec3(-0.75f, 12.5f, 0.0f));
		// zNear/zFar: distance to the near/far clipping planes (always positive)
		// reverse depth for the defered shading mrt depth buffer
		//camera.setPerspective(60.0f, (float)width / (float)height, zFar, zNear);
		// its the distance not coord so no need to swap
		camera.setPerspective(60.0f, (float)width / (float)height, zNear, zFar);
		timerSpeed *= 0.25f;

		enabledDeviceExtensions.push_back(VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME);
	}

	~VulkanExample()
	{
		// Frame buffers
		if (frameBuffers.deferred)
		{
			delete frameBuffers.deferred;
		}
		if (frameBuffers.shadow)
		{
			delete frameBuffers.shadow;
		}

		vkDestroyPipeline(device, pipelines.deferred, nullptr);
		vkDestroyPipeline(device, pipelines.offscreen, nullptr);
		vkDestroyPipeline(device, pipelines.shadowpass, nullptr);

		vkDestroyPipelineLayout(device, pipelineLayout, nullptr);

		vkDestroyDescriptorSetLayout(device, descriptorSetLayoutRenderShadowMap, nullptr);
		vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);

		// Uniform buffers
		uniformBuffers.composition.destroy();
		uniformBuffers.offscreen.destroy();
		uniformBuffers.shadowGeometryShader.destroy();

		uniformBuffers.ubSceneCompute.destroy();

		scene.~Model();

		// Textures
		//textures.model.colorMap.destroy();
		//textures.model.normalMap.destroy();
		//textures.background.colorMap.destroy();
		//textures.background.normalMap.destroy();

		vkDestroySemaphore(device, shadowmapSemaphore, nullptr);
		vkDestroySemaphore(device, offscreenSemaphore, nullptr);
	}

	// Enable physical device features required for this example
	virtual void getEnabledFeatures()
	{
		//imageAtomicMin need this device feature
		if (deviceFeatures.fragmentStoresAndAtomics) 
		{
			enabledFeatures.fragmentStoresAndAtomics = VK_TRUE;
		}

		// Geometry shader support is required for writing to multiple shadow map layers in one single pass
		if (deviceFeatures.geometryShader) {
			enabledFeatures.geometryShader = VK_TRUE;
		}
		else {
			vks::tools::exitFatal("Selected GPU does not support geometry shaders!", VK_ERROR_FEATURE_NOT_PRESENT);
		}
		// Enable anisotropic filtering if supported
		if (deviceFeatures.samplerAnisotropy) {
			enabledFeatures.samplerAnisotropy = VK_TRUE;
		}
		// Enable texture compression
		if (deviceFeatures.textureCompressionBC) {
			enabledFeatures.textureCompressionBC = VK_TRUE;
		}
		else if (deviceFeatures.textureCompressionASTC_LDR) {
			enabledFeatures.textureCompressionASTC_LDR = VK_TRUE;
		}
		else if (deviceFeatures.textureCompressionETC2) {
			enabledFeatures.textureCompressionETC2 = VK_TRUE;
		}
	}

	// Prepare a layered shadow map with each layer containing depth from a light's point of view
	// The shadow mapping pass uses geometry shader instancing to output the scene from the different
	// light sources' point of view to the layers of the depth attachment in one single pass
	void shadowSetup()
	{
		frameBuffers.shadow = new vks::Framebuffer(vulkanDevice);

		// Shadowmap properties
#if defined(__ANDROID__)
		// Use smaller shadow maps on mobile due to performance reasons
		frameBuffers.shadow->width = 1024;
		frameBuffers.shadow->height = 1024;
#else
		frameBuffers.shadow->width = 512;
		frameBuffers.shadow->height = 512;
#endif

		vks::AttachmentCreateInfo attachmentInfoColorPlaceHolder = {};
		attachmentInfoColorPlaceHolder.format = VK_FORMAT_R8G8B8A8_UNORM;
		attachmentInfoColorPlaceHolder.width = frameBuffers.shadow->width;
		attachmentInfoColorPlaceHolder.height = frameBuffers.shadow->height;
		attachmentInfoColorPlaceHolder.layerCount = 1;
		attachmentInfoColorPlaceHolder.usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
		frameBuffers.shadow->addAttachment(attachmentInfoColorPlaceHolder);

		// Find a suitable depth format
		VkFormat shadowMapFormat;
		VkBool32 validShadowMapFormat = vks::tools::getSupportedDepthFormat(physicalDevice, &shadowMapFormat);
		assert(validShadowMapFormat);

		// Create a layered depth attachment for rendering the depth maps from the lights' point of view
		// Each layer corresponds to one of the lights
		// The actual output to the separate layers is done in the geometry shader using shader instancing
		// We will pass the matrices of the lights to the GS that selects the layer by the current invocation
		vks::AttachmentCreateInfo attachmentInfo = {};
		attachmentInfo.format = shadowMapFormat;
		attachmentInfo.width = frameBuffers.shadow->width;
		attachmentInfo.height = frameBuffers.shadow->height;
		attachmentInfo.layerCount = LIGHT_COUNT;
		attachmentInfo.usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
		frameBuffers.shadow->addAttachment(attachmentInfo);

		// Create sampler to sample from to depth attachment
		// Used to sample in the fragment shader for shadowed rendering
		VK_CHECK_RESULT(frameBuffers.shadow->createSampler(VK_FILTER_LINEAR, VK_FILTER_LINEAR, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE));

		// Create default renderpass for the framebuffer
		VK_CHECK_RESULT(frameBuffers.shadow->createRenderPass());
	}

	// Prepare the framebuffer for offscreen rendering with multiple attachments used as render targets inside the fragment shaders
	void deferredSetup()
	{
		frameBuffers.deferred = new vks::Framebuffer(vulkanDevice);

#if defined(__ANDROID__)
		// Use max. screen dimension as deferred framebuffer size
		frameBuffers.deferred->width = std::max(width, height);
		frameBuffers.deferred->height = std::max(width, height);
#else
		frameBuffers.deferred->width = 2048;
		frameBuffers.deferred->height = 2048;
#endif

		// Four attachments (3 color, 1 depth)
		vks::AttachmentCreateInfo attachmentInfo = {};
		attachmentInfo.width = frameBuffers.deferred->width;
		attachmentInfo.height = frameBuffers.deferred->height;
		attachmentInfo.layerCount = 1;
		attachmentInfo.usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;

		// Color attachments
		// Attachment 0: (World space) Positions
		attachmentInfo.format = VK_FORMAT_R16G16B16A16_SFLOAT;
		frameBuffers.deferred->addAttachment(attachmentInfo);

		// Attachment 1: (World space) Normals
		attachmentInfo.format = VK_FORMAT_R16G16B16A16_SFLOAT;
		frameBuffers.deferred->addAttachment(attachmentInfo);

		// Attachment 2: Albedo (color)
		attachmentInfo.format = VK_FORMAT_R8G8B8A8_UNORM;
		frameBuffers.deferred->addAttachment(attachmentInfo);

		// Depth attachment
		// Find a suitable depth format
		VkFormat attDepthFormat;
		VkBool32 validDepthFormat = vks::tools::getSupportedDepthFormat(physicalDevice, &attDepthFormat);
		assert(validDepthFormat);
		// D32 SFLOAT S8 UINT
		//attachmentInfo.width = GBUFFER_DEPTH_DIM;
		//attachmentInfo.height = GBUFFER_DEPTH_DIM;
		attachmentInfo.format = attDepthFormat;
		attachmentInfo.usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
		frameBuffers.deferred->addAttachment(attachmentInfo);

		// Create sampler to sample from the color attachments
		VK_CHECK_RESULT(frameBuffers.deferred->createSampler(VK_FILTER_NEAREST, VK_FILTER_NEAREST, VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE));

		// Create default renderpass for the framebuffer
		VK_CHECK_RESULT(frameBuffers.deferred->createRenderPass());
	}

	// Put render commands for the scene into the given command buffer
	void renderScene(VkCommandBuffer cmdBuffer, bool shadow)
	{
		// https://vkguide.dev/docs/chapter-5/drawing_images/#:~:text=Drawing%20Images.%20There%20are%20a%20few%20things%20we%20have%20to
		// 
		// validation layer:
		//vkCmdBindDescriptorSets(): descriptorSet #0 being bound is not compatible with overlapping descriptorSetLayout at index 1 of VkPipelineLayout 0x370f4b0000000251[] 
		// due to: VkDescriptorSetLayout 0x4a74fd000000024f[] from pipeline layout has 3 total descriptors,
		//  but VkDescriptorSetLayout 0xaa0f60000000024a[], which is bound, has 6 total descriptors.. 
		// The Vulkan spec states: Each element of pDescriptorSets must have been allocated with a VkDescriptorSetLayout that matches (is the same as, or identically defined as) the VkDescriptorSetLayout at set n in layout, where n is the sum of firstSet and the index into pDescriptorSets 
		// (https://vulkan.lunarg.com/doc/view/1.2.198.1/windows/1.2-extensions/vkspec.html#VUID-vkCmdBindDescriptorSets-pDescriptorSets-00358)
		// 
		// Background
		
		//vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, shadow ? &descriptorSets.shadow : &descriptorSets.background, 0, NULL);
		//models.background.draw(cmdBuffer);

		if (shadow)
		{
			vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSets.shadow, 0, NULL);
			if (enableVirtualShadowMap)
			{
				//The Vulkan spec states: For each set n that is statically used by the VkPipeline bound to the pipeline bind point used by this command, 
				// a descriptor set must have been bound to n at the same pipeline bind point, 
				// with a VkPipelineLayout that is compatible for set n, with the VkPipelineLayout used to create the current VkPipeline
				// VSM: first set = 1
				vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 1, 1, &descriptorSets.virtualShadowMap, 0, NULL);
			}

			// at the end of the prepare function the prepared variable was set to true after all the prepare work was done
			// 
			// the code doesnt enter this branch.......
			if (prepared && enablePerTileFrustumCulling)
			{
				//if (!culledNodeIndexes.empty())
				{
					int count = usedVirtualTileCounter.atomicCounter;
					//uint32_t count = static_cast<uint32_t>(usedVirtualTileCounter.atomicCounter);
					for (int j = 0; j < count; j++)
					{
						vkCmdPushConstants(
							cmdBuffer,
							pipelineLayout,
							VK_SHADER_STAGE_VERTEX_BIT,
							0,
							sizeof(int),
							&j);
						std::vector<int> nodeIndexTmp(nodeIndicesPerTiles[j].begin(), nodeIndicesPerTiles[j].end());
						scene.drawFrustumCulledNodes(cmdBuffer, vkglTF::RenderFlags::BindImages, pipelineLayout, 2, nodeIndexTmp);
					}
					//scene.drawFrustumCulledNodes(cmdBuffer, culledNodeIndexes);
				}
			}
			else
			{
				// this is where the code running in...
				scene.draw(cmdBuffer, vkglTF::RenderFlags::BindImages, pipelineLayout, 2);
			}
			
			//uint32_t count = static_cast<uint32_t>(usedVirtualTileCounter.atomicCounter);
			//for (uint32_t j = 0; j < count; j++) 
			//{
			//	vkCmdPushConstants(
			//		cmdBuffer,
			//		pipelineLayout,
			//		VK_SHADER_STAGE_VERTEX_BIT,
			//		0,
			//		sizeof(int),
			//		&j);
			//	// vsm:  usedVirtualTileCounter.atomicCounter
			//	// scene.drawFrustumCulledNodes
			//}
		}
		else
		{
			
			vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSets.offscreen, 0, NULL);

			scene.draw(cmdBuffer, vkglTF::RenderFlags::BindImages, pipelineLayout, 2);
		}
		
		//scene.draw(cmdBuffer);
		//scene.draw(cmdBuffer, vkglTF::RenderFlags::BindImages, pipelineLayout, 0);
		//scene.draw(cmdBuffer, vkglTF::RenderFlags::BindImages, pipelineLayout, 2);


		// Objects
		//if (enableVirtualShadowMap)
		//{
		//	vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 1, 1, shadow ? &descriptorSets.virtualShadowMap : &descriptorSets.background, 0, NULL);
		//}
		//
		//vkCmdBindDescriptorSets(cmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, shadow ? &descriptorSets.shadow : &descriptorSets.model, 0, NULL);
		////models.model.bindBuffers(cmdBuffer);
		////vkCmdDrawIndexed(cmdBuffer, models.model.indices.count, 3, 0, 0, 0);
		//models.model.draw(cmdBuffer);
	}

	void buildShadowMapCommandBuffer()
	{
		if (shadowmapCmdBuffer == VK_NULL_HANDLE) {
			shadowmapCmdBuffer = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, false);
		}

		// Create a semaphore used to synchronize offscreen rendering and usage
		VkSemaphoreCreateInfo semaphoreCreateInfo = vks::initializers::semaphoreCreateInfo();
		VK_CHECK_RESULT(vkCreateSemaphore(device, &semaphoreCreateInfo, nullptr, &shadowmapSemaphore));

		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VkRenderPassBeginInfo renderPassBeginInfo = vks::initializers::renderPassBeginInfo();
		std::array<VkClearValue, 2> clearValues = {};
		VkViewport viewport;
		VkRect2D scissor;

		// First pass: Shadow map generation
		// -------------------------------------------------------------------------------------------------------
		//frameBuffers.shadow->width frameBuffers.shadow->height

		clearValues[0].color = { { 0.0f, 0.0f, 0.0f, 0.0f } };
		//reverse depth for shadow map
		//clearValues[1].depthStencil = { 1.0f, 0 };
		clearValues[1].depthStencil = { 0.0f, 0 };

		renderPassBeginInfo.renderPass = frameBuffers.shadow->renderPass;
		renderPassBeginInfo.framebuffer = frameBuffers.shadow->framebuffer;
		//renderPassBeginInfo.renderArea.extent.width = frameBuffers.shadow->width;
		//renderPassBeginInfo.renderArea.extent.height = frameBuffers.shadow->height;
		renderPassBeginInfo.renderArea.extent.width = TILE_DIM;
		renderPassBeginInfo.renderArea.extent.height = TILE_DIM;

		renderPassBeginInfo.clearValueCount = static_cast<uint32_t>(clearValues.size());;
		renderPassBeginInfo.pClearValues = clearValues.data();

		VK_CHECK_RESULT(vkBeginCommandBuffer(shadowmapCmdBuffer, &cmdBufInfo));

		//TILE_DIM
		//viewport = vks::initializers::viewport((float)frameBuffers.shadow->width, (float)frameBuffers.shadow->height, 0.0f, 1.0f);
		//reverse depth for shadow map
		viewport = vks::initializers::viewport((float)TILE_DIM, (float)TILE_DIM, 1.0f, 0.0f);
		//viewport = vks::initializers::viewport((float)TILE_DIM, (float)TILE_DIM, 0.0f, 1.0f);
		vkCmdSetViewport(shadowmapCmdBuffer, 0, 1, &viewport);

		//scissor = vks::initializers::rect2D(frameBuffers.shadow->width, frameBuffers.shadow->height, 0, 0);
		scissor = vks::initializers::rect2D(TILE_DIM, TILE_DIM, 0, 0);
		vkCmdSetScissor(shadowmapCmdBuffer, 0, 1, &scissor);

		// Set depth bias (aka "Polygon offset")
		vkCmdSetDepthBias(
			shadowmapCmdBuffer,
			depthBiasConstant,
			0.0f,
			depthBiasSlope);

		vkCmdBeginRenderPass(shadowmapCmdBuffer, &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);
		vkCmdBindPipeline(shadowmapCmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.shadowpass);
		renderScene(shadowmapCmdBuffer, true);
		vkCmdEndRenderPass(shadowmapCmdBuffer);

		VK_CHECK_RESULT(vkEndCommandBuffer(shadowmapCmdBuffer));
	}

	// Build a secondary command buffer for rendering the scene values to the offscreen frame buffer attachments
	void buildGBufferCommandBuffer()
	{
		if (offScreenCmdBuffer == VK_NULL_HANDLE) {
			offScreenCmdBuffer = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, false);
		}

		// Create a semaphore used to synchronize offscreen rendering and usage
		VkSemaphoreCreateInfo semaphoreCreateInfo = vks::initializers::semaphoreCreateInfo();
		VK_CHECK_RESULT(vkCreateSemaphore(device, &semaphoreCreateInfo, nullptr, &offscreenSemaphore));

		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VkRenderPassBeginInfo renderPassBeginInfo = vks::initializers::renderPassBeginInfo();
		std::array<VkClearValue, 4> clearValues = {};
		VkViewport viewport;
		VkRect2D scissor;

		// First pass: Shadow map generation
		// -------------------------------------------------------------------------------------------------------

		//clearValues[0].depthStencil = { 1.0f, 0 };
		//
		//renderPassBeginInfo.renderPass = frameBuffers.shadow->renderPass;
		//renderPassBeginInfo.framebuffer = frameBuffers.shadow->framebuffer;
		//renderPassBeginInfo.renderArea.extent.width = frameBuffers.shadow->width;
		//renderPassBeginInfo.renderArea.extent.height = frameBuffers.shadow->height;
		//renderPassBeginInfo.clearValueCount = 1;
		//renderPassBeginInfo.pClearValues = clearValues.data();
		//
		//VK_CHECK_RESULT(vkBeginCommandBuffer(offScreenCmdBuffer, &cmdBufInfo));
		//
		//viewport = vks::initializers::viewport((float)frameBuffers.shadow->width, (float)frameBuffers.shadow->height, 0.0f, 1.0f);
		//vkCmdSetViewport(offScreenCmdBuffer, 0, 1, &viewport);
		//
		//scissor = vks::initializers::rect2D(frameBuffers.shadow->width, frameBuffers.shadow->height, 0, 0);
		//vkCmdSetScissor(offScreenCmdBuffer, 0, 1, &scissor);
		//
		//// Set depth bias (aka "Polygon offset")
		//vkCmdSetDepthBias(
		//	offScreenCmdBuffer,
		//	depthBiasConstant,
		//	0.0f,
		//	depthBiasSlope);
		//
		//vkCmdBeginRenderPass(offScreenCmdBuffer, &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);
		//vkCmdBindPipeline(offScreenCmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.shadowpass);
		//renderScene(offScreenCmdBuffer, true);
		//vkCmdEndRenderPass(offScreenCmdBuffer);

		// Second pass: Deferred calculations
		// -------------------------------------------------------------------------------------------------------

		// Clear values for all attachments written in the fragment shader
		clearValues[0].color = { { 0.0f, 0.0f, 0.0f, 0.0f } };
		clearValues[1].color = { { 0.0f, 0.0f, 0.0f, 0.0f } };
		clearValues[2].color = { { 0.0f, 0.0f, 0.0f, 0.0f } };

		// reverse depth for the mrt depth buffer
		//clearValues[3].depthStencil = { 1.0f, 0 };
		clearValues[3].depthStencil = { 0.0f, 0 };

		renderPassBeginInfo.renderPass = frameBuffers.deferred->renderPass;
		renderPassBeginInfo.framebuffer = frameBuffers.deferred->framebuffer;
		renderPassBeginInfo.renderArea.extent.width = frameBuffers.deferred->width;
		renderPassBeginInfo.renderArea.extent.height = frameBuffers.deferred->height;
		renderPassBeginInfo.clearValueCount = static_cast<uint32_t>(clearValues.size());
		renderPassBeginInfo.pClearValues = clearValues.data();

		VK_CHECK_RESULT(vkBeginCommandBuffer(offScreenCmdBuffer, &cmdBufInfo));

		vkCmdBeginRenderPass(offScreenCmdBuffer, &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);

		// reverse depth for the mrt depth buffer
		//https://docs.vulkan.org/guide/latest/depth.html
		//Despite their names, minDepth can be less than, equal to, or greater than maxDepth
		viewport = vks::initializers::viewport((float)frameBuffers.deferred->width, (float)frameBuffers.deferred->height, 1.0f, 0.0f);
		//viewport = vks::initializers::viewport((float)frameBuffers.deferred->width, (float)frameBuffers.deferred->height, 0.0f, 1.0f);
		vkCmdSetViewport(offScreenCmdBuffer, 0, 1, &viewport);

		scissor = vks::initializers::rect2D(frameBuffers.deferred->width, frameBuffers.deferred->height, 0, 0);
		vkCmdSetScissor(offScreenCmdBuffer, 0, 1, &scissor);

		vkCmdBindPipeline(offScreenCmdBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.offscreen);
		renderScene(offScreenCmdBuffer, false);
		vkCmdEndRenderPass(offScreenCmdBuffer);

		VK_CHECK_RESULT(vkEndCommandBuffer(offScreenCmdBuffer));
	}

	/*bool compareIndex(vkglTF::Model::NodeMinMax& A, vkglTF::Model::NodeMinMax& B)
	{
		return A.nodeIndex > B.nodeIndex;
	}

	bool indexEqual(vkglTF::Model::NodeMinMax& A, vkglTF::Model::NodeMinMax& B)
	{
		return A.nodeIndex == B.nodeIndex;
	}*/

	void loadAssets()
	{
		const uint32_t glTFLoadingFlags = vkglTF::FileLoadingFlags::PreTransformVertices | vkglTF::FileLoadingFlags::PreMultiplyVertexColors | vkglTF::FileLoadingFlags::FlipY;
		//const uint32_t glTFLoadingFlags = vkglTF::FileLoadingFlags::PreMultiplyVertexColors | vkglTF::FileLoadingFlags::FlipY;
		//scene.loadFromFile(getAssetPath() + "models/BistroGltf/Bistro.gltf", vulkanDevice, queue, glTFLoadingFlags);
		scene.loadFromFile(getAssetPath() + "models/sponza/sponza.gltf", vulkanDevice, queue, glTFLoadingFlags);
		scene.getSceneDimensions();

		if (!scene.nodesAABB.empty())
		{
			// todo: calculate bbx for each one node so no need to erase repeat
			std::sort(scene.nodesAABB.begin(), scene.nodesAABB.end(), [](vkglTF::Model::NodeMinMax& A, vkglTF::Model::NodeMinMax& B){ return A.nodeIndex < B.nodeIndex; });
			scene.nodesAABB.erase(std::unique(scene.nodesAABB.begin(), scene.nodesAABB.end(), 
				[](vkglTF::Model::NodeMinMax& A, vkglTF::Model::NodeMinMax& B) { return A.nodeIndex == B.nodeIndex; }), scene.nodesAABB.end());
			
			culledNodeIndexes.resize(PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT * scene.nodesAABB.size());

		}
		
		nodeIndicesPerTiles.resize(PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT);
		/*for (int i = 0; i < PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT; i++)
		{
			nodeIndicesPerTiles[i].resize(NUM_OF_NODES_PER_TILE);
		}*/

		sceneRadius = scene.dimensions.radius;

		//models.model.loadFromFile(getAssetPath() + "models/armor/armor.gltf", vulkanDevice, queue, glTFLoadingFlags);
		//models.background.loadFromFile(getAssetPath() + "models/deferred_box.gltf", vulkanDevice, queue, glTFLoadingFlags);
		//models.model.getSceneDimensions();
		//models.background.getSceneDimensions();
		//if (models.model.dimensions.radius >= models.background.dimensions.radius)
		//{
		//	sceneRadius = models.model.dimensions.radius;
		//}
		//else
		//{
		//	sceneRadius = models.background.dimensions.radius;
		//}
		//
		////sceneRadius
		////scene.loadFromFile(getAssetPath() + "models/sponza/sponza.gltf", vulkanDevice, queue, glTFLoadingFlags);
		//textures.model.colorMap.loadFromFile(getAssetPath() + "models/armor/colormap_rgba.ktx", VK_FORMAT_R8G8B8A8_UNORM, vulkanDevice, queue);
		//textures.model.normalMap.loadFromFile(getAssetPath() + "models/armor/normalmap_rgba.ktx", VK_FORMAT_R8G8B8A8_UNORM, vulkanDevice, queue);
		//textures.background.colorMap.loadFromFile(getAssetPath() + "textures/stonefloor02_color_rgba.ktx", VK_FORMAT_R8G8B8A8_UNORM, vulkanDevice, queue);
		//textures.background.normalMap.loadFromFile(getAssetPath() + "textures/stonefloor02_normal_rgba.ktx", VK_FORMAT_R8G8B8A8_UNORM, vulkanDevice, queue);
	}

	void buildMarkUsedVirtualTileCommandBuffer()
	{
		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VK_CHECK_RESULT(vkBeginCommandBuffer(computeMarkUsedVirtualTiles.commandBuffer, &cmdBufInfo));

		// Compute particle movement

		// GBuffer pass depth -> mark used virtual tile compute pass
		// need barrier for depth writing?
		// need semaphore
		// 
		// Add memory barrier to ensure that the (graphics) vertex shader has fetched attributes before compute starts to write to the buffer
		/*if (graphics.queueFamilyIndex != compute.queueFamilyIndex)
		{
			VkBufferMemoryBarrier buffer_barrier =
			{
				VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
				nullptr,
				0,
				VK_ACCESS_SHADER_WRITE_BIT,
				graphics.queueFamilyIndex,
				compute.queueFamilyIndex,
				storageBuffer.buffer,
				0,
				storageBuffer.size
			};

			vkCmdPipelineBarrier(
				compute.commandBuffer,
				VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,
				VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
				0,
				0, nullptr,
				1, &buffer_barrier,
				0, nullptr);
		}*/

		// Dispatch the compute job
		vkCmdBindPipeline(computeMarkUsedVirtualTiles.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computeMarkUsedVirtualTiles.pipeline);
		vkCmdBindDescriptorSets(computeMarkUsedVirtualTiles.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computeMarkUsedVirtualTiles.pipelineLayout, 0, 1, &computeMarkUsedVirtualTiles.descriptorSet, 0, 0);
		// dim = gbuffer depth map
		//vkCmdDispatch(compute.commandBuffer, PARTICLE_COUNT / 256, 1, 1);
		vkCmdDispatch(computeMarkUsedVirtualTiles.commandBuffer, frameBuffers.deferred->width, frameBuffers.deferred->width, 1);
		//GBUFFER_DEPTH_DIM

		// Add barrier to ensure that compute shader has finished writing to the buffer
		// Without this the (rendering) vertex shader may display incomplete results (partial data from last frame)
		/*if (graphics.queueFamilyIndex != compute.queueFamilyIndex)
		{
			VkBufferMemoryBarrier buffer_barrier =
			{
				VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
				nullptr,
				VK_ACCESS_SHADER_WRITE_BIT,
				0,
				compute.queueFamilyIndex,
				graphics.queueFamilyIndex,
				storageBuffer.buffer,
				0,
				storageBuffer.size
			};

			vkCmdPipelineBarrier(
				compute.commandBuffer,
				VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
				VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
				0,
				0, nullptr,
				1, &buffer_barrier,
				0, nullptr);
		}*/

		vkEndCommandBuffer(computeMarkUsedVirtualTiles.commandBuffer);
	}

	void buildPreparePhysicalTilesCommandBuffer()
	{
		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VK_CHECK_RESULT(vkBeginCommandBuffer(computePreparePhysicalTiles.commandBuffer, &cmdBufInfo));

		// Dispatch the compute job
		vkCmdBindPipeline(computePreparePhysicalTiles.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePreparePhysicalTiles.pipeline);
		vkCmdBindDescriptorSets(computePreparePhysicalTiles.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePreparePhysicalTiles.pipelineLayout, 0, 1, &computePreparePhysicalTiles.descriptorSet, 0, 0);
		
		// invocation dim = local size = 32
		//invocation dim = local size = 8
		vkCmdDispatch(computePreparePhysicalTiles.commandBuffer, TILE_COUNT / 8, TILE_COUNT / 8, 1);

		vkEndCommandBuffer(computePreparePhysicalTiles.commandBuffer);
	}

	void buildFrustumCullingCommandBuffer()
	{
		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VK_CHECK_RESULT(vkBeginCommandBuffer(computePerTileFrustumCulling.commandBuffer, &cmdBufInfo));

		// Dispatch the compute job
		vkCmdBindPipeline(computePerTileFrustumCulling.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePerTileFrustumCulling.pipeline);
		vkCmdBindDescriptorSets(computePerTileFrustumCulling.commandBuffer, VK_PIPELINE_BIND_POINT_COMPUTE, computePerTileFrustumCulling.pipelineLayout, 0, 1, &computePerTileFrustumCulling.descriptorSet, 0, 0);

		vkCmdDispatch(computePerTileFrustumCulling.commandBuffer, PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT / 16, 1, 1);

		//Queue ownership transfer is only required when we need the content to remain valid across queues
		// 
		//// Add barrier to ensure that compute shader has finished writing to the buffer
		/*if (graphicsQueueFamilyIndex != computePerTileFrustumCulling.queueFamilyIndex)
		{
			VkBufferMemoryBarrier bufferBarrier =
			{
				VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
				nullptr,
				VK_ACCESS_SHADER_WRITE_BIT,
				0,
				computePerTileFrustumCulling.queueFamilyIndex,
				graphicsQueueFamilyIndex,
				visibleNodesIndexBuffer.buffer,
				0,
				visibleNodesIndexBuffer.size
			};

			vkCmdPipelineBarrier(
				computePerTileFrustumCulling.commandBuffer,
				VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
				VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
				0,
				0, nullptr,
				1, &bufferBarrier,
				0, nullptr);
		}*/

		VkMemoryBarrier toCPUBarrier{ VK_STRUCTURE_TYPE_MEMORY_BARRIER };
		toCPUBarrier.srcAccessMask = VK_ACCESS_SHADER_WRITE_BIT;    // Make shader writes
		toCPUBarrier.dstAccessMask = VK_ACCESS_HOST_READ_BIT;       // Readable by the CPU
		vkCmdPipelineBarrier(computePerTileFrustumCulling.commandBuffer,                             // The command buffer
			VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,  // From the compute shader
			VK_PIPELINE_STAGE_HOST_BIT,            // To the CPU
			0,                                     // No special flags
			1,
			&toCPUBarrier,  // An array of memory barriers
			0, nullptr, 0,
			nullptr);  // No other barriers
	

		/*VkBufferMemoryBarrier bufferBarrier2 =
		{
			VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
			nullptr,
			VK_ACCESS_SHADER_WRITE_BIT,
			VK_ACCESS_HOST_READ_BIT,
			computePerTileFrustumCulling.queueFamilyIndex,
			graphicsQueueFamilyIndex,
			visibleNodesIndexBuffer.buffer,
			0,
			visibleNodesIndexBuffer.size
		};

		vkCmdPipelineBarrier(
			computePerTileFrustumCulling.commandBuffer,
			VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
			VK_PIPELINE_STAGE_HOST_BIT,
			0,
			0, nullptr,
			1, &bufferBarrier2,
			0, nullptr);*/

		vkEndCommandBuffer(computePerTileFrustumCulling.commandBuffer);
	}

	void buildCommandBuffers()
	{
		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VkClearValue clearValues[2];
		clearValues[0].color = { { 0.0f, 0.0f, 0.2f, 0.0f } };
		clearValues[1].depthStencil = { 1.0f, 0 };

		VkRenderPassBeginInfo renderPassBeginInfo = vks::initializers::renderPassBeginInfo();
		renderPassBeginInfo.renderPass = renderPass;
		renderPassBeginInfo.renderArea.offset.x = 0;
		renderPassBeginInfo.renderArea.offset.y = 0;
		renderPassBeginInfo.renderArea.extent.width = width;
		renderPassBeginInfo.renderArea.extent.height = height;
		renderPassBeginInfo.clearValueCount = 2;
		renderPassBeginInfo.pClearValues = clearValues;

		for (int32_t i = 0; i < drawCmdBuffers.size(); ++i)
		{
			// Set target frame buffer
			renderPassBeginInfo.framebuffer = VulkanExampleBase::frameBuffers[i];

			VK_CHECK_RESULT(vkBeginCommandBuffer(drawCmdBuffers[i], &cmdBufInfo));

			vkCmdBeginRenderPass(drawCmdBuffers[i], &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);

			VkViewport viewport = vks::initializers::viewport((float)width, (float)height, 0.0f, 1.0f);
			vkCmdSetViewport(drawCmdBuffers[i], 0, 1, &viewport);

			VkRect2D scissor = vks::initializers::rect2D(width, height, 0, 0);
			vkCmdSetScissor(drawCmdBuffers[i], 0, 1, &scissor);

			vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSets.composition, 0, nullptr);
			vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 1, 1, &descriptorSets.virtualShadowMap, 0, NULL);

			// Final composition as full screen quad
			// Note: Also used for debug display if debugDisplayTarget > 0
			vkCmdBindPipeline(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelines.deferred);
			vkCmdDraw(drawCmdBuffers[i], 3, 1, 0, 0);

			drawUI(drawCmdBuffers[i]);

			vkCmdEndRenderPass(drawCmdBuffers[i]);

			VK_CHECK_RESULT(vkEndCommandBuffer(drawCmdBuffers[i]));
		}
	}

	void setupDescriptors()
	{
		// Pool
		/*std::vector<VkDescriptorPoolSize> poolSizes = {
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 18),
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 16),
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 3)
		};*/

		// TODO reuse; dynamic/bindless
		std::vector<VkDescriptorPoolSize> poolSizes = {
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 60), // 30
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 20),
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 20), // 4
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, 3)
			//VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
		};

		VkDescriptorPoolCreateInfo descriptorPoolInfo =vks::initializers::descriptorPoolCreateInfo(poolSizes, 8);
		//descriptorPoolInfo.maxSets ??

		VK_CHECK_RESULT(vkCreateDescriptorPool(device, &descriptorPoolInfo, nullptr, &descriptorPool));

		// Layout
		std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings = {
			// Binding 0: Vertex shader uniform buffer   VK_SHADER_STAGE_GEOMETRY_BIT
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, VK_SHADER_STAGE_VERTEX_BIT, 0),
			// Binding 1: Position texture
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 1),
			// Binding 2: Normals texture
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 2),
			// Binding 3: Albedo texture
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 3),
			// Binding 4: Fragment shader uniform buffer
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, VK_SHADER_STAGE_FRAGMENT_BIT, 4),
			// Binding 5: Shadow map
			//vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 5),
			//vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, VK_SHADER_STAGE_FRAGMENT_BIT, 5),
			//vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, VK_SHADER_STAGE_FRAGMENT_BIT, 6),
		};
		VkDescriptorSetLayoutCreateInfo descriptorLayout = vks::initializers::descriptorSetLayoutCreateInfo(setLayoutBindings);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &descriptorSetLayout));

		// Sets
		std::vector<VkWriteDescriptorSet> writeDescriptorSets;
		VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &descriptorSetLayout, 1);

		// Image descriptors for the offscreen color attachments
		VkDescriptorImageInfo texDescriptorPosition =
			vks::initializers::descriptorImageInfo(
				frameBuffers.deferred->sampler,
				frameBuffers.deferred->attachments[0].view,
				VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

		VkDescriptorImageInfo texDescriptorNormal =
			vks::initializers::descriptorImageInfo(
				frameBuffers.deferred->sampler,
				frameBuffers.deferred->attachments[1].view,
				VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

		VkDescriptorImageInfo texDescriptorAlbedo =
			vks::initializers::descriptorImageInfo(
				frameBuffers.deferred->sampler,
				frameBuffers.deferred->attachments[2].view,
				VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

		VkDescriptorImageInfo texDescriptorShadowMap =
			vks::initializers::descriptorImageInfo(
				frameBuffers.shadow->sampler,
				//frameBuffers.shadow->attachments[0].view,
				frameBuffers.shadow->attachments[1].view,
				VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL);

		// Deferred composition
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSets.composition));
		writeDescriptorSets = {
			// Binding 1: World space position texture
			vks::initializers::writeDescriptorSet(descriptorSets.composition, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, &texDescriptorPosition),
			// Binding 2: World space normals texture
			vks::initializers::writeDescriptorSet(descriptorSets.composition, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2, &texDescriptorNormal),
			// Binding 3: Albedo texture
			vks::initializers::writeDescriptorSet(descriptorSets.composition, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 3, &texDescriptorAlbedo),
			// Binding 4: Fragment shader uniform buffer
			vks::initializers::writeDescriptorSet(descriptorSets.composition, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 4, &uniformBuffers.composition.descriptor),
			// Binding 5: Shadow map
			//vks::initializers::writeDescriptorSet(descriptorSets.composition, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 5, &texDescriptorShadowMap),

			/*vks::initializers::writeDescriptorSet(
				descriptorSets.composition,
				VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
				5,
				&textureComputeTarget.descriptor),
			vks::initializers::writeDescriptorSet(
				descriptorSets.composition,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				6,
				&virtualTileTableBuffer.descriptor),*/

		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(writeDescriptorSets.size()), writeDescriptorSets.data(), 0, nullptr);

		// Offscreen (scene)

		// Model
		//VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSets.model));
		//writeDescriptorSets = {
		//	// Binding 0: Vertex shader uniform buffer
		//	vks::initializers::writeDescriptorSet(descriptorSets.model, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 0, &uniformBuffers.offscreen.descriptor),
		//	// Binding 1: Color map
		//	//vks::initializers::writeDescriptorSet(descriptorSets.model, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, &textures.model.colorMap.descriptor),
		//	// Binding 2: Normal map
		//	//vks::initializers::writeDescriptorSet(descriptorSets.model, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2, &textures.model.normalMap.descriptor)
		//};
		//vkUpdateDescriptorSets(device, static_cast<uint32_t>(writeDescriptorSets.size()), writeDescriptorSets.data(), 0, nullptr);

		// offscreen
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSets.offscreen));
		writeDescriptorSets = {
			// Binding 0: Vertex shader uniform buffer
			vks::initializers::writeDescriptorSet(descriptorSets.offscreen, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 0, &uniformBuffers.offscreen.descriptor),
			// Binding 1: Color map
			//vks::initializers::writeDescriptorSet(descriptorSets.background, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, &textures.background.colorMap.descriptor),
			// Binding 2: Normal map
			//vks::initializers::writeDescriptorSet(descriptorSets.background, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2, &textures.background.normalMap.descriptor)
		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(writeDescriptorSets.size()), writeDescriptorSets.data(), 0, nullptr);

		// Shadow mapping
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSets.shadow));
		writeDescriptorSets = {
			// Binding 0: Vertex shader uniform buffer
			vks::initializers::writeDescriptorSet(descriptorSets.shadow, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 0, &uniformBuffers.shadowGeometryShader.descriptor),
		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(writeDescriptorSets.size()), writeDescriptorSets.data(), 0, nullptr);

		// output depth in mrt frag
		// adjust the prepare function
		// 
		// VSM render shadow map into the physical tile:
		// descriptor sets, desciptor pool, pipelines, cmdbuffer, 
		// fragment shader FS world coord from VS->GS->FS
		// GS: cancel mvp multiply
		// make a new descriptor set layout for the shadow map rendering
		// binding 0: ssbo used virtual tile pagetable
		// binding 1: ssbo used vt view proj matrix
		// binding 2: storage image for physical image
		// 

		// Layout
		std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings2 = 
		{
			// Binding 0: Vertex shader 
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT, 0),
			// Binding 1: 
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, VK_SHADER_STAGE_VERTEX_BIT, 1),
			// Binding 2: 
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, VK_SHADER_STAGE_FRAGMENT_BIT, 2),
			// Binding 3:
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, VK_SHADER_STAGE_FRAGMENT_BIT, 3),
		};
		VkDescriptorSetLayoutCreateInfo descriptorLayout2 = vks::initializers::descriptorSetLayoutCreateInfo(setLayoutBindings2);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout2, nullptr, &descriptorSetLayoutRenderShadowMap));

		// desciptor pool needs to be changed
		VkDescriptorSetAllocateInfo allocInfo2 = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &descriptorSetLayoutRenderShadowMap, 1);
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo2, &descriptorSets.virtualShadowMap));
		std::vector<VkWriteDescriptorSet> shadowmapDescriptorSets = 
		{
			// Binding 0 : 
			vks::initializers::writeDescriptorSet(
				descriptorSets.virtualShadowMap,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				0,
				&virtualTileTableBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				descriptorSets.virtualShadowMap,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				1,
				&usedVirtualTileMatrixBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				descriptorSets.virtualShadowMap,
				VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
				2,
				&textureComputeTarget.descriptor), // storage image
			vks::initializers::writeDescriptorSet(
				descriptorSets.virtualShadowMap,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				3,
				&tileOffsetTableBuffer.descriptor),
		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(shadowmapDescriptorSets.size()), shadowmapDescriptorSets.data(), 0, NULL);
	}

	void preparePipelines()
	{

		VkPushConstantRange pushConstantRange{};
		pushConstantRange.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		pushConstantRange.offset = 0;
		pushConstantRange.size = sizeof(int);
		
		const std::vector<VkDescriptorSetLayout> setLayouts = { descriptorSetLayout, descriptorSetLayoutRenderShadowMap, vkglTF::descriptorSetLayoutImage }; 
		// descriptorSetLayout set = 0, 
		// descriptorSetLayoutRenderShadowMap virtual shadowmap: set = 1
		//vkglTF::descriptorSetLayoutImage set = 2 also set in firstSet of vkCmdBindDescriptorSets 

		// Layout
		VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = vks::initializers::pipelineLayoutCreateInfo(&descriptorSetLayout, 1);
		pipelineLayoutCreateInfo.pPushConstantRanges        = &pushConstantRange;
		pipelineLayoutCreateInfo.pushConstantRangeCount     = 1;
		pipelineLayoutCreateInfo.setLayoutCount             = setLayouts.size();
		pipelineLayoutCreateInfo.pSetLayouts                = setLayouts.data();
		VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pipelineLayoutCreateInfo, nullptr, &pipelineLayout));

		// Pipelines
		VkPipelineInputAssemblyStateCreateInfo inputAssemblyState = vks::initializers::pipelineInputAssemblyStateCreateInfo(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 0, VK_FALSE);
		VkPipelineRasterizationStateCreateInfo rasterizationState = vks::initializers::pipelineRasterizationStateCreateInfo(VK_POLYGON_MODE_FILL, VK_CULL_MODE_BACK_BIT, VK_FRONT_FACE_COUNTER_CLOCKWISE, 0);
		VkPipelineColorBlendAttachmentState blendAttachmentState = vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE);
		VkPipelineColorBlendStateCreateInfo colorBlendState = vks::initializers::pipelineColorBlendStateCreateInfo(1, &blendAttachmentState);
		VkPipelineDepthStencilStateCreateInfo depthStencilState = vks::initializers::pipelineDepthStencilStateCreateInfo(VK_TRUE, VK_TRUE, VK_COMPARE_OP_LESS_OR_EQUAL);
		VkPipelineViewportStateCreateInfo viewportState = vks::initializers::pipelineViewportStateCreateInfo(1, 1, 0);
		VkPipelineMultisampleStateCreateInfo multisampleState = vks::initializers::pipelineMultisampleStateCreateInfo(VK_SAMPLE_COUNT_1_BIT, 0);
		std::vector<VkDynamicState> dynamicStateEnables = {VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR};
		VkPipelineDynamicStateCreateInfo dynamicState = vks::initializers::pipelineDynamicStateCreateInfo(dynamicStateEnables);
		std::array<VkPipelineShaderStageCreateInfo, 2> shaderStages;

		VkGraphicsPipelineCreateInfo pipelineCI = vks::initializers::pipelineCreateInfo(pipelineLayout, renderPass);
		pipelineCI.pInputAssemblyState = &inputAssemblyState;
		pipelineCI.pRasterizationState = &rasterizationState;
		pipelineCI.pColorBlendState = &colorBlendState;
		pipelineCI.pMultisampleState = &multisampleState;
		pipelineCI.pViewportState = &viewportState;
		pipelineCI.pDepthStencilState = &depthStencilState;
		pipelineCI.pDynamicState = &dynamicState;
		pipelineCI.stageCount = static_cast<uint32_t>(shaderStages.size());
		pipelineCI.pStages = shaderStages.data();

		depthStencilState.depthTestEnable = VK_FALSE;
		// Final fullscreen composition pass pipeline
		rasterizationState.cullMode = VK_CULL_MODE_FRONT_BIT;
		shaderStages[0] = loadShader(getShadersPath() + "deferredshadows/deferred.vert.spv", VK_SHADER_STAGE_VERTEX_BIT);
		shaderStages[1] = loadShader(getShadersPath() + "deferredshadows/deferred.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT);
		// Empty vertex input state, vertices are generated by the vertex shader
		VkPipelineVertexInputStateCreateInfo emptyInputState = vks::initializers::pipelineVertexInputStateCreateInfo();
		pipelineCI.pVertexInputState = &emptyInputState;
		VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCI, nullptr, &pipelines.deferred));

		// Vertex input state from glTF model for pipeline rendering models
		pipelineCI.pVertexInputState = vkglTF::Vertex::getPipelineVertexInputState({ vkglTF::VertexComponent::Position, vkglTF::VertexComponent::UV, vkglTF::VertexComponent::Color, vkglTF::VertexComponent::Normal, vkglTF::VertexComponent::Tangent });
		rasterizationState.cullMode = VK_CULL_MODE_BACK_BIT;
		//rasterizationState.cullMode = VK_CULL_MODE_FRONT_BIT;
		//rasterizationState.frontFace = VK_FRONT_FACE_CLOCKWISE;

		// Offscreen pipeline
		// Separate render pass
		pipelineCI.renderPass = frameBuffers.deferred->renderPass;

		// Blend attachment states required for all color attachments
		// This is important, as color write mask will otherwise be 0x0 and you
		// won't see anything rendered to the attachment
		std::array<VkPipelineColorBlendAttachmentState, 3> blendAttachmentStates =
		{
			vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE),
			vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE),
			vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE)
		};
		colorBlendState.attachmentCount = static_cast<uint32_t>(blendAttachmentStates.size());
		colorBlendState.pAttachments = blendAttachmentStates.data();

		// reverse depth for the mrt depth buffer
		depthStencilState.depthTestEnable = VK_TRUE;
		depthStencilState.depthWriteEnable = VK_TRUE;
		depthStencilState.depthCompareOp = VK_COMPARE_OP_GREATER_OR_EQUAL;

		shaderStages[0] = loadShader(getShadersPath() + "deferredshadows/mrt.vert.spv", VK_SHADER_STAGE_VERTEX_BIT);
		shaderStages[1] = loadShader(getShadersPath() + "deferredshadows/mrt.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT);
		VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCI, nullptr, &pipelines.offscreen));

		// Shadow mapping pipeline
		// The shadow mapping pipeline uses geometry shader instancing (invocations layout modifier) to output
		// shadow maps for multiple lights sources into the different shadow map layers in one single render pass
		std::array<VkPipelineShaderStageCreateInfo, 2> shadowStages;
		shadowStages[0] = loadShader(getShadersPath() + "deferredshadows/shadow.vert.spv", VK_SHADER_STAGE_VERTEX_BIT);
		//shadowStages[1] = loadShader(getShadersPath() + "deferredshadows/shadow.geom.spv", VK_SHADER_STAGE_GEOMETRY_BIT);
		shadowStages[1] = loadShader(getShadersPath() + "deferredshadows/shadow.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT);

		pipelineCI.pStages = shadowStages.data();
		pipelineCI.stageCount = static_cast<uint32_t>(shadowStages.size());

		// shadowpass color attachment
		std::array<VkPipelineColorBlendAttachmentState, 1> blendAttachmentStatesShadowPass =
		{
			vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE)
		};
		// Shadow pass doesn't use any color attachments
		colorBlendState.attachmentCount = static_cast<uint32_t>(blendAttachmentStatesShadowPass.size());;
		colorBlendState.pAttachments = blendAttachmentStatesShadowPass.data();
		// Cull front faces
		rasterizationState.cullMode = VK_CULL_MODE_FRONT_BIT;

		// virtual shadowmap close the depth test here and use imageAtomicMin to compare and write depth
		depthStencilState.depthTestEnable = VK_FALSE;
		//depthStencilState.depthCompareOp = VK_COMPARE_OP_LESS_OR_EQUAL;
		// 
		// turn off this depth bias?
		// Enable depth bias
		rasterizationState.depthBiasEnable = VK_TRUE;
		// Add depth bias to dynamic state, so we can change it at runtime
		dynamicStateEnables.push_back(VK_DYNAMIC_STATE_DEPTH_BIAS);
		dynamicState = vks::initializers::pipelineDynamicStateCreateInfo(dynamicStateEnables);
		// Reset blend attachment state
		pipelineCI.renderPass = frameBuffers.shadow->renderPass;
		VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCI, nullptr, &pipelines.shadowpass));
	}

	// Prepare and initialize uniform buffer containing shader uniforms
	void prepareUniformBuffers()
	{
		// Offscreen vertex shader
		VK_CHECK_RESULT(vulkanDevice->createBuffer(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, &uniformBuffers.offscreen, sizeof(UniformDataOffscreen), nullptr));

		// Deferred fragment shader
		VK_CHECK_RESULT(vulkanDevice->createBuffer(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, &uniformBuffers.composition, sizeof(UniformDataComposition), nullptr));

		// Shadow map vertex shader (matrices from shadow's pov)
		VK_CHECK_RESULT(vulkanDevice->createBuffer(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, &uniformBuffers.shadowGeometryShader, sizeof(UniformDataShadows), nullptr));

		// Map persistent
		VK_CHECK_RESULT(uniformBuffers.offscreen.map());
		VK_CHECK_RESULT(uniformBuffers.composition.map());
		VK_CHECK_RESULT(uniformBuffers.shadowGeometryShader.map());

		// Setup instanced model positions
		uniformDataOffscreen.instancePos[0] = glm::vec4(0.0f);
		uniformDataOffscreen.instancePos[1] = glm::vec4(-7.0f, 0.0, -4.0f, 0.0f);
		uniformDataOffscreen.instancePos[2] = glm::vec4(4.0f, 0.0, -6.0f, 0.0f);

		VK_CHECK_RESULT(vulkanDevice->createBuffer(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, &uniformBuffers.ubSceneCompute, sizeof(UniformDataCompute0), nullptr));
		VK_CHECK_RESULT(uniformBuffers.ubSceneCompute.map());

		VK_CHECK_RESULT(vulkanDevice->createBuffer(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, &uniformBuffers.ubSceneComputeShadowView, sizeof(glm::mat4), nullptr));
		VK_CHECK_RESULT(uniformBuffers.ubSceneComputeShadowView.map());
	}

	void updateUniformBufferOffscreen()
	{
		uniformDataOffscreen.projection = camera.matrices.perspective;
		uniformDataOffscreen.view = camera.matrices.view;
		uniformDataOffscreen.model = glm::mat4(1.0f);
		memcpy(uniformBuffers.offscreen.mapped, &uniformDataOffscreen, sizeof(uniformDataOffscreen));
	}

	void updateUniformBufferCompute0()
	{
		uniformDataComputeScene.sceneRadius = sceneRadius;
		uniformDataComputeScene.camInvViewProj = glm::inverse(camera.matrices.perspective * camera.matrices.view);

		memcpy(uniformBuffers.ubSceneCompute.mapped, &uniformDataComputeScene, sizeof(uniformDataComputeScene));

	}

	Light initLight(glm::vec3 pos, glm::vec3 target, glm::vec3 color)
	{
		Light light;
		light.position = glm::vec4(pos, 1.0f);
		light.target = glm::vec4(target, 0.0f);
		light.color = glm::vec4(color, 0.0f);
		return light;
	}

	void initLights()
	{
		//scene.dimensions
		//scene.dimensions.center
		uniformDataComposition.lights[0] = initLight(glm::vec3(40.0f, -50.0f, 25.0f), scene.dimensions.center, glm::vec3(1.0f, 0.5f, 0.5f));
		//uniformDataComposition.lights[0] = initLight(glm::vec3(40.0f, -50.0f, 25.0f), glm::vec3(-2.0f, 0.0f, 0.0f), glm::vec3(1.0f, 0.5f, 0.5f));
		//uniformDataComposition.lights[1] = initLight(glm::vec3(14.0f, -4.0f, 12.0f), glm::vec3(2.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
		//uniformDataComposition.lights[2] = initLight(glm::vec3(0.0f, -10.0f, 4.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(1.0f, 1.0f, 1.0f));
	}

	// Update deferred composition fragment shader light position and parameters uniform block
	void updateUniformBufferDeferred()
	{
		// Animate
		if (enableDynamicLighting)
		{
			uniformDataComposition.lights[0].position.x = -14.0f + std::abs(sin(glm::radians(timer * 360.0f)) * 20.0f);
			uniformDataComposition.lights[0].position.z = 15.0f + cos(glm::radians(timer * 360.0f)) * 1.0f;
		}
		else
		{
			// -14 + [0, 20] = [-14, 6]
			//uniformDataComposition.lights[0].position.x = -14.0f + std::abs(sin(glm::radians(timer * 360.0f)) * 20.0f);
			//uniformDataComposition.lights[0].position.x = 40.0f;
			//uniformDataComposition.lights[0].position.z = 25.0f;
		}

		//uniformDataComposition.lights[1].position.x = 14.0f - std::abs(sin(glm::radians(timer * 360.0f)) * 2.5f);
		//uniformDataComposition.lights[1].position.z = 13.0f + cos(glm::radians(timer *360.0f)) * 4.0f;

		//uniformDataComposition.lights[2].position.x = 0.0f + sin(glm::radians(timer *360.0f)) * 4.0f;
		//uniformDataComposition.lights[2].position.z = 4.0f + cos(glm::radians(timer *360.0f)) * 2.0f;

		for (uint32_t i = 0; i < LIGHT_COUNT; i++) 
		{
			// mvp from light's pov (for shadows)
			//glm::mat4 shadowProj = glm::perspective(glm::radians(lightFOV), 1.0f, zNear, zFar);
			// 
			//tmp otho proj and the shadows should be parallel
			//glm::mat4 shadowProj = glm::ortho(-sceneRadius, sceneRadius, -sceneRadius, sceneRadius, zNear, sceneRadius * 2);
			//glm::mat4 shadowProj = glm::ortho(-100.0f, 100.0f, -100.0f, 100.0f, zNear, sceneRadius * 2);
			glm::mat4 shadowProj = glm::ortho(-sceneRadius, sceneRadius, -sceneRadius, sceneRadius, sceneRadius * 2, zNear);
			// zFar = 200.0f
			//glm::mat4 shadowProj = glm::ortho(-sceneRadius, sceneRadius, -sceneRadius, sceneRadius, sceneRadius * 2, zNear);

			glm::vec3 dir = glm::normalize(glm::vec3(uniformDataComposition.lights[i].target) - glm::vec3(uniformDataComposition.lights[i].position));
			//glm::vec3 shadowCamera = glm::vec3(uniformDataComposition.lights[i].target) - dir * sceneRadius * 3.0f;
			// camera is too far away from the target
			glm::vec3 shadowCamera = glm::vec3(uniformDataComposition.lights[i].target) + 8.0f * dir;
			glm::mat4 shadowView = glm::lookAt(shadowCamera, glm::vec3(uniformDataComposition.lights[i].target), glm::vec3(0.0f, 1.0f, 0.0f));
			glm::mat4 shadowModel = glm::mat4(1.0f);

			uniformDataShadows.proj[i] = shadowProj;
			uniformDataShadows.modelView[i] = shadowView * shadowModel;
			uniformDataComposition.lights[i].viewMatrix = shadowProj * shadowView * shadowModel;
		}

		memcpy(uniformDataShadows.instancePos, uniformDataOffscreen.instancePos, sizeof(UniformDataOffscreen::instancePos));
		memcpy(uniformBuffers.shadowGeometryShader.mapped, &uniformDataShadows, sizeof(UniformDataShadows));

		memcpy(uniformBuffers.ubSceneComputeShadowView.mapped, &uniformDataShadows.modelView, sizeof(UniformDataShadows::modelView));

		uniformDataComposition.viewPos = glm::vec4(camera.position, 0.0f) * glm::vec4(-1.0f, 1.0f, -1.0f, 1.0f);;
		uniformDataComposition.debugDisplayTarget = debugDisplayTarget;

		memcpy(uniformBuffers.composition.mapped, &uniformDataComposition, sizeof(uniformDataComposition));
	}

	// --------------------------VSM--------------------------------------------
	void prepareStorageBuffers()
	{
		// whole size of SSBO = 128 * 128 * 72 = 1179648
		// 128 * 128 * 4

		// size is now 16*16*4
		std::vector<VirtualTile> virtualTileBuffer(TILE_COUNT * TILE_COUNT);
		for (auto& VirtualTile : virtualTileBuffer) 
		{
			VirtualTile.flag = -1;
			//VirtualTile.id   = -1;
			//VirtualTile.viewProj = glm::mat4(1);
		}

		VkDeviceSize storageBufferSize = virtualTileBuffer.size() * sizeof(VirtualTile);

		// Staging
		// SSBO won't be changed on the host after upload so copy to device local memory

		vks::Buffer stagingBuffer;

		vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&stagingBuffer,
			storageBufferSize,
			virtualTileBuffer.data());

		vulkanDevice->createBuffer(
			// The SSBO will be used as a storage buffer for the compute pipeline
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			&virtualTileFlagBuffer,
			storageBufferSize);

		vulkanDevice->copyBuffer(&stagingBuffer, &virtualTileFlagBuffer, queue);

		stagingBuffer.destroy();

		//std::vector<VirtualTile> virtualTileTableBuffer(TILE_COUNT * TILE_COUNT);
		/*for (auto& id : virtualTileTable.id)
		{
			id = -1;
		}*/

		memset(&virtualTileTable, -1, sizeof(virtualTileTable));
		//virtualTileTable.count = 0;

		VkDeviceSize storageBufferSize3 = sizeof(virtualTileTable);

		// Staging
		// SSBO won't be changed on the host after upload so copy to device local memory

		vks::Buffer stagingBuffer3;

		//virtualTileTable.id
		vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&stagingBuffer3,
			storageBufferSize3,
			&virtualTileTable);

		vulkanDevice->createBuffer(
			// The SSBO will be used as a storage buffer for the compute pipeline
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			&virtualTileTableBuffer,
			storageBufferSize3);

		vulkanDevice->copyBuffer(&stagingBuffer3, &virtualTileTableBuffer, queue);

		stagingBuffer3.destroy();

		//tileOffsetTableBuffer
		std::vector<TileOffset> TileBuffer(TILE_COUNT * TILE_COUNT);
		for (auto& tileOffset : TileBuffer)
		{
			tileOffset.tileOffset = 0;
		}

		VkDeviceSize storageBufferSize4 = TileBuffer.size() * sizeof(TileOffset);

		// Staging
		// SSBO won't be changed on the host after upload so copy to device local memory

		vks::Buffer stagingBuffer4;

		vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&stagingBuffer4,
			storageBufferSize4,
			TileBuffer.data());

		vulkanDevice->createBuffer(
			// The SSBO will be used as a storage buffer for the graphic pipeline
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			&tileOffsetTableBuffer,
			storageBufferSize4);

		vulkanDevice->copyBuffer(&stagingBuffer4, &tileOffsetTableBuffer, queue);

		stagingBuffer4.destroy();

		std::vector<UsedVirtualTileViewProj> matrixBuffer(PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT);
		for (auto& usedVirtualTileViewProj : matrixBuffer)
		{
			usedVirtualTileViewProj.virtualTileViewProj = glm::mat4(1.0f);
		}

		VkDeviceSize storageBufferSize2 = matrixBuffer.size() * sizeof(UsedVirtualTileViewProj);

		// Staging
		// SSBO won't be changed on the host after upload so copy to device local memory

		vks::Buffer stagingBuffer2;

		vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&stagingBuffer2,
			storageBufferSize2,
			matrixBuffer.data());

		vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			&usedVirtualTileMatrixBuffer,
			storageBufferSize2,
			false);

		vulkanDevice->copyBuffer(&stagingBuffer2, &usedVirtualTileMatrixBuffer, queue);

		stagingBuffer2.destroy();

		usedVirtualTileCounter.atomicCounter = 0;
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&usedVirualTileCountBuffer,
			sizeof(usedVirtualTileCounter),
			nullptr));

		// Map for host access
		VK_CHECK_RESULT(usedVirualTileCountBuffer.map());


		//nodesAABBBuffer
		//padding?
		struct NodeMinMaxTmp
		{
			glm::vec4 minPt;
			glm::vec4 maxPt;
			int nodeIndex;
			float __padding[3];
			//cpp struct¡¯s size rounded up to a multiple of a vec4 (= 16 bytes)
		};

		if (enablePerTileFrustumCulling)
		{
			VkDeviceSize storageBufferSize4 = 0;
			if (!scene.nodesAABB.empty())
			{
				storageBufferSize4 = scene.nodesAABB.size() * sizeof(NodeMinMaxTmp);

				vks::Buffer stagingBuffer4;

				vulkanDevice->createBuffer(
					VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
					VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
					&stagingBuffer4,
					storageBufferSize4,
					scene.nodesAABB.data());

				vulkanDevice->createBuffer(
					// The SSBO will be used as a storage buffer for the compute pipeline
					VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
					VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
					&nodesAABBBuffer,
					storageBufferSize4,
					false);

				vulkanDevice->copyBuffer(&stagingBuffer4, &nodesAABBBuffer, queue);

				stagingBuffer4.destroy();
			}

			// VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
		// readback this visible nodes buffer
		// the size of this buffer?

		//sharingMode is a VkSharingMode value specifying the sharing mode of the buffer when it will be accessed by multiple queue families.
		//queueFamilyIndexCount is the number of entries in the pQueueFamilyIndices array.
		//pQueueFamilyIndices is a pointer to an array of queue families that will access this buffer.It is ignored if sharingMode is not VK_SHARING_MODE_CONCURRENT.

		/*VK_CHECK_RESULT(vulkanDevice->createBufferAsync(
			VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&visibleNodesIndexBuffer,
			PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT * NUM_OF_NODES_PER_TILE * sizeof(int),
			asyncQueueFamilyIndices));*/

			//NUM_OF_NODES__PER_TILES
			//scene.nodesAABB.size() * sizeof(int)

			VK_CHECK_RESULT(vulkanDevice->createBuffer(
				VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
				VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
				&visibleNodesIndexBuffer,
				PHYSICAL_TILE_COUNT* PHYSICAL_TILE_COUNT* scene.nodesAABB.size() * sizeof(uint32_t)));

			// all the nodes are visible to every frustum
			//PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT * scene.nodesAABB.size() * sizeof(uint32_t)


			// Map for host access
			VK_CHECK_RESULT(visibleNodesIndexBuffer.map());

			//visibleNodesCounter
			visibleNodesCounter.atomicCounter = 0;
			VK_CHECK_RESULT(vulkanDevice->createBuffer(
				VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
				VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
				&visibleNodesCountBuffer,
				sizeof(visibleNodesCounter),
				nullptr));

			// Map for host access
			VK_CHECK_RESULT(visibleNodesCountBuffer.map());
		}
		
	}

	void prepareComputeMarkUsedVirtualTiles()
	{
		// Create a compute capable device queue
		// The VulkanDevice::createLogicalDevice functions finds a compute capable queue and prefers queue families that only support compute
		// Depending on the implementation this may result in different queue family indices for graphics and computes,
		// requiring proper synchronization (see the memory and pipeline barriers)
		vkGetDeviceQueue(device, vulkanDevice->queueFamilyIndices.compute, 0, &computeMarkUsedVirtualTiles.queue);

		// Create compute pipeline
		// Compute pipelines are created separate from graphics pipelines even if they use the same queue (family index)

		// add the VK_IMAGE_USAGE_SAMPLED_BIT usage flag for the depth attachment otherwise it could not be sampled
		// for the GBuffer renderpass: C=Store, D=Store, S=DontCare
		VkDescriptorImageInfo texDescriptorDepth =
			vks::initializers::descriptorImageInfo(
				frameBuffers.shadow->sampler, // VK_FILTER_LINEAR need to change;  frameBuffers.deferred->sampler frameBuffers.shadow->sampler
				frameBuffers.deferred->attachments[3].view,
				VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL);

		VkDescriptorImageInfo texDescriptorShadowMap =
			vks::initializers::descriptorImageInfo(
				frameBuffers.shadow->sampler,
				frameBuffers.shadow->attachments[0].view,
				VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL);

		std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings = {
			// Binding 0 : 
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				0),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				1),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				2),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				3)
		};
		VkDescriptorSetLayoutCreateInfo descriptorLayout = vks::initializers::descriptorSetLayoutCreateInfo(setLayoutBindings);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &computeMarkUsedVirtualTiles.descriptorSetLayout));

		VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &computeMarkUsedVirtualTiles.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &computeMarkUsedVirtualTiles.descriptorSet));
		std::vector<VkWriteDescriptorSet> computeWriteDescriptorSets = {
			// Binding 0 : 
			vks::initializers::writeDescriptorSet(
				computeMarkUsedVirtualTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				0,
				&virtualTileFlagBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computeMarkUsedVirtualTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				1,
				&uniformBuffers.composition.descriptor),
			vks::initializers::writeDescriptorSet(
				computeMarkUsedVirtualTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				2,
				&uniformBuffers.ubSceneCompute.descriptor),
			vks::initializers::writeDescriptorSet(
				computeMarkUsedVirtualTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
				3,
				&texDescriptorDepth) //texDescriptorDepth
			
		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(computeWriteDescriptorSets.size()), computeWriteDescriptorSets.data(), 0, NULL);

		// Create pipeline
		VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = vks::initializers::pipelineLayoutCreateInfo(&computeMarkUsedVirtualTiles.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pipelineLayoutCreateInfo, nullptr, &computeMarkUsedVirtualTiles.pipelineLayout));
		VkComputePipelineCreateInfo computePipelineCreateInfo = vks::initializers::computePipelineCreateInfo(computeMarkUsedVirtualTiles.pipelineLayout, 0);
		computePipelineCreateInfo.stage = loadShader(getShadersPath() + "deferredshadows/markUsedVirtualTiles.comp.spv", VK_SHADER_STAGE_COMPUTE_BIT);
		VK_CHECK_RESULT(vkCreateComputePipelines(device, pipelineCache, 1, &computePipelineCreateInfo, nullptr, &computeMarkUsedVirtualTiles.pipeline));
		//VkPhysicalDeviceLimits::maxComputeWorkGroupInvocations 1024= 32*32

		// Separate command pool as queue family for compute may be different than graphics
		VkCommandPoolCreateInfo cmdPoolInfo = {};
		cmdPoolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		cmdPoolInfo.queueFamilyIndex = vulkanDevice->queueFamilyIndices.compute;
		cmdPoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
		VK_CHECK_RESULT(vkCreateCommandPool(device, &cmdPoolInfo, nullptr, &computeMarkUsedVirtualTiles.commandPool));

		// Create a command buffer for compute operations
		computeMarkUsedVirtualTiles.commandBuffer = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, computeMarkUsedVirtualTiles.commandPool);

		// Semaphore for compute & graphics sync
		VkSemaphoreCreateInfo semaphoreCreateInfo = vks::initializers::semaphoreCreateInfo();
		VK_CHECK_RESULT(vkCreateSemaphore(device, &semaphoreCreateInfo, nullptr, &computeMarkUsedVirtualTiles.semaphore));

		// Build a single command buffer containing the compute dispatch commands
		//buildComputeCommandBuffer();
	}

	void preparePreparePhysicalTiles()
	{
		// Create a compute capable device queue
		// The VulkanDevice::createLogicalDevice functions finds a compute capable queue and prefers queue families that only support compute
		// Depending on the implementation this may result in different queue family indices for graphics and computes,
		// requiring proper synchronization (see the memory and pipeline barriers)
		vkGetDeviceQueue(device, vulkanDevice->queueFamilyIndices.compute, 0, &computePreparePhysicalTiles.queue);

		// Create compute pipeline
		// Compute pipelines are created separate from graphics pipelines even if they use the same queue (family index)

		// need 2 ssbo, 2 ubo for now
		std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings = {
			// Binding 0 : 
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				0),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				1),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				2),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				3),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				4),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				5),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				6),
		};
		VkDescriptorSetLayoutCreateInfo descriptorLayout = vks::initializers::descriptorSetLayoutCreateInfo(setLayoutBindings);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &computePreparePhysicalTiles.descriptorSetLayout));

		VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &computePreparePhysicalTiles.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &computePreparePhysicalTiles.descriptorSet));
		std::vector<VkWriteDescriptorSet> computeWriteDescriptorSets = {
			// Binding 0 : 
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				0,
				&virtualTileFlagBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				1,
				&uniformBuffers.composition.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				2,
				&uniformBuffers.ubSceneCompute.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				3,
				&usedVirtualTileMatrixBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				4,
				&virtualTileTableBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				5,
				&usedVirualTileCountBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePreparePhysicalTiles.descriptorSet,
				VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
				2,
				&uniformBuffers.ubSceneComputeShadowView.descriptor),

		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(computeWriteDescriptorSets.size()), computeWriteDescriptorSets.data(), 0, NULL);

		// Create pipeline
		VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = vks::initializers::pipelineLayoutCreateInfo(&computePreparePhysicalTiles.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pipelineLayoutCreateInfo, nullptr, &computePreparePhysicalTiles.pipelineLayout));
		VkComputePipelineCreateInfo computePipelineCreateInfo = vks::initializers::computePipelineCreateInfo(computePreparePhysicalTiles.pipelineLayout, 0);
		computePipelineCreateInfo.stage = loadShader(getShadersPath() + "deferredshadows/preparePhysicalTiles.comp.spv", VK_SHADER_STAGE_COMPUTE_BIT);
		VK_CHECK_RESULT(vkCreateComputePipelines(device, pipelineCache, 1, &computePipelineCreateInfo, nullptr, &computePreparePhysicalTiles.pipeline));
		//VkPhysicalDeviceLimits::maxComputeWorkGroupInvocations 1024= 32*32

		// Separate command pool as queue family for compute may be different than graphics
		VkCommandPoolCreateInfo cmdPoolInfo = {};
		cmdPoolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		cmdPoolInfo.queueFamilyIndex = vulkanDevice->queueFamilyIndices.compute;
		cmdPoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
		VK_CHECK_RESULT(vkCreateCommandPool(device, &cmdPoolInfo, nullptr, &computePreparePhysicalTiles.commandPool));

		// Create a command buffer for compute operations
		computePreparePhysicalTiles.commandBuffer = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, computePreparePhysicalTiles.commandPool);

		// Semaphore for compute & graphics sync
		VkSemaphoreCreateInfo semaphoreCreateInfo = vks::initializers::semaphoreCreateInfo();
		VK_CHECK_RESULT(vkCreateSemaphore(device, &semaphoreCreateInfo, nullptr, &computePreparePhysicalTiles.semaphore));

		// Build a single command buffer containing the compute dispatch commands
		//buildComputeCommandBuffer();
	}

	// virtual shadowmap tile render: CPU/GPU frustum culling + (todo)hiz culling
	void prepareComputeFrustumCulling()
	{
		vkGetDeviceQueue(device, vulkanDevice->queueFamilyIndices.compute, 0, &computePerTileFrustumCulling.queue);

		// Create compute pipeline
		// Compute pipelines are created separate from graphics pipelines even if they use the same queue (family index)

		// 
		std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings = {
			// Binding 0 : 
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				0),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				1),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				2),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				3),
			vks::initializers::descriptorSetLayoutBinding(
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				VK_SHADER_STAGE_COMPUTE_BIT,
				4)
		};
		VkDescriptorSetLayoutCreateInfo descriptorLayout = vks::initializers::descriptorSetLayoutCreateInfo(setLayoutBindings);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorLayout, nullptr, &computePerTileFrustumCulling.descriptorSetLayout));

		VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &computePerTileFrustumCulling.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &computePerTileFrustumCulling.descriptorSet));
		std::vector<VkWriteDescriptorSet> computeWriteDescriptorSets = {
			// Binding 0 : 
			vks::initializers::writeDescriptorSet(
				computePerTileFrustumCulling.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				0,
				&nodesAABBBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePerTileFrustumCulling.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				1,
				&usedVirtualTileMatrixBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePerTileFrustumCulling.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				2,
				&visibleNodesIndexBuffer.descriptor),
			//buffer->setupDescriptor();
			vks::initializers::writeDescriptorSet(
				computePerTileFrustumCulling.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				3,
				&visibleNodesCountBuffer.descriptor),
			vks::initializers::writeDescriptorSet(
				computePerTileFrustumCulling.descriptorSet,
				VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
				4,
				&usedVirualTileCountBuffer.descriptor)
		};
		vkUpdateDescriptorSets(device, static_cast<uint32_t>(computeWriteDescriptorSets.size()), computeWriteDescriptorSets.data(), 0, NULL);
		// draw() function : update Descriptors  for per-tile frustum culling:
		//   descriptor pools, descriptor set layout, descriptorSets
		//  
		//   need usedVirtualTileCounter.atomicCounter
		// 
		// descriptor set layout:
		//std::vector<VkDescriptorSetLayoutBinding> setLayoutBindings
		// 
		// descriptorSets:
		//std::vector<VkWriteDescriptorSet> computeWriteDescriptorSets 
		//visibleNodesIndexBuffer.setupDescriptor(); 
		//visibleNodesIndexBuffer.buffer;
		// 
		// for (int i = 0; i < usedVirtualTileCounter.atomicCounter; i++)
		//     
		//VkDescriptorBufferInfo descriptor;
		//descriptor.buffer = visibleNodesIndexBuffer.buffer;
		//descriptor.offset = i + ;
		//descriptor.range = NUM_OF_NODES_PER_TILE * sizeof(int);
		//  descriptor put in  vks::initializers::writeDescriptorSet
		// computeWriteDescriptorSets.push_back(VkWriteDescriptorSet )
		//vkUpdateDescriptorSets
		

		// Create pipeline
		VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = vks::initializers::pipelineLayoutCreateInfo(&computePerTileFrustumCulling.descriptorSetLayout, 1);
		VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pipelineLayoutCreateInfo, nullptr, &computePerTileFrustumCulling.pipelineLayout));
		VkComputePipelineCreateInfo computePipelineCreateInfo = vks::initializers::computePipelineCreateInfo(computePerTileFrustumCulling.pipelineLayout, 0);
		computePipelineCreateInfo.stage = loadShader(getShadersPath() + "deferredshadows/frustumCulling.comp.spv", VK_SHADER_STAGE_COMPUTE_BIT);
		VK_CHECK_RESULT(vkCreateComputePipelines(device, pipelineCache, 1, &computePipelineCreateInfo, nullptr, &computePerTileFrustumCulling.pipeline));
		//VkPhysicalDeviceLimits::maxComputeWorkGroupInvocations 1024= 32*32

		// Separate command pool as queue family for compute may be different than graphics
		VkCommandPoolCreateInfo cmdPoolInfo = {};
		cmdPoolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		cmdPoolInfo.queueFamilyIndex = vulkanDevice->queueFamilyIndices.compute;
		cmdPoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
		VK_CHECK_RESULT(vkCreateCommandPool(device, &cmdPoolInfo, nullptr, &computePerTileFrustumCulling.commandPool));

		// Create a command buffer for compute operations
		computePerTileFrustumCulling.commandBuffer = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, computePerTileFrustumCulling.commandPool);

		// Fence for compute CB sync
		VkFenceCreateInfo fenceCreateInfo = vks::initializers::fenceCreateInfo(); //VK_FENCE_CREATE_SIGNALED_BIT
		VK_CHECK_RESULT(vkCreateFence(device, &fenceCreateInfo, nullptr, &computePerTileFrustumCulling.fence));

		// Semaphore for compute & graphics sync
		VkSemaphoreCreateInfo semaphoreCreateInfo = vks::initializers::semaphoreCreateInfo();
		VK_CHECK_RESULT(vkCreateSemaphore(device, &semaphoreCreateInfo, nullptr, &computePerTileFrustumCulling.semaphore));
	}

	//for GL: float format need extension imageAtomicMin GL_EXT_shader_atomic_float2
	void prepareTextureTarget(vks::Texture* tex, uint32_t width, uint32_t height, VkFormat format)
	{
		VkFormatProperties formatProperties;

		// Get device properties for the requested texture format
		vkGetPhysicalDeviceFormatProperties(physicalDevice, format, &formatProperties);
		// Check if requested image format supports image storage operations
		assert(formatProperties.optimalTilingFeatures & VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT);

		if (!formatProperties.optimalTilingFeatures & VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT)
		{
			std::cout << "Provided format is not supported for atomic operations on storage images." << std::endl;
		}

		// Prepare blit target texture
		tex->width = width;
		tex->height = height;

		VkImageCreateInfo imageCreateInfo = vks::initializers::imageCreateInfo();
		imageCreateInfo.imageType = VK_IMAGE_TYPE_2D;
		imageCreateInfo.format = format;
		imageCreateInfo.extent = { width, height, 1 };
		imageCreateInfo.mipLevels = 1;
		imageCreateInfo.arrayLayers = 1;
		imageCreateInfo.samples = VK_SAMPLE_COUNT_1_BIT;
		imageCreateInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
		// Image will be sampled in the fragment shader and used as storage target in the compute shader
		imageCreateInfo.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_STORAGE_BIT;
		imageCreateInfo.flags = 0;
		// If compute and graphics queue family indices differ, we create an image that can be shared between them
		// This can result in worse performance than exclusive sharing mode, but save some synchronization to keep the sample simple
		std::vector<uint32_t> queueFamilyIndices;
		if (vulkanDevice->queueFamilyIndices.graphics != vulkanDevice->queueFamilyIndices.compute) {
			queueFamilyIndices = {
				vulkanDevice->queueFamilyIndices.graphics,
				vulkanDevice->queueFamilyIndices.compute
			};
			imageCreateInfo.sharingMode = VK_SHARING_MODE_CONCURRENT;
			imageCreateInfo.queueFamilyIndexCount = 2;
			imageCreateInfo.pQueueFamilyIndices = queueFamilyIndices.data();
		}

		VkMemoryAllocateInfo memAllocInfo = vks::initializers::memoryAllocateInfo();
		VkMemoryRequirements memReqs;

		VK_CHECK_RESULT(vkCreateImage(device, &imageCreateInfo, nullptr, &tex->image));

		vkGetImageMemoryRequirements(device, tex->image, &memReqs);
		memAllocInfo.allocationSize = memReqs.size;
		memAllocInfo.memoryTypeIndex = vulkanDevice->getMemoryType(memReqs.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
		VK_CHECK_RESULT(vkAllocateMemory(device, &memAllocInfo, nullptr, &tex->deviceMemory));
		VK_CHECK_RESULT(vkBindImageMemory(device, tex->image, tex->deviceMemory, 0));

		VkCommandBuffer layoutCmd = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, true);

		tex->imageLayout = VK_IMAGE_LAYOUT_GENERAL;
		vks::tools::setImageLayout(
			layoutCmd, tex->image,
			VK_IMAGE_ASPECT_COLOR_BIT,
			VK_IMAGE_LAYOUT_UNDEFINED,
			tex->imageLayout);

		vulkanDevice->flushCommandBuffer(layoutCmd, queue, true);

		// Create sampler
		VkSamplerCreateInfo sampler = vks::initializers::samplerCreateInfo();
		sampler.magFilter = VK_FILTER_LINEAR;
		sampler.minFilter = VK_FILTER_LINEAR;
		sampler.mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR;
		sampler.addressModeU = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
		sampler.addressModeV = sampler.addressModeU;
		sampler.addressModeW = sampler.addressModeU;
		sampler.mipLodBias = 0.0f;
		sampler.maxAnisotropy = 1.0f;
		sampler.compareOp = VK_COMPARE_OP_NEVER;
		sampler.minLod = 0.0f;
		sampler.maxLod = tex->mipLevels;
		sampler.borderColor = VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
		VK_CHECK_RESULT(vkCreateSampler(device, &sampler, nullptr, &tex->sampler));

		// Create image view
		VkImageViewCreateInfo view = vks::initializers::imageViewCreateInfo();
		view.image = VK_NULL_HANDLE;
		view.viewType = VK_IMAGE_VIEW_TYPE_2D;
		view.format = format;
		//view.components = { VK_COMPONENT_SWIZZLE_R, VK_COMPONENT_SWIZZLE_G, VK_COMPONENT_SWIZZLE_B, VK_COMPONENT_SWIZZLE_A };
		//VK_FORMAT_R32_UINT
		view.components = { VK_COMPONENT_SWIZZLE_R };
		view.subresourceRange = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1 };
		//VK_IMAGE_ASPECT_DEPTH_BIT?
		view.image = tex->image;
		VK_CHECK_RESULT(vkCreateImageView(device, &view, nullptr, &tex->view));

		// Initialize a descriptor for later use
		tex->descriptor.imageLayout = tex->imageLayout;
		tex->descriptor.imageView = tex->view;
		tex->descriptor.sampler = tex->sampler;
		tex->device = vulkanDevice;
	}

	// --------------------------VSM--------------------------------------------

	void prepare()
	{
		VulkanExampleBase::prepare();
		graphicsQueueFamilyIndex = vulkanDevice->queueFamilyIndices.graphics;
		asyncQueueFamilyIndices.push_back(graphicsQueueFamilyIndex);
		computePerTileFrustumCulling.queueFamilyIndex = vulkanDevice->queueFamilyIndices.compute;
		if (graphicsQueueFamilyIndex != computePerTileFrustumCulling.queueFamilyIndex)
		{
			asyncQueueFamilyIndices.push_back(computePerTileFrustumCulling.queueFamilyIndex);
		}
		loadAssets();
		prepareStorageBuffers();
		deferredSetup();
		shadowSetup();
		initLights();
		prepareUniformBuffers();

		// need to init the VkImage with a value of 1?
		// 
		//atomic operation on image have to be r32 int or uint
		//VK_FORMAT_R32_UINT VK_FORMAT_R8G8B8A8_UNORM
		prepareTextureTarget(&textureComputeTarget, PHYSICAL_TILE_COUNT * TILE_DIM, PHYSICAL_TILE_COUNT * TILE_DIM, VK_FORMAT_R32_UINT);

		// the image view of the textureComputeTarget has to be setup before setupDescriptors
		// textureComputeTarget.descriptor
		setupDescriptors();
		
		preparePipelines();

		// deferred shading
		buildGBufferCommandBuffer();
		buildCommandBuffers();
		
		//----virtual shadowmap----
		prepareComputeMarkUsedVirtualTiles();
		buildMarkUsedVirtualTileCommandBuffer();

		preparePreparePhysicalTiles();
		buildPreparePhysicalTilesCommandBuffer();

		if (enablePerTileFrustumCulling)
		{
			prepareComputeFrustumCulling();
			buildFrustumCullingCommandBuffer();
		}

		buildShadowMapCommandBuffer();
		//----virtual shadowmap----
		
		prepared = true;
	}

	void draw()
	{
		VulkanExampleBase::prepareFrame();

		// Offscreen rendering

		// Wait for swap chain presentation to finish
		submitInfo.pWaitSemaphores = &semaphores.presentComplete;
		// Signal ready with offscreen semaphore
		submitInfo.pSignalSemaphores = &offscreenSemaphore;

		// Submit work

		submitInfo.commandBufferCount = 1;
		submitInfo.pCommandBuffers = &offScreenCmdBuffer;
		VK_CHECK_RESULT(vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE));

		if (enableVirtualShadowMap)
		{
			VkPipelineStageFlags waitStageMask = VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;

			// Submit compute commands
			VkSubmitInfo computeSubmitInfo = vks::initializers::submitInfo();
			computeSubmitInfo.commandBufferCount = 1;
			computeSubmitInfo.pCommandBuffers = &computeMarkUsedVirtualTiles.commandBuffer;
			computeSubmitInfo.waitSemaphoreCount = 1;
			computeSubmitInfo.pWaitSemaphores = &offscreenSemaphore;
			computeSubmitInfo.pWaitDstStageMask = &waitStageMask;
			computeSubmitInfo.signalSemaphoreCount = 1;
			computeSubmitInfo.pSignalSemaphores = &computeMarkUsedVirtualTiles.semaphore;

			// submit compute work of mark used virtual tiles
			VK_CHECK_RESULT(vkQueueSubmit(computeMarkUsedVirtualTiles.queue, 1, &computeSubmitInfo, VK_NULL_HANDLE));

			// submit compute work of prepare physical tiles
			
			waitStageMask = VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;

			// Submit compute commands
			computeSubmitInfo = vks::initializers::submitInfo();
			computeSubmitInfo.commandBufferCount = 1;
			computeSubmitInfo.pCommandBuffers = &computePreparePhysicalTiles.commandBuffer;
			computeSubmitInfo.waitSemaphoreCount = 1;
			computeSubmitInfo.pWaitSemaphores = &computeMarkUsedVirtualTiles.semaphore;
			computeSubmitInfo.pWaitDstStageMask = &waitStageMask;
			computeSubmitInfo.signalSemaphoreCount = 1;
			computeSubmitInfo.pSignalSemaphores = &computePreparePhysicalTiles.semaphore;

			// submit compute work of computePreparePhysicalTiles
			VK_CHECK_RESULT(vkQueueSubmit(computePreparePhysicalTiles.queue, 1, &computeSubmitInfo, VK_NULL_HANDLE));

			//memcpy(&usedVirtualTileCounter, usedVirualTileCountBuffer.mapped, sizeof(usedVirtualTileCounter));

			//computePerTileFrustumCulling
			if (enablePerTileFrustumCulling)
			{
				computeSubmitInfo = vks::initializers::submitInfo();
				computeSubmitInfo.commandBufferCount = 1;
				computeSubmitInfo.pCommandBuffers = &computePerTileFrustumCulling.commandBuffer;
				computeSubmitInfo.waitSemaphoreCount = 1;
				computeSubmitInfo.pWaitSemaphores = &computePreparePhysicalTiles.semaphore;
				computeSubmitInfo.pWaitDstStageMask = &waitStageMask;
				computeSubmitInfo.signalSemaphoreCount = 1;
				computeSubmitInfo.pSignalSemaphores = &computePerTileFrustumCulling.semaphore;

				//vkQueueSubmit(): VkFence  submitted in SIGNALED state.
				VK_CHECK_RESULT(vkQueueSubmit(computePerTileFrustumCulling.queue, 1, &computeSubmitInfo, computePerTileFrustumCulling.fence));
			}
			
		}

		// CPU host to wait for the frustum culling compute task to finish by the fence
		// Wait for fence to ensure that compute buffer writes have finished
		if (enablePerTileFrustumCulling && computePerTileFrustumCulling.fence)
		{
			//https://docs.vulkan.org/samples/latest/samples/performance/async_compute/README.html
			//https://github.com/KhronosGroup/Vulkan-Docs/wiki/Synchronization-Examples-(Legacy-synchronization-APIs)#cpu-read-back-of-data-written-by-a-compute-shader
			vkWaitForFences(device, 1, &computePerTileFrustumCulling.fence, VK_TRUE, UINT64_MAX);
			vkResetFences(device, 1, &computePerTileFrustumCulling.fence);

			//visibleNodesCounter
			memcpy(&visibleNodesCounter, visibleNodesCountBuffer.mapped, sizeof(visibleNodesCounter));
			memcpy(&culledNodeIndexes[0], (uint32_t*)visibleNodesIndexBuffer.mapped, PHYSICAL_TILE_COUNT * PHYSICAL_TILE_COUNT * scene.nodesAABB.size() * sizeof(uint32_t));
			//usedVirtualTileCounter.atomicCounter * sizeof(uint32_t)
			// memcpy std::vector
			// 1. allocate space for culledNodeIndexes
			// 2. &culledNodeIndexes[0] : address of the first element

			if (!culledNodeIndexes.empty())
			{
				for (uint32_t i : culledNodeIndexes)
				{
					if (i != 0)
					{
						int tileIndex = (i >> 8) & 0xFF;
						int nodeIndex = i & 0xFF;
						//nodeIndicesPerTiles[tileIndex].push_back(nodeIndex);
						nodeIndicesPerTiles[tileIndex].insert(nodeIndex);
					}
				}
			}
		}

		// shadowmap physical image rendering
		if (enableVirtualShadowMap)
		{
			if (enablePerTileFrustumCulling)
			{
				submitInfo.pWaitSemaphores = &computePerTileFrustumCulling.semaphore;
			}
			else
			{
				submitInfo.pWaitSemaphores = &computePreparePhysicalTiles.semaphore;
			}
		}
		else
		{
			// Wait for offscreen semaphore
			submitInfo.pWaitSemaphores = &offscreenSemaphore;
		}
		submitInfo.pSignalSemaphores = &shadowmapSemaphore;
		// Submit work
		submitInfo.pCommandBuffers = &shadowmapCmdBuffer;
		VK_CHECK_RESULT(vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE));
		

		// Scene rendering
		// 
		//submitInfo.pWaitSemaphores = &computePreparePhysicalTiles.semaphore;//offscreenSemaphore
		// Signal ready with render complete semaphore
		submitInfo.pSignalSemaphores = &semaphores.renderComplete;
		submitInfo.pWaitSemaphores = &shadowmapSemaphore;
		// Submit work
		submitInfo.pCommandBuffers = &drawCmdBuffers[currentBuffer];
		VK_CHECK_RESULT(vkQueueSubmit(queue, 1, &submitInfo, VK_NULL_HANDLE));

		VulkanExampleBase::submitFrame();

		memcpy(&usedVirtualTileCounter, usedVirualTileCountBuffer.mapped, sizeof(usedVirtualTileCounter));

	}

	virtual void render()
	{
		if (!prepared)
			return;
		updateUniformBufferDeferred();
		updateUniformBufferOffscreen();
		updateUniformBufferCompute0();
		draw();
	}

	virtual void OnUpdateUIOverlay(vks::UIOverlay *overlay)
	{
		if (overlay->header("Settings")) {
			overlay->comboBox("Display", &debugDisplayTarget, { "Final composition", "Shadows", "Position", "Normals", "Albedo", "Specular" });
			bool shadows = (uniformDataComposition.useShadows == 1);
			if (overlay->checkBox("Shadows", &shadows)) {
				uniformDataComposition.useShadows = shadows;
			}

			if (overlay->header("VSM Settings"))
			{
				if (overlay->header("Statistics")) {
					overlay->text("used virtual tiles number: %d", usedVirtualTileCounter.atomicCounter);
					overlay->text("visibleNodesCounter number: %d", visibleNodesCounter.atomicCounter);
					overlay->text("culledNodeIndexes size number: %d", culledNodeIndexes.size());
				}
				
				if (overlay->checkBox("VirtualShadowMap", &enableVirtualShadowMap)) 
				{
					
				}
				if (overlay->checkBox("EnableDynamicLighting", &enableDynamicLighting))
				{

				}
				if (overlay->checkBox("PerTileFrustumCulling", &enablePerTileFrustumCulling))
				{

				}
				if (overlay->checkBox("MultiViewExtension", &enableMultiViewExtension))
				{

				}
				
			}
		}
	}
};

VULKAN_EXAMPLE_MAIN()
