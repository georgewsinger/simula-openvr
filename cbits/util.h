#ifndef SIMULA_OPENVR_UTIL_H
#define SIMULA_OPENVR_UTIL_H

#include <openvr_capi.h>

#ifdef __cplusplus
extern "C" {
#endif


void* VR_Init( EVRInitError *peError, EVRApplicationType eApplicationType, const char *pStartupInfo );

EVRCompositorError VR_IVRCompositor_Submit(EVREye eEye, const Texture_t *pTexture, const VRTextureBounds_t* pBounds, EVRSubmitFlags nSubmitFlags);

bool VR_IVRSystem_CaptureInputFocus();

// dummy
void VR_IVRCompositor_WaitGetPoses();

void VR_IVRSystem_GetOutputDevice(uint64_t* pnDevice, ETextureType textureType, struct VkInstance_T* pInstance);
uint32_t VR_IVRCompositor_GetVulkanInstanceExtensionsRequired(char* pchValue, uint32_t bufferSize);
uint32_t VR_IVRCompositor_GetVulkanDeviceExtensionsRequired(struct VkPhysicalDevice_T* pPhysicalDevice, char* pchValue, uint32_t bufferSize);


#ifdef __cplusplus
}
#endif

#endif
