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


#ifdef __cplusplus
}
#endif

#endif
