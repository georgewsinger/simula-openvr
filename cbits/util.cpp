#include <cstdlib>
#include <openvr.h>
#include "util.h"


extern "C" {

void* VR_Init( EVRInitError *peError, EVRApplicationType eApplicationType, const char *pStartupInfo ) {
  return (void*) vr::VR_Init(reinterpret_cast<vr::EVRInitError*>(peError), static_cast<vr::EVRApplicationType>(eApplicationType), pStartupInfo);
}

EVRCompositorError VR_IVRCompositor_Submit(EVREye eEye, const Texture_t *pTexture, const VRTextureBounds_t* pBounds, EVRSubmitFlags nSubmitFlags) {
  return static_cast<EVRCompositorError>(vr::VRCompositor()->Submit(static_cast<vr::EVREye>(eEye), reinterpret_cast<const vr::Texture_t*>(pTexture), reinterpret_cast<const vr::VRTextureBounds_t*>(pBounds), static_cast<vr::EVRSubmitFlags>(nSubmitFlags)));
}


bool VR_IVRSystem_CaptureInputFocus() {
  return vr::VRSystem()->CaptureInputFocus();
}

void VR_IVRCompositor_WaitGetPoses() {
  vr::VRCompositor()->WaitGetPoses(nullptr, 0, nullptr, 0);
}
}
