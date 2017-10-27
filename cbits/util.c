#include <stdlib.h>
#include <openvr_capi.h>
#include "util.h"

struct VR_IVRSystem_FnTable* VR_GetIVRSystem() {
  EVRInitError eError;
  return (struct VR_IVRSystem_FnTable*) VR_GetGenericInterface( IVRSystem_Version, &eError );
}

struct VR_IVRCompositor_FnTable* VR_GetIVRCompositor() {
  EVRInitError eError;
  return (struct VR_IVRCompositor_FnTable*) VR_GetGenericInterface( IVRCompositor_Version, &eError );
}


HmdMatrix34_t* VR_GetEyeToHeadTransform(struct VR_IVRSystem_FnTable* ivrSystem, EVREye eye) {
  HmdMatrix34_t* pMat = malloc(sizeof(*pMat));
  *pMat = ivrSystem->GetEyeToHeadTransform(eye);
  return pMat;
}

HmdMatrix44_t* VR_GetProjectionMatrix(struct VR_IVRSystem_FnTable* ivrSystem, EVREye eye, float near, float far) {
  HmdMatrix44_t* pMat = malloc(sizeof(*pMat));
  *pMat = ivrSystem->GetProjectionMatrix(eye, near, far);
  return pMat;
}
