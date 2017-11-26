#ifndef SIMULA_OPENVR_UTIL_H
#define SIMULA_OPENVR_UTIL_H

#include <openvr_capi_fixed.h>

void VR_IVRSystem_GetProjectionMatrix(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, float fNearZ, float fFarZ, HmdMatrix44_t* out);

void VR_IVRSystem_GetEyeToHeadTransform(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, HmdMatrix34_t* out);

#endif
