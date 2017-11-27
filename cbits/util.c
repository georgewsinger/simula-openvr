#include <stdlib.h>
#include <openvr_capi_fixed.h>
#include "util.h"

void VR_IVRSystem_GetProjectionMatrix(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, float fNearZ, float fFarZ, HmdMatrix44_t* out) {
  *out = vtbl->GetProjectionMatrix(this, eye, fNearZ, fFarZ);
}

void VR_IVRSystem_GetEyeToHeadTransform(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, HmdMatrix34_t* out) {
  *out = vtbl->GetEyeToHeadTransform(this, eye);
}

//virtual HmdColor_t GetCurrentFadeColor( bool bBackground = false ) = 0;
void VR_IVRCompositor_GetCurrentFadeColor( struct VR_IVRCompositor_FnTable* vtbl, void* this, bool bBackground , HmdColor_t* out) {
  *out = vtbl->GetCurrentFadeColor(this, bBackground);
}
