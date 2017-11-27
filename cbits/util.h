#ifndef SIMULA_OPENVR_UTIL_H
#define SIMULA_OPENVR_UTIL_H

#include <openvr_capi_fixed.h>

//virtual HmdMatrix44_t GetProjectionMatrix( EVREye eEye, float fNearZ, float fFarZ ) = 0;
void VR_IVRSystem_GetProjectionMatrix(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, float fNearZ, float fFarZ, HmdMatrix44_t* out);

//virtual HmdMatrix34_t GetEyeToHeadTransform( EVREye eEye ) = 0;
void VR_IVRSystem_GetEyeToHeadTransform(struct VR_IVRSystem_FnTable* vtbl, void* this, EVREye eye, HmdMatrix34_t* out);

//virtual HmdColor_t GetCurrentFadeColor( bool bBackground = false ) = 0;
void VR_IVRCompositor_GetCurrentFadeColor( struct VR_IVRCompositor_FnTable* vtbl, void* this, bool bBackground , HmdColor_t* out);
#endif
