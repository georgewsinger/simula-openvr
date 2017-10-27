#ifndef SIMULA_OPENVR_UTIL_H
#define SIMULA_OPENVR_UTIL_H

#include <openvr_capi.h>


S_API intptr_t VR_InitInternal( EVRInitError *peError, EVRApplicationType eType );
S_API void VR_ShutdownInternal();
S_API bool VR_IsHmdPresent();
S_API intptr_t VR_GetGenericInterface( const char *pchInterfaceVersion, EVRInitError *peError );
S_API bool VR_IsRuntimeInstalled();
S_API const char * VR_GetVRInitErrorAsSymbol( EVRInitError error );
S_API const char * VR_GetVRInitErrorAsEnglishDescription( EVRInitError error );


struct VR_IVRSystem_FnTable* VR_GetIVRSystem();

struct VR_IVRCompositor_FnTable* VR_GetIVRCompositor();

HmdMatrix34_t* VR_GetEyeToHeadTransform(struct VR_IVRSystem_FnTable* ivrSystem, EVREye eye);
HmdMatrix44_t* VR_GetProjectionMatrix(struct VR_IVRSystem_FnTable* ivrSystem, EVREye eye, float near, float far);

#endif
