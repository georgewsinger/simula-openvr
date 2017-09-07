{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CApiFFI #-}
module Simula.OpenVR where

import GHC.Generics (Generic)
import System.IO
import Control.Monad
import Data.Proxy
import Data.Word
import System.Posix.DynamicLinker
import Foreign
import Foreign.C
import Linear

#include <openvr/headers/openvr_capi.h>

{#enum EVREye                                  {underscoreToCase} #}
{#enum ETextureType                            {underscoreToCase} #}
{#enum EColorSpace                             {underscoreToCase} #}
{#enum ETrackingResult                         {underscoreToCase} #}
{#enum ETrackedDeviceClass                     {underscoreToCase} #}
{#enum ETrackedControllerRole                  {underscoreToCase} #}
{#enum ETrackingUniverseOrigin                 {underscoreToCase} #}
{#enum ETrackedDeviceProperty                  {underscoreToCase} #}
{#enum ETrackedPropertyError                   {underscoreToCase} #}
{#enum EVRSubmitFlags                          {underscoreToCase} #}
{#enum EVRState                                {underscoreToCase} #}
{#enum EVREventType                            {underscoreToCase} #}
{#enum EDeviceActivityLevel                    {underscoreToCase} #}
{#enum EVRButtonId                             {underscoreToCase} #}
{#enum EVRMouseButton                          {underscoreToCase} #}
{#enum EHiddenAreaMeshType                     {underscoreToCase} #}
{#enum EVRControllerAxisType                   {underscoreToCase} #}
{#enum EVRControllerEventOutputType            {underscoreToCase} #}
{#enum ECollisionBoundsStyle                   {underscoreToCase} #}
{#enum EVROverlayError                         {underscoreToCase} #}
{#enum EVRApplicationType                      {underscoreToCase} #}
{#enum EVRFirmwareError                        {underscoreToCase} #}
{#enum EVRNotificationError                    {underscoreToCase} #}
{#enum EVRInitError                            {underscoreToCase} #}
{#enum EVRScreenshotType                       {underscoreToCase} #}
{#enum EVRScreenshotPropertyFilenames          {underscoreToCase} #}
{#enum EVRTrackedCameraError                   {underscoreToCase} #}
{#enum EVRTrackedCameraFrameType               {underscoreToCase} #}
{#enum EVRApplicationError                     {underscoreToCase} #}
{#enum EVRApplicationProperty                  {underscoreToCase} #}
{#enum EVRApplicationTransitionState           {underscoreToCase} #}
{#enum ChaperoneCalibrationState               {underscoreToCase} #}
{#enum EChaperoneConfigFile                    {underscoreToCase} #}
{#enum EChaperoneImportFlags                   {underscoreToCase} #}
{#enum EVRCompositorError                      {underscoreToCase} #}
{#enum VROverlayInputMethod                    {underscoreToCase} #}
{#enum VROverlayTransformType                  {underscoreToCase} #}
{#enum VROverlayFlags                          {underscoreToCase} #}
{#enum VRMessageOverlayResponse                {underscoreToCase} #}
{#enum EGamepadTextInputMode                   {underscoreToCase} #}
{#enum EGamepadTextInputLineMode               {underscoreToCase} #}
{#enum EOverlayDirection                       {underscoreToCase} #}
{#enum EVROverlayIntersectionMaskPrimitiveType {underscoreToCase} #}
{#enum EVRRenderModelError                     {underscoreToCase} #}
{#enum EVRComponentProperty                    {underscoreToCase} #}
{#enum EVRNotificationType                     {underscoreToCase} #}
{#enum EVRNotificationStyle                    {underscoreToCase} #}
{#enum EVRSettingsError                        {underscoreToCase} #}
{#enum EVRScreenshotError                      {underscoreToCase} #}

type OSVR_ChannelCount = {#type OSVR_ChannelCount#}
{#typedef OSVR_ChannelCount OSVR_ChannelCount#}

type PropertyContainerHandle_t = {#type PropertyContainerHandle_t#}
{#typedef PropertyContainerHandle_t PropertyContainerHandle_t#}

type PropertyTypeTag_t = {#type PropertyTypeTag_t#}
{#typedef PropertyTypeTag_t PropertyTypeTag_t#}

type VRActionHandle_t = {#type VRActionHandle_t#}
{#typedef VRActionHandle_t VRActionHandle_t#}

type VRActionSetHandle_t = {#type VRActionSetHandle_t#}
{#typedef VRActionSetHandle_t VRActionSetHandle_t#}

type VRInputOriginHandle_t = {#type VRInputOriginHandle_t#}
{#typedef VRInputOriginHandle_t VRInputOriginHandle_t#}

{#pointer glSharedTextureHandle_t as GlSharedTextureHandle_t newtype#}
deriving instance Eq GlSharedTextureHandle_t

type GlInt_t = {#type GlInt_t#}
{#typedef GlInt_t GlInt_t#}

type GlUInt_t = {#type GlUInt_t#}
{#typedef GlUInt_t GlUInt_t#}

type SharedTextureHandle_t = {#type SharedTextureHandle_t#}
{#typedef SharedTextureHandle_t SharedTextureHandle_t#}

type DriverId_t = {#type DriverId_t#}
{#typedef DriverId_t DriverId_t#}

type TrackedDeviceIndex_t = {#type TrackedDeviceIndex_t#}
{#typedef TrackedDeviceIndex_t TrackedDeviceIndex_t#}

type PropertyContainerHandle_t = {#type PropertyContainerHandle_t#}
{#typedef PropertyContainerHandle_t PropertyContainerHandle_t#}

type PropertyTypeTag_t = {#type PropertyTypeTag_t#}
{#typedef PropertyTypeTag_t PropertyTypeTag_t#}

type VROverlayHandle_t = {#type VROverlayHandle_t#}
{#typedef VROverlayHandle_t VROverlayHandle_t#}

type TrackedCameraHandle_t = {#type TrackedCameraHandle_t#}
{#typedef TrackedCameraHandle_t TrackedCameraHandle_t#}

type ScreenshotHandle_t = {#type ScreenshotHandle_t#}
{#typedef ScreenshotHandle_t ScreenshotHandle_t#}

type VRComponentProperties = {#type VRComponentProperties#}
{#typedef VRComponentProperties VRComponentProperties#}

type TextureID_t = {#type TextureID_t#}
{#typedef TextureID_t TextureID_t#}

type VRNotificationId = {#type VRNotificationId#}
{#typedef VRNotificationId VRNotificationId#}

{- UNCLEAR HOW TO HANDLE THESE AT THIS POINT:
   typedef EVRInitError HmdError;
   typedef EVREye Hmd_Eye;
   typedef EColorSpace ColorSpace;
   typedef ETrackingResult HmdTrackingResult;
   typedef ETrackedDeviceClass TrackedDeviceClass;
   typedef ETrackingUniverseOrigin TrackingUniverseOrigin;
   typedef ETrackedDeviceProperty TrackedDeviceProperty;
   typedef ETrackedPropertyError TrackedPropertyError;
   typedef EVRSubmitFlags VRSubmitFlags_t;
   typedef EVRState VRState_t;
   typedef ECollisionBoundsStyle CollisionBoundsStyle_t;
   typedef EVROverlayError VROverlayError;
   typedef EVRFirmwareError VRFirmwareError;
   typedef EVRCompositorError VRCompositorError;
   typedef EVRScreenshotError VRScreenshotsError; -}


-- typedef structs
{#pointer *HmdMatrix34_t as HmdMatrix34_t newtype#}
deriving instance Eq HmdMatrix34_t
deriving instance Storable HmdMatrix34_t

{#pointer *HmdMatrix44_t as HmdMatrix44_t newtype#}
deriving instance Eq HmdMatrix44_t
deriving instance Storable HmdMatrix44_t

{#pointer *HmdVector3_t as HmdVector3_t newtype#}
deriving instance Eq HmdVector3_t
deriving instance Storable HmdVector3_t

{#pointer *HmdVector4_t as HmdVector4_t newtype#}
deriving instance Eq HmdVector4_t
deriving instance Storable HmdVector4_t

{#pointer *HmdVector3d_t as HmdVector3d_t newtype#}
deriving instance Eq HmdVector3d_t
deriving instance Storable HmdVector3d_t

{#pointer *HmdVector2_t as HmdVector2_t newtype#}
deriving instance Eq HmdVector2_t
deriving instance Storable HmdVector2_t

{#pointer *HmdQuaternion_t as HmdQuaternion_t newtype#}
deriving instance Eq HmdQuaternion_t
deriving instance Storable HmdQuaternion_t

{#pointer *HmdColor_t as HmdColor_t newtype#}
deriving instance Eq HmdColor_t
deriving instance Storable HmdColor_t

{#pointer *HmdQuad_t as HmdQuad_t newtype#}
deriving instance Eq HmdQuad_t
deriving instance Storable HmdQuad_t

{#pointer *HmdRect2_t as HmdRect2_t newtype#}
deriving instance Eq HmdRect2_t
deriving instance Storable HmdRect2_t

{#pointer *DistortionCoordinates_t as DistortionCoordinates_t newtype#}
deriving instance Eq DistortionCoordinates_t
deriving instance Storable DistortionCoordinates_t

{#pointer *Texture_t as Texture_t newtype#}
deriving instance Eq Texture_t
deriving instance Storable Texture_t

{#pointer *TrackedDevicePose_t as TrackedDevicePose_t newtype#}
deriving instance Eq TrackedDevicePose_t
deriving instance Storable TrackedDevicePose_t

{#pointer *VRTextureBounds_t as VRTextureBounds_t newtype#}
deriving instance Eq VRTextureBounds_t
deriving instance Storable VRTextureBounds_t

{#pointer *VRTextureWithPose_t as VRTextureWithPose_t newtype#}
deriving instance Eq VRTextureWithPose_t
deriving instance Storable VRTextureWithPose_t

{#pointer *VRVulkanTextureData_t as VRVulkanTextureData_t newtype#}
deriving instance Eq VRVulkanTextureData_t
deriving instance Storable VRVulkanTextureData_t

{#pointer *D3D12TextureData_t as D3D12TextureData_t newtype#}
deriving instance Eq D3D12TextureData_t
deriving instance Storable D3D12TextureData_t

{#pointer *VREvent_Controller_t as VREvent_Controller_t newtype#}
deriving instance Eq VREvent_Controller_t
deriving instance Storable VREvent_Controller_t

{#pointer *VREvent_Mouse_t as VREvent_Mouse_t newtype#}
deriving instance Eq VREvent_Mouse_t
deriving instance Storable VREvent_Mouse_t

{#pointer *VREvent_Scroll_t as VREvent_Scroll_t newtype#}
deriving instance Eq VREvent_Scroll_t
deriving instance Storable VREvent_Scroll_t

{#pointer *VREvent_TouchPadMove_t as VREvent_TouchPadMove_t newtype#}
deriving instance Eq VREvent_TouchPadMove_t
deriving instance Storable VREvent_TouchPadMove_t

{#pointer *VREvent_Notification_t as VREvent_Notification_t newtype#}
deriving instance Eq VREvent_Notification_t
deriving instance Storable VREvent_Notification_t

{#pointer *VREvent_Process_t as VREvent_Process_t newtype#}
deriving instance Eq VREvent_Process_t
deriving instance Storable VREvent_Process_t

{#pointer *VREvent_Overlay_t as VREvent_Overlay_t newtype#}
deriving instance Eq VREvent_Overlay_t
deriving instance Storable VREvent_Overlay_t

{#pointer *VREvent_Status_t as VREvent_Status_t newtype#}
deriving instance Eq VREvent_Status_t
deriving instance Storable VREvent_Status_t

{#pointer *VREvent_Keyboard_t as VREvent_Keyboard_t newtype#}
deriving instance Eq VREvent_Keyboard_t
deriving instance Storable VREvent_Keyboard_t

{#pointer *VREvent_Ipd_t as VREvent_Ipd_t newtype#}
deriving instance Eq VREvent_Ipd_t
deriving instance Storable VREvent_Ipd_t

{#pointer *VREvent_Chaperone_t as VREvent_Chaperone_t newtype#}
deriving instance Eq VREvent_Chaperone_t
deriving instance Storable VREvent_Chaperone_t

{#pointer *VREvent_Reserved_t as VREvent_Reserved_t newtype#}
deriving instance Eq VREvent_Reserved_t
deriving instance Storable VREvent_Reserved_t

{#pointer *VREvent_PerformanceTest_t as VREvent_PerformanceTest_t newtype#}
deriving instance Eq VREvent_PerformanceTest_t
deriving instance Storable VREvent_PerformanceTest_t

{#pointer *VREvent_SeatedZeroPoseReset_t as VREvent_SeatedZeroPoseReset_t newtype#}
deriving instance Eq VREvent_SeatedZeroPoseReset_t
deriving instance Storable VREvent_SeatedZeroPoseReset_t

{#pointer *VREvent_Screenshot_t as VREvent_Screenshot_t newtype#}
deriving instance Eq VREvent_Screenshot_t
deriving instance Storable VREvent_Screenshot_t

{#pointer *VREvent_ScreenshotProgress_t as VREvent_ScreenshotProgress_t newtype#}
deriving instance Eq VREvent_ScreenshotProgress_t
deriving instance Storable VREvent_ScreenshotProgress_t

{#pointer *VREvent_ApplicationLaunch_t as VREvent_ApplicationLaunch_t newtype#}
deriving instance Eq VREvent_ApplicationLaunch_t
deriving instance Storable VREvent_ApplicationLaunch_t

{#pointer *VREvent_EditingCameraSurface_t as VREvent_EditingCameraSurface_t newtype#}
deriving instance Eq VREvent_EditingCameraSurface_t
deriving instance Storable VREvent_EditingCameraSurface_t

{#pointer *VREvent_MessageOverlay_t as VREvent_MessageOverlay_t newtype#}
deriving instance Eq VREvent_MessageOverlay_t
deriving instance Storable VREvent_MessageOverlay_t

{#pointer *VREvent_Property_t as VREvent_Property_t newtype#}
deriving instance Eq VREvent_Property_t
deriving instance Storable VREvent_Property_t

{#pointer *HiddenAreaMesh_t as HiddenAreaMesh_t newtype#}
deriving instance Eq HiddenAreaMesh_t
deriving instance Storable HiddenAreaMesh_t

{#pointer *VRControllerAxis_t as VRControllerAxis_t newtype#}
deriving instance Eq VRControllerAxis_t
deriving instance Storable VRControllerAxis_t

{#pointer *VRControllerState_t as VRControllerState_t newtype#}
deriving instance Eq VRControllerState_t
deriving instance Storable VRControllerState_t

{#pointer *Compositor_OverlaySettings as Compositor_OverlaySettings newtype#}
deriving instance Eq Compositor_OverlaySettings
deriving instance Storable Compositor_OverlaySettings

{#pointer *CameraVideoStreamFrameHeader_t as CameraVideoStreamFrameHeader_t newtype#}
deriving instance Eq CameraVideoStreamFrameHeader_t
deriving instance Storable CameraVideoStreamFrameHeader_t

{#pointer *AppOverrideKeys_t as AppOverrideKeys_t newtype#}
deriving instance Eq AppOverrideKeys_t
deriving instance Storable AppOverrideKeys_t

{#pointer *Compositor_FrameTiming as Compositor_FrameTiming newtype#}
deriving instance Eq Compositor_FrameTiming
deriving instance Storable Compositor_FrameTiming

{#pointer *Compositor_CumulativeStats as Compositor_CumulativeStats newtype#}
deriving instance Eq Compositor_CumulativeStats
deriving instance Storable Compositor_CumulativeStats

{#pointer *VROverlayIntersectionParams_t as VROverlayIntersectionParams_t newtype#}
deriving instance Eq VROverlayIntersectionParams_t
deriving instance Storable VROverlayIntersectionParams_t

{#pointer *VROverlayIntersectionResults_t as VROverlayIntersectionResults_t newtype#}
deriving instance Eq VROverlayIntersectionResults_t
deriving instance Storable VROverlayIntersectionResults_t

{#pointer *IntersectionMaskRectangle_t as IntersectionMaskRectangle_t newtype#}
deriving instance Eq IntersectionMaskRectangle_t
deriving instance Storable IntersectionMaskRectangle_t

{#pointer *IntersectionMaskCircle_t as IntersectionMaskCircle_t newtype#}
deriving instance Eq IntersectionMaskCircle_t
deriving instance Storable IntersectionMaskCircle_t

{#pointer *RenderModel_ComponentState_t as RenderModel_ComponentState_t newtype#}
deriving instance Eq RenderModel_ComponentState_t
deriving instance Storable RenderModel_ComponentState_t

{#pointer *RenderModel_Vertex_t as RenderModel_Vertex_t newtype#}
deriving instance Eq RenderModel_Vertex_t
deriving instance Storable RenderModel_Vertex_t

{#pointer *RenderModel_TextureMap_t as RenderModel_TextureMap_t newtype#}
deriving instance Eq RenderModel_TextureMap_t
deriving instance Storable RenderModel_TextureMap_t

{#pointer *RenderModel_t as RenderModel_t newtype#}
deriving instance Eq RenderModel_t
deriving instance Storable RenderModel_t

{#pointer *RenderModel_ControllerMode_State_t as RenderModel_ControllerMode_State_t newtype#}
deriving instance Eq RenderModel_ControllerMode_State_t
deriving instance Storable RenderModel_ControllerMode_State_t

{#pointer *NotificationBitmap_t as NotificationBitmap_t newtype#}
deriving instance Eq NotificationBitmap_t
deriving instance Storable NotificationBitmap_t

{#pointer *COpenVRContext as COpenVRContext newtype#}
deriving instance Eq COpenVRContext
deriving instance Storable COpenVRContext

-- typedef unions
{#pointer *VREvent_Data_t as VREvent_Data_t newtype#}
deriving instance Eq VREvent_Data_t
deriving instance Storable VREvent_Data_t                            -- needed for unions?

{#pointer *VROverlayIntersectionMaskPrimitive_Data_t as VROverlayIntersectionMaskPrimitive_Data_t newtype#}
deriving instance Eq VROverlayIntersectionMaskPrimitive_Data_t
deriving instance Storable VROverlayIntersectionMaskPrimitive_Data_t -- needed for unions?

-- pure structs
{-
struct VREvent_t
{
	uint32_t eventType; // EVREventType enum
	TrackedDeviceIndex_t trackedDeviceIndex;
	float eventAgeSeconds;
	// event data must be the end of the struct as its size is variable
	VREvent_Data_t data;
};

struct VROverlayIntersectionMaskPrimitive_t
{
	EVROverlayIntersectionMaskPrimitiveType m_nPrimitiveType;
	VROverlayIntersectionMaskPrimitive_Data_t m_Primitive;
};

--pure struct function tables
struct VR_IVRSystem_FnTable
{
	void (OPENVR_FNTABLE_CALLTYPE *GetRecommendedRenderTargetSize)(uint32_t * pnWidth, uint32_t * pnHeight);
	struct HmdMatrix44_t (OPENVR_FNTABLE_CALLTYPE *GetProjectionMatrix)(EVREye eEye, float fNearZ, float fFarZ);
	void (OPENVR_FNTABLE_CALLTYPE *GetProjectionRaw)(EVREye eEye, float * pfLeft, float * pfRight, float * pfTop, float * pfBottom);
	bool (OPENVR_FNTABLE_CALLTYPE *ComputeDistortion)(EVREye eEye, float fU, float fV, struct DistortionCoordinates_t * pDistortionCoordinates);
	struct HmdMatrix34_t (OPENVR_FNTABLE_CALLTYPE *GetEyeToHeadTransform)(EVREye eEye);
	bool (OPENVR_FNTABLE_CALLTYPE *GetTimeSinceLastVsync)(float * pfSecondsSinceLastVsync, uint64_t * pulFrameCounter);
	int32_t (OPENVR_FNTABLE_CALLTYPE *GetD3D9AdapterIndex)();
	void (OPENVR_FNTABLE_CALLTYPE *GetDXGIOutputInfo)(int32_t * pnAdapterIndex);
	void (OPENVR_FNTABLE_CALLTYPE *GetOutputDevice)(uint64_t * pnDevice, ETextureType textureType, struct VkInstance_T * pInstance);
	bool (OPENVR_FNTABLE_CALLTYPE *IsDisplayOnDesktop)();
	bool (OPENVR_FNTABLE_CALLTYPE *SetDisplayVisibility)(bool bIsVisibleOnDesktop);
	void (OPENVR_FNTABLE_CALLTYPE *GetDeviceToAbsoluteTrackingPose)(ETrackingUniverseOrigin eOrigin, float fPredictedSecondsToPhotonsFromNow, struct TrackedDevicePose_t * pTrackedDevicePoseArray, uint32_t unTrackedDevicePoseArrayCount);
	void (OPENVR_FNTABLE_CALLTYPE *ResetSeatedZeroPose)();
	struct HmdMatrix34_t (OPENVR_FNTABLE_CALLTYPE *GetSeatedZeroPoseToStandingAbsoluteTrackingPose)();
	struct HmdMatrix34_t (OPENVR_FNTABLE_CALLTYPE *GetRawZeroPoseToStandingAbsoluteTrackingPose)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetSortedTrackedDeviceIndicesOfClass)(ETrackedDeviceClass eTrackedDeviceClass, TrackedDeviceIndex_t * punTrackedDeviceIndexArray, uint32_t unTrackedDeviceIndexArrayCount, TrackedDeviceIndex_t unRelativeToTrackedDeviceIndex);
	EDeviceActivityLevel (OPENVR_FNTABLE_CALLTYPE *GetTrackedDeviceActivityLevel)(TrackedDeviceIndex_t unDeviceId);
	void (OPENVR_FNTABLE_CALLTYPE *ApplyTransform)(struct TrackedDevicePose_t * pOutputPose, struct TrackedDevicePose_t * pTrackedDevicePose, struct HmdMatrix34_t * pTransform);
	TrackedDeviceIndex_t (OPENVR_FNTABLE_CALLTYPE *GetTrackedDeviceIndexForControllerRole)(ETrackedControllerRole unDeviceType);
	ETrackedControllerRole (OPENVR_FNTABLE_CALLTYPE *GetControllerRoleForTrackedDeviceIndex)(TrackedDeviceIndex_t unDeviceIndex);
	ETrackedDeviceClass (OPENVR_FNTABLE_CALLTYPE *GetTrackedDeviceClass)(TrackedDeviceIndex_t unDeviceIndex);
	bool (OPENVR_FNTABLE_CALLTYPE *IsTrackedDeviceConnected)(TrackedDeviceIndex_t unDeviceIndex);
	bool (OPENVR_FNTABLE_CALLTYPE *GetBoolTrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, ETrackedPropertyError * pError);
	float (OPENVR_FNTABLE_CALLTYPE *GetFloatTrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, ETrackedPropertyError * pError);
	int32_t (OPENVR_FNTABLE_CALLTYPE *GetInt32TrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, ETrackedPropertyError * pError);
	uint64_t (OPENVR_FNTABLE_CALLTYPE *GetUint64TrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, ETrackedPropertyError * pError);
	struct HmdMatrix34_t (OPENVR_FNTABLE_CALLTYPE *GetMatrix34TrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, ETrackedPropertyError * pError);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetStringTrackedDeviceProperty)(TrackedDeviceIndex_t unDeviceIndex, ETrackedDeviceProperty prop, char * pchValue, uint32_t unBufferSize, ETrackedPropertyError * pError);
	char * (OPENVR_FNTABLE_CALLTYPE *GetPropErrorNameFromEnum)(ETrackedPropertyError error);
	bool (OPENVR_FNTABLE_CALLTYPE *PollNextEvent)(struct VREvent_t * pEvent, uint32_t uncbVREvent);
	bool (OPENVR_FNTABLE_CALLTYPE *PollNextEventWithPose)(ETrackingUniverseOrigin eOrigin, struct VREvent_t * pEvent, uint32_t uncbVREvent, TrackedDevicePose_t * pTrackedDevicePose);
	char * (OPENVR_FNTABLE_CALLTYPE *GetEventTypeNameFromEnum)(EVREventType eType);
	struct HiddenAreaMesh_t (OPENVR_FNTABLE_CALLTYPE *GetHiddenAreaMesh)(EVREye eEye, EHiddenAreaMeshType type);
	bool (OPENVR_FNTABLE_CALLTYPE *GetControllerState)(TrackedDeviceIndex_t unControllerDeviceIndex, VRControllerState_t * pControllerState, uint32_t unControllerStateSize);
	bool (OPENVR_FNTABLE_CALLTYPE *GetControllerStateWithPose)(ETrackingUniverseOrigin eOrigin, TrackedDeviceIndex_t unControllerDeviceIndex, VRControllerState_t * pControllerState, uint32_t unControllerStateSize, struct TrackedDevicePose_t * pTrackedDevicePose);
	void (OPENVR_FNTABLE_CALLTYPE *TriggerHapticPulse)(TrackedDeviceIndex_t unControllerDeviceIndex, uint32_t unAxisId, unsigned short usDurationMicroSec);
	char * (OPENVR_FNTABLE_CALLTYPE *GetButtonIdNameFromEnum)(EVRButtonId eButtonId);
	char * (OPENVR_FNTABLE_CALLTYPE *GetControllerAxisTypeNameFromEnum)(EVRControllerAxisType eAxisType);
	bool (OPENVR_FNTABLE_CALLTYPE *CaptureInputFocus)();
	void (OPENVR_FNTABLE_CALLTYPE *ReleaseInputFocus)();
	bool (OPENVR_FNTABLE_CALLTYPE *IsInputFocusCapturedByAnotherProcess)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *DriverDebugRequest)(TrackedDeviceIndex_t unDeviceIndex, char * pchRequest, char * pchResponseBuffer, uint32_t unResponseBufferSize);
	EVRFirmwareError (OPENVR_FNTABLE_CALLTYPE *PerformFirmwareUpdate)(TrackedDeviceIndex_t unDeviceIndex);
	void (OPENVR_FNTABLE_CALLTYPE *AcknowledgeQuit_Exiting)();
	void (OPENVR_FNTABLE_CALLTYPE *AcknowledgeQuit_UserPrompt)();
};

struct VR_IVRExtendedDisplay_FnTable
{
	void (OPENVR_FNTABLE_CALLTYPE *GetWindowBounds)(int32_t * pnX, int32_t * pnY, uint32_t * pnWidth, uint32_t * pnHeight);
	void (OPENVR_FNTABLE_CALLTYPE *GetEyeOutputViewport)(EVREye eEye, uint32_t * pnX, uint32_t * pnY, uint32_t * pnWidth, uint32_t * pnHeight);
	void (OPENVR_FNTABLE_CALLTYPE *GetDXGIOutputInfo)(int32_t * pnAdapterIndex, int32_t * pnAdapterOutputIndex);
};

struct VR_IVRTrackedCamera_FnTable
{
	char * (OPENVR_FNTABLE_CALLTYPE *GetCameraErrorNameFromEnum)(EVRTrackedCameraError eCameraError);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *HasCamera)(TrackedDeviceIndex_t nDeviceIndex, bool * pHasCamera);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetCameraFrameSize)(TrackedDeviceIndex_t nDeviceIndex, EVRTrackedCameraFrameType eFrameType, uint32_t * pnWidth, uint32_t * pnHeight, uint32_t * pnFrameBufferSize);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetCameraIntrinsics)(TrackedDeviceIndex_t nDeviceIndex, EVRTrackedCameraFrameType eFrameType, HmdVector2_t * pFocalLength, HmdVector2_t * pCenter);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetCameraProjection)(TrackedDeviceIndex_t nDeviceIndex, EVRTrackedCameraFrameType eFrameType, float flZNear, float flZFar, HmdMatrix44_t * pProjection);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *AcquireVideoStreamingService)(TrackedDeviceIndex_t nDeviceIndex, TrackedCameraHandle_t * pHandle);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *ReleaseVideoStreamingService)(TrackedCameraHandle_t hTrackedCamera);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetVideoStreamFrameBuffer)(TrackedCameraHandle_t hTrackedCamera, EVRTrackedCameraFrameType eFrameType, void * pFrameBuffer, uint32_t nFrameBufferSize, CameraVideoStreamFrameHeader_t * pFrameHeader, uint32_t nFrameHeaderSize);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetVideoStreamTextureSize)(TrackedDeviceIndex_t nDeviceIndex, EVRTrackedCameraFrameType eFrameType, VRTextureBounds_t * pTextureBounds, uint32_t * pnWidth, uint32_t * pnHeight);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetVideoStreamTextureD3D11)(TrackedCameraHandle_t hTrackedCamera, EVRTrackedCameraFrameType eFrameType, void * pD3D11DeviceOrResource, void ** ppD3D11ShaderResourceView, CameraVideoStreamFrameHeader_t * pFrameHeader, uint32_t nFrameHeaderSize);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *GetVideoStreamTextureGL)(TrackedCameraHandle_t hTrackedCamera, EVRTrackedCameraFrameType eFrameType, glUInt_t * pglTextureId, CameraVideoStreamFrameHeader_t * pFrameHeader, uint32_t nFrameHeaderSize);
	EVRTrackedCameraError (OPENVR_FNTABLE_CALLTYPE *ReleaseVideoStreamTextureGL)(TrackedCameraHandle_t hTrackedCamera, glUInt_t glTextureId);
};

struct VR_IVRApplications_FnTable
{
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *AddApplicationManifest)(char * pchApplicationManifestFullPath, bool bTemporary);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *RemoveApplicationManifest)(char * pchApplicationManifestFullPath);
	bool (OPENVR_FNTABLE_CALLTYPE *IsApplicationInstalled)(char * pchAppKey);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationCount)();
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *GetApplicationKeyByIndex)(uint32_t unApplicationIndex, char * pchAppKeyBuffer, uint32_t unAppKeyBufferLen);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *GetApplicationKeyByProcessId)(uint32_t unProcessId, char * pchAppKeyBuffer, uint32_t unAppKeyBufferLen);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *LaunchApplication)(char * pchAppKey);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *LaunchTemplateApplication)(char * pchTemplateAppKey, char * pchNewAppKey, struct AppOverrideKeys_t * pKeys, uint32_t unKeys);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *LaunchApplicationFromMimeType)(char * pchMimeType, char * pchArgs);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *LaunchDashboardOverlay)(char * pchAppKey);
	bool (OPENVR_FNTABLE_CALLTYPE *CancelApplicationLaunch)(char * pchAppKey);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *IdentifyApplication)(uint32_t unProcessId, char * pchAppKey);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationProcessId)(char * pchAppKey);
	char * (OPENVR_FNTABLE_CALLTYPE *GetApplicationsErrorNameFromEnum)(EVRApplicationError error);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationPropertyString)(char * pchAppKey, EVRApplicationProperty eProperty, char * pchPropertyValueBuffer, uint32_t unPropertyValueBufferLen, EVRApplicationError * peError);
	bool (OPENVR_FNTABLE_CALLTYPE *GetApplicationPropertyBool)(char * pchAppKey, EVRApplicationProperty eProperty, EVRApplicationError * peError);
	uint64_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationPropertyUint64)(char * pchAppKey, EVRApplicationProperty eProperty, EVRApplicationError * peError);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *SetApplicationAutoLaunch)(char * pchAppKey, bool bAutoLaunch);
	bool (OPENVR_FNTABLE_CALLTYPE *GetApplicationAutoLaunch)(char * pchAppKey);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *SetDefaultApplicationForMimeType)(char * pchAppKey, char * pchMimeType);
	bool (OPENVR_FNTABLE_CALLTYPE *GetDefaultApplicationForMimeType)(char * pchMimeType, char * pchAppKeyBuffer, uint32_t unAppKeyBufferLen);
	bool (OPENVR_FNTABLE_CALLTYPE *GetApplicationSupportedMimeTypes)(char * pchAppKey, char * pchMimeTypesBuffer, uint32_t unMimeTypesBuffer);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationsThatSupportMimeType)(char * pchMimeType, char * pchAppKeysThatSupportBuffer, uint32_t unAppKeysThatSupportBuffer);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetApplicationLaunchArguments)(uint32_t unHandle, char * pchArgs, uint32_t unArgs);
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *GetStartingApplication)(char * pchAppKeyBuffer, uint32_t unAppKeyBufferLen);
	EVRApplicationTransitionState (OPENVR_FNTABLE_CALLTYPE *GetTransitionState)();
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *PerformApplicationPrelaunchCheck)(char * pchAppKey);
	char * (OPENVR_FNTABLE_CALLTYPE *GetApplicationsTransitionStateNameFromEnum)(EVRApplicationTransitionState state);
	bool (OPENVR_FNTABLE_CALLTYPE *IsQuitUserPromptRequested)();
	EVRApplicationError (OPENVR_FNTABLE_CALLTYPE *LaunchInternalProcess)(char * pchBinaryPath, char * pchArguments, char * pchWorkingDirectory);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetCurrentSceneProcessId)();
};

struct VR_IVRChaperone_FnTable
{
	ChaperoneCalibrationState (OPENVR_FNTABLE_CALLTYPE *GetCalibrationState)();
	bool (OPENVR_FNTABLE_CALLTYPE *GetPlayAreaSize)(float * pSizeX, float * pSizeZ);
	bool (OPENVR_FNTABLE_CALLTYPE *GetPlayAreaRect)(struct HmdQuad_t * rect);
	void (OPENVR_FNTABLE_CALLTYPE *ReloadInfo)();
	void (OPENVR_FNTABLE_CALLTYPE *SetSceneColor)(struct HmdColor_t color);
	void (OPENVR_FNTABLE_CALLTYPE *GetBoundsColor)(struct HmdColor_t * pOutputColorArray, int nNumOutputColors, float flCollisionBoundsFadeDistance, struct HmdColor_t * pOutputCameraColor);
	bool (OPENVR_FNTABLE_CALLTYPE *AreBoundsVisible)();
	void (OPENVR_FNTABLE_CALLTYPE *ForceBoundsVisible)(bool bForce);
};

struct VR_IVRChaperoneSetup_FnTable
{
	bool (OPENVR_FNTABLE_CALLTYPE *CommitWorkingCopy)(EChaperoneConfigFile configFile);
	void (OPENVR_FNTABLE_CALLTYPE *RevertWorkingCopy)();
	bool (OPENVR_FNTABLE_CALLTYPE *GetWorkingPlayAreaSize)(float * pSizeX, float * pSizeZ);
	bool (OPENVR_FNTABLE_CALLTYPE *GetWorkingPlayAreaRect)(struct HmdQuad_t * rect);
	bool (OPENVR_FNTABLE_CALLTYPE *GetWorkingCollisionBoundsInfo)(struct HmdQuad_t * pQuadsBuffer, uint32_t * punQuadsCount);
	bool (OPENVR_FNTABLE_CALLTYPE *GetLiveCollisionBoundsInfo)(struct HmdQuad_t * pQuadsBuffer, uint32_t * punQuadsCount);
	bool (OPENVR_FNTABLE_CALLTYPE *GetWorkingSeatedZeroPoseToRawTrackingPose)(struct HmdMatrix34_t * pmatSeatedZeroPoseToRawTrackingPose);
	bool (OPENVR_FNTABLE_CALLTYPE *GetWorkingStandingZeroPoseToRawTrackingPose)(struct HmdMatrix34_t * pmatStandingZeroPoseToRawTrackingPose);
	void (OPENVR_FNTABLE_CALLTYPE *SetWorkingPlayAreaSize)(float sizeX, float sizeZ);
	void (OPENVR_FNTABLE_CALLTYPE *SetWorkingCollisionBoundsInfo)(struct HmdQuad_t * pQuadsBuffer, uint32_t unQuadsCount);
	void (OPENVR_FNTABLE_CALLTYPE *SetWorkingSeatedZeroPoseToRawTrackingPose)(struct HmdMatrix34_t * pMatSeatedZeroPoseToRawTrackingPose);
	void (OPENVR_FNTABLE_CALLTYPE *SetWorkingStandingZeroPoseToRawTrackingPose)(struct HmdMatrix34_t * pMatStandingZeroPoseToRawTrackingPose);
	void (OPENVR_FNTABLE_CALLTYPE *ReloadFromDisk)(EChaperoneConfigFile configFile);
	bool (OPENVR_FNTABLE_CALLTYPE *GetLiveSeatedZeroPoseToRawTrackingPose)(struct HmdMatrix34_t * pmatSeatedZeroPoseToRawTrackingPose);
	void (OPENVR_FNTABLE_CALLTYPE *SetWorkingCollisionBoundsTagsInfo)(uint8_t * pTagsBuffer, uint32_t unTagCount);
	bool (OPENVR_FNTABLE_CALLTYPE *GetLiveCollisionBoundsTagsInfo)(uint8_t * pTagsBuffer, uint32_t * punTagCount);
	bool (OPENVR_FNTABLE_CALLTYPE *SetWorkingPhysicalBoundsInfo)(struct HmdQuad_t * pQuadsBuffer, uint32_t unQuadsCount);
	bool (OPENVR_FNTABLE_CALLTYPE *GetLivePhysicalBoundsInfo)(struct HmdQuad_t * pQuadsBuffer, uint32_t * punQuadsCount);
	bool (OPENVR_FNTABLE_CALLTYPE *ExportLiveToBuffer)(char * pBuffer, uint32_t * pnBufferLength);
	bool (OPENVR_FNTABLE_CALLTYPE *ImportFromBufferToWorking)(char * pBuffer, uint32_t nImportFlags);
};

struct VR_IVRCompositor_FnTable
{
	void (OPENVR_FNTABLE_CALLTYPE *SetTrackingSpace)(ETrackingUniverseOrigin eOrigin);
	ETrackingUniverseOrigin (OPENVR_FNTABLE_CALLTYPE *GetTrackingSpace)();
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *WaitGetPoses)(struct TrackedDevicePose_t * pRenderPoseArray, uint32_t unRenderPoseArrayCount, struct TrackedDevicePose_t * pGamePoseArray, uint32_t unGamePoseArrayCount);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *GetLastPoses)(struct TrackedDevicePose_t * pRenderPoseArray, uint32_t unRenderPoseArrayCount, struct TrackedDevicePose_t * pGamePoseArray, uint32_t unGamePoseArrayCount);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *GetLastPoseForTrackedDeviceIndex)(TrackedDeviceIndex_t unDeviceIndex, struct TrackedDevicePose_t * pOutputPose, struct TrackedDevicePose_t * pOutputGamePose);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *Submit)(EVREye eEye, struct Texture_t * pTexture, struct VRTextureBounds_t * pBounds, EVRSubmitFlags nSubmitFlags);
	void (OPENVR_FNTABLE_CALLTYPE *ClearLastSubmittedFrame)();
	void (OPENVR_FNTABLE_CALLTYPE *PostPresentHandoff)();
	bool (OPENVR_FNTABLE_CALLTYPE *GetFrameTiming)(struct Compositor_FrameTiming * pTiming, uint32_t unFramesAgo);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetFrameTimings)(struct Compositor_FrameTiming * pTiming, uint32_t nFrames);
	float (OPENVR_FNTABLE_CALLTYPE *GetFrameTimeRemaining)();
	void (OPENVR_FNTABLE_CALLTYPE *GetCumulativeStats)(struct Compositor_CumulativeStats * pStats, uint32_t nStatsSizeInBytes);
	void (OPENVR_FNTABLE_CALLTYPE *FadeToColor)(float fSeconds, float fRed, float fGreen, float fBlue, float fAlpha, bool bBackground);
	struct HmdColor_t (OPENVR_FNTABLE_CALLTYPE *GetCurrentFadeColor)(bool bBackground);
	void (OPENVR_FNTABLE_CALLTYPE *FadeGrid)(float fSeconds, bool bFadeIn);
	float (OPENVR_FNTABLE_CALLTYPE *GetCurrentGridAlpha)();
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *SetSkyboxOverride)(struct Texture_t * pTextures, uint32_t unTextureCount);
	void (OPENVR_FNTABLE_CALLTYPE *ClearSkyboxOverride)();
	void (OPENVR_FNTABLE_CALLTYPE *CompositorBringToFront)();
	void (OPENVR_FNTABLE_CALLTYPE *CompositorGoToBack)();
	void (OPENVR_FNTABLE_CALLTYPE *CompositorQuit)();
	bool (OPENVR_FNTABLE_CALLTYPE *IsFullscreen)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetCurrentSceneFocusProcess)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetLastFrameRenderer)();
	bool (OPENVR_FNTABLE_CALLTYPE *CanRenderScene)();
	void (OPENVR_FNTABLE_CALLTYPE *ShowMirrorWindow)();
	void (OPENVR_FNTABLE_CALLTYPE *HideMirrorWindow)();
	bool (OPENVR_FNTABLE_CALLTYPE *IsMirrorWindowVisible)();
	void (OPENVR_FNTABLE_CALLTYPE *CompositorDumpImages)();
	bool (OPENVR_FNTABLE_CALLTYPE *ShouldAppRenderWithLowResources)();
	void (OPENVR_FNTABLE_CALLTYPE *ForceInterleavedReprojectionOn)(bool bOverride);
	void (OPENVR_FNTABLE_CALLTYPE *ForceReconnectProcess)();
	void (OPENVR_FNTABLE_CALLTYPE *SuspendRendering)(bool bSuspend);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *GetMirrorTextureD3D11)(EVREye eEye, void * pD3D11DeviceOrResource, void ** ppD3D11ShaderResourceView);
	void (OPENVR_FNTABLE_CALLTYPE *ReleaseMirrorTextureD3D11)(void * pD3D11ShaderResourceView);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *GetMirrorTextureGL)(EVREye eEye, glUInt_t * pglTextureId, glSharedTextureHandle_t * pglSharedTextureHandle);
	bool (OPENVR_FNTABLE_CALLTYPE *ReleaseSharedGLTexture)(glUInt_t glTextureId, glSharedTextureHandle_t glSharedTextureHandle);
	void (OPENVR_FNTABLE_CALLTYPE *LockGLSharedTextureForAccess)(glSharedTextureHandle_t glSharedTextureHandle);
	void (OPENVR_FNTABLE_CALLTYPE *UnlockGLSharedTextureForAccess)(glSharedTextureHandle_t glSharedTextureHandle);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetVulkanInstanceExtensionsRequired)(char * pchValue, uint32_t unBufferSize);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetVulkanDeviceExtensionsRequired)(struct VkPhysicalDevice_T * pPhysicalDevice, char * pchValue, uint32_t unBufferSize);
	void (OPENVR_FNTABLE_CALLTYPE *SetExplicitTimingMode)(bool bExplicitTimingMode);
	EVRCompositorError (OPENVR_FNTABLE_CALLTYPE *SubmitExplicitTimingData)();
};

struct VR_IVROverlay_FnTable
{
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *FindOverlay)(char * pchOverlayKey, VROverlayHandle_t * pOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *CreateOverlay)(char * pchOverlayKey, char * pchOverlayName, VROverlayHandle_t * pOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *DestroyOverlay)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetHighQualityOverlay)(VROverlayHandle_t ulOverlayHandle);
	VROverlayHandle_t (OPENVR_FNTABLE_CALLTYPE *GetHighQualityOverlay)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetOverlayKey)(VROverlayHandle_t ulOverlayHandle, char * pchValue, uint32_t unBufferSize, EVROverlayError * pError);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetOverlayName)(VROverlayHandle_t ulOverlayHandle, char * pchValue, uint32_t unBufferSize, EVROverlayError * pError);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayName)(VROverlayHandle_t ulOverlayHandle, char * pchName);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayImageData)(VROverlayHandle_t ulOverlayHandle, void * pvBuffer, uint32_t unBufferSize, uint32_t * punWidth, uint32_t * punHeight);
	char * (OPENVR_FNTABLE_CALLTYPE *GetOverlayErrorNameFromEnum)(EVROverlayError error);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayRenderingPid)(VROverlayHandle_t ulOverlayHandle, uint32_t unPID);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetOverlayRenderingPid)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayFlag)(VROverlayHandle_t ulOverlayHandle, VROverlayFlags eOverlayFlag, bool bEnabled);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayFlag)(VROverlayHandle_t ulOverlayHandle, VROverlayFlags eOverlayFlag, bool * pbEnabled);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayColor)(VROverlayHandle_t ulOverlayHandle, float fRed, float fGreen, float fBlue);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayColor)(VROverlayHandle_t ulOverlayHandle, float * pfRed, float * pfGreen, float * pfBlue);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayAlpha)(VROverlayHandle_t ulOverlayHandle, float fAlpha);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayAlpha)(VROverlayHandle_t ulOverlayHandle, float * pfAlpha);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTexelAspect)(VROverlayHandle_t ulOverlayHandle, float fTexelAspect);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTexelAspect)(VROverlayHandle_t ulOverlayHandle, float * pfTexelAspect);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlaySortOrder)(VROverlayHandle_t ulOverlayHandle, uint32_t unSortOrder);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlaySortOrder)(VROverlayHandle_t ulOverlayHandle, uint32_t * punSortOrder);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayWidthInMeters)(VROverlayHandle_t ulOverlayHandle, float fWidthInMeters);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayWidthInMeters)(VROverlayHandle_t ulOverlayHandle, float * pfWidthInMeters);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayAutoCurveDistanceRangeInMeters)(VROverlayHandle_t ulOverlayHandle, float fMinDistanceInMeters, float fMaxDistanceInMeters);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayAutoCurveDistanceRangeInMeters)(VROverlayHandle_t ulOverlayHandle, float * pfMinDistanceInMeters, float * pfMaxDistanceInMeters);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTextureColorSpace)(VROverlayHandle_t ulOverlayHandle, EColorSpace eTextureColorSpace);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTextureColorSpace)(VROverlayHandle_t ulOverlayHandle, EColorSpace * peTextureColorSpace);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTextureBounds)(VROverlayHandle_t ulOverlayHandle, struct VRTextureBounds_t * pOverlayTextureBounds);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTextureBounds)(VROverlayHandle_t ulOverlayHandle, struct VRTextureBounds_t * pOverlayTextureBounds);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetOverlayRenderModel)(VROverlayHandle_t ulOverlayHandle, char * pchValue, uint32_t unBufferSize, struct HmdColor_t * pColor, EVROverlayError * pError);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayRenderModel)(VROverlayHandle_t ulOverlayHandle, char * pchRenderModel, struct HmdColor_t * pColor);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTransformType)(VROverlayHandle_t ulOverlayHandle, VROverlayTransformType * peTransformType);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTransformAbsolute)(VROverlayHandle_t ulOverlayHandle, ETrackingUniverseOrigin eTrackingOrigin, struct HmdMatrix34_t * pmatTrackingOriginToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTransformAbsolute)(VROverlayHandle_t ulOverlayHandle, ETrackingUniverseOrigin * peTrackingOrigin, struct HmdMatrix34_t * pmatTrackingOriginToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTransformTrackedDeviceRelative)(VROverlayHandle_t ulOverlayHandle, TrackedDeviceIndex_t unTrackedDevice, struct HmdMatrix34_t * pmatTrackedDeviceToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTransformTrackedDeviceRelative)(VROverlayHandle_t ulOverlayHandle, TrackedDeviceIndex_t * punTrackedDevice, struct HmdMatrix34_t * pmatTrackedDeviceToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTransformTrackedDeviceComponent)(VROverlayHandle_t ulOverlayHandle, TrackedDeviceIndex_t unDeviceIndex, char * pchComponentName);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTransformTrackedDeviceComponent)(VROverlayHandle_t ulOverlayHandle, TrackedDeviceIndex_t * punDeviceIndex, char * pchComponentName, uint32_t unComponentNameSize);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTransformOverlayRelative)(VROverlayHandle_t ulOverlayHandle, VROverlayHandle_t * ulOverlayHandleParent, struct HmdMatrix34_t * pmatParentOverlayToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTransformOverlayRelative)(VROverlayHandle_t ulOverlayHandle, VROverlayHandle_t ulOverlayHandleParent, struct HmdMatrix34_t * pmatParentOverlayToOverlayTransform);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *ShowOverlay)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *HideOverlay)(VROverlayHandle_t ulOverlayHandle);
	bool (OPENVR_FNTABLE_CALLTYPE *IsOverlayVisible)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetTransformForOverlayCoordinates)(VROverlayHandle_t ulOverlayHandle, ETrackingUniverseOrigin eTrackingOrigin, struct HmdVector2_t coordinatesInOverlay, struct HmdMatrix34_t * pmatTransform);
	bool (OPENVR_FNTABLE_CALLTYPE *PollNextOverlayEvent)(VROverlayHandle_t ulOverlayHandle, struct VREvent_t * pEvent, uint32_t uncbVREvent);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayInputMethod)(VROverlayHandle_t ulOverlayHandle, VROverlayInputMethod * peInputMethod);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayInputMethod)(VROverlayHandle_t ulOverlayHandle, VROverlayInputMethod eInputMethod);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayMouseScale)(VROverlayHandle_t ulOverlayHandle, struct HmdVector2_t * pvecMouseScale);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayMouseScale)(VROverlayHandle_t ulOverlayHandle, struct HmdVector2_t * pvecMouseScale);
	bool (OPENVR_FNTABLE_CALLTYPE *ComputeOverlayIntersection)(VROverlayHandle_t ulOverlayHandle, struct VROverlayIntersectionParams_t * pParams, struct VROverlayIntersectionResults_t * pResults);
	bool (OPENVR_FNTABLE_CALLTYPE *HandleControllerOverlayInteractionAsMouse)(VROverlayHandle_t ulOverlayHandle, TrackedDeviceIndex_t unControllerDeviceIndex);
	bool (OPENVR_FNTABLE_CALLTYPE *IsHoverTargetOverlay)(VROverlayHandle_t ulOverlayHandle);
	VROverlayHandle_t (OPENVR_FNTABLE_CALLTYPE *GetGamepadFocusOverlay)();
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetGamepadFocusOverlay)(VROverlayHandle_t ulNewFocusOverlay);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayNeighbor)(EOverlayDirection eDirection, VROverlayHandle_t ulFrom, VROverlayHandle_t ulTo);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *MoveGamepadFocusToNeighbor)(EOverlayDirection eDirection, VROverlayHandle_t ulFrom);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayTexture)(VROverlayHandle_t ulOverlayHandle, struct Texture_t * pTexture);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *ClearOverlayTexture)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayRaw)(VROverlayHandle_t ulOverlayHandle, void * pvBuffer, uint32_t unWidth, uint32_t unHeight, uint32_t unDepth);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayFromFile)(VROverlayHandle_t ulOverlayHandle, char * pchFilePath);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTexture)(VROverlayHandle_t ulOverlayHandle, void ** pNativeTextureHandle, void * pNativeTextureRef, uint32_t * pWidth, uint32_t * pHeight, uint32_t * pNativeFormat, ETextureType * pAPIType, EColorSpace * pColorSpace, struct VRTextureBounds_t * pTextureBounds);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *ReleaseNativeOverlayHandle)(VROverlayHandle_t ulOverlayHandle, void * pNativeTextureHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayTextureSize)(VROverlayHandle_t ulOverlayHandle, uint32_t * pWidth, uint32_t * pHeight);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *CreateDashboardOverlay)(char * pchOverlayKey, char * pchOverlayFriendlyName, VROverlayHandle_t * pMainHandle, VROverlayHandle_t * pThumbnailHandle);
	bool (OPENVR_FNTABLE_CALLTYPE *IsDashboardVisible)();
	bool (OPENVR_FNTABLE_CALLTYPE *IsActiveDashboardOverlay)(VROverlayHandle_t ulOverlayHandle);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetDashboardOverlaySceneProcess)(VROverlayHandle_t ulOverlayHandle, uint32_t unProcessId);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetDashboardOverlaySceneProcess)(VROverlayHandle_t ulOverlayHandle, uint32_t * punProcessId);
	void (OPENVR_FNTABLE_CALLTYPE *ShowDashboard)(char * pchOverlayToShow);
	TrackedDeviceIndex_t (OPENVR_FNTABLE_CALLTYPE *GetPrimaryDashboardDevice)();
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *ShowKeyboard)(EGamepadTextInputMode eInputMode, EGamepadTextInputLineMode eLineInputMode, char * pchDescription, uint32_t unCharMax, char * pchExistingText, bool bUseMinimalMode, uint64_t uUserValue);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *ShowKeyboardForOverlay)(VROverlayHandle_t ulOverlayHandle, EGamepadTextInputMode eInputMode, EGamepadTextInputLineMode eLineInputMode, char * pchDescription, uint32_t unCharMax, char * pchExistingText, bool bUseMinimalMode, uint64_t uUserValue);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetKeyboardText)(char * pchText, uint32_t cchText);
	void (OPENVR_FNTABLE_CALLTYPE *HideKeyboard)();
	void (OPENVR_FNTABLE_CALLTYPE *SetKeyboardTransformAbsolute)(ETrackingUniverseOrigin eTrackingOrigin, struct HmdMatrix34_t * pmatTrackingOriginToKeyboardTransform);
	void (OPENVR_FNTABLE_CALLTYPE *SetKeyboardPositionForOverlay)(VROverlayHandle_t ulOverlayHandle, struct HmdRect2_t avoidRect);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *SetOverlayIntersectionMask)(VROverlayHandle_t ulOverlayHandle, struct VROverlayIntersectionMaskPrimitive_t * pMaskPrimitives, uint32_t unNumMaskPrimitives, uint32_t unPrimitiveSize);
	EVROverlayError (OPENVR_FNTABLE_CALLTYPE *GetOverlayFlags)(VROverlayHandle_t ulOverlayHandle, uint32_t * pFlags);
	VRMessageOverlayResponse (OPENVR_FNTABLE_CALLTYPE *ShowMessageOverlay)(char * pchText, char * pchCaption, char * pchButton0Text, char * pchButton1Text, char * pchButton2Text, char * pchButton3Text);
	void (OPENVR_FNTABLE_CALLTYPE *CloseMessageOverlay)();
};

struct VR_IVRRenderModels_FnTable
{
	EVRRenderModelError (OPENVR_FNTABLE_CALLTYPE *LoadRenderModel_Async)(char * pchRenderModelName, struct RenderModel_t ** ppRenderModel);
	void (OPENVR_FNTABLE_CALLTYPE *FreeRenderModel)(struct RenderModel_t * pRenderModel);
	EVRRenderModelError (OPENVR_FNTABLE_CALLTYPE *LoadTexture_Async)(TextureID_t textureId, struct RenderModel_TextureMap_t ** ppTexture);
	void (OPENVR_FNTABLE_CALLTYPE *FreeTexture)(struct RenderModel_TextureMap_t * pTexture);
	EVRRenderModelError (OPENVR_FNTABLE_CALLTYPE *LoadTextureD3D11_Async)(TextureID_t textureId, void * pD3D11Device, void ** ppD3D11Texture2D);
	EVRRenderModelError (OPENVR_FNTABLE_CALLTYPE *LoadIntoTextureD3D11_Async)(TextureID_t textureId, void * pDstTexture);
	void (OPENVR_FNTABLE_CALLTYPE *FreeTextureD3D11)(void * pD3D11Texture2D);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetRenderModelName)(uint32_t unRenderModelIndex, char * pchRenderModelName, uint32_t unRenderModelNameLen);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetRenderModelCount)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetComponentCount)(char * pchRenderModelName);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetComponentName)(char * pchRenderModelName, uint32_t unComponentIndex, char * pchComponentName, uint32_t unComponentNameLen);
	uint64_t (OPENVR_FNTABLE_CALLTYPE *GetComponentButtonMask)(char * pchRenderModelName, char * pchComponentName);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetComponentRenderModelName)(char * pchRenderModelName, char * pchComponentName, char * pchComponentRenderModelName, uint32_t unComponentRenderModelNameLen);
	bool (OPENVR_FNTABLE_CALLTYPE *GetComponentState)(char * pchRenderModelName, char * pchComponentName, VRControllerState_t * pControllerState, struct RenderModel_ControllerMode_State_t * pState, struct RenderModel_ComponentState_t * pComponentState);
	bool (OPENVR_FNTABLE_CALLTYPE *RenderModelHasComponent)(char * pchRenderModelName, char * pchComponentName);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetRenderModelThumbnailURL)(char * pchRenderModelName, char * pchThumbnailURL, uint32_t unThumbnailURLLen, EVRRenderModelError * peError);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetRenderModelOriginalPath)(char * pchRenderModelName, char * pchOriginalPath, uint32_t unOriginalPathLen, EVRRenderModelError * peError);
	char * (OPENVR_FNTABLE_CALLTYPE *GetRenderModelErrorNameFromEnum)(EVRRenderModelError error);
};

struct VR_IVRNotifications_FnTable
{
	EVRNotificationError (OPENVR_FNTABLE_CALLTYPE *CreateNotification)(VROverlayHandle_t ulOverlayHandle, uint64_t ulUserValue, EVRNotificationType type, char * pchText, EVRNotificationStyle style, struct NotificationBitmap_t * pImage, VRNotificationId * pNotificationId);
	EVRNotificationError (OPENVR_FNTABLE_CALLTYPE *RemoveNotification)(VRNotificationId notificationId);
};

struct VR_IVRSettings_FnTable
{
	char * (OPENVR_FNTABLE_CALLTYPE *GetSettingsErrorNameFromEnum)(EVRSettingsError eError);
	bool (OPENVR_FNTABLE_CALLTYPE *Sync)(bool bForce, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *SetBool)(char * pchSection, char * pchSettingsKey, bool bValue, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *SetInt32)(char * pchSection, char * pchSettingsKey, int32_t nValue, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *SetFloat)(char * pchSection, char * pchSettingsKey, float flValue, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *SetString)(char * pchSection, char * pchSettingsKey, char * pchValue, EVRSettingsError * peError);
	bool (OPENVR_FNTABLE_CALLTYPE *GetBool)(char * pchSection, char * pchSettingsKey, EVRSettingsError * peError);
	int32_t (OPENVR_FNTABLE_CALLTYPE *GetInt32)(char * pchSection, char * pchSettingsKey, EVRSettingsError * peError);
	float (OPENVR_FNTABLE_CALLTYPE *GetFloat)(char * pchSection, char * pchSettingsKey, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *GetString)(char * pchSection, char * pchSettingsKey, char * pchValue, uint32_t unValueLen, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *RemoveSection)(char * pchSection, EVRSettingsError * peError);
	void (OPENVR_FNTABLE_CALLTYPE *RemoveKeyInSection)(char * pchSection, char * pchSettingsKey, EVRSettingsError * peError);
};

struct VR_IVRScreenshots_FnTable
{
	EVRScreenshotError (OPENVR_FNTABLE_CALLTYPE *RequestScreenshot)(ScreenshotHandle_t * pOutScreenshotHandle, EVRScreenshotType type, char * pchPreviewFilename, char * pchVRFilename);
	EVRScreenshotError (OPENVR_FNTABLE_CALLTYPE *HookScreenshot)(EVRScreenshotType * pSupportedTypes, int numTypes);
	EVRScreenshotType (OPENVR_FNTABLE_CALLTYPE *GetScreenshotPropertyType)(ScreenshotHandle_t screenshotHandle, EVRScreenshotError * pError);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetScreenshotPropertyFilename)(ScreenshotHandle_t screenshotHandle, EVRScreenshotPropertyFilenames filenameType, char * pchFilename, uint32_t cchFilename, EVRScreenshotError * pError);
	EVRScreenshotError (OPENVR_FNTABLE_CALLTYPE *UpdateScreenshotProgress)(ScreenshotHandle_t screenshotHandle, float flProgress);
	EVRScreenshotError (OPENVR_FNTABLE_CALLTYPE *TakeStereoScreenshot)(ScreenshotHandle_t * pOutScreenshotHandle, char * pchPreviewFilename, char * pchVRFilename);
	EVRScreenshotError (OPENVR_FNTABLE_CALLTYPE *SubmitScreenshot)(ScreenshotHandle_t screenshotHandle, EVRScreenshotType type, char * pchSourcePreviewFilename, char * pchSourceVRFilename);
};

struct VR_IVRResources_FnTable
{
	uint32_t (OPENVR_FNTABLE_CALLTYPE *LoadSharedResource)(char * pchResourceName, char * pchBuffer, uint32_t unBufferLen);
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetResourceFullPath)(char * pchResourceName, char * pchResourceTypeDirectory, char * pchPathBuffer, uint32_t unBufferLen);
};

struct VR_IVRDriverManager_FnTable
{
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetDriverCount)();
	uint32_t (OPENVR_FNTABLE_CALLTYPE *GetDriverName)(DriverId_t nDriver, char * pchValue, uint32_t unBufferSize);
};
-}

{-global entry point functions; unclear how to handle at this point
#if 0
// Global entry points
S_API intptr_t VR_InitInternal( EVRInitError *peError, EVRApplicationType eType );
S_API void VR_ShutdownInternal();
S_API bool VR_IsHmdPresent();
S_API intptr_t VR_GetGenericInterface( const char *pchInterfaceVersion, EVRInitError *peError );
S_API bool VR_IsRuntimeInstalled();
S_API const char * VR_GetVRInitErrorAsSymbol( EVRInitError error );
S_API const char * VR_GetVRInitErrorAsEnglishDescription( EVRInitError error );
#endif
-}
