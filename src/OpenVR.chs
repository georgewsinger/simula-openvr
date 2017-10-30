{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CApiFFI #-}
module OpenVR where


import GHC.Generics (Generic)
import System.IO
import Control.Monad
import Data.Proxy
import Data.Word
import System.Posix.DynamicLinker
import Foreign
import Foreign.C
import Linear
import Control.Lens
import Data.Coerce

#include <openvr_capi.h>
#include <util.h>


--  This code is from C2HS, but it has been added
--  here since C2HS is deprecated.
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

peekEnum :: (Enum a, Integral b, Storable b) => Ptr b -> IO a
peekEnum  = liftM (toEnum . fromIntegral) . peek
 
 
{#enum EVREye {}
 with prefix="EVREye" deriving (Show, Eq)#}
{#enum ETextureType {}
 with prefix="ETextureType" deriving (Show, Eq)#}
{#enum EColorSpace {}
  with prefix="EColorSpace" deriving (Show, Eq)#}
{#enum ETrackingResult {}
  with prefix="ETrackingResult" deriving (Show, Eq)#}
{#enum ETrackedDeviceClass                     {underscoreToCase} #}
{#enum ETrackedControllerRole                  {underscoreToCase} #}
{#enum ETrackingUniverseOrigin                 {underscoreToCase} #}
{#enum ETrackedDeviceProperty                  {underscoreToCase} #}
{#enum ETrackedPropertyError                   {underscoreToCase} #}
{#enum EVRSubmitFlags {}
 with prefix="EVRSubmitFlags" deriving (Show, Eq)#}
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
{#enum EVRApplicationType {}
  with prefix="EVRApplicationType" deriving (Show, Eq)#}
{#enum EVRFirmwareError                        {underscoreToCase} #}
{#enum EVRNotificationError                    {underscoreToCase} #}
{#enum EVRInitError {}
  with prefix="EVRInitError" deriving (Show, Eq)#}
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
{#enum EVRCompositorError {}
 with prefix="EVRCompositorError" deriving (Show, Eq)#}
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

type PropertyContainerHandle_t = {#type PropertyContainerHandle_t#}
{#typedef PropertyContainerHandle_t PropertyContainerHandle_t#}

type PropertyTypeTag_t = {#type PropertyTypeTag_t#}
{#typedef PropertyTypeTag_t PropertyTypeTag_t#}

--type VRActionHandle_t = {#type VRActionHandle_t#}
--{#typedef VRActionHandle_t VRActionHandle_t #}

--type VRActionSetHandle_t = {#type VRActionSetHandle_t#}
--{#typedef VRActionSetHandle_t VRActionSetHandle_t#}

--type VRInputOriginHandle_t = {#type VRInputOriginHandle_t#}
--{#typedef VRInputOriginHandle_t VRInputOriginHandle_t#}

{#pointer glSharedTextureHandle_t as GlSharedTextureHandle_t newtype#}
deriving instance Eq GlSharedTextureHandle_t

type GlInt_t = {#type glInt_t#}
{#typedef glInt_t GlInt_t#}

type GlUInt_t = {#type glUInt_t#}
{#typedef glUInt_t GlUInt_t#}

type SharedTextureHandle_t = {#type SharedTextureHandle_t#}
{#typedef SharedTextureHandle_t SharedTextureHandle_t#}

type DriverId_t = {#type DriverId_t#}
{#typedef DriverId_t DriverId_t#}

type TrackedDeviceIndex_t = {#type TrackedDeviceIndex_t#}
{#typedef TrackedDeviceIndex_t TrackedDeviceIndex_t#}

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

-- enum typedefs
type HmdError               = EVRInitError
type HMD_Eye                = EVREye
type ColorSpace             = EColorSpace
type HmdTrackingResult      = ETrackingResult
type TrackedDeviceClass     = ETrackedDeviceClass
type TrackingUniverseOrigin = ETrackingUniverseOrigin
type TrackedDeviceProperty  = ETrackedDeviceProperty
type TrackedPropertyError   = ETrackedPropertyError
type VRSubmitFlags_t        = EVRSubmitFlags
type VRState_t              = EVRState
type CollisionBoundsStyle_t = ECollisionBoundsStyle
type VROverlayError         = EVROverlayError
type VRFirmwareError        = EVRFirmwareError
type VRCompositorError      = EVRCompositorError
type VRScreenshotsError     = EVRScreenshotError

-- typedef structs
{#pointer *HmdMatrix34_t as HmdMatrix34Ptr -> M34 Float #}

{#pointer *HmdMatrix44_t as HmdMatrix44Ptr -> M44 Float #}

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

{#pointer *Texture_t as TexturePtr -> OVRTexture#}

data OVRTexture = OVRTexture {
  textureHandle :: Ptr (),
  textureType :: ETextureType,
  textureColorSpace :: EColorSpace
  } deriving Show

instance Storable OVRTexture where
  sizeOf _ = {#sizeof Texture_t#}
  alignment _ = {#alignof Texture_t#}
  peek ptr = OVRTexture <$> {#get Texture_t->handle#} ptr
                     <*> ((toEnum . fromIntegral) <$> {#get Texture_t->eType#} ptr)
                     <*> ((toEnum . fromIntegral) <$> {#get Texture_t->eColorSpace#} ptr)
  poke ptr (OVRTexture handle ty csp) = do
    {#set Texture_t->handle#} ptr handle
    {#set Texture_t->eType#} ptr (cFromEnum ty)
    {#set Texture_t->eColorSpace#} ptr (cFromEnum csp)


{#pointer *TrackedDevicePose_t as TrackedDevicePosePtr -> TrackedDevicePose#}

data TrackedDevicePose = TrackedDevicePose {
  poseDeviceToAbsoluteTracking :: M34 Float,
  poseVelocity :: V3 Float,
  poseAngularVelocity :: V3 Float,
  poseTrackingResult :: ETrackingResult,
  poseIsValid :: Bool,
  poseDeviceIsConnected :: Bool
  } deriving Show

instance Storable TrackedDevicePose where
  sizeOf _ = {#sizeof TrackedDevicePose_t#}
  alignment _ = {#alignof TrackedDevicePose_t#}
  peek ptr = TrackedDevicePose <$> peekByteOff ptr {#offsetof TrackedDevicePose_t->mDeviceToAbsoluteTracking #}
                               <*> peekByteOff ptr {#offsetof TrackedDevicePose_t->vVelocity #}
                               <*> peekByteOff ptr {#offsetof TrackedDevicePose_t->vAngularVelocity #}
                               <*> ((toEnum . fromIntegral) <$> {#get TrackedDevicePose_t->eTrackingResult #} ptr)
                               <*> {#get TrackedDevicePose_t->bPoseIsValid #} ptr
                               <*> {#get TrackedDevicePose_t->bDeviceIsConnected #} ptr
  poke ptr (TrackedDevicePose tf v av res valid conn) = do
    pokeByteOff ptr {#offsetof TrackedDevicePose_t->mDeviceToAbsoluteTracking #} tf 
    pokeByteOff ptr {#offsetof TrackedDevicePose_t->vVelocity #} v
    pokeByteOff ptr {#offsetof TrackedDevicePose_t->vAngularVelocity #} av
    {#set TrackedDevicePose_t->eTrackingResult #} ptr (cFromEnum res)
    {#set TrackedDevicePose_t->bPoseIsValid #} ptr valid
    {#set TrackedDevicePose_t->bDeviceIsConnected #} ptr conn


{#pointer *VRTextureBounds_t as VRTextureBounds_t newtype#}
deriving instance Eq VRTextureBounds_t
deriving instance Storable VRTextureBounds_t

--{#pointer *VRTextureWithPose_t as VRTextureWithPose_t newtype#}
--deriving instance Eq VRTextureWithPose_t
--deriving instance Storable VRTextureWithPose_t

data VRVulkanTextureData = VRVulkanTextureData Word64 (Ptr ()) (Ptr ()) (Ptr ()) (Ptr ()) Word32 Word32 Word32 Word32 Word32

{#pointer *VRVulkanTextureData_t as VRVulkanTextureData_t -> VRVulkanTextureData#}

instance Storable VRVulkanTextureData where
  sizeOf _ = {#sizeof VRVulkanTextureData_t#}
  alignment _ = {#alignof VRVulkanTextureData_t#}
  peek ptr = VRVulkanTextureData <$> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nImage #} ptr)
                                 <*> {#get VRVulkanTextureData_t->m_pDevice #} ptr
                                 <*> {#get VRVulkanTextureData_t->m_pPhysicalDevice #} ptr
                                 <*> {#get VRVulkanTextureData_t->m_pInstance #} ptr
                                 <*> {#get VRVulkanTextureData_t->m_pQueue #} ptr
                                 <*> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nQueueFamilyIndex #} ptr)
                                 <*> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nWidth #} ptr)
                                 <*> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nHeight #} ptr)
                                 <*> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nFormat #} ptr)
                                 <*> (fromIntegral <$> {#get VRVulkanTextureData_t->m_nSampleCount #} ptr)
  poke ptr (VRVulkanTextureData image dev phys inst queue idx w h fmt smp) = do
    {#set VRVulkanTextureData_t->m_nImage #} ptr (fromIntegral image)
    {#set VRVulkanTextureData_t->m_pDevice #} ptr dev
    {#set VRVulkanTextureData_t->m_pPhysicalDevice #} ptr phys
    {#set VRVulkanTextureData_t->m_pInstance #} ptr inst
    {#set VRVulkanTextureData_t->m_pQueue #} ptr queue
    {#set VRVulkanTextureData_t->m_nQueueFamilyIndex #} ptr (fromIntegral idx)
    {#set VRVulkanTextureData_t->m_nWidth #} ptr (fromIntegral w)
    {#set VRVulkanTextureData_t->m_nHeight #} ptr (fromIntegral h)
    {#set VRVulkanTextureData_t->m_nFormat #} ptr (fromIntegral fmt)
    {#set VRVulkanTextureData_t->m_nSampleCount #} ptr (fromIntegral smp)




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
{#pointer *VREvent_t as VREvent_t newtype#}
deriving instance Eq VREvent_t
deriving instance Storable VREvent_t

{#pointer *VROverlayIntersectionMaskPrimitive_t as VROverlayIntersectionMaskPrimitive_t newtype#}
deriving instance Eq VROverlayIntersectionMaskPrimitive_t
deriving instance Storable VROverlayIntersectionMaskPrimitive_t

--pure struct function tables
{#pointer *VR_IVRSystem_FnTable as VR_IVRSystem_FnTable newtype#}
deriving instance Eq VR_IVRSystem_FnTable
deriving instance Storable VR_IVRSystem_FnTable

{#pointer *VR_IVRExtendedDisplay_FnTable as VR_IVRExtendedDisplay_FnTable newtype#}
deriving instance Eq VR_IVRExtendedDisplay_FnTable
deriving instance Storable VR_IVRExtendedDisplay_FnTable

{#pointer *VR_IVRTrackedCamera_FnTable as VR_IVRTrackedCamera_FnTable newtype#}
deriving instance Eq VR_IVRTrackedCamera_FnTable
deriving instance Storable VR_IVRTrackedCamera_FnTable

{#pointer *VR_IVRApplications_FnTable as VR_IVRApplications_FnTable newtype#}
deriving instance Eq VR_IVRApplications_FnTable
deriving instance Storable VR_IVRApplications_FnTable

{#pointer *VR_IVRChaperone_FnTable as VR_IVRChaperone_FnTable newtype#}
deriving instance Eq VR_IVRChaperone_FnTable
deriving instance Storable VR_IVRChaperone_FnTable

{#pointer *VR_IVRChaperoneSetup_FnTable as VR_IVRChaperoneSetup_FnTable newtype#}
deriving instance Eq VR_IVRChaperoneSetup_FnTable
deriving instance Storable VR_IVRChaperoneSetup_FnTable

{#pointer *VR_IVRCompositor_FnTable as VR_IVRCompositor_FnTable newtype#}
deriving instance Eq VR_IVRCompositor_FnTable
deriving instance Storable VR_IVRCompositor_FnTable

{#pointer *VR_IVROverlay_FnTable as VR_IVROverlay_FnTable newtype#}
deriving instance Eq VR_IVROverlay_FnTable
deriving instance Storable VR_IVROverlay_FnTable

{#pointer *VR_IVRRenderModels_FnTable as VR_IVRRenderModels_FnTable newtype#}
deriving instance Eq VR_IVRRenderModels_FnTable
deriving instance Storable VR_IVRRenderModels_FnTable

{#pointer *VR_IVRNotifications_FnTable as VR_IVRNotifications_FnTable newtype#}
deriving instance Eq VR_IVRNotifications_FnTable
deriving instance Storable VR_IVRNotifications_FnTable

{#pointer *VR_IVRSettings_FnTable as VR_IVRSettings_FnTable newtype#}
deriving instance Eq VR_IVRSettings_FnTable
deriving instance Storable VR_IVRSettings_FnTable

{#pointer *VR_IVRScreenshots_FnTable as VR_IVRScreenshots_FnTable newtype#}
deriving instance Eq VR_IVRScreenshots_FnTable
deriving instance Storable VR_IVRScreenshots_FnTable

{#pointer *VR_IVRResources_FnTable as VR_IVRResources_FnTable newtype#}
deriving instance Eq VR_IVRResources_FnTable
deriving instance Storable VR_IVRResources_FnTable

{#pointer *VR_IVRDriverManager_FnTable as VR_IVRDriverManager_FnTable newtype#}
deriving instance Eq VR_IVRDriverManager_FnTable
deriving instance Storable VR_IVRDriverManager_FnTable

-- global entry point functions
-- but first: two functions below make use of the `intptr_t` C type
type IntPtr_t = {#type intptr_t#} -- defines Haskell type synonym IntPtr_t
{#typedef intptr_t IntPtr_t #}     -- tells c2hs to associate C type intptr_t with HS type IntPtr_t

{#fun VR_Init as vrInit {alloca- `EVRInitError' peekEnum* , `EVRApplicationType', `String'} -> `Ptr ()'#}

-- need to turn it back into the underlying Ptr
{#fun VR_IVRCompositor_Submit as ivrCompositorSubmit
      { `EVREye'
      , `TexturePtr'
      , `VRTextureBounds_t'
      , `EVRSubmitFlags' } -> `EVRCompositorError' #}

{#fun VR_IVRSystem_CaptureInputFocus as ivrSystemCaptureInputFocus {} -> `Bool' #}

{#fun VR_IVRCompositor_WaitGetPoses as ivrCompositorWaitGetPoses {} -> `()' #}

{#fun VR_IVRSystem_GetOutputDevice as ivrSystemGetOutputDevice {alloca- `CULong' peek*, `ETextureType', id `Ptr ()'} -> `()'#}
{#fun VR_IVRCompositor_GetVulkanInstanceExtensionsRequired as ivrCompositorGetVulkanInstanceExtensionsRequired' {id `Ptr CChar', `Int'} -> `Int'#}

ivrCompositorGetVulkanInstanceExtensionsRequired :: IO [String]
ivrCompositorGetVulkanInstanceExtensionsRequired = do
  len <- ivrCompositorGetVulkanInstanceExtensionsRequired' nullPtr 0
  str <- allocaArray len $ \ptr -> ivrCompositorGetVulkanInstanceExtensionsRequired' ptr len >> peekCStringLen (ptr, len)
  return (words str)

{#fun VR_IVRCompositor_GetVulkanDeviceExtensionsRequired as ivrCompositorGetVulkanDeviceExtensionsRequired' {id `Ptr ()', id `Ptr CChar', `Int'} -> `Int'#}

ivrCompositorGetVulkanDeviceExtensionsRequired :: Ptr () -> IO [String]
ivrCompositorGetVulkanDeviceExtensionsRequired dev = do
  len <- ivrCompositorGetVulkanDeviceExtensionsRequired' dev nullPtr 0
  str <- allocaArray len $ \ptr -> ivrCompositorGetVulkanDeviceExtensionsRequired' dev ptr len >> peekCStringLen (ptr, len)
  return (words str)



  
