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


