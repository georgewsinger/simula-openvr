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
