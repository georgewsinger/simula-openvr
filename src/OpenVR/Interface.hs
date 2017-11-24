module OpenVR.Interface where

import Control.Concurrent.MVar
import Control.Lens
import Data.Proxy
import Foreign
import Foreign.C

import System.IO.Unsafe

import OpenVR.Internal
import OpenVR.TH

data OpenVRContext = OpenVRContext
  { _vrSystem :: Ptr VR_IVRSystem_FnTable
  , _vrChaperone :: Ptr VR_IVRChaperone_FnTable
  , _vrChaperoneSetup :: Ptr VR_IVRChaperoneSetup_FnTable
  , _vrCompositor :: Ptr VR_IVRCompositor_FnTable
  , _vrOverlay :: Ptr VR_IVROverlay_FnTable
  , _vrResources :: Ptr VR_IVRResources_FnTable
  , _vrRenderModels :: Ptr VR_IVRRenderModels_FnTable
  , _vrExtendedDisplay :: Ptr VR_IVRExtendedDisplay_FnTable
  , _vrSettings :: Ptr VR_IVRSettings_FnTable
  , _vrApplications :: Ptr VR_IVRApplications_FnTable
  , _vrTrackedCamera :: Ptr VR_IVRTrackedCamera_FnTable
  , _vrScreenshots :: Ptr VR_IVRScreenshots_FnTable
  , _vrDriverManager :: Ptr VR_IVRDriverManager_FnTable
  , _vrNotifications :: Ptr VR_IVRNotifications_FnTable }

makeLenses ''OpenVRContext

class Storable a => VRInterface a where
  interfaceVersion :: Proxy a -> String
  interface :: Lens' OpenVRContext (Ptr a)

parseOpenVRJSON "openvr/headers/openvr_api.json"

vrGetInterface :: forall a. VRInterface a => IO (Ptr a)
vrGetInterface =  do
      (iptr, VRInitError_None) <- vrGetGenericInterface $ interfaceVersion ifProxy
      return $ intPtrToPtr iptr
  where
    ifProxy = Proxy :: Proxy a

newOpenVRContext :: IO OpenVRContext
newOpenVRContext =
  OpenVRContext
  <$> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface
  <*> vrGetInterface

openVRContext :: MVar OpenVRContext
openVRContext = unsafePerformIO $ newOpenVRContext >>= newMVar
{-# NOINLINE openVRContext #-}

getOpenVRContext :: IO OpenVRContext
getOpenVRContext = readMVar openVRContext

makeVrCall 'ivrCompositorSubmit_ "ivrCompositorSubmit"
makeVrCall 'ivrCompositorWaitGetPoses_ "ivrCompositorWaitGetPoses'"
makeVrCall 'ivrCompositorGetVulkanInstanceExtensionsRequired_ "ivrCompositorGetVulkanInstanceExtensionsRequired'"
makeVrCall 'ivrCompositorGetVulkanDeviceExtensionsRequired_ "ivrCompositorGetVulkanDeviceExtensionsRequired'"

makeVrCall 'ivrSystemGetOutputDevice_ "ivrSystemGetOutputDevice"
makeVrCall 'ivrSystemGetRecommendedRenderTargetSize_ "ivrSystemGetRecommendedRenderTargetSize"
makeVrCall 'ivrSystemGetProjectionMatrix_ "ivrSystemGetProjectionMatrix"
makeVrCall 'ivrSystemGetEyeToHeadTransform_ "ivrSystemGetEyeToHeadTransform"
makeVrCall 'ivrSystemPollNextEvent_ "ivrSystemPollNextEvent'"
makeVrCall 'ivrSystemGetStringTrackedDeviceProperty_ "ivrSystemGetStringTrackedDeviceProperty'"

makeVrCall 'ivrRenderModelsLoadRenderModel_Async_ "ivrRenderModelsLoadRenderModel_Async"
makeVrCall 'ivrRenderModelsLoadTexture_Async_ "ivrRenderModelsLoadTexture_Async"
makeVrCall 'ivrRenderModelsFreeRenderModel_ "ivrRenderModelsFreeRenderModel"
makeVrCall 'ivrRenderModelsFreeTexture_ "ivrRenderModelsFreeTexture"

-- modified lengthArray0 that respects maximum size
lengthArray :: (Storable a, Eq a) => Int ->  a -> Ptr a -> IO Int
lengthArray maxSize marker ptr  = loop 0
  where
    loop i | i >= maxSize = return maxSize
           | otherwise = do
               val <- peekElemOff ptr i
               if val == marker then return i else loop (i+1)

-- |returns render pose, predicted pose
ivrCompositorWaitGetPoses :: IO (EVRCompositorError, [TrackedDevicePose], [TrackedDevicePose])
ivrCompositorWaitGetPoses =
  allocaTracked $ \renderPtr ->
  allocaTracked $ \gamePtr -> do
    err <- ivrCompositorWaitGetPoses' renderPtr k_unMaxTrackedDeviceCount gamePtr k_unMaxTrackedDeviceCount
    render <- peekTracked renderPtr
    game <- peekTracked gamePtr
    return (err, render, game)

  where
    allocaTracked = allocaArray k_unMaxTrackedDeviceCount
    peekTracked ptr = getRealLength ptr >>= \n -> peekArray n ptr
    getRealLength ptr = lengthArray k_unMaxTrackedDeviceCount nullPtr (castPtr ptr)


ivrCompositorGetVulkanInstanceExtensionsRequired :: IO [String]
ivrCompositorGetVulkanInstanceExtensionsRequired  = do
  len <- ivrCompositorGetVulkanInstanceExtensionsRequired' nullPtr 0
  str <- allocaArray len $ \ptr -> ivrCompositorGetVulkanInstanceExtensionsRequired' ptr len >> peekCString ptr
  return (words str)


ivrCompositorGetVulkanDeviceExtensionsRequired ::  Ptr () -> IO [String]
ivrCompositorGetVulkanDeviceExtensionsRequired dev = do
  len <- ivrCompositorGetVulkanDeviceExtensionsRequired' dev nullPtr 0
  str <- allocaArray len $ \ptr -> ivrCompositorGetVulkanDeviceExtensionsRequired' dev ptr len >> peekCString ptr
  return (words str)

ivrSystemGetStringTrackedDeviceProperty :: TrackedDeviceIndex -> ETrackedDeviceProperty
                                        -> IO (ETrackedPropertyError, String)

ivrSystemGetStringTrackedDeviceProperty idx prop = allocaArray k_unMaxPropertyStringSize $ \strPtr ->
  ivrSystemGetStringTrackedDeviceProperty' idx prop strPtr k_unMaxPropertyStringSize >>= \(_, err) ->
  (err,) <$> peekCString strPtr


ivrSystemPollNextEvent :: IO (Maybe VREvent)
ivrSystemPollNextEvent = alloca $ \ptr ->
  ivrSystemPollNextEvent' ptr (sizeOf (undefined :: VREvent)) >>= peekEventMaybe ptr

  where
    peekEventMaybe ptr True = Just <$> peek ptr
    peekEventMaybe _ False = return Nothing
