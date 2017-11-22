module OpenVR.TH where

import Control.Lens (view)
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Foreign (peek)
import Language.Haskell.TH

-- dummy out the stuff we don't care about
data OpenVRAPI = OpenVRAPI
  { typedefs :: [Object]
  , enums :: [Object]
  , consts :: [Const]
  , structs :: [Object]
  , methods :: [Method]
  } deriving (Show, Eq)

data Const = Const
  { constname :: Text
  , consttype :: Text
  , constval :: Text
  } deriving (Show, Eq)

data Method = Method
  { classname :: Text
  , methodname :: Text
  , returntype :: Text
  , params :: Maybe [Object]
  } deriving (Show, Eq)

deriveJSON defaultOptions {omitNothingFields = True} ''Method
deriveJSON defaultOptions ''Const
deriveJSON defaultOptions ''OpenVRAPI

parseOpenVRJSON :: FilePath -> Q [Dec]
parseOpenVRJSON path = do
  json <- runIO $ BL.readFile path
  api <- case eitherDecode json of
    Right api -> return api
    Left err -> error $ "Failed to parse " ++ path ++ ":\n" ++ err
  let cts = constToMap $ consts api
  let classes = S.toList . S.fromList . mapMaybe (T.stripPrefix "vr::") . map classname $ methods api
  ifs <- mapM (createOpenVRInterface cts) classes
  constDecs <- createConstDecs (consts api)

  return (ifs ++ constDecs)

  where
    constToMap :: [Const] -> Map Text Text
    constToMap = M.fromList . map (\(Const n _ v) -> (n,v))


vrInterfaceClass :: Q Type
vrInterfaceClass = conT $ mkName "VRInterface"

createOpenVRInterface :: Map Text Text -- ^ constants
                      -> Text -- ^ class
                      -> Q Dec
createOpenVRInterface cts cls
  = instanceD (cxt []) (appT vrInterfaceClass myClass) [mkInterfaceVersion, mkInterfaceLens]

  where
    versionKey = cls `T.append` "_Version"
    lensName = mkName . T.unpack $ "vr" `T.append `(fromJust $ T.stripPrefix "IVR" cls)
    hsName = mkName . T.unpack $ "VR_" `T.append` cls `T.append` "_FnTable"
    myClass = conT hsName

    mkInterfaceVersion = funD (mkName "interfaceVersion")
      [clause
       [wildP]
       (normalB . stringE . T.unpack $ M.findWithDefault (error "couldn't find version") versionKey cts)
       []
      ]
      
    mkInterfaceLens = valD (varP $ mkName "interface") (normalB $ varE lensName) []


-- messy
createConstDecs :: [Const] -> Q [Dec]
createConstDecs cts' = do
  let cts = filter (\c -> "k_" `T.isPrefixOf` constname c) cts'
  sdecs <- mapM createStringDec $ filter (\c -> consttype c == "const char *const") cts
  idecs <- mapM createIntDec $ filter (\c -> consttype c == "const uint32_t") cts
  return . concat $ sdecs ++ idecs

  where
    createStringDec ct = sequence [ sigD decName [t|String|]
                                  , valD (varP decName) (normalB . stringE . T.unpack $ constval ct) [] ]
      where
        decName = mkName . T.unpack $ constname ct

    createIntDec ct = sequence [ sigD decName [t|Int|]
                               , valD (varP decName) (normalB . litE . integerL . read . T.unpack $ constval ct) [] ]
      where
        decName = mkName . T.unpack $ constname ct
    

makeVrCall :: Name -- ^ function that needs to be converted
           -> String -- ^ new name
           -> Q [Dec]
makeVrCall n s = do
      -- strip the first two arguments
      -- verify that the second arg is Ptr ifTy?
      (VarI _ funTy@(AppT (AppT ArrowT _)
                     (AppT (AppT ArrowT _) resTy)) _) <- reify n
      
      tyDec <- sigD funName (return resTy)

      funDec <- mkFunD resTy
      return [tyDec, funDec]
  where
    funName = mkName s
    mkFunD resTy = funD funName [clause
                                 []
                                 (normalB (innerExp $ arity resTy))
                                 []
                                ]
    arity (AppT (AppT ArrowT _) rem) = 1 + arity rem
    arity _ = 0
    innerExp args
      = do
        names <- replicateM args (newName "x")
        ctx <- newName "ctx"
        vtbl <- newName "vtbl"
        ptr <- newName "ptr"
        let calledExp = foldl (\r n -> AppE r (VarE n)) (AppE
                                                         (AppE (VarE n) (VarE vtbl))
                                                         (VarE ptr)) names
        return $ LamE (VarP <$> names)
          (DoE [ BindS (VarP ctx) (VarE $ mkName "getOpenVRContext")
               , LetS [ValD
                       (VarP ptr)
                       (NormalB $ AppE (AppE (VarE 'view) (VarE $ mkName "interface"))
                         (VarE $ mkName "ctx")
                       ) []
                     ]
               , BindS (VarP vtbl) (AppE (VarE 'peek) (VarE ptr))
               , NoBindS calledExp])

