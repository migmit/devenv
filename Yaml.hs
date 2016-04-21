module Yaml(
            ReadYamlError(FileDoesNotExist, YamlParsingError),
            YamlM, YamlMap,
            recoverMaybeYamlM, recoverYamlM, runYamlM,
            yamlGetInt, yamlGetKeys, yamlGetMap, yamlGetString, yamlGetStrings, yamlParseFile
           ) where
import Control.Exception (IOException, tryJust)
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import qualified Data.Map as M (Map, empty, keys, lookup, union)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Yaml.YamlLight (YamlLight(YStr), parseYamlFile, unMap, unSeq, unStr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, isUserError)
import Text.Read (readMaybe)

import FailureOr

type OverrideName = String
type Variables = M.Map YamlLight YamlLight

emptyVariables :: Variables
emptyVariables = M.empty
                  
data YamlMap = YamlMap {yamlVars :: Variables, yamlMap :: M.Map YamlLight YamlLight}

type YamlM = ReaderT [OverrideName] FailureOr
runYamlM :: YamlM a -> [OverrideName] -> FailureOr a
runYamlM = runReaderT
recoverYamlM :: a -> YamlM a -> YamlM a
recoverYamlM a = mapReaderT $ return . recoverFromFailure a
recoverMaybeYamlM :: YamlM a -> YamlM (Maybe a)
recoverMaybeYamlM = recoverYamlM Nothing . fmap Just

substitute :: Variables -> String -> YamlM String
substitute _ [] = return []
substitute vars ('{':xs) =
    case span (/= '}') xs of
      (_, "") -> return $ '{':xs
      (name, _:xs') ->
          do ySubst <- recoverMaybeYamlM $ yamlLookupKey vars name
             let value = fromMaybe ('{' : name ++ "}") $ fmap U.toString $ ySubst >>= unStr
             fmap (value ++) $ substitute vars xs'
substitute vars (x:xs) = fmap (x :) $ substitute vars xs

yKey :: String -> YamlLight
yKey = YStr . U.fromString

yamlLookupKey :: M.Map YamlLight YamlLight -> String -> YamlM YamlLight
yamlLookupKey kvMap key =
    do let readMap mp name = M.lookup (yKey name) mp >>= unMap
           allOverrides = fromMaybe M.empty $ readMap kvMap "overrides"
       overrides <- asks $ catMaybes . map (readMap allOverrides)
       let maybeResult = foldr (\o r -> r `mplus` M.lookup (yKey key) o) mzero (kvMap : overrides)
       lift $ orErr ("key " ++ key ++ " not found") maybeResult

yamlGetString :: YamlMap -> String -> YamlM String
yamlGetString yMap key =
    do yVal <- yamlLookupKey (yamlMap yMap) key
       bsValue <- lift $ orErr ("value for key " ++ key ++ " is not a string") $ unStr yVal
       substitute (yamlVars yMap) $ U.toString bsValue

yamlGetInt :: YamlMap -> String -> YamlM Int
yamlGetInt yMap key =
    do stringVal <- yamlGetString yMap key
       lift $ orErr ("not a number: " ++ stringVal) $ readMaybe stringVal

yamlGetStrings :: YamlMap -> String -> YamlM [String]
yamlGetStrings yMap key =
    do yVal <- yamlLookupKey (yamlMap yMap) key
       seqVal <- lift $ orErr ("value for key " ++ key ++ " is not a sequence") $ unSeq yVal
       let readStr v =
               lift $ orErr ("one of the values under " ++ key ++ " is not a string") $ unStr v
       values <- sequence $ map readStr seqVal
       mapM (substitute (yamlVars yMap) . U.toString) values

yamlMakeMap :: String -> YamlLight -> Variables -> YamlM YamlMap
yamlMakeMap errorMessage yVal vars =
    do mapVal <- lift $ orErr errorMessage $ unMap yVal
       mNewVars <- recoverYamlM Nothing $ fmap unMap $ yamlLookupKey mapVal "variables"
       return YamlMap {yamlVars = M.union (fromMaybe M.empty mNewVars) vars, yamlMap = mapVal}

yamlGetMap :: YamlMap -> String -> YamlM YamlMap
yamlGetMap yMap key =
    do yVal <- yamlLookupKey (yamlMap yMap) key
       yamlMakeMap ("value for key " ++ key ++ " is not a map") yVal (yamlVars yMap)

initYamlMap :: YamlLight -> YamlM YamlMap
initYamlMap yVal = yamlMakeMap "yaml provided is not a map" yVal emptyVariables

yamlParseFile :: FilePath -> IO (Either ReadYamlError (YamlM YamlMap))
yamlParseFile file =
    fmap (fmap initYamlMap) $ tryJust exceptionToReadYamlError $ parseYamlFile file

yamlGetKeys :: YamlMap -> [String]
yamlGetKeys = catMaybes . map (fmap U.toString . unStr) . M.keys . yamlMap

data ReadYamlError = FileDoesNotExist | YamlParsingError String
exceptionToReadYamlError :: IOException -> Maybe ReadYamlError
exceptionToReadYamlError ioe =
    if isDoesNotExistError ioe then Just FileDoesNotExist
    else if isUserError ioe then Just $ YamlParsingError (ioeGetErrorString ioe)
         else Nothing
