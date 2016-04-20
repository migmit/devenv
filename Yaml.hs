module Yaml where
import Control.Exception (IOException)
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import qualified Data.Map as M (Map, foldlWithKey, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Yaml.YamlLight (YamlLight(YStr), unMap, unSeq, unStr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, isUserError)
import Text.Read (readMaybe)

import FailureOr
import Wait

data ReadYamlError = FileDoesNotExist | YamlParsingError String
exceptionToReadYamlError :: IOException -> Maybe ReadYamlError
exceptionToReadYamlError ioe =
    if isDoesNotExistError ioe then Just FileDoesNotExist
    else if isUserError ioe then Just $ YamlParsingError (ioeGetErrorString ioe)
         else Nothing

getStringValue :: M.Map YamlLight YamlLight -> String -> FailureOr String
getStringValue yamlMap key =
    do yamlValue <-
           orErr ("key " ++ key ++ " not found") $ M.lookup (YStr $ U.fromString key) yamlMap
       bsValue <- orErr ("value for key " ++ key ++ " isn't a string") $ unStr yamlValue
       return $ U.toString bsValue

getIntValue :: M.Map YamlLight YamlLight -> String -> FailureOr Int
getIntValue yamlMap key =
    do stringValue <- getStringValue yamlMap key
       orErr ("not a number: " ++ stringValue) $ readMaybe stringValue

getStrings :: M.Map YamlLight YamlLight -> String -> Maybe [String]
getStrings yamlMap key =
    do yamlValue <- M.lookup (YStr $ U.fromString key) yamlMap
       seqValue <- unSeq yamlValue
       values <- sequence $ map unStr seqValue
       return $ map U.toString values

getMapValue :: M.Map YamlLight YamlLight -> String -> Maybe (M.Map YamlLight YamlLight)
getMapValue yamlMap key =
    do yamlValue <- M.lookup (YStr $ U.fromString key) yamlMap
       unMap yamlValue

getOverridesList :: [String] -> M.Map YamlLight YamlLight -> [M.Map YamlLight YamlLight]
getOverridesList overrides realOverrides =
    catMaybes $ map getOverride overrides where
        getOverride o = M.lookup (YStr $ U.fromString o) realOverrides >>= unMap

overrideFields
    :: [String]
    -> M.Map YamlLight YamlLight
    -> M.Map YamlLight YamlLight
    -> M.Map YamlLight YamlLight
overrideFields fieldNames first second = foldl overrideField first fieldNames where
    overrideField config field =
        let yField = YStr $ U.fromString field in
        case M.lookup yField second of
          Nothing -> config
          Just v -> M.insert yField v config

readWait :: M.Map YamlLight YamlLight -> Maybe [Wait]
readWait =
    flip M.foldlWithKey (return []) $ \waits key val ->
    do ws <- waits
       keyStr <- unStr key
       w <-
           case U.toString keyStr of
             "port" ->
                 do portStr <- unStr val
                    port <- readMaybe $ U.toString portStr
                    return $ Wait $ Port port
             _ -> Nothing
       return $ w : ws
