{-# LANGUAGE DeriveFunctor, GADTs #-}
module Main(main) where
import Control.Monad (forM, forM_, when)
import Control.Exception (throwIO)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (Set, empty, fromList, insert, isSubsetOf, member, toList)
import qualified Options.Applicative as O
import Options.Applicative ((<>), (<|>))
import System.Directory (getHomeDirectory, removeFile)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)
import System.Process
    (callProcess, createProcess, cwd, proc, readProcessWithExitCode, waitForProcess)

import FailureOr
import Yaml
import ScreenTarget (ScreenTarget)
import Target

data Config = Config {
      configInit :: Maybe String,
      configTargets :: M.Map TargetName Target
    }

yamlToConfig :: FilePath -> YamlMap -> YamlM Config
yamlToConfig configFile firstMap =
    do let fileDirName = takeDirectory configFile
       baseName <- recoverYamlM "" $ yamlGetString firstMap "basedir"
       confInit <- recoverMaybeYamlM $ yamlGetString firstMap "init"
       secondMap <- yamlGetMap firstMap "targets"
       let targetNames = yamlGetKeys secondMap
       targetsList <-
           forM targetNames $ \yamlKey ->
               do yamlTarget <- yamlGetMap secondMap yamlKey
                  target <- readYamlTarget yamlKey (fileDirName </> baseName) yamlTarget
                  return (yamlKey, Target (target :: ScreenTarget))
       return $ Config {configInit = confInit, configTargets = M.fromList targetsList}
                          
readConfig :: FilePath -> IO (YamlM Config)
readConfig configFile =
    do yamlOrErr <- yamlParseFile configFile
       case yamlOrErr of
         Left FileDoesNotExist -> return $ fail $ "file " ++ configFile ++ " doesn't exist"
         Left (YamlParsingError s) -> return $ fail $ "parsing error: " ++ s
         Right yaml -> return $ yaml >>= yamlToConfig configFile

getTargetList :: Config -> S.Set TargetName -> FailureOr [(TargetName, Target)]
getTargetList config targets =
    fmap (reverse . snd) $ getTargetList' S.empty [] config $ S.toList targets where
        getTargetList'
            :: S.Set TargetName
            -> [(TargetName, Target)]
            -> Config
            -> [TargetName]
            -> FailureOr (S.Set TargetName, [(TargetName, Target)])
        getTargetList' processed result _ [] = return (processed, result)
        getTargetList' processed result conf (n:ns) =
            if S.member n processed then getTargetList' processed result conf ns
            else do
              target <- orErr ("target not found: " ++ n) $ M.lookup n $ configTargets conf
              let deps = case target of Target t -> dependencies t
              (newProcessed, newList) <- getTargetList' processed result conf $ S.toList deps
              getTargetList' (S.insert n newProcessed) ((n, target):newList) conf ns

getSimpleTargetList :: Config -> [TargetName] -> FailureOr [(TargetName, Target)]
getSimpleTargetList config targets =
    forM targets $ \t ->
        fmap ((,) t) $ orErr ("target not found: " ++ t) $ M.lookup t $ configTargets config

checkConfigForLoops :: Config -> Bool
checkConfigForLoops = checkConfig' S.empty . configTargets where
    checkConfig' :: S.Set TargetName -> M.Map TargetName Target -> Bool
    checkConfig' found conf =
        let filtered = M.filter (\(Target t) -> S.isSubsetOf (dependencies t) found) conf
        in case M.minViewWithKey filtered of
             Nothing -> M.null conf
             Just ((name, _), _) ->
                 checkConfig' (S.insert name found) $ M.delete name conf

checkConfigDirectories :: Config -> IO (Maybe String)
checkConfigDirectories = checkConfigDirectories' . M.assocs . configTargets where
    checkConfigDirectories' [] = return Nothing
    checkConfigDirectories' ((n, Target t):nts) =
        do e <- checkTarget t n
           case e of
             Nothing -> checkConfigDirectories' nts
             Just err -> return $ Just err

runWhileOK :: String -> (a -> IO Bool) -> [a] -> IO ()
runWhileOK _ _ [] = return ()
runWhileOK err run (a:as) =
    do ok <- run a
       guardIO ok err
       runWhileOK err run as
                  
startTargets :: String -> Config -> RunOptions -> IO ()
startTargets sessionName config runOptions =
    let skipRunning = runSkipRunning runOptions
        execOne (n, Target t) = executeTarget skipRunning False sessionName n t
    in if runOnly runOptions then
           do ts <- runFailureOr $ getSimpleTargetList config $ runTargets runOptions
              runWhileOK "task failed" execOne ts
       else
           do names <- runFailureOr $ getTargetList config $ S.fromList $ runTargets runOptions
              case names of
                [] -> return ()
                ((n, Target t):ts) ->
                    do firstSuccess <-
                           executeTarget skipRunning (not skipRunning) sessionName n t
                       guardIO firstSuccess "task failed"
                       runWhileOK "task failed" execOne ts

checkSession :: String -> IO Bool
checkSession sessionName =
    do let screenArgs = ["-S", sessionName, "-X", "select", "."]
       (exitCode, _, _) <- readProcessWithExitCode "screen" screenArgs ""
       return $ exitCode == ExitSuccess

data ProgramOptions =
    ProgramOptions {
      optConfigFile :: Maybe FilePath,
      optSessionName :: Maybe String,
      optOverrides :: [String],
      optOperation :: OperationMode
    }

data RunOptions =
    RunOptions {
      runOnly :: Bool,
      runSkipRunning :: Bool,
      runTargets :: [TargetName]
    }

data KillOptions =
    KillOptions {
      killTargets :: Maybe [TargetName]
    }

data OperationMode = RunMode RunOptions | KillMode KillOptions

data TechnicalOptions =
    TechnicalOptions {
      techDir :: FilePath,
      techShell :: String,
      techErrFile :: Maybe FilePath
    }

data FullOptions = PO ProgramOptions | TO TechnicalOptions

configHelpMessage :: String
configHelpMessage = "List of possible targets in YAML"

sessionHelpMessage :: String
sessionHelpMessage = "Name of screen session to use"

runHelpMessage :: String
runHelpMessage = "Don't run target dependencies"

addHelpMessage :: String
addHelpMessage = "Add to running session"

overHelpMessage :: String
overHelpMessage = "Use overridden values"

runOptionsParser :: O.Parser RunOptions
runOptionsParser =
    RunOptions <$>
    O.switch (O.short 'j' <> O.long "just" <> O.help runHelpMessage) <*>
    O.switch (O.short 'a' <> O.long "add" <> O.help addHelpMessage) <*>
    O.some (O.argument O.str (O.metavar "Target..."))

killOptionsParser :: O.Parser KillOptions
killOptionsParser =
    KillOptions <$
    O.flag' () (O.short 'k' <> O.long "kill" <> O.help "Kill everything and clean up") <*>
    O.optional (O.some $ O.argument O.str (O.metavar "Target..."))
                        
optionsParser :: O.Parser ProgramOptions
optionsParser =
    ProgramOptions <$>
    O.optional (O.strOption
                     (O.short 'f' <> O.long "file" <>
                       O.metavar "CONFIG" <> O.help configHelpMessage)) <*>
    O.optional (O.strOption
                     (O.short 's' <> O.long "session" <>
                       O.metavar "SESSION" <> O.help sessionHelpMessage)) <*>
    O.many (O.strOption (O.short 'o' <> O.long "override" <>
                          O.metavar "OVERRIDE" <> O.help overHelpMessage)) <*>
    (
     RunMode <$> runOptionsParser
     <|> KillMode <$> killOptionsParser
    )

technicalParser :: O.Parser TechnicalOptions
technicalParser =
    TechnicalOptions <$>
    O.argument O.str (O.hidden <> O.internal) <*>
    O.argument O.str (O.hidden <> O.internal) <*>
    O.optional (O.argument O.str (O.hidden <> O.internal))

fullParser :: O.Parser FullOptions
fullParser =
    PO <$> optionsParser <|>
    TO <$ (O.flag' () (O.short 't' <> O.hidden <> O.internal)) <*> technicalParser

optionsParserInfo :: O.ParserInfo FullOptions
optionsParserInfo =
    O.info (O.helper <*> fullParser) $
    O.fullDesc <>
    O.progDesc "Run targets in screen" <>
    O.header "devenv - start all projects quickly"

maybeError :: Maybe a -> (a -> String) -> IO ()
maybeError me h = forM_ me $ \a -> throwIO $ userError $ h a

main :: IO ()
main =
    do fullOptions <- O.execParser optionsParserInfo
       case fullOptions of
         PO options -> userProcessing options
         TO options -> techProcessing options

cleanUp :: KillOptions -> String -> Bool -> Config -> IO ()
cleanUp (KillOptions {killTargets = Nothing}) sessionName sessionExists config =
    do let cmdArgs = ["-S", sessionName, "-X", "quit"]
       when sessionExists $ callProcess "screen" cmdArgs
       forM_ (configTargets config) $ \(Target t) -> cleanOneTarget t
cleanUp (KillOptions {killTargets = Just targetNames}) sessionName sessionExists config =
    do targets <- runFailureOr $ getSimpleTargetList config targetNames
       forM_ targets $ \(name, Target target) ->
           do let cmdArgs = ["-S", sessionName, "-X", "eval", "select " ++ name, "kill"]
              when sessionExists $ callProcess "screen" cmdArgs
              cleanOneTarget target

userProcessing :: ProgramOptions -> IO ()
userProcessing options =
    do hSetBuffering stdin NoBuffering
       homeDir <- getHomeDirectory
       let configFile = fromMaybe (homeDir </> ".devenv.yml") $ optConfigFile options
       let sessionName = fromMaybe "devenv" $ optSessionName options
       sessionExists <- checkSession sessionName
       eConfig <- readConfig configFile
       config <- runFailureOr $ runYamlM eConfig $ optOverrides options
       case optOperation options of
         RunMode runOptions ->
             do guardIO (checkConfigForLoops config) "loops in config"
                notExisting <- checkConfigDirectories config
                maybeError notExisting id
                forM_ (configInit config) $ \i ->
                    do (e, _, _) <- readProcessWithExitCode "bash" ["-c", i] ""
                       guardIO (e == ExitSuccess) "failed on global init"
                startTargets sessionName config runOptions
         KillMode killOptions -> cleanUp killOptions sessionName sessionExists config

techProcessing :: TechnicalOptions -> IO ()
techProcessing options =
    do let cp = proc "bash" ["-c", techShell options]
       (_, _, _, handle) <- createProcess (cp {cwd = Just (techDir options)})
       exitCode <- waitForProcess handle
       forM_ (techErrFile options) removeFile
       exitWith exitCode
