module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Data.Version
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Regex.Posix

import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import Language.Egison

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optPrompt = prompt} -> do
                env <- primitiveEnv >>= loadLibraries
                case nonOpts of
                    [] -> showBanner >> repl env prompt >> showByebyeMessage
                    (file:args) -> do
                        result <- evalEgisonTopExprs env [LoadFile file, Execute args]
                        either print (const $ return ()) result

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optPrompt :: String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optPrompt = "> "
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "set prompt string"
  ]

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egison [options] file"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --prompt string       Set prompt of the interpreter"
  putStrLn ""
  exitWith ExitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion version 
  exitWith ExitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Tutrial for Version " ++ showVersion version ++ " (C) 2013 Satoshi Egi"
  putStrLn $ "http://egison.pira.jp"
  putStrLn $ "Welcome to Egison Tutorial!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Tutorial.\nByebye."

repl :: Env -> String -> IO ()
repl env prompt = do
  home <- getHomeDirectory
  liftIO $ runInputT (settings home) $ loop tutorials env prompt
  where
    settings :: MonadIO m => FilePath -> Settings m
    settings home = defaultSettings { historyFile = Just (home </> ".egison_tutorial_history") }
    
    loop :: [Tutorial] -> Env -> String -> InputT IO ()
    loop [] env prompt' = do
      loop' [] env prompt' ""
    loop ts@((before, _, _, _):_) env prompt' = do
      liftIO $ putStrLn before
      loop' ts env prompt' ""
    loop' :: [Tutorial] -> Env -> String -> String -> InputT IO ()
    loop' ts env prompt' rest = do
      input <- getInputLine prompt'
      case input of
        Nothing -> return () 
        Just "quit" -> return () 
        Just "" ->  loop' ts env prompt ""
        Just input' -> do
          let newInput = rest ++ input'
          result <- liftIO $ runEgisonTopExpr env newInput
          case result of
            Left err | show err =~ "unexpected end of input" -> do
              loop' ts env (take (length prompt) (repeat ' ')) $ newInput ++ "\n"
            Left err -> do
              liftIO $ putStrLn $ show err
              loop' ts env prompt ""
            Right env' ->
              case ts of
                [] -> loop [] env' prompt
                (before, expAfter, valAfter, endMsg):ts' -> do
                  case undefined of
                    Just valMsg -> do
                      liftIO $ putStrLn valMsg
                      loop ts env' prompt
                    Nothing ->
                      case undefined of
                        Just expMsg -> do
                          liftIO $ putStrLn expMsg
                          loop ts env' prompt
                        Nothing -> do
                          liftIO $ putStrLn endMsg
                          loop ts' env prompt
        
type Tutorial = (String, (EgisonTopExpr -> Maybe String), (EgisonValue -> Maybe String), String)

tutorials :: [Tutorial]
tutorials = [
  ("before-explanation1", \topexpr -> return "after-explanation1", \val -> return "after-explanation1", "good"),
  ("before-explanation2", \topexpr -> return "after-explanation2", \val -> return "after-explanation2", "good")
  ]