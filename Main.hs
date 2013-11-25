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

import System.IO
import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)
import Language.Egison
import qualified Paths_egison_tutorial as P

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
                    [] -> showBanner >> repl env prompt
                    _ -> printHelp

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
  putStrLn "Usage: egison-tutorial [options]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --prompt string       Set prompt of the interpreter"
  putStrLn ""
  exitWith ExitSuccess

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn $ showVersion P.version 
  exitWith ExitSuccess

showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Tutorial for Version " ++ showVersion P.version ++ " (C) 2013 Satoshi Egi"
  putStrLn $ "http://www.egison.org"
  putStrLn $ "Welcome to Egison Tutorial!"

showFinishMessage :: IO ()
showFinishMessage = do
  putStrLn $ "You have finished this chapter."
  putStrLn $ "Thank you!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Tutorial.\nByebye."

selectChapter :: [Chapter] -> IO [String]
selectChapter chaps = do
  putStrLn "Select a chapter to learn."
  foldM (\x chap -> do
          putStr $ "  " ++ show x ++ ": "
          putStrLn (fst chap)
          return (x + 1))
        1 chaps
  let m = length chaps
  putStr $ "(1-" ++ show m ++ "): "
  hFlush stdout
  input <- getLine
  let n = (read input :: Int)
  let chap = head $ drop (n - 1) chaps
  return (snd chap)

askUser :: String -> IO Bool
askUser question = do
  putStr $ question
  putStr $ " (Y/n): "
  hFlush stdout
  input <- getLine
  case input of
   [] -> return True
   ('y':_) -> return True
   ('Y':_) -> return True
   ('n':_) -> return False
   _ -> askUser question

repl :: Env -> String -> IO ()
repl env prompt = do
  home <- getHomeDirectory
  tutorials <- selectChapter chapters
  liftIO (runInputT (settings home) $ loop env prompt "" tutorials True)
  where
    settings :: MonadIO m => FilePath -> Settings m
    settings home = defaultSettings { historyFile = Just (home </> ".egison_tutorial_history") }
    
    loop :: Env -> String -> String -> [String] -> Bool -> InputT IO ()
    loop env prompt' _ [] _ = do
      liftIO $ showFinishMessage
      tutorials <- liftIO $ selectChapter chapters
      loop env prompt' "" tutorials True
    loop env prompt' rest ts@(t:rs) True = do
      liftIO $ putStrLn t
      loop env prompt' rest ts False
    loop env prompt' rest ts@(t:rs) False = do
      input <- getInputLine prompt'
      case input of
        Nothing -> do
          response1 <- liftIO $ askUser "Do you want to proceed next?"
          case response1 of
            True -> loop env prompt' rest rs True
            False -> do
              response2 <- liftIO $ askUser "Do you want to quit egison-tutorial?"
              case response2 of
                True -> do
                  liftIO $ showByebyeMessage
                  return ()
                False -> loop env prompt' rest ts False
        Just "quit" -> do
          liftIO $ showByebyeMessage
          return () 
        Just "" ->
          case rest of
            "" -> do
              response1 <- liftIO $ askUser "Do you want to proceed next?"
              case response1 of
                True -> loop env prompt' rest rs True
                False -> loop env prompt' rest ts False
            _ -> loop env (take (length prompt) (repeat ' ')) rest ts False
        Just input' -> do
          let newInput = rest ++ input'
          result <- liftIO $ runEgisonTopExpr env newInput
          case result of
            Left err | show err =~ "unexpected end of input" -> do
              loop env (take (length prompt) (repeat ' ')) (newInput ++ "\n") ts False
            Left err | show err =~ "expecting (top-level|\"define\")" -> do
              result <- liftIO $ fromEgisonM (readExpr newInput) >>= either (return . Left) (evalEgisonExpr env)
              case result of
                Left err | show err =~ "unexpected end of input" -> do
                  loop env (take (length prompt) (repeat ' ')) (newInput ++ "\n") ts False
                Left err -> do
                  liftIO $ putStrLn $ show err
                  loop env prompt "" ts False
                Right val -> do
                  liftIO $ putStrLn $ show val
                  loop env prompt "" ts False
            Left err -> do
              liftIO $ putStrLn $ show err
              loop env prompt "" ts False
            Right env' ->
              loop env' prompt "" ts False
        
type Chapter = (String, [String])

chapters :: [Chapter]
chapters = [
  ("Buildin Data", tutorialsForBuildinData),
  ("Functions", tutorialsForFunction),
  ("Pattern-Matching", tutorialsForPatternMatch)
  ]

tutorialsForBuildinData :: [String]
tutorialsForBuildinData = [
  "You can do arithmetic operations with `+`, `-`, `*`, `div`. Try them as `(+ 1 2)` or `(* 10 20)`.",
  "You can bind a value to a variable with a `define` expression. Try it as `(define $x 10))`.",
  "You can get a value you binded to the variable. Try them as `x`.",
  "You can do boolean operations with `and`, `or`, `not`. Try them as `(and #t #f)`, `(or #t #f)` or `(not #t)`.",
  "You can construct a tuple with `[]`. Try it as `[1 2]`.",
  "A tuple which consists of only one elment is equal with that element itself. Try it as `[1]` or `[[[1]]]`.",
  "You can construct a collection with `{}`. Try it as `{1 2 3}`.",
  "The collection after `@` in a collection is called a subcollection. Try it as `{1 @{2 3} @{4 @{5}} 6}`."
  ]

tutorialsForFunction :: [String]
tutorialsForFunction = [
  "You can define a function. Try it as `(define $f (lambda [$x] (+ x 1)))`.\nThen try the function as `(f 10)`.",
  "You can define local variables with a `let` expression. Try it as `(let {[$x 10] [$y 20]} (+ x y))`.",
  "Try `if` expressions as `(if #t 1 2)` or `(let {[$x 10]} (if (eq? x 10) 1 2))`.",
  "Now, you can define a factorial function that gets a natural number 'n' and returns 'n * n-1 * n-2 * ... * 1'. Let's try!"
  ]


tutorialsForPatternMatch :: [String]
tutorialsForPatternMatch = [
  "You can do pattern-matching against multisets. Try it as `(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])`.",
  "You can do non-linear pattern-matching as `(match-all {1 2 1 3} (multiset integer) [<cons $x <cons ,x _>> x])`.\nTry this expression against various targets.",
  "A pattern that has `^' ahead of which is called a not-pattern.\nA not-pattern matches when the target does not match against the pattern.\nTry it as `(match-all {1 2 1 3} (multiset integer) [<cons $x ^<cons ,x _>> x])`.",
  "You can change the way of pattern-matching by changing \"matcher\".\nTry following expressions.\n`(match-all {1 2 3} (list integer) [<cons $x $xs> [x xs]])`\n`(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])`\n`(match-all {1 2 3} (set integer) [<cons $x $xs> [x xs]])`\n"
  ]
