module Main where

import           Control.Exception          (AsyncException(..), catch)
import           Control.Monad.Except

import           Data.Version
import           Data.List
import           Text.Regex.Posix

import           System.Environment
import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))
import           System.Console.Haskeline   hiding (handle, catch, throwTo)
import           System.Console.GetOpt
import           System.Exit                (ExitCode (..), exitWith)

import           Language.Egison
import qualified Language.Egison.CmdOptions  as ET
import           Language.Egison.Completion  (completeEgison)
import qualified Language.Egison.Parser.NonS as Parser
import qualified Paths_egison_tutorial       as P

main :: IO ()
main = do args <- getArgs
          let (actions, nonOpts, _) = getOpt Permute options args
          let opts = foldl (flip id) defaultOptions actions
          case opts of
            Options {optShowSections = True} -> putStrLn $ show tutorial
            Options {optSection = Just sn, optSubSection = Just ssn} -> do
              let sn' = (read sn) :: Int
              let ssn' = (read ssn) :: Int
              let ret = case tutorial of
                          Tutorial ss ->
                            if 0 < sn' && sn' <= length ss
                              then case nth sn' ss of
                                     Section _ cs ->
                                       if 0 < ssn' && ssn' <= length cs
                                         then showContent $ nth ssn' cs
                                         else "error: content out of range"
                              else "error: section out of range"
              putStrLn ret
            Options {optShowHelp = True} -> printHelp
            Options {optShowVersion = True} -> printVersionNumber
            Options {optPrompt = prompt} -> do
                env <- initialEnv ET.defaultOption
                case nonOpts of
                    [] -> showBanner >> repl env prompt
                    _ -> printHelp

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optPrompt :: String,
    optShowSections :: Bool,
    optSection :: Maybe String,
    optSubSection :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optPrompt = "> ",
    optShowSections = False,
    optSection = Nothing,
    optSubSection = Nothing
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
    "set prompt string",
  Option ['l'] ["list"]
    (NoArg (\opts -> opts {optShowSections = True}))
    "show section list",
  Option ['s'] ["section"]
    (ReqArg (\sn opts -> opts {optSection = Just sn})
            "String")
    "set section number",
  Option ['c'] ["subsection"]
    (ReqArg (\ssn opts -> opts {optSubSection = Just ssn})
            "String")
    "set subsection number"
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
  putStrLn $ "Egison Tutorial Version " ++ showVersion P.version
  putStrLn $ "Welcome to Egison Tutorial!"
  putStrLn $ "** Information **"
  putStrLn $ "We can use a \"Tab\" key to complete keywords on the interpreter."
  putStrLn $ "If we type a \"Tab\" key after a closed parenthesis, the next closed parenthesis will be completed."
  putStrLn $ "*****************"

showFinishMessage :: IO ()
showFinishMessage = do
  putStrLn $ "You have finished this section."
  putStrLn $ "Thank you!"

showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison Tutorial.\nByebye."

yesOrNo :: String -> IO Bool
yesOrNo question = do
  input <- liftIO $ runInputT nonReplSettings $ getInputLine $ question ++ " (Y/n): "
  case input of
   Nothing -> return True
   (Just "") -> return True
   (Just "y") -> return True
   (Just "Y") -> return True
   (Just "n") -> return False
   (Just "N") -> return False
   _ -> yesOrNo question

nth :: Int -> [a] -> a
nth n = head . drop (n - 1)

selectSection :: Tutorial -> IO Section
selectSection tutorial@(Tutorial sections) = do
  putStrLn $ take 30 $ repeat '='
  putStrLn $ "List of sections in the tutorial."
  putStrLn $ show tutorial
  putStrLn $ take 30 $ repeat '='
  putStrLn $ "Choose a section to learn."
  n <- getNumber (length sections)
  return $ nth n sections

getNumber :: Int -> IO Int
getNumber n = do
  input <- liftIO $ runInputT nonReplSettings $ getInputLine $ "(1-" ++ show n  ++ "): "
  case input of
    (Just "1") -> return 1
    (Just "2") -> return 2
    (Just "3") -> return 3
    (Just "4") -> return 4
    (Just "5") -> return 5
    (Just "6") -> return 6
    (Just "7") -> return 7
    _ -> do
      putStrLn "Invalid input!"
      getNumber n

-- |Get Egison expression from the prompt. We can handle multiline input.
getEgisonExprOrNewLine :: Options -> InputT IO (Either Bool (String, EgisonTopExpr))
getEgisonExprOrNewLine opts = getEgisonExprOrNewLine' opts ""

getEgisonExprOrNewLine' :: Options -> String -> InputT IO (Either Bool (String, EgisonTopExpr))
getEgisonExprOrNewLine' opts prev = do
  mLine <- case prev of
             "" -> getInputLine $ optPrompt opts
             _  -> getInputLine $ replicate (length $ optPrompt opts) ' '
  case mLine of
    Nothing -> return $ Left False -- The user's input is 'Control-D'.
    Just [] -> return $ Left True  -- The user's input is 'Enter'.
    Just line -> do
      let input = prev ++ line
      let parsedExpr = Parser.parseTopExpr input
      case parsedExpr of
        Left err | show err =~ "unexpected end of input" ->
          getEgisonExprOrNewLine' opts $ input ++ "\n"
        Left err -> do
          liftIO $ print err
          getEgisonExprOrNewLine opts
        Right topExpr -> return $ Right (input, topExpr)

replSettings :: MonadIO m => FilePath -> Settings m
replSettings home = Settings
  { complete       = completeEgison
  , historyFile    = Just (home </> ".egison_history")
  , autoAddHistory = True
  }

nonReplSettings :: MonadIO m => Settings m
nonReplSettings = Settings
  { complete       = noCompletion
  , historyFile    = Nothing
  , autoAddHistory = False
  }

repl :: Env -> String -> IO ()
repl env prompt = do
  section <- selectSection tutorial
  case section of
    Section _ cs -> loop env cs True
 where
  loop :: Env -> [Content] -> Bool -> IO ()
  loop env [] _ = do
--    liftIO $ showFinishMessage
    liftIO $ repl env prompt
  loop env (content:contents) b = (do
    if b
      then liftIO $ putStrLn $ show content
      else return ()
    home <- getHomeDirectory
    input <- liftIO $ runInputT (replSettings home) $ getEgisonExprOrNewLine defaultOptions
    case input of
      -- The user input 'Control-D'.
      Left False -> do
        b <- yesOrNo "Do you want to quit?"
        if b
          then return ()
          else do
            b <- yesOrNo "Do you want to proceed next?"
            if b
              then loop env contents True
              else loop env (content:contents) False
      -- The user input just 'Enter'.
      Left True -> do
        b <- yesOrNo "Do you want to proceed next?"
        if b
          then loop env contents True
          else loop env (content:contents) False
      Right (topExpr, _) -> do
        result <- liftIO $ runEgisonTopExpr ET.defaultOption env topExpr
        case result of
          Left err -> do
            liftIO $ putStrLn $ show err
            loop env (content:contents) False
          Right env' -> loop env' (content:contents) False)
    `catch`
    (\e -> case e of
             UserInterrupt -> putStrLn "" >> loop env (content:contents) False
             StackOverflow -> putStrLn "Stack over flow!" >> loop env (content:contents) False
             HeapOverflow -> putStrLn "Heap over flow!" >> loop env (content:contents) False
             _ -> putStrLn "error!" >> loop env (content:contents) False
     )

data Tutorial = Tutorial [Section]

-- |title and contents
data Section = Section String [Content]

-- |explanation, examples, and exercises
data Content = Content String [String] [String]

instance Show Tutorial where
  show = showTutorial

instance Show Section where
  show = showSection

instance Show Content where
  show = showContent

showTutorial :: Tutorial -> String
showTutorial (Tutorial sections) =
  let n = length sections in
  intercalate "\n" $ map (\(n, section) -> show n ++ ": " ++ show section) $ zip [1..n] sections

showSection :: Section -> String
showSection (Section title _) = title

showContent :: Content -> String
showContent (Content msg examples exercises) =
  "====================\n" ++
  msg ++ "\n" ++
  (case examples of
     [] -> ""
     _ -> "\nExamples:\n" ++ (intercalate "\n" (map (\example -> "  " ++ example) examples)) ++ "\n") ++
  (case exercises of
     [] -> ""
     _ -> "\nExercises:\n" ++ (intercalate "\n" (map (\exercise -> "  " ++ exercise) exercises)) ++ "\n") ++
  "===================="

tutorial :: Tutorial
tutorial = Tutorial
 [Section "Arithmetic"
   [
    Content "We can do arithmetic operations with \"+\", \"-\", \"*\", \"/\", \"^\" and \"modulo\"."
     ["1 + 2", "30 - 15", "10 * 20", "20 / 5", "2 ^ 10", "modulo 17 4"]
     [],
    Content "We support rational numbers."
     ["2 / 3 + 1 / 5", "42 / 84"]
     [],
    Content "We support floating-point numbers, too."
     ["10.2 + 1.3", "10.2 + 1"]
     [],
    Content "We can convert a rational number to a floating-point number using \"rtof\"."
     ["rtof (1 / 5)", "rtof (1 / 100)"]
     [],
    Content "We can handle lists of numbers.\nWe construct a list by enclosing its elements with \"[]\"."
     ["[]", "[10]", "[1, 2, 3, 4, 5]"]
     [],
    Content "We can decompose a list using the \"head\" and \"tail\" function."
     ["head [1, 2, 3, 4, 5]", "tail [1, 2, 3, 4, 5]", "head (tail [1, 2, 3, 4, 5])"]
     ["Try to extract the third element of the list \"[1, 2, 3, 4, 5]\" with \"head\" and \"tail\"."],
    Content "Using the \"take\" function, we can extract a head part of a list."
     ["take 3 [1, 2, 3, 4, 5]", "take 0 [1, 2, 3, 4, 5]"]
     [],
    Content "We can handle infinite lists.\nFor example, \"nats\" and \"primes\" are an infinite list that contains all natural numbers and prime numbers respectively.\nTry to extract a head part from them."
     ["take 10 nats", "take 30 nats", "take 10 primes", "take 30 primes"]
     ["What is the 100th prime number?"],
    Content "We can change an infix operator to a prefix operator by enclosing the operator by \"()\".\nThis notation is similar to the section notation in Haskell."
     ["(+) 2 3", "(/ 2) 3", "(2 /) 3"]
     [],
    Content "We can create functions using the \"lambda\" notation.\nFunctions are written like \"\\x -> ... \".\n\"(\\x -> x + 2)\" is equal to \"(+ 2)\".\n\"(\\x y -> x + y)\" is equal to \"(+)\"."
     ["(\\x -> x + 2) 10", "(\\x y -> x + y) 2 3"]
     [],
    Content "The \"map\" function applies the first argument function to each element of the second argument list.\nThe \"map\" function is one of the most important function in functional programming."
     ["take 100 (map (* 2) nats)", "take 100 (map (\\x -> modulo x 3) nats)"]
     ["Try to create a sequence of numbers \"[1, 1/2, 1/3, 1/4, ..., 1/100]\"."],
    Content "The \"foldl\" function gathers together all elements of the third argument list using the operator specified by the first argument.\nThe second argument is an initial value.\nThe \"foldl\" function is also important in functional programming."
     ["foldl (+) 0 [1, 2, 3, 4, 5]", "foldl (*) 1 [1, 2, 3, 4, 5]"]
     ["Try to get the sum of from 1 to 100."],
    Content "Try calculating \"1 + 1/2 + 1/3 + 1/4 + ... + 1/100\".\nRemember that we can convert a rational number to a floating-point number with \"rtof\"."
     ["rtof (2 / 3)"]
     [],
    Content "Try calculating \"1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ... + (1/100)^2\".\nIn fact, \"1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ...\" converges to \"pi * pi / 6\"."
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Basics of functional programming"
   [
    Content "We can bind a value to a variable with \":=\" (not \"=\").\nWe can easily get the value we bound to a variable."
     ["x := 10", "x", "y := 1 + x", "y"]
     [],
    Content "We support recursive definitions. It enables us to define a list with infinite elements."
     ["ones := 1 :: ones", "take 100 ones", "nats := 1 :: map (\\n -> n + 1) nats", "take 100 nats", "odds := 1 :: map (\\n -> n + 2) odds", "take 100 odds"]
     ["Try to define the infinite list of even numbers like [2, 4, 6, 8, 10, ...]."],
    Content "Let's define functions and test them."
     ["increment x := x + 1", "increment 10", "multiply x y := x * y", "multiply 10 20", "fact n := foldl (*) 1 (take n nats)", "fact 10"]
     [],
    Content "We can compare numbers using functions, \"=\", \"<\", \"<=\", \">\", \">=\".\nThese functions return boolean values, \"True\" and \"False\".\nFunctions that return boolean values are called \"predicates\"."
     ["1 = 1", "1 < 1", "1 <= 1",  "1 > 1", "1 >= 1"]
     [],
    Content "Using the \"takeWhile\" function, we can get the prefix of the second argument list whose elements satisfy the predicate of the first argument.\n\"primes\" is a infinite list that contains all prime numbers."
     ["takeWhile (\\n -> n < 100) primes", "takeWhile (\\n -> n < 1000) primes"]
     [],
    Content "Using the \"filter\" function, we can extract all elements that satisfy the given predicate."
     ["take 100 (filter isEven nats)", "take 100 (filter isPrime nats)", "take 100 (filter (\\p -> (modulo p 4) = 1) primes)"]
     ["Try to enumerate the first 100 primes that are congruent to 3 modulo 4."],
    Content "We can create a tuple by enclosing objects by \"(\", and \")\".\n\nNote that a tuple that consists of only one element is equal to that element itself."
     ["(1, 2)", "(1, 2, 3)", "(1)", "((1))"]
     [],
    Content "Using the \"zip\" function, we can combine two lists as follows."
     ["take 100 (zip nats nats)", "take 100 (zip primes primes)"]
     ["Try to generate the prime table as \"[(1, 2), (2, 3), (3, 5), (4, 7), (5, 11), ...]\"."],
    Content "Try to create a Fibonacci sequence \"[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...]\".\n\nHint:\n  Replace \"???\" in the following expression to a proper function.\n  fibs := 1 :: 1 :: map ??? (zip fibs (tail fibs))"
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Basics of pattern matching"
   [
    Content "Let's try pattern matching for a list.\nThe \"join\" pattern (++) divides a list into two lists.\nNote that the matchAll expression enumerates all the decompositions."
     ["matchAll [1, 2, 3]       as list integer with $hs ++ $ts -> (hs, ts)",
      "matchAll [1, 2, 3, 4, 5] as list integer with $hs ++ $ts -> (hs, ts)"]
     [],
    Content "Try another pattern constructor \"cons\" (::).\nThe \"cons\" pattern (::) divides a list into the head element and the rest.\n"
     ["matchAll [1, 2, 3]       as list integer with $x :: $xs -> (x ,xs)",
      "matchAll [1, 2, 3, 4, 5] as list integer with $x :: $xs -> (x, xs)"]
     [],
    Content "\"_\" is a wildcard and matches with any objects."
     ["matchAll [1, 2, 3]       as list integer with $x :: _ -> x",
      "matchAll [1, 2, 3, 4, 5] as list integer with $hs ++ _ -> hs"]
     [],
    Content "We can write non-linear patterns.\nA non-linear pattern is a pattern that allows multiple occurrences of the same variables in a pattern.\nA pattern that begins with \"#\" matches the target when it is equal with the evaluation result of the expression after \"#\"."
     ["matchAll [1, 1, 2, 3, 3, 2] as list integer with _ ++ $x :: #x :: _ -> x",
      "matchAll [1, 1, 2, 3, 3, 2] as list integer with _ ++ $x :: #(x + 1) :: _ -> x"]
     [],
    Content "Egison can handle pattern matching with infinitely many results.\nFor example, we can enumerate twin primes using pattern matching as follows."
     ["take 10 (matchAll primes as list integer with _ ++ $p :: #(p + 2) :: _ -> (p, p + 2))"]
     ["What is the 100th twin prime?"],
    Content "Try to enumerate the first 10 prime pairs whose form is (p, p + 6) like \"[(5, 11), (7, 13), (11, 17), (13, 19), (17, 23), ...]\"."
     []
     [],
    Content "A pattern that begins with \"!\" is called not-pattern.\nA not-pattern matches when the content of the not-pattern does not match the target."
     ["matchAll [1, 1, 2, 3, 3, 2] as list integer with _ ++ $x :: #x :: _ -> x",
      "matchAll [1, 1, 2, 3, 3, 2] as list integer with _ ++ $x :: !#x :: _ -> x"]
     [],
    Content "A pattern whose form is \"p1 & p2\" is called and-pattern.\nAn and-pattern is a pattern that matches the target if and only if both \"p1\" and \"p2\" matches.\nThe and-pattern in the following sample is used like an as-pattern."
     ["take 10 (matchAll primes as list integer with _ ++ $p :: (!#(p + 2) & $q) :: _ -> (p, q))"]
     [],
    Content "A pattern whose form is \"p1 | p2\" is called or-pattern.\nAn or-pattern matches with the target, if \"p1\" or \"p2\" matches the target.\nIn the following sample, we enumerate prime triplets."
     ["take 10 (matchAll primes as list integer with _ ++ $p :: ($m & (#(p + 2) | #(p + 4))) :: #(p + 6) :: _ -> (p, m, (p + 6)))"]
     ["What is the 20th prime triplet?"],
    Content "Try to enumerate the first 4 prime quadruples whose form is (p, p + 2, p + 6, p + 8) like \"[(5, 7, 11, 13), (11, 13, 17, 19), ...]\"."
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Pattern matching for multisets and sets"
   [
    Content "We can describe pattern matching for multisets and sets.\nWe can change the interpretation of patterns by changing the matcher, the second argument of the matchAll expression.\nThe meaning of the cons pattern (::) is generalized to divide a collection into \"an\" element and the rest."
     ["matchAll [1, 2, 3] as list integer     with $x :: $xs -> (x, xs)",
      "matchAll [1, 2, 3] as multiset integer with $x :: $xs -> (x, xs)",
      "matchAll [1, 2, 3] as set integer      with $x :: $xs -> (x, xs)"]
     [],
    Content "Try another pattern constructor \"join\" (++).\nThe \"join\" pattern (++) divides a collection into two collections."
     ["matchAll [1, 2, 3, 4, 5] as list integer     with $xs ++ $ys -> (xs, ys)",
      "matchAll [1, 2, 3, 4, 5] as multiset integer with $xs ++ $ys -> (xs, ys)",
      "matchAll [1, 2, 3, 4, 5] as set integer      with $xs ++ $ys -> (xs, ys)"]
     [],
    Content "Try non-linear pattern matching for multiset."
     ["matchAll [1, 2, 1, 3, 2] as multiset integer with $x :: #x :: _ -> x",
      "matchAll [1, 2, 1, 3, 2] as multiset integer with $x :: #(x + 2) :: _ -> x",
      "matchAll [1, 2, 1, 3, 2] as multiset integer with $x :: !(#(x + 2) :: _) -> x"]
     [],
    Content "Pattern matching of Egison efficiently backtracks for non-linear patterns.\nFor example, all the following pattern-matching expressions are processed in O(n^2)."
     ["matchAll [1..30] as multiset integer with $x :: #x :: _ -> x",
      "matchAll [1..30] as multiset integer with $x :: #x :: #x :: _ -> x",
      "matchAll [1..30] as multiset integer with $x :: #x :: #x :: #x _ -> x"]
     [],
    Content "Egison is designed to enumerate all the infinitely many pattern-matching results.\nThe following samples enumerate all the pairs and triplets of natural numbers."
     ["matchAll nats as set integer with $x :: $y :: _ -> (x, y)",
      "matchAll nats as set integer with $x :: $y :: $z :: _ -> (x, y, z)"]
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Symbolic computation"
   [
    Content "Egison treats unbound variables as a symbol."
     ["x + 1",
      "x + x",
      "2 * x + y"]
     [],
    Content "Egison automatically expands an expression to the canonical form."
     ["(x + y) * (x + y)",
      "(x + y)^2",
      "(x + y)^3"]
     [],
    Content "Egison can handle complex numbers.\n\"i\" represents the imaginary unit."
     ["i * i",
      "(1 + i)^2",
      "(1 + i)^4"]
     [],
    Content "Egison can handle algebraic numbers such as \"sqrt 2\" and \"sqrt 3\"."
     ["sqrt 12",
      "sqrt 2 * sqrt 2",
      "sqrt 2 * sqrt 3",
      "(rt 3 2)^3"]
     [],
    Content "Egison can handle the trigonometric functions such as \"cos θ\" and \"sin θ\"."
     ["(cos θ)^2 + (sin θ)^2"]
     [],
    Content "Here are several samples for symbolic computation in Egison.\nPlease visit the link!\nhttps://www.egison.org/math/"
     [
      ]
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Differential geometry: tensor analysis"
   [
    Content "We can handle vectors.\nWe construct vectors with \"[| |]\"."
     ["[| 1, 2, 3 |]",
      "[| 1, 2, 3 |] + [| 1, 2, 3 |]"
      ]
     [],
    Content "We can append a symbolical index to vectors."
     ["[| 1, 2, 3 |]_i + [| 1, 2, 3 |]_i",
      "[| 1, 2, 3 |]_i + [| 1, 2, 3 |]_j"
      ]
     [],
    Content "The \".\" function is a function for multiplying tensors."
     ["[| 1, 2, 3 |]_i . [| 1, 2, 3 |]_i",
      "[| 1, 2, 3 |]_i . [| 1, 2, 3 |]_j"
      ]
     [],
    Content "We can handle both of superscripts (~) and subscripts(_).\nThe \".\" function supports Einstein summation notation."
     ["[| 1, 2, 3 |]~i . [| 1, 2, 3 |]_i"
      ]
     [],
    Content "Matrix is represented as a vector of vectors."
     ["[| [| 1, 2, 3 |], [| 10, 20, 30 |] |]"
      ]
     [],
    Content "Matrix multiplication is represented as follows using tensor index notation."
     ["[| [| a, b |], [| c, d |] |]~i_j . [| [| x, y |], [| z, w |] |]~j_k"
      ]
     [],
    Content "The function defined using scalar parameters (prepended by \"$\") are automatically mapped to each component of tensors."
     ["min $x $y := if x < y then x else y",
      "min [| 1, 2, 3 |]_i [| 10, 20, 30 |]_i",
      "min [| 1, 2, 3 |]_i [| 10, 20, 30 |]_j"
      ]
     [],
    Content "The function defined using tensor parameters (prepended by \"%\") treats a tensor as a whole.\nIf we prepend "
     ["det2 %X := X_1_1 * X_2_2 -  X_1_2 * X_2_1",
      "det2 [| [| 2, 1 |], [| 1, 2 |] |]",
      "det2 [| [| a, b |], [| c, d |] |]"
      ]
     [],
    Content "Here are several samples of tensor analysis in programming.\nPlease visit the link!\nhttps://www.egison.org/math/"
     [
      ]
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Differential geometry: differential forms"
   [
    Content "By default, the same indices are completed to each tensor of the arguments."
     ["[| 1, 2, 3 |] + [| 1, 2, 3 |] -- => [| 1, 2, 3 |]_t1 + [| 1, 2, 3 |]_t1"
      ]
     [],
    Content "When “!” is prepended to the function application, the different indices are completed to each tensor of the arguments."
     ["[| 1, 2, 3 |] !+ [| 1, 2, 3 |] -- => [| 1, 2, 3 |]_t1 + [| 1, 2, 3 |]_t2"
      ]
     [],
    Content "1-forms on Euclid space and Wedge product are represented as follows.\n\"!\" is effectively used in the definition of Wedge product."
     ["dx := [| 1, 0, 0 |]",
      "dy := [| 0, 1, 0 |]",
      "dz := [| 0, 0, 1 |]",
      "wedge %A %B := A !. B",
      "wedge dx dy"
      ]
     [],
    Content "The \"dfNormalize\" function converts a differential form to the antisymmetric tensor."
     ["wedge dx dy",
      "dfNormalize (wedge dx dy)"
      ]
     [],
    Content "Exterior derivative is defined as follows.\n\"!\" is effectively used in the definition of exterior derivative."
     ["params := [| x, y, z |]",
      "d %A := !((flip ∂/∂) params A)",
      "d (f x y z)",
      "d (d (f x y z))",
      "dfNormalize (d (d (f x y z)))"
      ]
     [],
    Content "Here are several samples for representing differential forms in programming.\nPlease visit the link!\nhttps://www.egison.org/math/"
     [
      ]
     [],
    Content "This is the end of our tutorial.\nThank you for enjoying our tutorial!\nPlease check our paper, manual and code for further reference!"
     []
     []
    ]

  ]
