module Main where

import Prelude hiding (catch)
import Control.Exception ( AsyncException(..), catch )
import Control.Monad.Error

import Data.Version
import Data.List

import System.IO
import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)

import Language.Egison
import Language.Egison.Util
import qualified Paths_egison_tutorial as P

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
                env <- initialEnv
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
  putStrLn $ "Egison Tutorial Version " ++ showVersion P.version ++ " (C) 2013-2014 Satoshi Egi"
  putStrLn $ "Welcome to Egison Tutorial!"
  putStrLn $ "** Information **"
  putStrLn $ "We can use a \'Tab\' key to complete keywords on the interpreter."
  putStrLn $ "If we type a \'Tab\' key after a closed parenthesis, the next closed parenthesis will be completed."
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
  putStr $ question
  putStr $ " (Y/n): "
  hFlush stdout
  input <- getLine
  case input of
   [] -> return True
   ('y':_) -> return True
   ('Y':_) -> return True
   ('n':_) -> return False
   ('N':_) -> return False
   _ -> yesOrNo question

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
  putStr   $ "(1-" ++ show n  ++ "): "
  hFlush stdout
  input <- getLine
  case input of
    ('1':_) -> return 1
    ('2':_) -> return 2
    ('3':_) -> return 3
    ('4':_) -> return 4
--    ('5':_) -> return 5
--    ('6':_) -> return 6
--    ('7':_) -> return 7
--    ('9':_) -> return 9
    _ -> do
      putStrLn "Invalid input!"
      getNumber n

repl :: Env -> String -> IO ()
repl env prompt = do
  section <- selectSection tutorial
  case section of
    Section _ cs -> loop env cs True
 where
  settings :: MonadIO m => FilePath -> Settings m
  settings home = setComplete completeEgison $ defaultSettings { historyFile = Just (home </> ".egison_history") }

  loop :: Env -> [Content] -> Bool -> IO ()
  loop env [] _ = do
--    liftIO $ showFinishMessage
    liftIO $ repl env prompt
  loop env (content:contents) b = (do
    if b
      then liftIO $ putStrLn $ show content
      else return ()
    home <- getHomeDirectory
    input <- liftIO $ runInputT (settings home) $ getEgisonExprOrNewLine prompt
    case input of
      Left Nothing -> do
        b <- yesOrNo "Do you want to quit?"
        if b
          then return ()
          else do
            b <- yesOrNo "Do you want to procced next?"
            if b
              then loop env contents True
              else loop env (content:contents) False
      Left (Just "") -> do
        b <- yesOrNo "Do you want to procced next?"
        if b
          then loop env contents True
          else loop env (content:contents) False
      Right (topExpr, _) -> do
        result <- liftIO $ runEgisonTopExpr env topExpr
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
 [Section "Calculate numbers                             (10 minutes)"
   [
    Content "We can do arithmetic operations with '+', '-', '*', '/', 'modulo' and 'power'."
     ["(+ 1 2)", "(- 30 15)", "(* 10 20)", "(/ 20 5)", "(modulo 17 4)", "(power 2 10)"]
     [],
    Content "We can write nested expressions."
     ["(+ (* 10 20) 2)", "(/ (* 10 20) (+ 10 20))"]
     ["Try to calculate '(100 - 1) * (100 + 1)'."],
    Content "We are supporting rational numbers."
     ["(+ 2/3 1/5)", "(/ 42 84)"]
     [],
    Content "We are supporting floats, too."
     ["(+ 10.2 1.3)", "(* 10.2 1.3)"]
     [],
    Content "We can convert a rational number to a float number with 'rtof'."
     ["(rtof 1/5)", "(rtof 1/100)"]
     [],
    Content "We can handle collections of numbers.\nWe construct collections with '{}'."
     ["{}", "{10}", "{1 2 3 4 5}"]
     [],
    Content "We can decompose a collection using the 'car' and 'cdr' function."
     ["(car {1 2 3 4 5})", "(cdr {1 2 3 4 5})", "(car (cdr {1 2 3 4 5}))"]
     ["Try to extract the third element of the collection '{1 2 3 4 5}' with 'car' and 'cdr'."],
    Content "With the 'take' function, we can extract a head part of a collection.'."
     ["(take 0 {1 2 3 4 5})", "(take 3 {1 2 3 4 5})"]
     [],
    Content "We can handle infinite lists.\nFor example, 'nats' and 'primes' are an infinite list that contains all natural numbers and prime numbers respectively.\nTry to extract a head part from them."
     ["(take 10 nats)", "(take 30 nats)", "(take 10 primes)", "(take 30 primes)"]
     ["What is the 100th prime number."],
    Content "We can create a \"partial\" function using '$' as an argument."
     ["((* $ 2) 10)", "((modulo $ 3) 10)"]
     [],
    Content "With the 'map' function, we can operate each element of the collection at once."
     ["(take 100 (map (* $ 2) nats))", "(take 100 (map (modulo $ 3) nats))"]
     [],
    Content "With the 'foldl' function, we can gather together all elements of the collection using an operator you like."
     ["(foldl + 0 {1 2 3 4 5})", "(foldl * 1 {1 2 3 4 5})"]
     ["Try to get the sum of from 1 to 100?"],
    Content "Try to create a sequence of numbers '{1 1/2 1/3 1/4 ... 1/100}'."
     []
     [],
    Content "Try to calculate '1 + 1/2 + 1/3 + 1/4 + ... + 1/100'.\nRemember that we can convert a rational number to a float number with 'rtof'."
     ["(rtof 2/3)"]
     [],
    Content "Try to calculate '1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ... + (1/100)^2'.\nIn fact, '1 + (1/2)^2 + (1/3)^2 + (1/4)^2 + ...' converges to '(/ (power pi 2) 6)'.\nPlease observe that."
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Basics of functional programming              (10 minutes)"
   [
    Content "We can bind a value to a variable with a 'define' expression.\nWe can easily get the value we bound to a variable."
     ["(define $x 10)", "x", "(define $y (+ 1 x))", "y"]
     [],
    Content "We support recursive definitions. It enables us to define an collection with infinite elements."
     ["(define $ones {1 @ones})", "(take 100 ones)", "(define $nats {1 @(map (+ $ 1) nats)})", "(take 100 nats)", "(define $odds {1 @(map (+ $ 2) odds)})", "(take 100 odds)"]
     ["Try to define the infinite list of even numbers that is like {2 4 6 8 10 ...}."],
    Content "We can create a function with a 'lambda' expression. Let's define functions and test them."
     ["(define $increment (lambda [$x] (+ x 1)))", "(increment 10)", "(define $multiply (lambda [$x $y] (* x y)))", "(multiply 10 20)", "(define $sum (lambda [$n] (foldl + 0 (take n nats))))", "(sum 10)"]
     ["Try to define a 'fact' function, which obtains an natural number 'n' and returns 'n * (n - 1) * ... * 2 * 1'."],
    Content "We can compare numbers using functions that return '#t' or '#f'.\n'#t' means the true.\n'#f' means the false.\nFunctions that return '#t' or '#f' are called \"predicates\"."
     ["(eq? 1 1)", "(gt? 1 1)", "(lt? 1 1)",  "(gte? 1 1)", "(lte? 1 1)"]
     [],
    Content "With the 'while' function, we can extract all head elements that satisfy the predicate.\n'primes' is a infinite list that contains all prime numbers."
     ["(while (lt? $ 100) primes)", "(while (lt? $ 1000) primes)"]
     [],
    Content "With the 'filter' function, we can extract all elements that satisfy the predicate."
     ["(take 100 (filter even? nats))", "(take 100 (filter prime? nats))", "(take 100 (filter (lambda [$p] (eq? (modulo p 4) 1)) primes))"]
     ["Try to enumerate the first 100 primes that are congruent to 3 modulo 4."],
    Content "We combine numbers using '[]'.\nThese things are called 'tuples'."
     ["[1 2]", "[1 2 3]"]
     [],
    Content "Note that a tuple that consists of only one element is equal with that element itself."
     ["[1]", "[[[1]]]"]
     [],
    Content "With the 'zip' function, we can combine two lists as follow."
     ["(take 100 (zip nats nats))", "(take 100 (zip primes primes))"]
     ["Try to generate the prime table that is like '{[1 2] [2 3] [3 5] [4 7] [5 11] ...}'"],
    Content "Try to create a fibonacci sequence that is like '{1 1 2 3 5 8 13 21 34 55 ...}'.\n\nHint:\n  Replace '???' in the following expression to a proper function.\n  (define $fibs {1 1 @(map ??? (zip fibs (cdr fibs)))})"
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Basics of pattern-matching                    (10 minutes)"
   [
    Content "We can pattern-match against a collection.\nThe 'join' pattern divides a collection into two collections.\nPlease note that the 'match-all' expression enumerates all results of pattern-matching."
     ["(match-all {1 2 3}     (list integer) [<join $hs $ts> [hs ts]])",
      "(match-all {1 2 3 4 5} (list integer) [<join $hs $ts> [hs ts]])"]
     [],
    Content "Try another pattern-constructor 'cons'.\nThe 'cons' pattern divides a collection into the head element and the rest collection.\n"
     ["(match-all {1 2 3}     (list integer) [<cons $x $xs> [x xs]])",
      "(match-all {1 2 3 4 5} (list integer) [<cons $x $xs> [x xs]])"]
     [],
    Content "'_' is a wildcard and matches with any objects."
     ["(match-all {1 2 3}     (list integer) [<cons $x  _>  x])",
      "(match-all {1 2 3 4 5} (list integer) [<join $hs _> hs])"]
     [],
    Content "We can write non-linear patterns.\nIn the following case, patterns that begins with ',' matches the object only if the object is equal with the expression after ','.\nPlease try the following expression."
     ["(match-all {1 1 2 2 3 2} (list integer) [<join _ <cons $x <cons ,x _>>> x])",
      "(match-all {1 2 2 3 3 2} (list integer) [<join _ <cons $x <cons ,x _>>> x])"]
     [],
    Content "We can express various things using 'cons' and 'join'.\nThe most of functions in the collection library of Egison are written using pattern-matching!"
     ["(match-all {1 1 2 3 2} (list integer) [<join _ <cons $x <join _ <cons $y _>>>> [x y]])",
      "(match-all {1 1 2 3 2} (list integer) [<join _ <cons $x <join _ <cons ,x _>>>> [x x]])"]
     ["Try to enumerate three combinations of numbers."],
    Content "We can pattern-match against infinite collections.\nWe can enumerate twin primes using pattern-matching as follow.\nNote that we can write any expression after ','."
     ["(take 10 (match-all primes (list integer) [<join _ <cons $p <cons ,(+ p 2) _>>> [p (+ p 2)]]))"]
     ["What is the 100th twin prime?"],
    Content "Try to enumerate the first 10 prime pairs whose form is (p, p+6) like '{{[5 11] [7 13] [11 17] [13 19] [17 23] ...}'."
     []
     [],
    Content "This is the end of this section.\nPlease play freely or proceed to the next section.\nThank you for enjoying our tutorial!"
     []
     []
    ],
  Section "Pattern-matching against various data types   (10 minutes)"
   [
    Content "We can pattern-match even against multisets and sets.\nWe can change the way of pattern-matching by changing the \"matcher\".\nPlease try the following expressions."
     ["(match-all {1 2 3} (list integer)     [<cons $x $xs> [x xs]])",
      "(match-all {1 2 3} (multiset integer) [<cons $x $xs> [x xs]])",
      "(match-all {1 2 3} (set integer)      [<cons $x $xs> [x xs]])"]
     [],
    Content "Try another pattern-constructor 'join'.\nThe 'join' pattern divides a collection into two collections."
     ["(match-all {1 2 3 4 5} (list integer)     [<join $xs $ys> [xs ys]])",
      "(match-all {1 2 3 4 5} (multiset integer) [<join $xs $ys> [xs ys]])",
      "(match-all {1 2 3 4 5} (set integer)      [<join $xs $ys> [xs ys]])"]
     [],
    Content "We can write non-linear patterns.\nTry the following expression."
     ["(match-all {1 1 2 3 2} (multiset integer) [<cons $x <cons ,x       _>> x])",
      "(match-all {1 1 2 3 2} (multiset integer) [<cons $x <cons ,(+ x 2) _>> x])"]
     [],
    Content "A pattern that has '^' ahead of which is called a not-pattern.\nA not-pattern matches when the target does not match against the pattern."
     ["(match-all {1 2 1 3 2} (multiset integer) [<cons $x  <cons ,x _>> x])",
      "(match-all {1 2 1 3 2} (multiset integer) [<cons $x ^<cons ,x _>> x])"]
     [],
    Content "We can pattern-match against infinite collections with infinite results.\nNote that Egison really enumerates all pairs of two natural numbers in the following example."
     ["(take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))"]
     [],
    Content "This is the end of our tutorial.\nThank you for enjoying our tutorial!\nPlease check our paper, manual and code for further reference!"
     []
     []
    ]
  ]
--    Content "We support \"and-patterns\" and \"or-patterns\".\nWe can enumerate prime triplets using them as follow.\n\"And-patterns\" and \"or-patterns\" are represented using '&' and '|' respectively."
--     ["(take 10 (match-all primes (list integer) [<join _ <cons $p <cons (& $m (| ,(+ p 2) ,(+ p 4))) <cons ,(+ p 6) _>>>> [p m (+ p 6)]]))"]
--     ["What is the 20th prime triplet?"],
--    Content "Try to enumerate the first 8 prime quadruplets whose form is (p, p+2, p+6, p+8) like '{{[5 7 11 13] [11 13 17 19] ...}'."
--     []
--     [],
--  Section "Define your own functions"
--   [
--    Content "Did we think how about \"n\" combinations of the elements of the collection?\nWe already have a solution.\nWe can write a pattern that include '...' as the following demonstrations."
--     ["(match-all {1 2 3 4 5} (list integer) [(loop $i [1 3] <join _ <cons $a_i ...>> _) a])", "(match-all {1 2 3 4 5} (list integer) [(loop $i [1 4] <join _ <cons $a_i ...>> _) a])"]
--     [],
--    Content "Let's try 'if' expressions."
--     ["(if #t 1 2)", "(if #f 1 2)", "(let {[$x 10]} (if (eq? x 10) 1 2))"]
--     [],
--    Content "Using 'define' and 'if', we can write recursive functions as follow."
--     ["(define $your-take (lambda [$n $xs] (if (eq? n 0) {} {(car xs) @(your-take (- n 1) (cdr xs))})))", "(your-take 10 nats)"]
--     ["Try to write a 'your-while' function."],
--    Content "Try to write a 'your-map' function.\nWe may need 'empty?' function inside 'your-map' function."
--     ["(empty? {})", "(empty? {1 2 3})"]
--     []
--  Section "Writing scripts in Egison"
--   [
--    Content "Let's write a famous Hello world program in Egison.\nTry the following expression.\nIt is evaluated to the 'io-function'.\nTo execute an io-function, we use 'io' primitive as follow."
--     ["(io (print \"Hello, world!\"))"]
--     [],
--    Content "We can execute multiple io-functions in sequence as follow.\nThe io-functions is executed from the head."
--     ["(io (do {[(print \"a\")] [(print \"b\")] [(print \"c\")]} []))", "(io (do {[(write-string \"Type your name: \")] [(flush)] [$name (read-line)] [(print {@\"Hello, \" @name @\"!\"})]} []))"]
--     [],
--    Content "The following is a hello world program in Egison.\nTry to create a file with the following content and save it as \"hello.egi\", and execute it in the terminal as '% egison hello.egi'\n"
--     ["(define $main (lambda [$args] (print \"Hello, world!\")))"]
--     [],
--    Content "That's all. Thank you for finishing our tutorail! Did you enjoy it?\nIf you got into Egison programming. I'd like you to try Rosseta Code.\nThere are a lot of interesting problems.\n\n  http://rosettacode.org/wiki/Category:Egison"
--     []
--     []
--    ]
--  ]
