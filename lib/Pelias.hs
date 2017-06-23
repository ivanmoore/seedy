module Pelias where

import Data.Char
import Data.String.Utils

-- (E) for the letter 'E'. (L)owercase. (P)lus. (M)inus.
data Exponent = E | EP | EM | LE | LEP | LEM
  deriving (Show, Eq)

-- True. False. Null
data Constant = T | F | N
  deriving (Show, Eq)

data Token = Digit String | Minus | Dot | Exp Exponent | KeyChar String | ValueChar String | Quote | 
             LSquare | RSquare | Comma | LCurly | RCurly | Colon | Const Constant | 
             Key String | StringValue String | Number String | Pair (String, [Token])
  deriving (Show, Eq)

-- Used for the tokenise function
data GrammarPart = JDigits | JInt | JSimpleNumber | JExp | JNumber | JKeyString | JValueString |
                   JArray | JElements | JObject | JMembers | JPair | JBool | JNull | JValue
  deriving (Show, Eq)

-- Part of the API. None of the types above this are.
data Value = SValue String | NValue String | BValue Bool | NullValue |
             OValue [(String, Value)] | AValue [Value]
  deriving (Show, Eq)

data JSONOperation = Index Int | Get String
  deriving (Show, Eq)

type Optimiser = ([JSONOperation] -> String -> (String, [JSONOperation]))

defaultOptimiser :: Optimiser
defaultOptimiser ops json = (json, ops)

extract :: [JSONOperation] -> String -> Maybe Value
extract = optimisedExtract defaultOptimiser

optimisedExtract :: Optimiser -> [JSONOperation] -> String -> Maybe Value
optimisedExtract optimiser ops json = (extractValue remainingOps) $ parse result
  where
    (result, remainingOps) = prepareInput optimiser ops json

removeNewlinesAndTabs :: String -> String
removeNewlinesAndTabs = concat . (map strip) . lines

prepareInput :: Optimiser -> [JSONOperation] -> String -> (String, [JSONOperation])
prepareInput optimiser ops = (optimiser ops) . removeNewlinesAndTabs

countStrBalance :: String -> Int
countStrBalance = countStrBalance' 1

countStrBalance' :: Int -> String -> Int
countStrBalance' _ ""                   = 0
countStrBalance' x ('\"' : rest)        = countStrBalance' (1 - x) rest
countStrBalance' 0 (_    : rest)        = countStrBalance' 0 rest
countStrBalance' x ('\\' : '\"' : rest) = countStrBalance' x rest
countStrBalance' x ('{'  : rest)        = x    + countStrBalance' x rest
countStrBalance' x ('}'  : rest)        = (-x) + countStrBalance' x rest
countStrBalance' x (_    : rest)        = countStrBalance' x rest

extractValue :: [JSONOperation] -> Value -> Maybe Value
extractValue []         json = Just json
extractValue (op : ops) json =
  case applyOperation op json of
    Just v  -> extractValue ops v
    Nothing -> Nothing

applyOperation :: JSONOperation -> Value -> Maybe Value
applyOperation (Index i)   (AValue values) = if i < length values then Just (values !! i)  else Nothing
applyOperation (Get field) (OValue pairs)  = lookup field pairs
applyOperation _ _                         = Nothing

parse :: String -> Value
parse ('{' : json) = (evaluate . (tokens JObject)) ('{' : json)
parse ('[' : json) = (evaluate . (tokens JArray))  ('[' : json)

evaluate :: [Token] -> Value
evaluate [StringValue s]    = SValue s
evaluate [Number n]         = NValue n
evaluate [Const T]          = BValue True
evaluate [Const F]          = BValue False
evaluate [Const N]          = NullValue

-- THIS IS A HORRIBLE BUGFIX --
evaluate [Comma]            = SValue ""
-- Because "reduce" strips quotes and colons - assignments to
--   empty strings like "abc" in this example become:
--                before "123" >> abc , << after "456"
-- And so during pairing it becomes:
--                ("abc", [Comma])
-- Which leads us here.

evaluate (LCurly  : tokens) = OValue (evaluatePairs tokens)
evaluate (LSquare : tokens) = AValue (evaluateArrayContents (init tokens))

evaluatePairs :: [Token] -> [(String, Value)]
evaluatePairs []                         = []
evaluatePairs (LCurly : tokens)          = evaluatePairs tokens
evaluatePairs (RCurly : tokens)          = evaluatePairs tokens
evaluatePairs (Comma  : tokens)          = evaluatePairs tokens
evaluatePairs ((Pair (k, [t])) : tokens) = (k, evaluate [t]) : evaluatePairs tokens
evaluatePairs ((Pair (k, ts))  : tokens) = (k, evaluate ts)  : evaluatePairs tokens

evaluateArrayContents :: [Token] -> [Value]
evaluateArrayContents []                       = []
evaluateArrayContents (Comma         : tokens) = evaluateArrayContents tokens
evaluateArrayContents (StringValue s : tokens) = (SValue s)     : evaluateArrayContents tokens
evaluateArrayContents (Number n      : tokens) = (NValue n)     : evaluateArrayContents tokens
evaluateArrayContents (Const T       : tokens) = (BValue True)  : evaluateArrayContents tokens
evaluateArrayContents (Const F       : tokens) = (BValue False) : evaluateArrayContents tokens
evaluateArrayContents (Const N       : tokens) = NullValue      : evaluateArrayContents tokens
evaluateArrayContents (LCurly        : tokens) =
  case dropObject tokens of
    []   -> [OValue (evaluatePairs $ init tokens)]
    rest -> (OValue $ evaluatePairs $ takeObject tokens) : (evaluateArrayContents rest)
evaluateArrayContents (LSquare       : tokens) = 
  case dropArray tokens of
    []    -> [AValue (evaluateArrayContents $ init tokens)]
    rest  -> (evaluateArrayContents $ takeArray tokens) ++ (evaluateArrayContents rest)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

tokens :: GrammarPart -> String -> [Token]
tokens _        ""   = []
tokens jsonPart json = ((converge reduce) . (snd . (tokenise jsonPart))) json

reduce :: [Token] -> [Token]
reduce []                          = []
reduce (Quote  : tokens)           = reduce tokens
reduce (LCurly : tokens)           = LCurly : reduce tokens
reduce (RCurly : tokens)           = RCurly : reduce tokens
reduce (Colon  : tokens)           = reduce tokens
reduce (Comma  : tokens)           = Comma : reduce tokens
reduce [KeyChar x]                 = [Key x]
reduce [ValueChar x]               = [StringValue x]
reduce ((KeyChar k)   : tokens)    = (reduceString Key         k  (takeChars tokens)) : reduce (dropChars tokens)
reduce ((ValueChar v) : tokens)    = (reduceString StringValue v  (takeChars tokens)) : reduce (dropChars tokens)
reduce ((Digit d)     : tokens)    = (reduceNumber             d  (takeNumber tokens)) : reduce (dropNumber tokens)
reduce (Minus : tokens)            = (reduceNumber            "-" (takeNumber tokens)) : reduce (dropNumber tokens)
reduce (Key k : LCurly  : tokens)  = (Pair (k, reduce (takeObject tokens))) : reduce (dropObject tokens)
reduce (Key k : LSquare : tokens)  = (Pair (k, reduce (takeArray  tokens))) : reduce (dropArray tokens)
reduce (Key k : v : tokens)        = (Pair (k, [v])) : reduce tokens
reduce (token : tokens)            = token : reduce tokens

takeParenthesis :: Token -> Token -> [Token] -> [Token]
takeParenthesis lb rb = takeParenthesis' 0 lb rb [] 

takeParenthesis' :: Int -> Token -> Token -> [Token] -> [Token] -> [Token]
takeParenthesis' x lb rb acc []           = []
takeParenthesis' x lb rb acc (t : tokens) =
  if x == 0 && t == rb then [lb] ++ acc ++ [rb] else
  if t == rb then takeParenthesis' (x - 1) lb rb (acc ++ [rb]) tokens else
  if t == lb then takeParenthesis' (x + 1) lb rb (acc ++ [lb]) tokens else
  takeParenthesis' x lb rb (acc ++ [t]) tokens

dropParenthesis :: Token -> Token -> [Token] -> [Token]
dropParenthesis lb rb = dropParenthesis' 0 lb rb

dropParenthesis' :: Int -> Token -> Token -> [Token] -> [Token]
dropParenthesis' x lb rb []           = []
dropParenthesis' x lb rb (t : tokens) =
  if x == 0 && t == rb then tokens else
  if t == rb then dropParenthesis' (x - 1) lb rb tokens else
  if t == lb then dropParenthesis' (x + 1) lb rb tokens else
  dropParenthesis' x lb rb tokens

-- Don't give it the LSquare
takeArray :: [Token] -> [Token]
takeArray = takeParenthesis LSquare RSquare

-- Don't give it the LSquare
dropArray :: [Token] -> [Token]
dropArray = dropParenthesis LSquare RSquare

-- Don't give it the LCurly
takeObject :: [Token] -> [Token]
takeObject = takeParenthesis LCurly RCurly

-- Don't give it the LCurly
dropObject :: [Token] -> [Token]
dropObject = dropParenthesis LCurly RCurly

reduceString :: (String -> Token) -> String -> [Token] -> Token
reduceString tokenType initialAcc subsequentTokens = tokenType (foldl accumulateString initialAcc subsequentTokens)

tokenValue :: Token -> String
tokenValue (KeyChar c)   = c
tokenValue (ValueChar c) = c
tokenValue _             = ""

accumulateString :: String -> Token -> String
accumulateString acc token = if isChar token then acc ++ tokenValue token else acc

takeChars :: [Token] -> [Token]
takeChars = takeWhile isChar

dropChars :: [Token] -> [Token]
dropChars = dropWhile isChar

isChar :: Token -> Bool
isChar (KeyChar _)   = True
isChar (ValueChar _) = True
isChar _             = False

reduceNumber :: String -> [Token] -> Token
reduceNumber initialAcc subsequentTokens = Number (foldl accumulateNumber initialAcc subsequentTokens)

accumulateNumber :: String -> Token -> String
accumulateNumber acc (Digit d) = acc ++ d
accumulateNumber acc Dot       = acc ++ "."
accumulateNumber acc Minus     = acc ++ "-"
accumulateNumber acc (Exp LE)  = acc ++ "e"
accumulateNumber acc (Exp LEP) = acc ++ "e+"
accumulateNumber acc (Exp LEM) = acc ++ "e-"
accumulateNumber acc (Exp E)   = acc ++ "E"
accumulateNumber acc (Exp EP)  = acc ++ "E+"
accumulateNumber acc (Exp EM)  = acc ++ "E-"
accumulateNumber acc _         = acc

takeNumber :: [Token] -> [Token]
takeNumber = takeWhile isNumeric

dropNumber :: [Token] -> [Token]
dropNumber = dropWhile isNumeric

isNumeric :: Token -> Bool
isNumeric (Digit _) = True
isNumeric (Exp _)   = True
isNumeric Dot       = True
isNumeric Minus     = True
isNumeric _         = False

-- The "tokenise" functions take the String, tokenise what they are meant to
--   and return the remainder of the input and the tokens they gleaned.

-- This is so I don't have to litter my code with strip. Although as I have now
--   learnt, I have had to include some anyway.
tokenise :: GrammarPart -> String -> (String, [Token])
tokenise JDigits       = tokeniseDigits . strip
tokenise JInt          = tokeniseInt . strip
tokenise JSimpleNumber = tokeniseSimpleNumber . strip
tokenise JExp          = tokeniseExp . strip
tokenise JNumber       = tokeniseNumber . strip
tokenise JKeyString    = (tokeniseString KeyChar) . strip
tokenise JValueString  = (tokeniseString ValueChar) . strip
tokenise JArray        = tokeniseArray . strip
tokenise JElements     = tokeniseElements . strip
tokenise JObject       = tokeniseObject . strip
tokenise JMembers      = tokeniseMembers . strip
tokenise JPair         = tokenisePair . strip
tokenise JBool         = tokeniseBool . strip
tokenise JNull         = tokeniseNull . strip
tokenise JValue        = tokeniseValue . strip

tokeniseDigits :: String -> (String, [Token])
tokeniseDigits input = (drop nDigits input, map (Digit . (: [])) (take nDigits input))
  where nDigits = length (takeWhile isDigit input)

-- Minus? Digit+
tokeniseInt :: String -> (String, [Token])
tokeniseInt ('-' : input) = (noLeadingDigits, Minus : digits)
  where (noLeadingDigits, digits) = tokenise JDigits input
tokeniseInt input = tokenise JDigits input

-- Int (Dot Digit+)?
tokeniseSimpleNumber :: String -> (String, [Token])
tokeniseSimpleNumber input = 
  case noLeadingInt of
    ('.' : digits) -> (noLeadingDecimal, leadingInt ++ [Dot] ++ decimalPart)
    _              -> (noLeadingInt, leadingInt)
  where
    (noLeadingInt, leadingInt)      = tokenise JInt input
    (noLeadingDecimal, decimalPart) = tokenise JDigits (drop 1 noLeadingInt)

tokeniseExp :: String -> (String, [Token])
tokeniseExp ('E' : '+' : rest) = (rest, [Exp EP])
tokeniseExp ('E' : '-' : rest) = (rest, [Exp EM])
tokeniseExp ('E'       : rest) = (rest, [Exp E])
tokeniseExp ('e' : '+' : rest) = (rest, [Exp LEP])
tokeniseExp ('e' : '-' : rest) = (rest, [Exp LEM])
tokeniseExp ('e'       : rest) = (rest, [Exp LE])

-- SimpleNum (Exp Digit+)?
tokeniseNumber :: String -> (String, [Token])
tokeniseNumber input =
  if check JExp noLeadingSimpleNum
  then (noNumber, leadingSimpleNum ++ exp ++ expDigits)
  else (noLeadingSimpleNum, leadingSimpleNum)
  where
    (noLeadingSimpleNum, leadingSimpleNum) = tokenise JSimpleNumber input
    (noExp, exp)                           = tokenise JExp noLeadingSimpleNum
    (noNumber, expDigits)                  = tokenise JDigits noExp

tokeniseString :: (String -> Token) -> String -> (String, [Token])
tokeniseString _ ('\"' : '\"' : rest) = (rest, Quote : [Quote])
tokeniseString t ('\"' : rest)        = tokeniseChars' t [] (strip rest)
tokeniseString t something            = error ("Confused by " ++ something ++ "\n")
tokeniseChars' :: (String -> Token) -> [Token] -> String -> (String, [Token])
tokeniseChars' _ acc ('\"' : rest)       = (rest, [Quote] ++ acc ++ [Quote])
tokeniseChars' t acc ('\\' : 'u' : rest) =
  if all isHexDigit (take 4 rest)
  then tokeniseChars' t (acc ++ [t ("\\u" ++ (take 4 rest))]) (drop 4 rest) 
  else error "String with bad usage of \\u - expected 4 hex digits - tokenising failed"
tokeniseChars' t acc ('\\' : c : rest)   =
  if elem c "\"\\/bfnrt"
  then tokeniseChars' t (acc ++ [t ("\\" ++ [c])]) rest
  else error "String with bad backslash usage - tokenising failed"
tokeniseChars' t acc (c : rest)          = tokeniseChars' t (acc ++ [t [c]]) rest
tokeniseChars' _ acc []                  = ([], acc)

tokeniseArray :: String -> (String, [Token])
tokeniseArray ('[' : ']' : rest) = (rest, LSquare : [RSquare])
tokeniseArray input = ((drop 1 rest), [LSquare] ++ tokenised ++ [RSquare])
  where (rest, tokenised) = tokenise JElements (drop 1 input)

tokeniseElements :: String -> (String, [Token])
tokeniseElements input =
  case strip noValue of
    (',' : rest) -> (noElements, value ++ [Comma] ++ elements)
    _            -> (noValue, value)
  where 
    (noValue, value)       = tokenise JValue input
    (noElements, elements) = tokenise JElements (drop 1 (strip noValue))

tokeniseObject :: String -> (String, [Token])
tokeniseObject ('{' : '}' : rest) = (rest, LCurly : [RCurly])
tokeniseObject input = ((drop 1 (strip rest)), [LCurly] ++ tokenised ++ [RCurly])
  where (rest, tokenised) = tokenise JMembers (drop 1 input)

tokeniseMembers :: String -> (String, [Token])
tokeniseMembers input =
  case noPair of
    (',' : rest) -> (noMembers, pair ++ [Comma] ++ members)
    _            -> (noPair, pair)
  where 
    (noPair, pair)       = tokenise JPair input
    (noMembers, members) = tokenise JMembers (drop 1 noPair)

tokenisePair :: String -> (String, [Token])
tokenisePair input =
  if checkPairWithoutKey noKey
  then (noPair, key ++ [Colon] ++ value)
  else error "Bad pair - tokenising failed"
  where
    (noKey, key)    = tokenise JKeyString input
    (noPair, value) = tokenise JValue (drop 1 (strip noKey))

tokeniseBool :: String -> (String, [Token])
tokeniseBool input
  | take 4 input == "true"  = (drop 4 input, [Const T])
  | take 5 input == "false" = (drop 5 input, [Const F])
  | otherwise               = error "Failed to tokenise bool - not true or false"

tokeniseNull :: String -> (String, [Token])
tokeniseNull input
  | take 4 input == "null" = (drop 4 input, [Const N])
  | otherwise              = error "Failed to tokenise null"

tokeniseValue :: String -> (String, [Token])
tokeniseValue input
  | check JDigits input      = tokenise JNumber input
  | check JValueString input = tokenise JValueString input
  | check JBool   input      = tokenise JBool   input
  | check JNull   input      = tokenise JNull   input
  | check JArray  input      = tokenise JArray  input
  | check JObject input      = tokenise JObject input
  | otherwise                = error ("Tokenising failed, couldn't understand " ++ (take 30 input) ++ "...")

-- The "check" functions return true if it is able to tokenise the next tokens
--   according to the specified member of the grammar.

check :: GrammarPart -> String -> Bool
check JDigits      = checkDigits . strip
check JExp         = checkExp . strip
check JValueString = checkString . strip
check JBool        = checkBool . strip
check JNull        = checkNull . strip
check JArray       = checkArray . strip
check JObject      = checkObject . strip

checkDigits :: String -> Bool
checkDigits []              = False
checkDigits (firstChar : _) = isDigit firstChar || '-' == firstChar

checkExp :: String -> Bool
checkExp ('e':'+':rest) = checkDigits rest
checkExp ('e':'-':rest) = checkDigits rest
checkExp ('e'    :rest) = checkDigits rest
checkExp ('E':'+':rest) = checkDigits rest
checkExp ('E':'-':rest) = checkDigits rest
checkExp ('E'    :rest) = checkDigits rest
checkExp _              = False

checkString :: String -> Bool
checkString = (== '\"') . head

checkBool :: String -> Bool
checkBool input
  | take 4 input == "true"  = True
  | take 5 input == "false" = True
  | otherwise               = False

checkNull :: String -> Bool
checkNull = (== "null") . (take 4)

checkArray :: String -> Bool
checkArray = (== '[') . head

checkObject :: String -> Bool
checkObject = (== '{') . head

checkPairWithoutKey :: String -> Bool
checkPairWithoutKey = (== ':') . head . strip
