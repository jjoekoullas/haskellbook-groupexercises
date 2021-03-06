module Main where

--1. a, weekday is a type with 5 data constructors
--2. c, Weekday -> String
--3. b, must begin with a capital letter
--4. c, delivers the final element of xs
--   it also produces an error for an empty list

--Vigenere Cipher
--  Note: I don't have my original Cipher work saved anywhere,
--so I decided to more or less wing this and just assume I understood
--the Ceaser cipher exercises.  Basically I need to be able to encode
--and decode.
import Data.Char (chr, isLetter, isSpace, isUpper, toLower, toUpper, ord)
import Data.List (elemIndex, maximumBy, sort, sortBy, group)

keywordOffsets :: String -> [Int]
keywordOffsets = map (\x -> ord x - 65)

vigenereShift :: [Int] -> Char -> Int -> Char
vigenereShift _ ' ' _ = ' '
vigenereShift keyOffsets c offset =
  let boundToAlphabet i = ((i - 65) `mod` 26) + 65
  in chr $ boundToAlphabet $ (ord c) + (keyOffsets !! offset)

--Trying out a design thought..
--By putting these in a datatype I can say I definitely need both, together
--more thoughts in decode, where this comes into play more
data VignereEncoded =
  VignereEncoded { key     :: String
                 , encoded :: String }
                 deriving (Show)

encode :: String -> String -> VignereEncoded
encode key toEncode =
  let offsets = keywordOffsets key
      shift = vigenereShift offsets
      boundToOffsets i = mod i $ length offsets
      encoder c i []  = [shift c i]
      encoder ' ' i s = ' ' : encoder (head s) (boundToOffsets i) (tail s)
      encoder c i s   = (shift c i) : encoder (head s) (boundToOffsets (i + 1)) (tail s)
  in VignereEncoded { key = key, encoded = encoder (head toEncode) 0 (tail toEncode) }

--I don't think I like introducing the VignereEncoded type because of what
--that means for using this function. I can no longer use partial application
--to have a decoder ready for a specific key.  Instead I'd need some mechanism to construct
--a VignereEncoded.
decode :: VignereEncoded -> String
decode v =
  let reverseOffsets = map negate $ keywordOffsets $ key v
      shift = vigenereShift reverseOffsets
      boundToOffsets i = mod i $ length reverseOffsets
      decoder c   i [] = [shift c i]
      decoder ' ' i s  = ' '         : decoder (head s) (boundToOffsets   i)     (tail s)
      decoder c   i s  = (shift c i) : decoder (head s) (boundToOffsets $ i + 1) (tail s)
      toDecode = encoded v
  in decoder (head toDecode) 0 (tail toDecode)

--As-patterns
--1.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _  = True
isSubsequenceOf _  [] = False
--I think using elem counts as "cheating" here
--but I also can't think of a way to use as-patterns here
isSubsequenceOf (a:as) sequence = elem a sequence && isSubsequenceOf as sequence

--2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = (\word@(w:ord) -> (word, (toUpper w) : ord)) `map` (words s)


--Language exercises
--1.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (w:ord) = toUpper w : ord
main :: IO ()
main = do
  putStrLn "hello world"

--2.
--Lordy this isn't efficient!  A parser would do this a lot better, likely more clearly
--to boot. :)
capitalizeParagraph :: String -> String
capitalizeParagraph p =
  let sentences = lines $ map (\x -> if x == '.' then '\n' else x) p
      capSentence s =
        let words' = map (dropWhile isSpace) (words s)
        in (capitalizeWord $ head words') : (tail words')
      replaceSpaces x = init $ foldl (\b a -> b ++ a ++ " ") [] x
      cappedSentences = map replaceSpaces $ map capSentence sentences
  in init $ foldl (\b a -> b ++ a ++ ". ") "" cappedSentences

--Phone exercise
data DaPhone = DaPhone {
                 keys    :: [(Char, String)]
                ,mkUpper :: Char }

myPhone :: DaPhone
myPhone = DaPhone {
   keys = [('1', "1"),     ('2',"abc2"), ('3', "def3")
          ,('4', "ghi4"),  ('5',"jkl5"), ('6', "mno6")
          ,('7', "pqrs7"), ('8',"tuv8"), ('9', "wxyz9")
          ,('0', " "),     ('#', ".,")]
  ,mkUpper = '*' }


convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just amking sure rofl ur turn"]

type Digit = Char
type Presses = Int

--2.
reverseTaps :: DaPhone -> Char -> [(Digit,Presses)]
reverseTaps phone c =
  let key = filter (\x -> elem (toLower c) $ snd x) $ keys phone
      numberTaps = if null key
                   then 0
                   else case (elemIndex (toLower c) (snd $ head key)) of
                          Nothing -> 0
                          Just x  -> x + 1
  in if (null key)
     then []
     else
       (if isUpper c
       then [(mkUpper phone, 1)]
       else []) ++ [(fst $ head key, numberTaps)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone s = concat $ map (reverseTaps phone) s

convoPresses :: [[(Digit,Presses)]]
convoPresses = map (cellPhonesDead myPhone) convo

--3.
fingerTaps :: [(Digit,Presses)] -> Presses
fingerTaps taps = sum $ map snd taps

--4.
frequencies :: Ord a => [a] -> [(Int, a)]
frequencies as = map (\x -> (length x, head x)) (group $ sort as)

mostPopularLetter :: String -> Char
mostPopularLetter s =
  let letterFrequencies = frequencies $ filter isLetter s
  in snd $ head $ sortBy (flip compare) (letterFrequencies)

mostPopularLetterPerMessage :: [Digit]
mostPopularLetterPerMessage = map mostPopularLetter convo

cost :: Digit -> Presses
cost c = fingerTaps $ reverseTaps myPhone c

mostPopularWithCost :: [(Digit, Presses)]
mostPopularWithCost = map (\x -> let mpl = mostPopularLetter x in (mpl, cost mpl)) convo

coolestLtr :: [String] -> Char
coolestLtr s = snd $ head $ sortBy (flip compare) (frequencies $ filter isLetter (concat s))

coolestWord :: [String] -> String
coolestWord s = snd $ head $ sortBy (flip compare) (frequencies $ words $ concat s)

--Hutton's Razor
--1.
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)    = x
eval (Add e e') = (eval e) + (eval e')

--2.
printExpr :: Expr -> String
printExpr (Lit x)    = show x
printExpr (Add e e') = printExpr e ++ " + " ++ printExpr e'