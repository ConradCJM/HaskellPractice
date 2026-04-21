module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, toUpper)

responseFor :: String -> String
responseFor xs
  | isSilence xs        = "Fine. Be that way!"
  | isYell xs && isQ xs = "Calm down, I know what I'm doing!"
  | isYell xs           = "Whoa, chill out!"
  | isQ xs              = "Sure."
  | otherwise           = "Whatever."

-- Helper: silence = empty or only whitespace
isSilence :: String -> Bool
isSilence s = all isSpace s

-- Helper: yelling = has letters AND all letters are uppercase
isYell :: String -> Bool
isYell s =
  let letters = filter isAlpha s
  in not (null letters) && all (\c -> c == toUpper c) letters

-- Helper: question = ends with '?', ignoring trailing spaces
isQ :: String -> Bool
isQ s =
  case dropWhile isSpace (reverse s) of
    ('?':_) -> True
    _       -> False
