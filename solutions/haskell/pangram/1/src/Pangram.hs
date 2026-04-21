module Pangram (isPangram) where


import Data.List (nub)

import Data.Char (toLower, isAsciiLower, isAsciiUpper)

isPangram :: String -> Bool
isPangram text =
  let letters = [ toLower c | c <- text, isAsciiLower c || isAsciiUpper c ]
  in all (`elem` letters) ['a'..'z']

