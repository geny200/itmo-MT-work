module Sheol.Utils
  ( -- * Functions
    genericJoin,
    replace,
    join,
  )
where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

join :: [a] -> [[a]] -> [a]
join = intercalate

genericJoin :: Show a => String -> [a] -> String
genericJoin delimit l = join delimit (map show l)

--replace :: Eq a => [a] -> [a] -> [a] -> [a]
--replace old new = join new . splitOn old

replace :: String -> (String -> String) -> String -> String
replace regExp repl str
  = firstMach [] (str =~ regExp :: (String, String, String))
  where
    firstMach :: String -> (String, String, String) -> String
    firstMach acc (x, [], _) = acc ++ x
    firstMach acc (x, y, xs) = firstMach (acc ++ x ++ repl y) (xs =~ regExp)
