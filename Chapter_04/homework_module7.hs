import Data.Char

-- ord:
-- 'A' = 65
-- 'Z' = 90
-- 'a' = 97
-- 'z' = 122

-- Needs to return numbers 0..25 for a-z. We need a version also does 0..25 for A-Z
let2int :: Char -> Int
let2int c = ord c - ord cc where cc = if isLower c then 'a' else 'A'

-- Returns a-x for 0..25. 
int2letL :: Int -> Char
int2letL n = chr (ord 'a' + n)

-- Returns a-x for 0..25. 
int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c 
	| isLower c = int2letL ((let2int c + n) `mod` 26)
	| isUpper c = int2letU ((let2int c + n) `mod` 26)
	| otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]