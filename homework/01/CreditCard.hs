main = undefined

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = (x `mod` 10) : toDigitsRev ((x - (x `mod` 10)) `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleSeconds (reverse x) False

doubleSeconds :: [Integer] -> Bool -> [Integer]
doubleSeconds [] _         = []
doubleSeconds (x:xs) True  = (x * 2) : doubleSeconds xs False
doubleSeconds (x:xs) False = x : doubleSeconds xs True

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = sumDigits (toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate 0 = False
validate x
 | remainder == 0  = True
 | otherwise       = False
    where sum       = sumDigits $ doubleEveryOther $ toDigits x
          remainder = sum `mod` 10
