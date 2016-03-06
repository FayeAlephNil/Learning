module H1.Validate (validate) where

-- Taken from digits library as I'd already seen that source code
-- Not using libraries because that would be cheating, but I knew this impl
digitsRev :: Int -> [Int]
digitsRev 0 = []
digitsRev i = lastDigit : digitsRev rest
  where
    (rest, lastDigit) = quotRem i 10

digits :: Int -> [Int]
digits = reverse . digitsRev

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = [if i `mod` 2 == 0 then x * 2 else x | (x, i) <- zip xs [0 ..]]

digitSum :: Int -> Int
digitSum = sum . digitsRev

getSum :: Int -> Int
getSum n = sum $ map digitSum $ doubleEveryOther $ digits n

validate :: Int -> Bool
validate n = (getSum n) `mod` 10 == 0
