import Data.List (reverse, concatMap)

toDigits :: (Integral a) => a -> [a]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: (Integral a) => a -> [a]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: (Num a) => [a] -> [a]
doubleEveryOther n =
	let pattern = cycle [1, 2]
	in reverse (zipWith (*) pattern (reverse n))

sumDigits :: (Integral a) => [a] -> a
sumDigits n = sum (concatMap toDigits n)

type CreditCardNumber = Integer
data Validity = Invalid | Valid

validate :: CreditCardNumber -> Validity
validate n
	| check == 0 = Valid
	| check /= 0 = Invalid
	where check = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10

result :: Validity -> IO ()
result Valid   = putStrLn "Hooray! Your number is valid!"
result Invalid = putStrLn "Sorry, your number is invalid."

main :: IO ()
main = do
	putStrLn "Input a credit card number:"
	number_str <- getLine
	let number = read number_str :: Integer
	result (validate number)
