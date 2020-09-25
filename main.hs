module Main where

-- Function for the J. Stein Algorithm for GCD
-- gcdNum :: Integral a => a -> a -> a -> a
gcdNum :: Integral a => a -> a -> a -> a
gcdNum a b c
  | a == b = (a * c) --stop and assign result, if both are equal
  | (a `rem` 2 == 0 && b `rem` 2 == 0) = gcdNum (a `div` 2) (b `div` 2) (2 * c) --if both are even
  | (a `rem` 2 /= 0 && b `rem` 2 /= 0) = gcdNum (abs (a - b)) (minimum [a, b]) c --if both are odd
  | (a `rem` 2 == 0 && b `rem` 2 == 1) = gcdNum (a `div` 2) b c -- if a is even an b is odd
  | otherwise = gcdNum a (b `div` 2) c -- if a is odd and b is even

main :: IO ()
main = do
  putStrLn ("Welcome to GCD calculator!, we use the Stein algorithm to get the results")
  -- Enter the first number in a
  let a = 56
  putStrLn ("First number: " ++ show (a))
  -- Enter the second number in b
  let b = 98
  putStrLn ("Second number: " ++ show (b))
  putStrLn ("\nThe maximum common divisor is " ++ (show (gcdNum a b 1)))
