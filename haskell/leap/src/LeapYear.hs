module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year =
  let divisible divisor = year `rem` divisor == 0 in
      divisible 400 || divisible 4 && not (divisible 100)
