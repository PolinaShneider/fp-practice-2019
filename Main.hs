module Main where

import Task1_1
import Task1_2

main
 = do
  putStrLn "Task1_1:"
  -- (2 + 1) * 2 - (23 - 14) = 3 * 2 - 9 = -3
  let a = (IntConstant 2 |+| IntConstant 1) |*| IntConstant 2 |-| (IntConstant 23 |-| IntConstant 14)
  putStrLn "a:"
  print a
  putStrLn "---"
  putStrLn "evaluate a:"
  print $ evaluate a
  putStrLn "---"
  -- (15 + 2 + 1) * 2 - (23 - 14) = (18) * 2 - 9 = 36 - 9 = 27
  let b =
        replaceVar "Count" (IntConstant 15) (Variable "Count" |+| IntConstant 2 |+| IntConstant 1) |*| IntConstant 2 |-|
        (IntConstant 23 |-| IntConstant 14)
  putStrLn "replaceVar:"
  print b
  putStrLn "---"
  putStrLn "evaluate b:"
  print $ evaluate b
  putStrLn "---"
  putStrLn "Task1_2:"
  putStrLn "GCD tests:"
  print $ gcdFunc 3 4 == 1
  print $ gcdFunc 2 4 == 2
  print $ gcdFunc 100 25 == 25
  putStrLn "---"
  putStrLn "doesSquareBetweenExist tests:"
  print $ doesSquareBetweenExist 1 15
  print $ doesSquareBetweenExist 3 7
  print $ doesSquareBetweenExist 8 7
  putStrLn "---"
  putStrLn "pow tests:"
  print $ pow 2 0 == 1
  print $ pow 3 4 == 81
  print $ pow 2 10 == 1024
  putStrLn "---"
  putStrLn "isPrime tests:"
  print $ isPrime 13
  print $ isPrime 1001
  print $ isPrime 24
  putStrLn "---"
