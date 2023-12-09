module Lib
  ( someFunc,
  )
where

-- p1 p2 w    h   x_b y_b vx_b vy_b y_p1 y_p2
-- d  u  100  50  50  25  1.41 1.41 10   30

someFunc :: IO ()
someFunc = do
  input <- getLine
  putStrLn input
  putStrLn "someFunc2"
