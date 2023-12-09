{-# LANGUAGE OverloadedStrings #-}

-- p1 p2 w    h   x_b y_b vx_b vy_b y_p1 y_p2
-- d  u  100  50  50  25  1.41 1.41 10   30

import Data.Text qualified as T

data Input = Up | Down | None
  deriving (Show)

data State = State
  { player1input :: Input,
    player2input :: Input,
    width :: Integer,
    height :: Integer,
    ballPos :: (Integer, Integer),
    ballVelo :: (Float, Float),
    player1y :: Integer,
    player2y :: Integer
  }
  deriving (Show)

-- applyState :: String -> String
-- applyState state =
parseInput :: String -> Input
parseInput i
  | i == "u" = Up
  | i == "d" = Down
  | otherwise = None

parseInteger :: String -> Integer
parseInteger i = read i :: Integer

parseFloat :: String -> Float
parseFloat i = read i :: Float

parseState :: String -> State
parseState input =
  State
    { player1input = parseInput p1i,
      player2input = parseInput p2i,
      width = parseInteger w,
      height = parseInteger h,
      ballPos = (parseInteger bX, parseInteger bY),
      ballVelo = (parseFloat bvX, parseFloat bvY),
      player1y = parseInteger p1y,
      player2y = parseInteger p2y
    }
  where
    [p1i, p2i, w, h, bX, bY, bvX, bvY, p1y, p2y] = map T.unpack (T.splitOn " " (T.pack input))

main = do
  input <- getLine
  print (parseState input)
