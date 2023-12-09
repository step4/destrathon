{-# LANGUAGE OverloadedStrings #-}

-- p1 p2 w    h   x_b y_b vx_b vy_b y_p1 y_p2
-- d  u  100  50  50  25  1.41 1.41 10   30

import Data.Text qualified as T

data Key = Up | Down | None
  deriving (Show, Eq)

data PlayerInput = PlayerInput
  { player1input :: Key,
    player2input :: Key
  }
  deriving (Show)

data WorldState = WorldState
  { width :: Integer,
    height :: Integer,
    ballPos :: (Integer, Integer),
    ballVelo :: (Float, Float),
    player1y :: Integer,
    player2y :: Integer
  }
  deriving (Show)

data Winner = Player1 | Player2
  deriving (Show)

data Return = Running WorldState | GameEnd Winner
  deriving (Show)

parseInput :: String -> Key
parseInput i
  | i == "u" = Up
  | i == "d" = Down
  | otherwise = None

parseInteger :: String -> Integer
parseInteger i = read i :: Integer

parseFloat :: String -> Float
parseFloat i = read i :: Float

parsePlayerInput :: [String] -> PlayerInput
parsePlayerInput input =
  PlayerInput
    { player1input = parseInput p1i,
      player2input = parseInput p2i
    }
  where
    [p1i, p2i] = input

parseState :: [String] -> WorldState
parseState state =
  WorldState
    { width = parseInteger w,
      height = parseInteger h,
      ballPos = (parseInteger bX, parseInteger bY),
      ballVelo = (parseFloat bvX, parseFloat bvY),
      player1y = parseInteger p1y,
      player2y = parseInteger p2y
    }
  where
    [w, h, bX, bY, bvX, bvY, p1y, p2y] = state

parseWholeInput :: String -> (PlayerInput, WorldState)
parseWholeInput wholeState = (parsePlayerInput pi, parseState st)
  where
    splits = T.splitOn " " (T.pack wholeState)
    pi = map T.unpack (take 2 splits)
    st = map T.unpack (drop 2 splits)

advancePlayer :: Integer -> Key -> Integer -> Integer
advancePlayer currentPos key height
  | key == None = currentPos
  | key == Up = if currentPos <= 0 then currentPos else currentPos - 1
  | key == Down = if currentPos + 10 >= height then currentPos else currentPos + 1

ballCollidesWithPlayer :: (Integer, Integer) -> Integer -> Integer -> Integer -> Bool
ballCollidesWithPlayer (px, py) player1y player2y width
  | px <= 3 = player1y < py && py < (player1y + 10)
  | px >= width - 3 = player2y < py && py < (player2y + 10)
  | otherwise = False

advanceBall :: WorldState -> ((Integer, Integer), (Float, Float))
advanceBall worldState = ((px, py), (vx, vy))
  where
    px = old_px + floor (vx * dt)
    py = old_py + floor (vy * dt)
    (old_px, old_py) = ballPos worldState
    dt = 1.0
    vy = if old_py == 0 || old_py == height worldState then -old_vy else old_vy
    vx = if ballCollidesWithPlayer (ballPos worldState) (player1y worldState) (player2y worldState) (width worldState) then -old_vx else old_vx
    (old_vx, old_vy) = ballVelo worldState

advanceState :: PlayerInput -> WorldState -> WorldState
advanceState input worldState =
  WorldState
    { width = width worldState,
      height = height worldState,
      ballPos = p,
      ballVelo = v,
      player1y = advancePlayer (player1y worldState) (player1input input) (height worldState),
      player2y = advancePlayer (player2y worldState) (player2input input) (height worldState)
    }
  where
    (p, v) = advanceBall worldState

applyInputToState :: (PlayerInput, WorldState) -> Return
applyInputToState (input, worldState)
  | x >= w - 1 = GameEnd Player1
  | x <= 1 = GameEnd Player2
  | otherwise = Running (advanceState input worldState)
  where
    x = fst (ballPos worldState)
    w = width worldState

serializeReturn :: Return -> String
serializeReturn return = case return of
  Running worldState -> serializeWorldState worldState
  GameEnd winner -> serializeWinner winner

serializeWinner :: Winner -> String
serializeWinner winner = "winner " ++ show winner

serializeWorldState :: WorldState -> String
serializeWorldState worldState = show (width worldState) ++ " " ++ show (height worldState) ++ " " ++ show (fst (ballPos worldState)) ++ " " ++ show (snd (ballPos worldState)) ++ " " ++ show (fst (ballVelo worldState)) ++ " " ++ show (snd (ballVelo worldState)) ++ " " ++ show (player1y worldState) ++ " " ++ show (player2y worldState)

main = do
  input <- getLine
  print (serializeReturn (applyInputToState (parseWholeInput input)))
