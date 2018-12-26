{-# LANGUAGE InstanceSigs #-}

module Hedra where

import Control.Monad (replicateM)
import System.Random (randomRIO)

data Die
  = Sides Int
  | Percent
  | Fudge

instance Num Die where
  fromInteger :: Integer -> Die
  fromInteger n =
    case n of
      100 -> Percent
      _ -> Sides (fromInteger n)

  (+) = error "Num instance for Die not implemented"
  (-) = error "Num instance for Die not implemented"
  (*) = error "Num instance for Die not implemented"
  abs = error "Num instance for Die not implemented"
  signum = error "Num instance for Die not implemented"

d :: Int -> Die -> IO [Int]
d = rollDice

f :: Die
f = Fudge

rollDice :: Int -> Die -> IO [Int]
rollDice count die =
  let oneRoll = case die of
        Sides sides | sides < 1 -> error "Dice must have at least one side."
        Sides sides -> rollDieWithSides sides
        Percent -> rollPercentDie
        Fudge -> rollFudgeDie
  in
    if count >= 0
      then concat <$> replicateM count oneRoll
      else error "Must roll at least zero dice."

rollDieWithSides :: Int -> IO [Int]
rollDieWithSides sides = do
  result <- randomRIO (1, sides)
  pure [result]

rollPercentDie :: IO [Int]
rollPercentDie = do
  tens <- randomRIO (1, 10)
  ones <- randomRIO (1, 10)
  pure [10 * tens, ones]

rollFudgeDie :: IO [Int]
rollFudgeDie = do
  result <- randomRIO ((-1), 1)
  pure [result]
