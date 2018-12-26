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
d = roll

f :: Die
f = Fudge

roll :: Int -> Die -> IO [Int]
roll count die
  | count < 0 = error "Must roll at least zero dice."
  | otherwise = replicateM count oneRoll
  where
    oneRoll :: IO Int
    oneRoll = case die of
      Sides sides | sides < 1 -> error "Dice must have at least one side."
      Sides sides -> randomRIO (1, sides)
      Percent -> randomRIO (1, 100)
      Fudge -> randomRIO ((-1), 1)
