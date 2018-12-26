{-# LANGUAGE InstanceSigs #-}

module Hedra where

import Control.Monad (replicateM)
import Data.List (intercalate)
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

d :: Int -> Die -> IO ()
d = printRoll

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

showRoll :: Int -> Die -> IO String
showRoll count die = do
  rolls <- roll count die
  let rollSum = sum rolls
      rollStrings = map showOneRoll rolls
      showSum = case die of
        Percent -> True
        _ -> count > 1
      str =
        (if showSum then show rollSum ++ " | "  else "")
        ++ intercalate " " rollStrings
  pure str
  where
    showOneRoll :: Int -> String
    showOneRoll n =
      case die of
        Sides _ ->
          show n
        Percent ->
          case n `divMod` 10 of
            (10, 0) -> "00 0"
            (0, ones) -> unwords ["00", show ones]
            (tens, ones) -> unwords [show (tens * 10), show ones]
        Fudge ->
          case n of
            -1 -> "[-]"
            0 -> "[ ]"
            1 -> "[+]"
            _ -> "Fudge die result out of range: " ++ show n

printRoll :: Int -> Die -> IO ()
printRoll count die =
  showRoll count die >>= putStrLn
