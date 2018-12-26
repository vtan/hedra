{-# LANGUAGE InstanceSigs #-}

module Hedra
  ( Die(..)
  , d, f
  , roll, showRoll, printRoll
  )
where

import Control.Monad (replicateM)
import Data.List (intercalate)
import System.Random (randomRIO)

-- $setup
-- >>> import System.Random (mkStdGen, setStdGen)
-- >>> setStdGen (mkStdGen 0)

data Die
  = Sides Int
  | Percentile
  | Fudge

instance Num Die where
  fromInteger :: Integer -> Die
  fromInteger n =
    case n of
      100 -> Percentile
      _ -> Sides (fromInteger n)

  (+) = error "Num instance for Die not implemented"
  (-) = error "Num instance for Die not implemented"
  (*) = error "Num instance for Die not implemented"
  abs = error "Num instance for Die not implemented"
  signum = error "Num instance for Die not implemented"

-- |
-- >>> 2 `d` 8
-- 10 | 4 6
-- >>> 1 `d` 20
-- 4
d :: Int -> Die -> IO ()
d = printRoll

-- |
-- >>> 4 `d` f
-- 0 | [+] [+] [-] [-]
f :: Die
f = Fudge

-- |
-- >>> roll 2 8
-- [4,6]
-- >>> roll 1 20
-- [4]
-- >>> roll 1 Percentile
-- [39]
-- >>> roll 4 Fudge
-- [0,0,-1,0]
roll :: Int -> Die -> IO [Int]
roll count die
  | count < 0 = error "Must roll at least zero dice."
  | otherwise = replicateM count oneRoll
  where
    oneRoll :: IO Int
    oneRoll = case die of
      Sides sides | sides < 1 -> error "Dice must have at least one side."
      Sides sides -> randomRIO (1, sides)
      Percentile -> randomRIO (1, 100)
      Fudge -> randomRIO ((-1), 1)

-- |
-- >>> showRoll 2 8
-- "10 | 4 6"
-- >>> showRoll 1 20
-- "4"
-- >>> showRoll 1 Percentile
-- "39 | 30 9"
-- >>> showRoll 4 Fudge
-- "-1 | [ ] [ ] [-] [ ]"
showRoll :: Int -> Die -> IO String
showRoll count die = do
  rolls <- roll count die
  let rollSum = sum rolls
      rollStrings = map showOneRoll rolls
      showSum = case die of
        Percentile -> True
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
        Percentile ->
          case n `divMod` 10 of
            (10, 0) -> "00 0"
            (0, ones) -> unwords ["00", show ones]
            (tens, ones) -> unwords [show (tens * 10), show ones]
        Fudge ->
          case n of
            -1 -> "[-]"
            0 -> "[ ]"
            1 -> "[+]"
            _ -> error ("Fudge die result out of range: " ++ show n)

-- |
-- >>> printRoll 2 8
-- 10 | 4 6
-- >>> printRoll 1 20
-- 4
-- >>> printRoll 1 Percentile
-- 39 | 30 9
-- >>> printRoll 4 Fudge
-- -1 | [ ] [ ] [-] [ ]
printRoll :: Int -> Die -> IO ()
printRoll count die =
  showRoll count die >>= putStrLn
