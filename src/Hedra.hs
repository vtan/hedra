{-# LANGUAGE InstanceSigs #-}

-- | Generating dice rolls.
-- The functions in this module use the global random generator
-- described in "System.Random#globalrng".

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

-- | Variants of dice.
data Die
  -- | A die with the given number of sides. The number is assumed to be positive.
  = Sides Int
  -- | Two 10-sided dice, one for tens and one for ones. Two zeros are interpreted as 100.
  | Percentile
  -- | A die with the values -1, 0 and +1.
  | Fudge

-- | Only for using 'Num' literals as 'Die' values, all other methods throw an error.
-- The literal @100@ corresponds to percentile dice,
-- all other literals are dice with that many sides.
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

-- | An alias of 'printRoll' to be used in the REPL.
--
-- >>> 2 `d` 8
-- 10 | 4 6
-- >>> 1 `d` 20
-- 4
d :: Int -> Die -> IO ()
d = printRoll

-- | An alias of 'Fudge' to be used in the REPL.
--
-- >>> 4 `d` f
-- 0 | [+] [+] [-] [-]
f :: Die
f = Fudge

-- | Generates a dice roll as a list of individual rolls.
--
-- >>> roll 2 8
-- [4,6]
-- >>> roll 1 20
-- [4]
-- >>> roll 1 Percentile
-- [39]
-- >>> roll 4 Fudge
-- [0,0,-1,0]
roll
  :: Int -- ^ The number of dice to roll, must be at least 0.
  -> Die -- ^ The variant of dice to roll.
  -> IO [Int] -- ^ The list of individual die rolls.
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

-- | Generates a dice roll as a string.
--
-- >>> showRoll 2 8
-- "10 | 4 6"
-- >>> showRoll 1 20
-- "4"
-- >>> showRoll 1 Percentile
-- "39 | 30 9"
-- >>> showRoll 4 Fudge
-- "-1 | [ ] [ ] [-] [ ]"
showRoll
  :: Int -- ^ The number of dice to roll, must be at least 0.
  -> Die -- ^ The variant of dice to roll.
  -> IO String -- ^ The pretty-printed dice roll.
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

-- | Generates and prints and a dice roll.
--
-- >>> printRoll 2 8
-- 10 | 4 6
-- >>> printRoll 1 20
-- 4
-- >>> printRoll 1 Percentile
-- 39 | 30 9
-- >>> printRoll 4 Fudge
-- -1 | [ ] [ ] [-] [ ]
printRoll
  :: Int -- ^ The number of dice to roll, must be at least 0.
  -> Die -- ^ The variant of dice to roll.
  -> IO ()
printRoll count die =
  showRoll count die >>= putStrLn
