{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Hedra

import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe)
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
  putStr "hedra> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "Exiting."
    else do
      line <- getLine
      let trimmedLine = (dropWhile isSpace . dropWhileEnd isSpace) line
      when ((not . null) trimmedLine) (processInput trimmedLine)
      main
  where
    processInput :: String -> IO ()
    processInput input =
      case parseInput input of
        Just (count, die) -> do
          putStr (input ++ ": ")
          Hedra.printRoll count die
        Nothing ->
          putStrLn "Invalid input. Valid formats: d4, 2d6, 1d100, 4df"

parseInput :: String -> Maybe (Int, Hedra.Die)
parseInput input =
  listToMaybe $ do
    (count, afterCount) <- reads input <|> [(1, input)]
    guard (count >= 0)
    (_, afterSeparator) <- parseSeparator afterCount
    (die, _) <- parseDie afterSeparator
    pure (count, die)

parseSeparator :: String -> [((), String)]
parseSeparator = \case
  'd' : rest -> [((), rest)]
  _ -> []

parseDie :: String -> [(Hedra.Die, String)]
parseDie = \case
  "%" -> [(Hedra.Percent, "")]
  "f" -> [(Hedra.Fudge, "")]
  digits@(_:_) | all isDigit digits ->
    case read digits of
      0 -> []
      positiveSides -> [(fromIntegral (positiveSides :: Int), "")]
  _ -> []
