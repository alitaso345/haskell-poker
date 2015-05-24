module Main where
import System.Random.Shuffle
import Data.List
import Cards
import Hands

main :: IO ()
main = do
  hand <- randomHand
  res <- return $ judgePoker hand

randomHand :: IO (Maybe Hand)
randomHand = do
  shuffled <- shuffleM allCards
  return . toHand . take 5 $ shuffled
