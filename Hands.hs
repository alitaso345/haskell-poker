module Hands
  ( Hand
  , toHand, fromHand
  ) where

import Cards
import Data.List
import Control.Monad

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
  if length l == 5
    then Just $ Hand (sort l)
    else Nothing

pokerHand :: Hand -> (PokerHand, Card)
pokerHand = undefined

data PokerHand
  = HighCards
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  deriving (Show, Read, Eq, Ord, Enum)

straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush = undefined

fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind = undefined

fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse = undefined

flush :: Hand -> Maybe (PokerHand, Card)
flush = undefined

straight :: Hand -> Maybe (PokerHand, Card)
straight = undefined

threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind = undefined

twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair = undefined

onePair :: Hand -> Maybe (PokerHand, Card)
onePair = undefined

straightHint :: Hand -> Maybe Card
straightHint (Hand l) =
  (judgeStraight . extract cardStrength $ l)
  `mplus`
  (judgeStraight . sort . extract cardNumber $ l)
    where
      isStraight :: [Int] -> Bool
      isStraight xs@(x:_) = xs == [x .. x+4]
      isStraight _ = False

      judgeStraight :: [(Int, Card)] -> Maybe Card
      judgeStraight l =
        if isStraight $ map fst l
          then Just . snd . last $ l
          else Nothing

flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) =
  if all ((cardSuit x==).cardSuit) xs then Just (last xs) else Nothing

nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint n (Hand h) = if cards /= [] then Just cards else Nothing
  where
    cards :: [[Card]]
    cards = filter ((==n).length)
      $ groupBy (\x y -> cardNumber x == cardNumber y) h

extract :: (b -> a) -> [b] -> [(a, b)]
extract f = map (\c -> (f c, c))
