module Hands
  ( Hand
  , toHand, fromHand
  ) where

import Cards
import Data.List

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
straightHint = undefined

flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) =
  if all ((cardSuit x==).cardSuit) xs then Just (last xs) else Nothing

nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint = undefined
