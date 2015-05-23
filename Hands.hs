newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

dicision :: [Card] -> Maybe [Card]
dicision l =
  if length l == 5
    then Just $ sort l
    else Nothing
