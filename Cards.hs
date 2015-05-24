module Cards
  ( Suit(..)
  , Card
  , allCards
  , cardSuit
  , cardNumber
  , cardStrength
  ) where

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Int Suit
  deriving (Eq, Ord)

showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x = show x ++ "_"

instance Show Card where
  show (Card i Hearts) = "H" ++ showCardNumber i
  show (Card i Diamonds) = "D" ++ showCardNumber i
  show (Card i Clubs) = "C" ++ showCardNumber i
  show (Card i Spades) = "S" ++ showCardNumber i

allCards :: [Card]
allCards = [Card num suit | suit <- [Hearts ..], num <- [2..14 ]]

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card n _) = n

cardStrength :: Card -> Int
cardStrength (Card n _) = n
