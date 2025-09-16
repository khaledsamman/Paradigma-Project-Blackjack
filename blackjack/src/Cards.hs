{-# LANGUAGE OverloadedStrings #-}


module Cards where

data GameState = GameState 
  {
    deck :: Deck,
    player :: Player,
    dealer :: Dealer
  } deriving (Show)
    
-- simple algebraic data types for card values and suites
data CardValue = Two | Three | Four | Five | Six | Seven | Eight 
               | Nine | Ten | Jack | Queen | King | Ace
               deriving (Eq, Enum)

data CardSuite = Clubs | Spades | Diamonds | Hearts
               deriving (Eq, Enum)

-- our card type - merely combining CardValue and CardSuite
data Card = Card CardValue CardSuite deriving (Eq)

-- synonym for list of cards to store decks
type Deck = [Card]
type Hand = [Card]


instance Show CardValue where
  show c = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"] !! fromEnum c

instance Show CardSuite where
  show Spades   = "S"
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"

instance Show Card where
  show (Card a b) = show a ++ show b

-- defining full deck of cards via comprehension; how cool is that?! :)
fullDeck :: Deck
fullDeck = [ Card x y | y <- [Clubs .. Hearts], x <- [Two .. Ace] ]
smallDeck :: Deck
smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]
dealCards :: Int -> Deck -> (Deck, Deck)
dealCards n deck = (drop n deck, take n deck)
countCards :: Deck -> (Int, Deck)
countCards deck = (length deck, deck)

-- state for the player
data Player = Player {
    playerHand :: Hand,
    playerBet :: Int,
    playerMoney :: Int
} deriving (Show)

--state for the dealer
data Dealer = Dealer {
    dealerHand :: Hand
} deriving (Show)

--functions that extract rank and suit from a card
rank :: Card -> CardValue
rank (Card r _) = r
suit :: Card -> CardSuite
suit (Card _ s) = s


--calculate the blackjack value of a card, Ace is tricky!
cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
  Ace   -> 11 -- We'll handle Ace's 1 or 11 value in handValue
  Jack  -> 10
  Queen -> 10
  King  -> 10
  _     -> fromEnum rank - fromEnum Two + 2

-- calculate the value of a hand
handValue :: Hand -> Int
handValue hand =
  let base = sum (map cardValue hand)                 -- Ace=11 initially
      nAces = length (filter (\c -> rank c == Ace) hand)
  in adjust base nAces
  where
    adjust total a
      | total > 21 && a > 0 = adjust (total - 10) (a - 1)
      | otherwise           = total


--pure functions changing player state after making a bet

makeBet :: Int -> Player -> Player
makeBet bet player = player { playerBet = bet, playerMoney = playerMoney player - bet }

-- payout function to update player money after win/loss
payout :: Int -> Player -> Player
payout amt p = p { playerMoney = playerMoney p + amt, playerBet = 0 }

