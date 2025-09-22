{-# LANGUAGE OverloadedStrings #-}

module BlackjackAI 
    (hiLoCard, runningCount, remainingDecks) where

import Cards 


--https://www.blackjackapprenticeship.com/how-to-count-cards/

hiLoCard :: Card -> Int
hiLoCard (Card v _) = case v of
    Two   -> 1
    Three -> 1
    Four  -> 1
    Five  -> 1
    Six   -> 1
    Seven -> 0
    Eight -> 0
    Nine  -> 0
    Ten   -> -1
    Jack  -> -1
    Queen -> -1
    King  -> -1
    Ace   -> -1


-- sum Hi-Lo values over a list of seen cards
runningCount :: [Card] -> Int
runningCount = sum . map hiLoCard


-- casinos use up to 8 decks in blackjack, so it's only fair if we do the same xd

-- To turn a running count into a true count, you divide by decks remaining. This function gives you that divisor (e.g., 26 cards left → 0.5 decks).
remainingDecks :: Deck -> Double
remainingDecks d =
  let rc = max 1 (length d)
  in fromIntegral rc / 52.0

trueCount :: [Card] -> Deck -> Double
trueCount seen d = fromIntegral (runningCount seen) / remainingDecks d

-- based on true count, decide whether to bet high or low
shouldBetHigh :: [Card] -> Deck -> Bool
shouldBetHigh seen d = trueCount seen d >= 2.0
shouldBetLow :: [Card] -> Deck -> Bool
shouldBetLow seen d = trueCount seen d < 2.0
shouldBet :: [Card] -> Deck -> Int
shouldBet seen d = if shouldBetHigh seen d then 100 else 10
