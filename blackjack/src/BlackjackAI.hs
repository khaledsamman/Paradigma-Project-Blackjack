{-# LANGUAGE OverloadedStrings #-}


import Cards 



--https://www.blackjackapprenticeship.com/how-to-count-cards/


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
