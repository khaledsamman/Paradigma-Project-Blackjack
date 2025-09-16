{-# LANGUAGE OverloadedStrings #-}

module GameFloor where

import Control.Monad.State
import Cards
import IOFloor (getPlayerBet)


--monad transformer stack
type Game = StateT GameState IO


--initial game state
initialState :: GameState
initialState = GameState {
    player = Player [] 0 1000,
    dealer = Dealer [],
    deck = fullDeck
}


--computation in game

playerBetAction :: Game ()
playerBetAction = do
  bet <- liftIO getPlayerBet
  p0  <- gets player
  let p1 = makeBet bet p0
  modify (\s -> s { player = p1 })



dealCardsToPlayer :: Int -> Game ()
dealCardsToPlayer n = do
    -- get current game state
    gs <- get
    let pl = player gs
    -- deal cards using pure function
    let (newDeck, playerDealt) = dealCards n (deck gs)
    -- new player state with new cards
    let pl' = pl { playerHand = (playerHand pl) ++ playerDealt }
    -- update game state
    modify (\s -> s { player = pl', deck = newDeck })


showPlayerState :: Game ()
showPlayerState = do
    pl <- gets player
    liftIO $ print pl

-- dealer takes cards
dealerAction :: Int -> Game ()
dealerAction pvalue = do
    dl <- gets dealer
    let value = handValue (dealerHand dl)
    if value < 17
        then do
            dealCardsToDealer 1
            showDealerHand
            dealerAction pvalue
        else do
            showDealerHand
            liftIO $ putStrLn $ "Dealer stands with value: " ++ show value
            if ((value > 21) || (value < pvalue))
                then do
                    liftIO $ putStrLn "Player wins!"
                    pl <- gets player
                    let pl' = payout ((playerBet pl)*3) pl
                    modify (\s -> s { player = pl' })
                    showPlayerState
                else do
                    liftIO $ putStrLn "Player Looses :("
                    pl <- gets player
                    let pl' = payout 0 pl
                    modify (\s -> s { player = pl' })
                    showPlayerState

-- main player round
playerAction :: Game ()
playerAction = do
    showDealerHand
    showPlayerState
    action <- liftIO $ do
        putStrLn "Choose action: (h)it or (s)tand"
        getLine
    case action of 
        "H" -> do
            dealCardsToPlayer 1
            pl <- gets player
            let newValue = handValue (playerHand pl)
            if newValue > 21
                then do
                    liftIO $ putStrLn "Player busts! Dealer wins :("
                    showPlayerState
                    let pl'' = payout 0 pl'
                    modify (\s -> s { player = pl'' })
                    showPlayerState
                    -- modifyPlayer 
    
                