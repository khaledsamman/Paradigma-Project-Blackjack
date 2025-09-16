{-# LANGUAGE OverloadedStrings #-}

module GameFloor where

import Control.Monad.State
import Cards (Player(playerBet))


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

playerBetAction :: Int -> Game ()
playerBetAction = do
    bet <- liftIO getPlayerBet
    player <- gets player
    let player' = makeBet bet player
    modify $ \s -> s { player = player' }