{-# LANGUAGE OverloadedStrings #-}

module GameFloor where

import Control.Monad.State
import Cards


--monad transformer stack
type Game = StateT GameState IO
