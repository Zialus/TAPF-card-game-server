{-# LANGUAGE RecordWildCards #-}

module Game.Types where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List        (delete, elemIndex, find, insert)
import           Data.STRef
import           System.Random

type DeckState = ([Card], StdGen)

data Card = Tempura
          | Sashimi
          | Dumpling
          | TwoMaki
          | ThreeMaki
          | OneMaki
          | SalmonNigiri
          | SquidNigiri
          | EggNigiri
          | Pudding
          | Wasabi
          | Chopsticks
          deriving (Show, Eq, Ord)

data Player = Player { id    :: Int
                     , state :: PlayerState
                     }

instance Eq Player where
    Player id state == Player id' state' = id == id'

instance Ord Player where
    Player id state <= Player id' state' = id <= id'

data PlayerState = PlayerState { gameHand     :: [Card]
                               , cardsOnTable :: [Card]
                               , wasabi       :: Bool
                               , turn         :: Int
                               } deriving (Show)

data GameState = GameState { roundN     :: Int
                           , numPlayers :: Int
                           , players    :: [Player]
                           , sessionID  :: Int
                           }

data Move = PlayCard Card
          | SpecialMoveChopStick Card Card
