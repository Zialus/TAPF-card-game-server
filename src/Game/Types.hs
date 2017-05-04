module Game.Types where

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

data Player = Player { pid   :: Int
                     , state :: PlayerState
                     } deriving (Show)

instance Eq Player where
    Player pid1 _state1 == Player pid2 _state2 = pid1 == pid2

instance Ord Player where
    Player pid1 _state1 <= Player pid2 _state2 = pid1 <= pid2

data PlayerState = PlayerState { gameHand     :: [Card]
                               , cardsOnTable :: [Card]
                               , wasabi       :: Bool
                               , turn         :: Int
                               } deriving (Show)

data GameState = GameState { roundN     :: Int
                           , numPlayers :: Int
                           , players    :: [Player]
                           , sessionID  :: Int
                           } deriving (Show)

data Move = PlayCard Card
          | SpecialMoveChopStick Card Card
