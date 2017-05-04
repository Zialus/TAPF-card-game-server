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

data Player = Player { id    :: Int
                     , state :: PlayerState
                     }

instance Eq Player where
    Player id1 _state1 == Player id2 _state2 = id1 == id2

instance Ord Player where
    Player id1 _state1 <= Player id2 _state2 = id1 <= id2

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
