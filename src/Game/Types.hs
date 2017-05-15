module Game.Types where

import           System.Random

type Deck = [Card]
type DeckState = (Deck, StdGen)

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
          deriving (Show, Read, Eq, Ord)

data Player = Player { pid   :: Int
                     , pname :: String
                     , state :: PlayerState
                     } deriving (Show)

instance Eq Player where
    Player pid1 _pname1 _state1 == Player pid2 _pname2 _state2 = pid1 == pid2

instance Ord Player where
    Player pid1 _pname1 _state1 <= Player pid2 _pname2 _state2 = pid1 <= pid2

data PlayerState = PlayerState { gameHand     :: Deck
                               , cardsOnTable :: Deck
                               , wasabi       :: Bool
                               , turn         :: Int
                               } deriving (Show)

data GameState = GameState { deckState  :: DeckState
                           , roundN     :: Int
                           , numPlayers :: Int
                           , players    :: [Player]
                           } deriving (Show)

data Move = PlayCard Card
          | SpecialMoveChopStick Card Card
          deriving (Read,Show)
