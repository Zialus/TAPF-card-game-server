{-# LANGUAGE RecordWildCards #-}

module Game where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List        (delete)
import           Data.STRef
import           System.Random

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
        where
            n = length xs
            newArray :: Int -> [a] -> ST s (STArray s Int a)
            newArray n xs =  newListArray (1,n) xs

type DeckState = ([Card], StdGen)

shuffleDeck :: DeckState -> DeckState
shuffleDeck (deck,gen) = shuffle' deck gen


allcardsDeck :: [Card]
allcardsDeck =     replicate 14  Tempura  ++
                   replicate 14  Sashimi  ++
                   replicate 14  Dumpling  ++
                   replicate 12  TwoMaki  ++
                   replicate 8   ThreeMaki  ++
                   replicate 6   OneMaki  ++
                   replicate 10  SalmonNigiri  ++
                   replicate 5   SquidNigiri  ++
                   replicate 5   EggNigiri  ++
                   replicate 10  Pudding  ++
                   replicate 6   Wasabi  ++
                   replicate 4   Chopsticks

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
          deriving (Show, Eq)

data Player = Player { id    :: Int
                     , state :: PlayerState
                     }

data PlayerState = PlayerState { gameHand :: [Card]
                               , wasabi   :: Bool
                               , turn     :: Int
                               } deriving (Show)

data GameState = GameState { round      :: Int
                           , numPlayers :: Int
                           , players    :: [PlayerState]
                           , sessionID  :: Int
                           }

data Move = PlayCard Card
          | SpecialMoveChopStick

takeCardFromPlayer :: Player -> Card -> Player
takeCardFromPlayer Player{..} card =
    Player { id = id , state = newState}
    where
        newHand = takeCardFromHand (gameHand state) card
        newState = PlayerState { gameHand = newHand
                               , wasabi = wasabi state
                               , turn = turn state
                               }

takeCardFromHand :: [Card] -> Card -> [Card]
takeCardFromHand gameHand card = delete card gameHand

applyMove :: Player -> Move -> GameState -> GameState
-- applyMove p m gState = state@{newGameState}
--         where newGameState = gameState
applyMove = undefined
