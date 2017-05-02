{-# LANGUAGE RecordWildCards #-}

module Game where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.List        (delete, elemIndex, find)
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

instance Eq Player where
    Player id state == Player id' state' = id == id'

data PlayerState = PlayerState { gameHand     :: [Card]
                               , cardsOnTable :: [Card]
                               , wasabi       :: Bool
                               , turn         :: Int
                               } deriving (Show)

data GameState = GameState { round      :: Int
                           , numPlayers :: Int
                           , players    :: [Player]
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
                               , cardsOnTable = cardsOnTable state
                               , wasabi = wasabi state
                               , turn = turn state
                               }

takeCardFromHand :: [Card] -> Card -> [Card]
takeCardFromHand gameHand card = delete card gameHand

checkValidMove :: Player -> Move -> GameState -> Bool
checkValidMove player move game = undefined

applyMove :: Int -> Move -> GameState -> GameState
applyMove playerIndex m game = newGameState
        where
            thisPlayer = Game.players game !! playerIndex
            -- take the player out of the list
            -- update the player
            -- put him back in the list
            newPlayers = [thisPlayer]
            newGameState = GameState { round      = Game.round game
                                     , numPlayers = Game.numPlayers game
                                     , players    = newPlayers
                                     , sessionID  = Game.sessionID game
                                     }

                                     --thisPlayer = find (==p) (Game.players game)
                                     --newPlayers :: [Player]
                                     --newPlayers = maybe thisPlayer [p]
                                     -- case thisPlayer of
                                     --     Nothing -> newPlayers = [thisPlayer]
                                     --     Just aPlayer -> newPlayers = [aPlayer]
                                     -- NOT FINISHED!!!!!


-- elemItSelf :: Eq a => a -> [a] -> Maybe a
-- elemItSelf x [] = Nothing
-- elemItSelf x (y:ys) | x==y = Just x
--                     | x/=y = elemItself x ys
