import Test.QuickCheck
import Game.Types
import Game.Funcs
import Game.Data

main :: IO ()
main = quickCheck prop_Conserv_Cards

prop_Conserv_Cards gs = length beforeDeck == (length afterDeck + length afterPlayers)
   where types = (gs::GameState, beforeDeck :: Deck, afterDeck :: Deck, afterPlayers :: [Player])
         beforeDeck = fst $ deckState gs
         gameStateAfterDistribution = distributeCards gs
         afterDeck = fst $ deckState gameStateAfterDistribution
         afterPlayers = players gameStateAfterDistribution

instance Arbitrary GameState where
    arbitrary = genGameState

genGameState :: Gen GameState
genGameState = undefined
-- genGameState = do
--     numPlayer <- choose (3,5)
--     repeat numPlayers genPlayer
--     return

genPlayer :: Gen Player
genPlayer = undefined
-- genPlayer = do
--     name <- elements ["Raul","Tiago","Miguel","Rui","Pedro","José","Daniel","Diogo"]
--     return
