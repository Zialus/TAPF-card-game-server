import           Game.Data
import           Game.Funcs
import           Game.Types
import           System.Random
import           Test.QuickCheck


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


genDeckState :: Gen DeckState
genDeckState = do
  deck <- genDeck
  number <- choose (0,9999)
  let state = mkStdGen number
  return (deck,state)

genDeck :: Gen Deck
genDeck = do
  cards <- vectorOf 100 $ elements allcardsDeck
  return cards

genGameState :: Gen GameState
genGameState = do
    num_players <- choose (3,5)
    round_n <- choose (1,5)
    various_players <- vectorOf num_players genPlayer
    deck_state <- genDeckState
    return GameState {deckState = deck_state , roundN = round_n, numPlayers = num_players, players  = various_players}

genPlayer :: Gen Player
genPlayer = do
    pid <- choose(1,40)
    pname <- elements ["Raul","Tiago","Miguel","Rui","Pedro","José","Daniel","Diogo"]
    state <- genPlayerState
    return Player {pid = pid, pname = pname, state = state}

genPlayerState :: Gen PlayerState
genPlayerState = do
  game_hand <- vectorOf 5 $ elements allcardsDeck
  cards_on_table <- vectorOf 5 $ elements allcardsDeck
  _turn_ <- choose(1,7)
  return PlayerState {gameHand = game_hand, cardsOnTable = cards_on_table, turn = _turn_}
