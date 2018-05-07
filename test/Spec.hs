import           Game.Data
import           Game.Funcs
import           Game.Types
import           System.Random
import           Test.QuickCheck


main :: IO ()
main = quickCheck prop_Conserv_Cards

-- !!!!!! Not working !!!!
prop_Conserv_Cards :: GameState -> Bool
prop_Conserv_Cards gs = length beforeDeck == (length afterDeck + sum (map howManyCardsOnThisPlayerHand afterPlayers))
   where types = (gs::GameState)
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
  cards <- vectorOf 108 $ elements allcardsDeck
  return cards

genGameState :: Gen GameState
genGameState = do
    num_players <- choose (3,5)
    _roundG <- choose (1,5)
    _turnG <- choose(1,5)
    various_players <- vectorOf num_players genPlayer
    deck_state <- genDeckState
    return GameState {deckState = deck_state , roundG = _roundG, turnG = _turnG, numPlayers = num_players, players  = various_players}

genPlayer :: Gen Player
genPlayer = do
    pid <- choose(1,40)
    pname <- elements ["Raul","Tiago","Miguel","Rui","Pedro","José","Daniel","Diogo"]
    state <- genPlayerState
    return Player {pid = pid, pname = pname, state = state}

genPlayerState :: Gen PlayerState
genPlayerState = do
  game_hand <- vectorOf 0 $ elements allcardsDeck
  cards_on_table <- vectorOf 0 $ elements allcardsDeck
  _turnP <- choose(1,5)
  _roundP <- choose(1,5)
  _score <- choose(1,100)
  return PlayerState {gameHand = game_hand, cardsOnTable = cards_on_table, turnP = _turnP, roundP = _roundP, score = _score}
