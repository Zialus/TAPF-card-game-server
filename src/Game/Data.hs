module Game.Data where

import           Game.Types

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
                   replicate 6   Wasabi {plusCard = Nothing} ++
                   replicate 4   Chopsticks

amountToDistributeMap :: [(Int,Int)]
amountToDistributeMap = [(2,10),(3,9),(4,8),(5,7)]

player1 :: Player
player1 = Player {pid = 21312, pname = "Manuel", state = undefined}

player2 :: Player
player2 = Player {pid = 1231, pname = "Pedro", state = undefined}

player3 :: Player
player3 = Player {pid = 34534, pname = "Jaquim", state = undefined}
