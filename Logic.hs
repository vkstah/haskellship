import Game

isEmpty :: Cell -> Bool
isEmpty Miss = True
isEmpty _ = False

isShip :: Cell -> Bool
isShip Ship = True
isShip _ = False

isHit :: Cell -> Bool
isHit Hit = True
isHit _ = False

isMiss :: Cell -> Bool
isMiss Miss = True
isMiss _ = False