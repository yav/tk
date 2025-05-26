module Rule where

import Data.Maybe(fromMaybe)
import Graphics.Gloss.Data.Picture qualified as P
import Graphics.Gloss.Data.Color   qualified as P
import Pattern
import Action

data Rule = Rule {
  rulePattern :: Pattern,
  ruleAction  :: Action
} deriving Show

isTrivial :: Rule -> Bool
isTrivial Rule { rulePattern, ruleAction } =
  isAnyPiece rulePattern && ruleAction == Leave 

drawRule :: Float -> Rule -> P.Picture
drawRule sz _ =
  P.color P.white $
  P.pictures [
    P.rectangleWire sz sz
  ]

parseRule :: String -> Maybe Rule
parseRule s =
  case s of
    [a,b,c,d] ->
      do
        rulePattern <- parsePattern [a,b,c]
        ruleAction  <- parseAction d
        pure Rule { rulePattern, ruleAction }
    _ -> Nothing