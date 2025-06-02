module Rule where

import GUI.Scene
import GUI.Color
import Pattern
import Action

data Rule = Rule {
  rulePattern :: Pattern,
  ruleAction  :: Action
} deriving Show

isTrivial :: Rule -> Bool
isTrivial Rule { rulePattern, ruleAction } =
  isAnyPiece rulePattern && ruleAction == Leave 

drawRule :: Float -> Rule -> Scene
drawRule sz _ =
  OutlineColor white $ Outline 1 $ Rectangle sz sz

parseRule :: String -> Maybe Rule
parseRule s =
  case s of
    [a,b,c,d] ->
      do
        rulePattern <- parsePattern [a,b,c]
        ruleAction  <- parseAction d
        pure Rule { rulePattern, ruleAction }
    _ -> Nothing