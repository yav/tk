module Action where

import Control.Monad(guard)
import Text.Read(readMaybe)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

data Action = Act Int MatchedAction
  deriving Show
  
data MatchedAction = Leave | Kill | Set Int | Upgrade | Demote
  deriving Show

drawAction :: Float -> Action -> Picture
drawAction sz a = color white (rectangleWire sz sz) -- XXX 

parseAction :: String -> Maybe Action
parseAction xs =
  case xs of
    [m,a,n] ->
      do
        lvl <- readMaybe [m]
        let done = guard (n == '.')
        act <-
          case a of
            '!' -> Set <$> readMaybe [n]
            '?' -> done >> pure Leave
            'x' -> done >> pure Kill
            '+' -> done >> pure Upgrade
            '-' -> done >> pure Demote
            _   -> Nothing
        pure (Act lvl act)
    _ -> Nothing