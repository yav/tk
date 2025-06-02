module Action where

import Text.Read(readMaybe)
import Piece

data Action =
    Leave         -- Leave matched piece as is
  | Set Int       -- Set piece to the given level, 0 to kill 
  | Upgrade       -- Upgrade matched piece
  | Demote        -- Demote matched piece
  | Claim         -- Change to our ownership
  deriving (Show, Eq)

execute :: PlayerId -> Action -> Maybe Piece -> Maybe Piece
execute pid act p =
  case act of
    Leave   -> p
    Set lvl -> Just Piece { pieceLevel = lvl, pieceOwner = pid }
    Upgrade -> update \pc -> pc { pieceLevel = pieceLevel pc + 1 }
    Demote  -> update \pc -> pc { pieceLevel = pieceLevel pc - 1 }
    Claim   -> update \pc -> pc { pieceOwner = pid }
  >>= check
  where
  update f = f <$> p 
  check newP = if pieceLevel newP > 0 then pure newP else Nothing

showAction :: Action -> Maybe String
showAction a =
  case a of
    Leave   -> pure "."
    Upgrade -> pure "+"
    Demote  -> pure "-"
    Claim   -> pure "!"
    Set n ->
      case show n of
        [c] -> pure [c]
        _   -> Nothing

parseAction :: Char -> Maybe Action
parseAction c =
  case c of
    '.' -> pure Leave
    '+' -> pure Upgrade
    '-' -> pure Demote
    '!' -> pure Claim
    _ | Just n <- readMaybe [c] -> pure (Set n)
    _ -> Nothing
   