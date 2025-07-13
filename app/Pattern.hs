module Pattern where

import Control.Monad(guard)
import Data.Maybe(isNothing)
import Text.Read(readMaybe)

import Piece

data Owner = Player | Opponent
  deriving (Show,Eq)

data Pattern = Pattern {
  matchAtLeast :: Maybe Int,
  matchAtMost  :: Maybe Int,
  matchOwner   :: Maybe Owner
} deriving Show

anyPiece :: Pattern
anyPiece = Pattern {
  matchAtLeast = Nothing,
  matchAtMost  = Nothing,
  matchOwner   = Nothing
}

isAnyPiece :: Pattern -> Bool
isAnyPiece Pattern { matchAtLeast, matchAtMost, matchOwner } =
  isNothing matchAtLeast && isNothing matchAtMost && isNothing matchOwner

matches :: PlayerId -> Pattern -> Piece -> Bool
matches pid pat Piece { pieceLevel, pieceOwner } =
  check (<= pieceLevel) matchAtLeast &&
  check (pieceLevel <=) matchAtMost &&
  check checkOwner      matchOwner
  where
  check :: (a -> Bool) -> (Pattern -> Maybe a) -> Bool
  check f g = maybe True f (g pat) 
  checkOwner o =
    case o of
      Player   -> pid == pieceOwner
      Opponent -> pid /= pieceOwner

showPattern :: Pattern -> Maybe String
showPattern Pattern { matchAtLeast, matchAtMost, matchOwner } =
  do
    a <- maybe (pure '.') num matchAtLeast
    b <- maybe (pure '.') (\n -> pure (if Just n == matchAtLeast then '!' else '.')) matchAtMost
    c <- maybe (pure '.') (\n -> pure (if n == Player then 'P' else 'O')) matchOwner
    pure [a,b,c] 
  where
  num n =
    case show n of
      [c] -> pure c
      _   -> Nothing

parsePattern :: String -> Maybe Pattern
parsePattern s =
  case s of
    [ a, b, c ] ->
      do
        lst <-
          case num a of
            Just n -> pure (Just n)
            Nothing -> guard (a == '.') >> pure Nothing
        mst <-
          case num b of
            Just n -> pure (Just n)
            _ | c == '!' -> lst >> pure lst
              | c == '.' -> pure Nothing
              | otherwise -> Nothing
        own <-
          case c of
            'O' -> pure (Just Opponent)
            'P' -> pure (Just Player)
            '.' -> pure Nothing
            _   -> Nothing  
        pure anyPiece { matchOwner = own , matchAtLeast = lst, matchAtMost = mst }

    _ -> Nothing
  where
  num n = readMaybe [n]