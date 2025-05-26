module Piece where

newtype PlayerId = PlayerId Int
  deriving (Show,Eq,Ord)

data Piece = Piece {
  pieceLevel  :: Int,
  pieceOwner  :: PlayerId
}