module Board where

import Data.Map(Map)

import Piece
import Vec2D
import Rule2D

data Board = Board {
  boardDim :: Vec2D Int,
  bordDat  :: Map (Vec2D Int) Piece
}

