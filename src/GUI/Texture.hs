module GUI.Texture (
  Texture,
  loadTexture,
  IntRect(..)
) where

import SFML.Graphics

loadTexture :: FilePath -> Maybe IntRect -> IO Texture
loadTexture f r = err (textureFromFile f r)