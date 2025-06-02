module GUI.Texture (
  Texture,
  loadTexture,
  IntRect(..),
  TextureSettings(..), defaultTextureSettings
) where

import SFML.Graphics

loadTexture :: FilePath -> TextureSettings -> IO Texture
loadTexture f s =
  do
    obj <- err (textureFromFile f (restrict s))
    setSmooth obj (smooth s)
    setRepeated obj (repeated s)
    pure obj


-- | How to load the texture.
data TextureSettings = TextureSettings {
  restrict :: Maybe IntRect,
  -- ^ If set, then load only the given sub-rectangle of the source
  smooth :: Bool,
  -- ^ Apply soomthing to the texture
  repeated :: Bool
  -- ^ Repeat the texture, if applied to a larger object
}


-- | No restriction, no smoothing, no repetion.
defaultTextureSettings :: TextureSettings 
defaultTextureSettings = TextureSettings {
  restrict = Nothing,
  smooth   = False,
  repeated = False
}

