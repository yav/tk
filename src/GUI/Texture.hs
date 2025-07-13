-- | Textures
module GUI.Texture (
  Texture,
  loadTexture,
  TextureSettings(..), defaultTextureSettings
) where

import SFML.Graphics qualified as SFML
import GUI.Geometry
import GUI.Utils


-- | A texture contains image data
type Texture = SFML.Texture

-- | Load a texture from the given file.
-- Throws an exception if the file cannot be processed.
loadTexture :: FilePath -> TextureSettings -> IO Texture
loadTexture f s =
  do
    obj <- SFML.err (SFML.textureFromFile f (toIntRect <$> restrict s))
    SFML.setSmooth obj (smooth s)
    SFML.setRepeated obj (repeated s)
    pure obj

-- | How to load a texture
data TextureSettings = TextureSettings {
  restrict :: Maybe (Rect Int),
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

