module GUI.Text (
  Font,
  loadFont,
  TextStyle(..)
) where

import SFML.Graphics

loadFont :: FilePath -> IO Font
loadFont f = err (fontFromFile f)