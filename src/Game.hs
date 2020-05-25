module Game where

import Graphics.Gloss

someFunc :: IO ()
someFunc =
  play
    (InWindow "Tak" (200, 200) (200, 200))
    (makeColor 0.2 0.4 0.5 1)
    60
    ""
    (const Blank)
    (const id)
    (const id)
