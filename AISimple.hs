module AISimple where

import Board
import Interactive

{-
    *** TODO ***

    Întoarce tabla rezultată din aplicarea acelei mutări care maximizează
    scorul adversarului.
-}
step :: Board -> (House, Board)
step board = undefined


{-
    Urmărește pas cu pas evoluția jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = humanVsAI step
