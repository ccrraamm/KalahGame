module Interactive where

import Board
import Control.Monad (when)

{-
    Avansează pas cu pas jocul, pe baza unei strategii primite ca parametru.
    Este folosită atât pentru jocul a doi utilizatori umani,
    cât și pentru urmărirea pas cu pas a jocului bazat pe euristică.
-}
interactive :: (Board -> IO Board) -> IO ()
interactive strategy = loop initialBoard
  where
    loop :: Board -> IO ()
    loop board = do
        print board
        if isOver board
            then putStrLn $ case scores board of
                (you, opponent)
                    | you > opponent -> "You win!"
                    | you < opponent -> "You lose!"
                    | otherwise      -> "Draw"
            else do
                newBoard <- strategy board
                loop =<< do
                    -- Tratăm și cazul în care mutarea nu a produs o schimbare
                    -- de configurație
                    when (newBoard == board) $ putStrLn "Invalid move!"
                    return newBoard

{-
    Jocul între doi utilizatori umani.
-}
twoHumans :: IO ()
twoHumans = interactive console

{-
    Urmărește pas cu pas evoluția jocului, pe baza unei euristici.
-}
humanVsAI :: (Board -> (House, Board)) -> IO ()
humanVsAI step = interactive $ \board -> do
    case who board of
        You      -> console board
        Opponent -> do
            let (house, newBoard) = step board
            putStrLn $ "Opponent played house " ++ show house
            return newBoard

{-
    Utilizată atât între doi jucători umani, cât și în jocul utilizatorului
    cu calculatorul, pentru citirea casei de la consolă.
-}
console :: Board -> IO Board
console board = do
    putStr "House number (1-6): "
    house <- getLine
    return $ move (read house :: Int) board
