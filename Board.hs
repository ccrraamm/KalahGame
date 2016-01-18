{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , Player (..)  -- Exportăm și constructorii de date 'You' și 'Opponent'.
    , House
    , build
    , yourSeeds
    , oppsSeeds
    , who
    , isOver
    , initialBoard
    , move
    , scores
    , successors
    ) where

import Consecutive

{-
    Jucătorul care urmează să mute.
-}
data Player = You | Opponent deriving (Eq, Show)

{-
    Tipul caselor, definit pentru lizibilitate.
-}
type House = Int

{-

    Definiți tipul 'Board', astfel încât să rețină informație despre starea
    jocului, inclusiv tabla de joc.

    Observați că viitorii constructori de date și eventualele câmpuri pe care
    le veți specifica nu vor apărea în lista de funcții exportate de modul
    (vezi explicația de la începutul fișierului).
-}
--                        table myScore hisScore
data Board = Board Player [House] House House deriving Eq

{-

    Instanțiați clasa 'Show' cu tipul 'Board'. Exemplu de reprezentare,
    unde scorurile sunt aferent jucătorilor 'You', respectiv 'Opponent':

       4  4  4  4  4  4
     0                  0    Next: You, Playing, Score: (0,0)
       4  4  4  4  4  4
-}
showHelper [] = []
showHelper table = show (head table) ++ "  " ++ showHelper (tail table)

instance Show Board where
    show (Board player table myScore hisScore) = "  " ++ (showHelper (drop 6 table)) ++ "\n"
                                 ++ show hisScore ++ "                  "
                                 ++ show myScore ++ "     Next: " ++ show player 
                                 ++ ", Playing, Score: (" ++ show myScore ++ "," ++ show hisScore ++ ")"++ "\n"
                                 ++ "  " ++ (showHelper (take 6 table))

{-

    Instanțiați clasa 'Consecutive', pentru a putea determina dacă în două
    configurații ale tablei trebuie să mute același jucător.
-}
instance Consecutive Board where
    b1 >< b2 = undefined

{-

    Construiește tabla de joc.

    Funcția trebuie să determine dacă jocul este deja încheiat, pe baza
    conținutului caselor.
-}
build :: ([Int], Int)  -- Conținutul caselor și al depozitului utilizatorului
      -> ([Int], Int)  -- Conținutul caselor și al depozitului adversarului
      -> Player        -- Jucătorul aflat la rând
      -> Board         -- Tabla construită

build firstPair secondPair player = Board player ((fst firstPair) ++ (fst secondPair)) (snd firstPair) (snd secondPair)

{-

    Întoarce conținutul caselor și al depozitului utilizatorului.
-}
yourSeeds :: Board -> ([Int], Int)
yourSeeds (Board _ table myScore _ ) = (take 6 table , myScore)

{-

    Întoarce conținutul caselor și al depozitului adversarului.
-}
oppsSeeds :: Board -> ([Int], Int)
oppsSeeds (Board _ table _  hisScore) = (drop 6 table , hisScore)

{-

    Întoarce jucătorul aflat la rând.
-}
who :: Board -> Player
who (Board player _ _ _) = player

{-
    Întoarce 'True' dacă jocul s-a încheiat.
-}
isOver :: Board -> Bool
isOver (Board _ table myScore hisScore) = if ( (length (filter ( == 0 ) (drop 6 table)) == 6 ) ||
                                             (length (filter ( == 0 )  (take 6 table)) == 6)  )
                                            then 
                                                True
                                            else
                                                False

{-

    Tabla inițială.
-}
initialBoard :: Board
initialBoard = build ([4,4,4,4,4,4],0) ([4,4,4,4,4,4],0) You

{-

    Realizează o mutare pornind de la casa furnizată ca parametru, în funcție
    de configurația actuală a tablei și de jucătorul aflat la rând.

    Întoarce aceeași configurație dacă mutarea nu poate fi efectuată
    din diverse motive, precum numărul eronat al casei, sau casa goală.
-}
move :: House -> Board -> Board

updateElementsforMe house table
    | house >= 0 = (take (house `mod` 12) table) ++ [(table!!(house `mod` 12)) + 1] ++ (drop ((house `mod` 12) + 1) table)
    | otherwise = table

captureSeeds house table = let first = (take 6 table);
                               second = (drop 6 table);
                                 in ((take (house - 6 - 1) first) ++ [0] ++ (drop (house - 6) first)) ++ (reverse (take (house -2) second) ++ [0] ++ (drop (house - 1) second))

updateElementsforHim house table
    | house >= 0 && house < 6 = (take (house `mod` 12) table) ++ [(table!!(house `mod` 12)) + 1] ++ (drop ((house `mod` 12) + 1) table) 
    | house >= 6 && house < 12 = (take ((house -1 ) `mod` 12) table) ++ [(table!!((house - 1) `mod` 12)) + 1] ++ (drop (((house - 1) `mod` 12) + 1) table) 
    | otherwise = table


howIuseSeeds player seeds house table myScore hisScore 
    | seeds == 1 && (house `mod` 12) == 6 = (Board You ((take 6 table) ++ (reverse(drop 6 table))) (myScore + 1) hisScore)
    | seeds == 0 = (Board Opponent ((take 6 table) ++ (reverse(drop 6 table))) myScore hisScore)
    | seeds == 1 && (table!!(house `mod` 12)) == 0 && (table!!((11 - house) `mod` 12)) /= 0 = Board You (captureSeeds house table) (myScore + (table!!((11 - house) `mod` 12)) + 1) hisScore
    | seeds  > 0 && (house `mod` 12) == 6 = howIuseSeeds player (seeds - 2) (house + 1) (updateElementsforMe house table) (myScore + 1) hisScore
    | seeds  > 0 = howIuseSeeds player (seeds - 1) (house + 1) (updateElementsforMe house table) myScore hisScore
    | otherwise = (Board player table 48569 84225)


howHeUseSeeds player seeds house table myScore hisScore 
    | seeds == 1 && (house `mod` 12) == 6 = (Board Opponent table myScore (hisScore + 1))
    | seeds == 0 = (Board You table myScore hisScore)
    | seeds == 1 && (table!!(house `mod` 12)) == 0 && (table!!((house - 6) `mod` 12)) /= 0 = Board You (captureSeeds house table) myScore (hisScore + 1 + (table!!((house - 7) `mod` 12)))
    | seeds  > 0 && (house `mod` 12) == 6 = howHeUseSeeds player (seeds - 2) (house + 1) (updateElementsforHim house table) myScore (hisScore + 1)
    | seeds  > 0 = howHeUseSeeds player (seeds - 1) (house + 1) (updateElementsforHim house table) myScore hisScore
    | otherwise = (Board player table 48569 84225)

move house (Board player table myScore hisScore) 
    | house <= 0 || house > 6 = (Board player table myScore hisScore)
    | player == You && (table!!(house - 1)) == 0 = (Board player table myScore hisScore)
    | player == You && (table!!(house - 1)) /= 0 = let takeSeeds = (take (house - 1) table) ++ [0] ++ (drop house  table)
                                                        in howIuseSeeds player (table!!(house - 1)) house ((take 6 takeSeeds) ++ (reverse(drop 6 takeSeeds))) myScore hisScore
    | player == Opponent && (table!!(house + 5)) == 0 = (Board player table myScore hisScore)
    | player == Opponent && (table!!(house + 5)) /= 0   = let takeSeeds = (take (house + 5) table) ++ [0] ++ (drop ((house + 5) + 1) table)
                                                                in howHeUseSeeds player (table!!(house + 5)) (house + 5) ((take 6 takeSeeds) ++ (reverse(drop 6 takeSeeds))) myScore hisScore


{-

    Întoarce scorurile (utilizator, adversar).

    Calculul trebuie să țină cont de eventuala încheiere a jocului.
-}
scores :: Board -> (Int, Int)
scores (Board player table myScore hisScore ) = if isOver (Board player table myScore hisScore ) == False
                                                    then
                                                        (myScore, hisScore)
                                                    else 
                                                        ( (myScore + (foldl (+) 0 (take 6 table))) , (hisScore + (foldl (+) 0 (drop 6 table))) )

{-

    Întoarce perechile casă-configurație, reprezentând mutările care pot fi
    făcute într-un singur pas, pornind de la configurația actuală.
-}
successors :: Board -> [(House, Board)]
successors (Board player table myScore hisScore ) = if(player == You)
                                                        then
                                                            myMoveList 1 [] (Board player table myScore hisScore)
                                                        else
                                                            hisMoveList 7 [] (Board player table myScore hisScore)

myMoveList house  acc (Board player table myScore hisScore ) 
    | house < 7 && (table!!(house - 1)) /= 0 = myMoveList (house + 1)  (acc ++ [(house, (move house (Board player table myScore hisScore )))]) (Board player table myScore hisScore)
    | house < 7 && (table!!(house - 1)) == 0 = myMoveList (house + 1)  acc (Board player table myScore hisScore)
    | otherwise = acc

hisMoveList house acc (Board player table myScore hisScore ) 
    | house < 12 && (table!!(house - 1)) /= 0 = hisMoveList (house + 1)  (acc ++ [((house - 6), (move ((house - 6)) (Board player table myScore hisScore )))]) (Board player table myScore hisScore)
    | house < 12 && (table!!(house - 1)) == 0 = hisMoveList (house + 1)  acc (Board player table myScore hisScore)

    | otherwise = acc
