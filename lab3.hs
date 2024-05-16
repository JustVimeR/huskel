module Main where

import Data.List (nub, (\\))

type State = Int
type Symbol = Char
type Transition = (State, Symbol, State)
type Automaton = ([State], [Transition], State, [State])

getAdjacent :: [Transition] -> State -> [State]
getAdjacent transitions state =
    nub [dest | (src, _, dest) <- transitions, src == state]

dfs :: [Transition] -> State -> [State]
dfs transitions start = dfs' [start] []
  where
    dfs' [] visited = visited
    dfs' (s:ss) visited
        | s `elem` visited = dfs' ss visited
        | otherwise = dfs' (ss ++ getAdjacent transitions s) (s:visited)

canReachFinal :: [State] -> [Transition] -> State -> Bool
canReachFinal finalStates transitions state = not . null $ dfs transitions state `intersect` finalStates

findProductive :: Automaton -> [State]
findProductive (states, transitions, _, finalStates) =
    filter (canReachFinal finalStates transitions) states

findUnproductive :: Automaton -> [State]
findUnproductive aut@(states, _, _, _) = 
    states \\ findProductive aut

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, x `elem` ys]

main :: IO ()
main = do
    let automata = [
            ([0..5], [(0, 'a', 1), (1, 'b', 2), (2, 'a', 3), (2, 'b', 3), (2, 'a', 4), (3, 'b', 3), (5, 'b', 3)], 0, [3, 4]),
            ([0..5], [(0, 'a', 1), (1, 'b', 2), (2, 'a', 0), (3, 'b', 4),(5, 'b', 3)], 1, [4]),
            ([0..4], [(0, 'a', 1), (1, 'b', 2), (3, 'a', 4)], 0, [2]),
            ([0..3], [(0, 'c', 1), (1, 'd', 2), (2, 'c', 3)], 0, [2]),
            ([0..5], [(0, 'e', 1), (1, 'f', 2), (4, 'e', 5)], 0, [5]),
            ([0..6], [(0, 'g', 1), (1, 'h', 2), (2, 'g', 3), (5, 'h', 6)], 0, [3, 6]),
            ([0..3], [(0, 'a', 1), (1, 'b', 2), (2, 'c', 3)], 0, [4])
            ]

    mapM_ (\(states, transitions, startState, finalStates) -> do
            let aut = (states, transitions, startState, finalStates)
            putStrLn "Productive states:"
            print $ findProductive aut
            putStrLn "Unproductive states:"
            print $ findUnproductive aut
            putStrLn "") automata
