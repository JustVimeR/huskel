import Data.List (nub, isPrefixOf)
import Data.Maybe (mapMaybe)

type Rule = (String, [String])
type Grammar = [Rule]

parseRule :: String -> Maybe Rule
parseRule str = 
    case words $ map (\c -> if c == '-' || c == '>' then ' ' else c) str of
        left:right:_ -> Just (left, words right)
        _ -> Nothing

readGrammarFromString :: String -> Grammar
readGrammarFromString str =
    let parsedRules = map parseRule $ lines str
    in mapMaybe id parsedRules

findLeftRecursive :: Grammar -> [String]
findLeftRecursive grammar = nub [a | (a, b:_) <- grammar, a == b]

processGrammar :: String -> IO ()
processGrammar grammarStr = do
    let grammar = readGrammarFromString grammarStr
    let leftRec = findLeftRecursive grammar
    putStrLn "Left recursive nonterminals:"
    mapM_ putStrLn leftRec
    putStrLn ""

main :: IO ()
main = do
    let examples = [ "S -> S a\nS -> b\nA -> S c\nA -> d"
                   , "S -> S a\nS -> S b\nS -> c"
                   , "S -> c S\nS -> d"
                   , "S -> S a b\nS -> a"
                   , "S -> b S a\nS -> a"
                   , "S -> S a\nS -> b\nA -> A c\nA -> S d\nA -> e\nB -> b B\nB -> A"
                   ]
    mapM_ processGrammar examples

