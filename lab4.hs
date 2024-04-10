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
    let examples = [unlines ["S -> S a","S -> b","A -> S c","A -> d"],unlines [ "S -> S a","S -> S b","S -> c"],unlines ["S -> c S","S -> d"],unlines ["S -> S a b","S -> a"],unlines ["S -> b S a","S -> a"],unlines ["S -> S a","S -> b","A -> A c","A -> S d","A -> e","B -> b B","B -> A"]]
    mapM_ processGrammar examples
