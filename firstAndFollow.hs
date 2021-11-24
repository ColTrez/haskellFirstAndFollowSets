import Data.Set as Set (Set, fromList, empty, singleton, toList)
--import Data.Map as Map

data Symbol = NonTerminal String | Terminal String
instance Show Symbol where
    show (NonTerminal s) = s
    show (Terminal s) = s

instance Eq Symbol where
    (NonTerminal s1) == (NonTerminal s2) = s1 == s2
    (Terminal s1) == (Terminal s2) = s1 == s2

instance Ord Symbol where
    (NonTerminal s1) `compare` (NonTerminal s2) = s1 `compare` s2
    (Terminal s1) `compare` (Terminal s2) = s1 `compare` s2
    
isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False

type Rule = (Symbol, [Symbol])
type Grammar = [Rule]

showRule :: Rule -> String
showRule (nt, xs) = show nt ++ " ->" ++ (concatMap (\s -> " " ++ show s) xs)

getRHS :: Rule -> [Symbol]
getRHS = snd

--some basic grammar
ruleS :: Rule
ruleS = (NonTerminal "S", [NonTerminal "A", NonTerminal "B"])

ruleA1 = (NonTerminal "A", [Terminal "a"])
ruleA2 = (NonTerminal "A", [NonTerminal "A", Terminal "a"])

ruleB = (NonTerminal "B", [Terminal "b"])

gr :: Grammar
gr = [ruleS, ruleA1, ruleA2, ruleB]

listSymbols :: Grammar -> [Symbol]
listSymbols g = let lhs = map fst g
                    rhs = concat $ map getRHS g
                in lhs ++ rhs
                    
type FirstSet = (Symbol, Set Symbol)

--first set rules
--initialize non terminals to empty set and terminals to set of themselves
--(NonTerminal, Terminal)
ntAndT :: Grammar -> (Set Symbol, Set Symbol)
ntAndT g = let symbols = listSymbols g
               terminals = filter isTerminal symbols
               nonTerminals = filter (not . isTerminal) symbols
           in (Set.fromList nonTerminals, Set.fromList terminals)

initFirstSets :: Grammar -> [FirstSet]
initFirstSets g = let (nonterminals, terminals) = ntAndT g
                      ntSets = map (\nt -> (nt, Set.empty)) (Set.toList nonterminals)
                      tSets = map (\t -> (t, Set.singleton t)) (Set.toList terminals)
                  in ntSets ++ tSets
