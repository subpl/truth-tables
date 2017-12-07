import Data.List
import Data.Char
import Foreign.Marshal.Utils
import Text.Printf

{- expression datatype -}
data Exp = Var Char
         | Not Exp
         | And Exp Exp
         | Or  Exp Exp
         | Imp Exp Exp
         deriving Show

{- stores truth values -}
type Val = [(Char,Bool)]

{- example expressions (mostly for testing) -}
e0, e1, e2 :: Exp
e0 = Not (Or (And (Var 'a') (Not (Var 'b'))) (Var 'c'))        -- ~((a & ~b)|c)   
e1 = And (Var 'a')(Var 'b')                                            -- a & b   
e2 = And (Not (Or (And (Var 'a') (Var 'b') ) 
                  (And (Var 'c') (Var 'd'))))
         (Or (Var 'a')(And (Var 'c') (And (Var 'd') (Not (Var 'b')))))
                                   -- ~((a & b) | (c & d)) & (a | (c & d & ~b))  
                      
{- returns a human-readable version of a given expression -}
readable :: Exp -> String
readable (Var c)     = [c]
readable (Not e)     = "!" ++ readable e 
readable (And e0 e1) = "(" ++ readable e0 ++ " AND " ++ readable e1 ++ ")" 
readable (Or  e0 e1) = "(" ++ readable e0 ++ " OR "  ++ readable e1 ++ ")" 
readable (Imp e0 e1) = "(" ++ readable e0 ++ " IMPLIES " ++ readable e1 ++ ")" 

{- returns a list of all character variables inside a given expression-}
getVars :: Exp -> [Char]
getVars (Var c)     = [c]
getVars (Not e)     = getVars e
getVars (And e0 e1) = getVars e0 ++ getVars e1
getVars (Or  e0 e1) = getVars e0 ++ getVars e1
getVars (Imp e0 e1) = getVars e0 ++ getVars e1

{- returns a list of possible truth set-ups -}
getLines :: [Char] -> [Val]
getLines []     = [[]]
getLines (c:cs) = [(c,b) : v | v <- getLines cs, b <- [True,False]]

{- checks an given truth set-up for the assignment for a given char.
   returns an error if char doesn't have an assignment (note: error should
   never be returned) -}
getAssn :: Val -> Char -> Bool
getAssn []     _ = error "var isn't assigned"
getAssn (l:ls) c | fst l == c = snd l
                 | otherwise  = getAssn ls c

{- evaluates a given expression for a given truth set-up, and returns the 
   overall result -}
eval :: Exp -> Val -> Bool
eval (Var c)     as = getAssn as c
eval (Not e)     as = not (eval e as)
eval (And e0 e1) as = (eval e0 as) && (eval e1 as)
eval (Or  e0 e1) as = (eval e0 as) || (eval e1 as)
eval (Imp e0 e1) as = (not $ eval e0 as) || (eval e1 as)

{- main body of printing -}
printme :: [(Val,Bool)] -> String
printme []                   = ""
printme (([]        ,br):rs) = (show br) ++ "\n" ++ printme rs
printme ((((_,b):vs),br):rs) = (show b) ++ "\t" ++ printme ((vs,br):rs)

{- get truth density stats -}
calcDens :: [Bool] -> String
calcDens l = printf "truth density: %d/%d = %.2f%%" trues len percent
  where
    percent = fromIntegral trues*100/fromIntegral len :: Float --trues/len as %
    len     = length l :: Int
    trues   = sum $ map fromBool l :: Int    --fromBool is f(True)=1;f(False)=0
--map to the list and sum to find out the total true values in a list of bools

{- standard higher-order sort; supply (id) for stardard quicksort -}
quickSortBy :: Ord b => (a->b) -> [a] -> [a]
quickSortBy f []     = []
quickSortBy f [a]    = [a]
quickSortBy f (p:ps) = quickSortBy f smaller ++ [p] ++ quickSortBy f greater
  where
    smaller = (filter (\x->(f x)<=(f p)) ps)
    greater = (filter (\x->(f x)> (f p)) ps)
      --or: = ps\\smaller

{- function for use in sorting; converts each boolean to 0/1, and forms an 
   integer from it. e.g., 
     orderBools [True,False,True] = 101
     orderBools [False,True,True] =  11 -}
orderBools :: ([(Char,Bool)],Bool) -> Int
orderBools (l,_) = read $ concatMap (show . fromBool) (map snd l)

{- the body of the coding; supplied with an expression, calculates truth table, 
   formats, prints, calculates and prints truth density -}
table :: Exp -> IO()
table e = putStrLn ("\ntruth table for " ++ readable e ++ ":\n" ++ 
                                                            --intro/table title
                   ((intersperse '\t' (map toUpper chs))++"\tRESULT\n") ++ 
                   ((intersperse '\t' (map toUpper chs))++"\tRESULT\n") ++ 
                                                                 --table header
                   (printme sortedLines) ++    --body of table, with all values
                   (calcDens $ map (eval e) lines))       --truth density stats
  where 
    chs   = nub $ getVars e                       --list of chars in expression
    lines = getLines chs     --list of possible truth set-ups for list of chars
    resultLines = zip lines $ map (eval e) lines 
                  --list of possible truth setups with their respective results
    sortedLines = quickSortBy orderBools resultLines