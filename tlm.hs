import Data.Char

tousLesMots :: String -> [String]
tousLesMots ch = let low = map toLower ch in
        let mot_only = filter (\x -> (isAlpha x) || (x == ' ')) low in
                words mot_only

genererTable :: [String] -> [(String,[String])]
genererTable l_mot = genererTable' l_mot []

genererTable' :: [String] -> [(String,[String])] -> [(String,[String])]
genererTable' [] table = table
genererTable' (x:xs) table =
        if length xs >= 1
                then if in_table x table
                        then genererTable' xs (add_words (x:xs) table)
                        else genererTable' xs (table ++ [(x,[head(xs)])])
                else table ++ [(x,[])]

in_table :: String -> [(String,[String])] -> Bool
in_table x [] = False
in_table x (y:ys) =
        if x == fst y
                then True
                else in_table x ys

add_words :: [String] -> [(String,[String])] -> [(String,[String])]
add_words l_mot table = add_words' l_mot table 0

add_words' :: [String] -> [(String,[String])] -> Int -> [(String,[String])]
add_words' [] table i = table
add_words' (x:xs) table i =
        if i == 0
                then if x == fst (table !! i) then [(x,snd(table !! i) ++ [head xs])] ++ drop 1 table else add_words' (x:xs) table (i+1)
        else if x == fst(table !! i)  
                then take (i-1) table ++ [(x,snd(table !! i) ++ [head xs])] ++ drop (i+1) table
        else if length xs == 0
                then table
                else add_words' xs table (i+1)


main = let l_mot = tousLesMots "Je suis en train de decouvrir le langage de programmation Haskell, et je suis tres content. Je pense que Haskell est le meilleur langage de programmation au monde." in
                print(genererTable l_mot)
