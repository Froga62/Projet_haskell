import Data.Char
import Data.List (unfoldr)
import Data.Time.Clock.POSIX (getPOSIXTime)

tousLesMots :: String -> [String]
tousLesMots ch = let low = map toLower ch in
        let mot_only = filter (\x -> (isAlpha x) || (x == ' ')) low in
                words mot_only

genererTable :: [String] -> [(String,[String])]
genererTable [] = []
genererTable [x] = [(x,[])]
genererTable (x:y:xs) =
        case lookup x tuples of
                Nothing -> (x,[y]):tuples
                Just ys -> (x,ys ++ [y]): filter ((/= x). fst) tuples
        where tuples = genererTable(y:xs)

-- Fin partie 1

genererNewText :: [String] -> [String]
genererNewText [] = []
genererNewText (x:xs) = []


-- "Je suis en train de decouvrir le langage de programmation Haskell, et je suis tres content. Je pense que Haskell est le meilleur langage de programmation au monde." in
main :: IO()
main = do

        source <- getContents
        print (genererTable (tousLesMots source))