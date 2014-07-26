module Model.Utils where
import qualified Data.Char as C

modifyHead :: (a -> a) -> [a] -> [a]
modifyHead _ [] = []
modifyHead f (x:xs) = (f x):xs

dropAndLowerHead :: Int -> String -> String
dropAndLowerHead n = (modifyHead C.toLower) . (drop n)

