module Model.Utils where
import qualified Data.Char as C
import Data.Maybe
import Control.Lens

import Language.Haskell.TH

lensName :: Name -> [DefName]
lensName n = [TopName $ mkName (nameBase n ++ "Lens")]

modifyHead :: (a -> a) -> [a] -> [a]
modifyHead _ [] = []
modifyHead f (x:xs) = f x : xs

dropAndLowerHead :: Int -> String -> String
dropAndLowerHead n = modifyHead C.toLower . drop n

setMaybe :: ASetter s t b b -> Maybe b -> s -> t
setMaybe l mb = over l (`fromMaybe` mb)

infixr 4 .~?
(.~?) :: ASetter s t b b -> Maybe b -> s -> t
(.~?) = setMaybe


