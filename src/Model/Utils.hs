{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE TupleSections #-}
module Model.Utils where

import qualified Data.Char as C
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Name, mkName, nameBase)

import Control.Applicative ((<*>))
import Control.Monad.State (State, execState, StateT, execStateT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Reader (MonadReader, Reader, runReader)
import Control.Lens.Setter (over, ASetter, (.=))
import Control.Lens.Getter (use, view)
import Control.Lens.Lens (Lens')
import Control.Lens.Tuple (_1, _2)

import Control.Lens.TH (DefName(TopName))

lensName :: Name -> [DefName]
lensName n = [TopName $ mkName (nameBase n ++ "Lens")]

mkLensName :: Name -> [Name] -> Name -> [DefName]
mkLensName = const (const lensName)

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

updateUnlessEq :: (MonadState (Bool, s) m, Eq b) => Lens' s b -> b -> m ()
updateUnlessEq l v = use (_2 . l) >>= updateWhen . (v /=)
    where updateWhen = bool (return ()) update
          update = do
            _1 .= True
            _2 . l .= v

resolveUpdate :: (MonadState (Bool, s) m, Eq b) => Lens' s b -> Maybe b -> m ()
resolveUpdate l = maybe (return ()) (updateUnlessEq l)

infixr 4 .=?
(.=?) :: (MonadState (Bool, s) m, Eq b) => Lens' s b -> Maybe b -> m ()
(.=?) = resolveUpdate

runUpdate :: a -> State (Bool, a) () -> (Bool, a)
runUpdate = flip execState . (False ,)

resolveUpdateR :: (MonadState (Bool, s) m, Eq b, MonadReader t m) => Lens' s b -> Lens' t (Maybe b) -> m ()
resolveUpdateR l lv = view lv >>= resolveUpdate l

infixr 4 ^.=?
(^.=?) :: (MonadState (Bool, s) m, Eq b, MonadReader t m) => Lens' s b -> Lens' t (Maybe b) -> m ()
(^.=?) = resolveUpdateR

runUpdateR :: StateT (Bool, a) (Reader b) () -> a -> b -> (Bool, a)
runUpdateR op = runReader . execStateT op . (False,)

-- | cure that boolean blindness
asMaybe :: (Bool, a) -> Maybe a
asMaybe = (bool Nothing . Just . snd <*> fst)
