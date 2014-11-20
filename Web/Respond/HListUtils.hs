{-|
Description: simple utilities for working with HLists. Unimportant.

makes it easier for this library to work with HLists.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Respond.HListUtils where

import Data.HList

type family HListElim (ts :: [*]) (a :: *) :: *
type instance HListElim '[] a = a
type instance HListElim (t ': ts) a = t -> HListElim ts a

type HList0 = HList '[]

type HList1 a = HList '[a]

hListUncurry :: HListElim ts a -> HList ts -> a
hListUncurry f HNil = f
hListUncurry f (HCons x xs) = hListUncurry (f x) xs

