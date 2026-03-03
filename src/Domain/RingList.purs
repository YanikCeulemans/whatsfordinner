module Domain.RingList (RingList, fromFoldable, asList, toListWithRange) where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as List

newtype RingList a = MkRingList (List a)

instance Semigroup (RingList a) where
  append (MkRingList xs) (MkRingList ys) = MkRingList (append xs ys)

instance Monoid (RingList a) where
  mempty = MkRingList mempty

fromFoldable :: forall f. Foldable f => f ~> RingList
fromFoldable = MkRingList <<< List.fromFoldable

asList :: forall a. RingList a -> List a
asList (MkRingList xs) = xs

length :: forall a. RingList a -> Int
length (MkRingList xs) = List.length xs

toListWithRange :: forall a. Int -> RingList a -> List a
toListWithRange to rs@(MkRingList xs) =
  go $ length rs
  where
  safeTo = max 0 to
  go len
    | len > 0 && to > len = toListWithRange to (rs <> rs)
    | otherwise = List.slice 0 safeTo xs

