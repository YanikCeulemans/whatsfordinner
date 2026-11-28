module Spa.App.RemoteRefreshData where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lens (Prism')
import Data.Lens.Prism as Prism
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)

data RemoteRefreshData e a
  = NotRequested
  | Loading
  | Error e
  | Success a
  | Refreshing a
  | RefreshFailed e a

instance Functor (RemoteRefreshData e) where
  map f =
    case _ of
      NotRequested -> NotRequested
      Loading -> Loading
      Error e -> Error e
      Success a -> Success $ f a
      Refreshing a -> Refreshing $ f a
      RefreshFailed e a -> RefreshFailed e $ f a

instance Foldable (RemoteRefreshData e) where
  foldl f s =
    case _ of
      NotRequested -> s
      Loading -> s
      Error _e -> s
      Success a -> f s a
      Refreshing a -> f s a
      RefreshFailed _e a -> f s a
  foldr f s =
    case _ of
      NotRequested -> s
      Loading -> s
      Error _e -> s
      Success a -> f a s
      Refreshing a -> f a s
      RefreshFailed _e a -> f a s
  foldMap f =
    case _ of
      NotRequested -> mempty
      Loading -> mempty
      Error _e -> mempty
      Success a -> f a
      Refreshing a -> f a
      RefreshFailed _e a -> f a

instance Traversable (RemoteRefreshData e) where
  traverse f =
    case _ of
      NotRequested -> pure NotRequested
      Loading -> pure Loading
      Error e -> pure $ Error e
      Success a -> Success <$> f a
      Refreshing a -> Refreshing <$> f a
      RefreshFailed e a -> RefreshFailed e <$> f a

  sequence =
    case _ of
      NotRequested -> pure NotRequested
      Loading -> pure Loading
      Error e -> pure $ Error e
      Success a -> Success <$> a
      Refreshing a -> Refreshing <$> a
      RefreshFailed e a -> RefreshFailed e <$> a

instance Apply (RemoteRefreshData e) where
  apply fm m = case fm, m of
    NotRequested, _ -> NotRequested
    Loading, _ -> Loading
    _, NotRequested -> NotRequested
    _, Loading -> Loading
    Error e, _ -> Error e
    _, Error e -> Error e
    Success f, Success a -> Success $ f a
    Success f, Refreshing a -> Success $ f a
    Success f, RefreshFailed _e a -> Success $ f a
    Refreshing f, Success a -> Refreshing $ f a
    Refreshing f, Refreshing a -> Refreshing $ f a
    Refreshing f, RefreshFailed _e a -> Refreshing $ f a
    RefreshFailed e f, Success a -> RefreshFailed e $ f a
    RefreshFailed e f, Refreshing a -> RefreshFailed e $ f a
    RefreshFailed e f, RefreshFailed _e a -> RefreshFailed e $ f a

instance Bind (RemoteRefreshData e) where
  bind remoteData f = case remoteData of
    NotRequested -> NotRequested
    Loading -> Loading
    Error e -> Error e
    Success a -> f a
    Refreshing a -> f a
    RefreshFailed _e a -> f a

note :: forall e a. e -> Maybe a -> RemoteRefreshData e a
note err = case _ of
  Nothing -> Error err
  Just x -> Success x

toMaybe :: forall e a. RemoteRefreshData e a -> Maybe a
toMaybe = case _ of
  NotRequested -> Nothing
  Loading -> Nothing
  Error _e -> Nothing
  Success a -> Just a
  Refreshing a -> Just a
  RefreshFailed _e a -> Just a

_Success :: forall e a. Prism' (RemoteRefreshData e a) a
_Success = Prism.prism' Success toMaybe

_Refreshing :: forall e a. Prism' (RemoteRefreshData e a) a
_Refreshing = Prism.prism' Refreshing $ case _ of
  Refreshing a -> Just a
  _ -> Nothing
