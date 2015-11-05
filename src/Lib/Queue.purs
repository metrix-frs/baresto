module Lib.Queue
  ( emptyQueue
  , push
  , pop
  , Queue()
  ) where

import Prelude

import Data.Tuple
import Data.Array
import Data.Exists
import Data.Maybe

data QueueF a q = QueueF
  { state :: q
  , pop :: q -> Maybe (Tuple q a)
  , push :: a -> q -> q
  }

data Queue a = Queue (Exists (QueueF a))

emptyQueue :: forall a. Queue a
emptyQueue = Queue $ mkExists $ QueueF
  { state: []
  , pop: \q -> case uncons q of
      Just { head: x, tail: xs } -> Just $ Tuple xs x
      Nothing -> Nothing
  , push: flip snoc
  }

push :: forall a. a -> Queue a -> Queue a
push x (Queue q) = runExists go q
  where
    go :: forall q. QueueF a q -> Queue a
    go (QueueF q) = Queue $ mkExists $ QueueF $ q { state = q.push x q.state }

pop :: forall a. Queue a -> Maybe (Tuple (Queue a) a)
pop (Queue q) = runExists go q
  where
    go :: forall q. QueueF a q -> Maybe (Tuple (Queue a) a)
    go (QueueF q) = case q.pop q.state of
      Nothing -> Nothing
      Just (Tuple newQ el) -> Just $ Tuple (Queue $ mkExists $ QueueF $ q { state = newQ }) el
