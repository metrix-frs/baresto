module Lib.Queue
  ( emptyQueue
  , push
  , pop
  , Queue()
  ) where

import Prelude (($), flip)

import Data.Tuple (Tuple(Tuple))
import Data.Array (snoc, uncons)
import Data.Exists (Exists, runExists, mkExists)
import Data.Maybe (Maybe(Just, Nothing))

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

pop :: forall a. Queue a -> { state :: Queue a, value :: Maybe a }
pop queue@(Queue q') = runExists go q'
  where
    go :: forall q. QueueF a q -> { state :: Queue a, value :: Maybe a }
    go (QueueF q) = case q.pop q.state of
      Nothing ->
        { state: queue
        , value: Nothing }
      Just (Tuple newQ el) ->
        { state: Queue $ mkExists $ QueueF $ q { state = newQ }
        , value: Just el }
