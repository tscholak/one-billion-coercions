{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -v2 
                -freduction-depth=0 #-}

module TestInstance where

import Coercions (HasForward (..), ReshapeStack, Tensor)

type NumLayers = 1

newtype BoxedStack shape = BoxedStack (ReshapeStack NumLayers shape)

-- | The number of coercions scales constant in 'NumLayers':
--
-- ReshapeStack 0:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 51, joins: 0/2}
-- ReshapeStack 1:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 181, joins: 0/2}
-- ReshapeStack 2:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 3:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 4:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 5:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 6:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 7:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 8:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 9:  Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
-- ReshapeStack 10: Result size of Desugar (before optimization)
--                    = {terms: 97, types: 119, coercions: 445, joins: 0/2}
instance
  HasForward (ReshapeStack NumLayers shape) (Tensor inputShape) =>
  HasForward (BoxedStack shape) (Tensor inputShape)
  where
  type
    ForwardOutput (BoxedStack shape) (Tensor inputShape) =
      ForwardOutput (ReshapeStack NumLayers shape) (Tensor inputShape)
  forward (BoxedStack stack) input = forward stack input
