{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-ds-preopt
                -freduction-depth=0
                -Wno-missing-signatures #-}

module TestForward where

import Coercions (HasForward (..), HasInitialize (..), ReshapeStack, Tensor (..))

type NumLayers = 1

-- 0 : 175
-- 1 : 2,685
-- 2 : 2,685 | 2,726
-- 1000 : 2,685 | 43,644

reshapeStack :: ReshapeStack NumLayers ('Just '[ 'Just 1, 'Just 100])
reshapeStack = initialize @(ReshapeStack NumLayers ('Just ['Just 1, 'Just 100]))

-- | The number of coercions scales linearly in 'NumLayers':
--
-- ReshapeStack 1:    Result size of Desugar (before optimization)
--                      = {terms: 144, types: 2,251, coercions: 3,676, joins: 0/45}
-- ReshapeStack 2:    Result size of Desugar (before optimization)
--                      = {terms: 186, types: 3,163, coercions: 8,717, joins: 0/59}
-- ReshapeStack 10:   Result size of Desugar (before optimization)
--                      = {terms: 522, types: 10,459, coercions: 49,045, joins: 0/171}
-- ReshapeStack 100:  Result size of Desugar (before optimization)
--                      = {terms: 4,302, types: 92,539, coercions: 502,735, joins: 0/1,431}
-- ReshapeStack 1000: Result size of Desugar (before optimization)
--                      = {terms: 42,102, types: 913,339, coercions: 5,039,635, joins: 0/14,031}
test =
  let input = UnsafeTensor @('Just ['Just 10, 'Just 10]) [10, 10]
      -- reshapeStack = initialize @(ReshapeStack NumLayers ('Just ['Just 1, 'Just 100]))
   in forward reshapeStack input

