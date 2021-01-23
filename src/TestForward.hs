{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -v2 
                -freduction-depth=0
                -Wno-missing-signatures #-}

module TestForward where

import Coercions (HasForward (..), HasInitialize (..), ReshapeStack, Tensor (..))

type NumLayers = 1

-- | The number of coercions scales linearly in 'NumLayers':
--
-- ReshapeStack 1:    Result size of Desugar (before optimization)
--                      = {terms: 149, types: 2,367, coercions: 3,706, joins: 0/47}
-- ReshapeStack 2:    Result size of Desugar (before optimization)
--                      = {terms: 191, types: 3,165, coercions: 7,081, joins: 0/61}
-- ReshapeStack 10:   Result size of Desugar (before optimization)
--                      = {terms: 527, types: 9,549, coercions: 34,081, joins: 0/173}
-- ReshapeStack 100:  Result size of Desugar (before optimization)
--                      = {terms: 4,307, types: 81,369, coercions: 337,831, joins: 0/1,433}
-- ReshapeStack 1000: Result size of Desugar (before optimization)
--                      = {terms: 42,107, types: 799,569, coercions: 3,375,331, joins: 0/14,033}
test =
  let input = UnsafeTensor @('Just ['Just 10, 'Just 10]) [10, 10]
      reshapeStack = initialize @(ReshapeStack NumLayers ('Just ['Just 1, 'Just 100]))
   in forward reshapeStack input
