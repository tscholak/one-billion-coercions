{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-ds-preopt
                -freduction-depth=0
                -Wno-missing-signatures #-}

module TestForward where

import Coercions (HasForward (..), HasInitialize (..), ReshapeStack, Tensor (..))

type NumLayers = 1000

reshapeStack :: ReshapeStack NumLayers ('Just '[ 'Just 1, 'Just 100])
reshapeStack = initialize @(ReshapeStack NumLayers ('Just ['Just 1, 'Just 100]))

-- | The number of coercions scales linearly in 'NumLayers':
--
-- ReshapeStack 0:    RHS size: {terms: 22, types: 351, coercions: 175, joins: 0/6}
-- ReshapeStack 1:    RHS size: {terms: 94, types: 1,842, coercions: 4,205, joins: 0/31}
-- ReshapeStack 2:    RHS size: {terms: 104, types: 2,124, coercions: 4,400, joins: 0/35}
-- ReshapeStack 10:   RHS size: {terms: 184, types: 4,380, coercions: 5,960, joins: 0/67}
-- ReshapeStack 100:  RHS size: {terms: 1,084, types: 29,760, coercions: 23,510, joins: 0/427}
-- ReshapeStack 1000: RHS size: {terms: 10,084, types: 283,560, coercions: 199,010, joins: 0/4,027}
test =
  let input = UnsafeTensor @('Just ['Just 10, 'Just 10]) [10, 10]
   in forward reshapeStack input
