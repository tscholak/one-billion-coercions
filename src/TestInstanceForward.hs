{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -v2 
                -freduction-depth=0
                -Wno-missing-signatures #-}

module TestInstanceForward where

import Coercions (HasForward (..), HasInitialize (..), ReshapeStack, Tensor (..))
import TestInstance (BoxedStack (..))

-- | The number of coercions scales linearly in 'NumLayers':
--
-- ReshapeStack 0:    Result size of Desugar (before optimization)
--                      = {terms: 61, types: 864, coercions: 357, joins: 0/17}
-- ReshapeStack 1:    Result size of Desugar (before optimization)
--                      = {terms: 149, types: 2,386, coercions: 3,716, joins: 0/46}
-- ReshapeStack 2:    Result size of Desugar (before optimization)
--                      = {terms: 191, types: 3,298, coercions: 8,757, joins: 0/60}
-- ReshapeStack 3:    Result size of Desugar (before optimization)
--                      = {terms: 233, types: 4,210, coercions: 13,798, joins: 0/74}
-- ReshapeStack 4:    Result size of Desugar (before optimization)
--                      = {terms: 275, types: 5,122, coercions: 18,839, joins: 0/88}
-- ReshapeStack 5:    Result size of Desugar (before optimization)
--                      = {terms: 317, types: 6,034, coercions: 23,880, joins: 0/102}
-- ReshapeStack 6:    Result size of Desugar (before optimization)
--                      = {terms: 359, types: 6,946, coercions: 28,921, joins: 0/116}
-- ReshapeStack 7:    Result size of Desugar (before optimization)
--                      = {terms: 401, types: 7,858, coercions: 33,962, joins: 0/130}
-- ReshapeStack 8:    Result size of Desugar (before optimization)
--                      = {terms: 443, types: 8,770, coercions: 39,003, joins: 0/144}
-- ReshapeStack 9:    Result size of Desugar (before optimization)
--                      = {terms: 485, types: 9,682, coercions: 44,044, joins: 0/158}
-- ReshapeStack 10:   Result size of Desugar (before optimization)
--                      = {terms: 527, types: 10,594, coercions: 49,085, joins: 0/172}
test =
  let input = UnsafeTensor @('Just ['Just 10, 'Just 10]) [10, 10]
      boxedStack = BoxedStack $ initialize @(ReshapeStack _ ('Just ['Just 1, 'Just 100]))
   in forward boxedStack input
