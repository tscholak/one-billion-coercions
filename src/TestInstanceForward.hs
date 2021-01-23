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
--                      = {terms: 61, types: 861, coercions: 354, joins: 0/17}
-- ReshapeStack 1:    Result size of Desugar (before optimization)
--                      = {terms: 154, types: 2,502, coercions: 3,746, joins: 0/48}
-- ReshapeStack 2:    Result size of Desugar (before optimization)
--                      = {terms: 196, types: 3,300, coercions: 7,121, joins: 0/62}
-- ReshapeStack 3:    Result size of Desugar (before optimization)
--                      = {terms: 175, types: 2,420, coercions: 10,239, joins: 0/20}
-- ReshapeStack 4:    Result size of Desugar (before optimization)
--                      = {terms: 280, types: 4,896, coercions: 13,871, joins: 0/90}
-- ReshapeStack 5:    Result size of Desugar (before optimization)
--                      = {terms: 322, types: 5,694, coercions: 17,246, joins: 0/104}
-- ReshapeStack 6:    Result size of Desugar (before optimization)
--                      = {terms: 364, types: 6,492, coercions: 20,621, joins: 0/118}
-- ReshapeStack 7:    Result size of Desugar (before optimization)
--                      = {terms: 406, types: 7,290, coercions: 23,996, joins: 0/132}
-- ReshapeStack 8:    Result size of Desugar (before optimization)
--                      = {terms: 448, types: 8,088, coercions: 27,371, joins: 0/146}
-- ReshapeStack 9:    Result size of Desugar (before optimization)
--                      =  {terms: 490, types: 8,886, coercions: 30,746, joins: 0/160}
-- ReshapeStack 10:   Result size of Desugar (before optimization)
--                      =  {terms: 532, types: 9,684, coercions: 34,121, joins: 0/174}
test =
  let input = UnsafeTensor @('Just ['Just 10, 'Just 10]) [10, 10]
      boxedStack = BoxedStack $ initialize @(ReshapeStack _ ('Just ['Just 1, 'Just 100]))
   in forward boxedStack input
