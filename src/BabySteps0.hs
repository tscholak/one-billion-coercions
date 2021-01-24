{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -v2 -freduction-depth=0 #-}

module BabySteps0 where

import BabySteps (C (..))

type N = 1

f :: C 'True N => T 'True N
f = ()