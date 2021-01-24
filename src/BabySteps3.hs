{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -v2 -freduction-depth=0 #-}

module BabySteps3 where

import BabySteps (C (..))

type N = 1000

f :: C 'True N => T 'True N
f = ()