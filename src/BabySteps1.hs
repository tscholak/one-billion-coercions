{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -v2 -freduction-depth=0 #-}

module BabySteps1 where

import BabySteps (C (..))

type N = 10

f :: C 'True N => T 'True N
f = ()