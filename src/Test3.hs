{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -v2 -freduction-depth=0 #-}

module Test3 where

import BabySteps (C (..))

type N = 3

f :: C 'True N => T 'True N
f = ()
