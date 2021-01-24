{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -v2 -freduction-depth=0 #-}

-- toggle AB
module Test0 where

import BabySteps (C (..))

type N = 0

f :: C 'False N => T 'False N
f = ()
