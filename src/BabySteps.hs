{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -ddump-ds-preopt -freduction-depth=0 #-}

module BabySteps where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+), type (-), type (<=?))

-- data Unary = S Unary | Z

type family ExpensiveF (n :: Nat) = (r :: Type) where
  ExpensiveF 0 = ()
  ExpensiveF n = ExpensiveF (n - 1)

data Stack (n :: Nat) where
  Nil :: Stack 0
  Cons :: forall n. Stack n -> Stack (n + 1)

class C (isCons :: Bool) (n :: Nat) where
  type T isCons n :: Type

instance C 'False 0 where
  type T 'False 0 = ()

instance
  ( C (1 <=? n - 1) (n - 1),
    ExpensiveF n ~ ()
  ) =>
  C 'True n
  where
  type T 'True n = T (1 <=? n - 1) (n - 1)
