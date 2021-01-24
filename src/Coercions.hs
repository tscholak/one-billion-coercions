{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -v2
                -freduction-depth=0
                -fplugin GHC.TypeLits.Normalise
                -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin GHC.TypeLits.Extra.Solver #-}

module Coercions where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, natVal, Nat, type (*), type (+), type (-), type (<=?))
import Type.Errors.Pretty (TypeError, type (%), type (<>))
import Data.Proxy (Proxy(..))
import Data.Coerce (coerce)

class HasForward f a where
  type ForwardOutput f a :: Type
  forward :: f -> a -> ForwardOutput f a

class HasInitialize f where
  type Initialize f :: Type
  initialize :: Initialize f

data Tensor (shape :: Maybe [Maybe Nat]) where
  UnsafeTensor :: forall shape. [Integer] -> Tensor shape

shape ::
  forall shape. KnownShape shape => Tensor shape -> [Integer]
shape (UnsafeTensor unsafeDims) =
  case shapeVal @shape of
    Nothing -> unsafeDims
    Just dims ->
      let sizeError size size' =
            error $
              "The compile- and runtime dimension sizes are not the same, '"
                <> show size
                <> "' != '"
                <> show size'
                <> "'. Please open a ticket on GitHub."
          f Nothing size' = size'
          f (Just size) size'
            | size == size' = size
            | otherwise = sizeError size size'
       in zipWith f dims unsafeDims

type ReshapeNumelMismatchMessage (numel :: Nat) (numel' :: Nat) (shape :: Maybe [Maybe Nat]) (shape' :: Maybe [Maybe Nat]) =
  "Cannot reshape the tensor. The original shape,"
    % ""
    % "    '" <> shape <> "',"
    % ""
    % "and the new shape,"
    % ""
    % "    '" <> shape' <> "',"
    % ""
    % "have different total numbers of elements,"
    % ""
    % "    '" <> numel <> "' versus '" <> numel' <> "',"
    % ""
    % "respectively."

type family NumelDimF (dim :: Maybe Nat) :: Maybe Nat where
  NumelDimF 'Nothing = 'Nothing
  NumelDimF ('Just size) = 'Just size

type family NumelDimsImplF (a :: Maybe Nat) (b :: Maybe Nat) :: Maybe Nat where
  NumelDimsImplF 'Nothing _ = 'Nothing
  NumelDimsImplF ('Just _) 'Nothing = 'Nothing
  NumelDimsImplF ('Just a) ('Just b) = 'Just (a * b)

type family NumelDimsF (dims :: [Maybe Nat]) :: Maybe Nat where
  NumelDimsF '[] = 'Just 1
  NumelDimsF (dim ': dims) = NumelDimsImplF (NumelDimF dim) (NumelDimsF dims)

type family NumelF (shape :: Maybe [Maybe Nat]) :: Maybe Nat where
  NumelF 'Nothing = 'Nothing
  NumelF ('Just dims) = NumelDimsF dims

type family ReshapeImplF (numel :: Maybe Nat) (numel' :: Maybe Nat) (shape :: Maybe [Maybe Nat]) (shape' :: Maybe [Maybe Nat]) :: Maybe [Maybe Nat] where
  ReshapeImplF ('Just numel) ('Just numel) _ shape' = shape'
  ReshapeImplF ('Just numel) ('Just numel') shape shape' = TypeError (ReshapeNumelMismatchMessage numel numel' shape shape')
  ReshapeImplF 'Nothing _ _ _ = 'Nothing
  ReshapeImplF _ 'Nothing _ _ = 'Nothing
  ReshapeImplF _ _ 'Nothing _ = 'Nothing
  ReshapeImplF _ _ _ 'Nothing = 'Nothing

type family ReshapeF (shape :: Maybe [Maybe Nat]) (shape' :: Maybe [Maybe Nat]) :: Maybe [Maybe Nat] where
  ReshapeF shape shape' = ReshapeImplF (NumelF shape) (NumelF shape') shape shape'

class KnownDim (dim :: Maybe Nat) where
  dimVal :: Maybe Integer

instance KnownDim 'Nothing where
  dimVal = Nothing

instance (KnownNat size) => KnownDim ('Just size) where
  dimVal = Just . natVal $ Proxy @size

class KnownShape (shape :: Maybe [Maybe Nat]) where
  shapeVal :: Maybe [Maybe Integer]

instance KnownShape 'Nothing where
  shapeVal = Nothing

instance KnownShape ('Just '[]) where
  shapeVal = Just []

instance (KnownShape ('Just dims), KnownDim dim) => KnownShape ('Just (dim ': dims)) where
  shapeVal =
    case shapeVal @('Just dims) of
      Just dims -> Just $ dimVal @dim : dims

class WithShapeC (shape :: Maybe [Maybe Nat]) (f :: Type) where
  type WithShapeF shape f :: Type
  withShape :: ([Integer] -> f) -> WithShapeF shape f
  withoutShape :: WithShapeF shape f -> ([Integer] -> f)

instance WithShapeC 'Nothing f where
  type WithShapeF 'Nothing f = [Integer] -> f
  withShape = id
  withoutShape = id

instance WithShapeC ('Just '[]) f where
  type WithShapeF ('Just '[]) f = f
  withShape f = f []
  withoutShape = const

instance
  (WithShapeC ('Just dims) f) =>
  WithShapeC ('Just ('Nothing ': dims)) f
  where
  type WithShapeF ('Just ('Nothing ': dims)) f = Integer -> WithShapeF ('Just dims) f
  withShape f dim = withShape @('Just dims) @f $ \dims -> f (dim : dims)
  withoutShape f (dim : dims) = withoutShape @('Just dims) @f (f dim) dims

instance
  (WithShapeC ('Just dims) f, KnownNat size) =>
  WithShapeC ('Just ('Just size ': dims)) f
  where
  type WithShapeF ('Just ('Just size ': dims)) f = WithShapeF ('Just dims) f
  withShape f = withShape @('Just dims) @f $ \dims -> f (natVal (Proxy @size) : dims)
  withoutShape f (_ : dims) = withoutShape @('Just dims) @f f dims

reshape ::
  forall shape inputShape outputShape.
  ( WithShapeC shape (Tensor inputShape -> Tensor outputShape),
    outputShape ~ ReshapeF inputShape shape
  ) =>
  WithShapeF shape (Tensor inputShape -> Tensor outputShape)
reshape = withShape @shape @(Tensor inputShape -> Tensor outputShape) $
  \dims (UnsafeTensor _) -> UnsafeTensor dims

data ReshapeBlock (shape :: Maybe [Maybe Nat]) where
  ReshapeBlock :: forall shape. [Integer] -> ReshapeBlock shape

instance
  WithShapeC shape (ReshapeBlock shape) =>
  HasInitialize (ReshapeBlock shape)
  where
    type Initialize (ReshapeBlock shape) = WithShapeF shape (ReshapeBlock shape)
    initialize = withShape @shape $ ReshapeBlock @shape

instance
  ( KnownShape inputShape,
    WithShapeC shape (Tensor inputShape -> Tensor (ReshapeF inputShape shape)),
    WithShapeC inputShape (Tensor (ReshapeF inputShape shape) -> Tensor (ReshapeF (ReshapeF inputShape shape) inputShape))
  ) =>
  HasForward
    (ReshapeBlock shape)
    (Tensor inputShape)
  where
  type ForwardOutput (ReshapeBlock shape) (Tensor inputShape) = Tensor (ReshapeF (ReshapeF inputShape shape) inputShape)
  forward (ReshapeBlock dims') input =
    let reshapedInput :: Tensor (ReshapeF inputShape shape) =
          withoutShape @shape
            (reshape @shape @inputShape)
            dims'
            input
     in withoutShape @inputShape
          (reshape @inputShape @(ReshapeF inputShape shape))
          (shape input)
          reshapedInput

data ReshapeStack (numLayers :: Nat) (shape :: Maybe [Maybe Nat]) where
  ReshapeStackNil ::
    forall shape.
    ReshapeStack 0 shape
  ReshapeStackCons ::
    forall numLayers shape.
    ReshapeBlock shape ->
    ReshapeStack numLayers shape ->
    ReshapeStack (numLayers + 1) shape

class
  HasInitializeReshapeStack
    (isCons :: Bool)
    (numLayers :: Nat)
    (shape :: Maybe [Maybe Nat])
  where
  initializeReshapeStack :: WithShapeF shape (ReshapeStack numLayers shape)

instance
  WithShapeC shape (ReshapeStack 0 shape) =>
  HasInitializeReshapeStack 'False 0 shape
  where
    initializeReshapeStack = withShape @shape $ \_dims -> ReshapeStackNil @shape

instance
  (((numLayers - 1) + 1) ~ numLayers,
   WithShapeC shape (ReshapeStack numLayers shape),
   WithShapeC shape (ReshapeBlock shape),
   WithShapeC shape (ReshapeStack (numLayers - 1) shape),
   HasInitializeReshapeStack (1 <=? (numLayers - 1)) (numLayers - 1) shape
  ) =>
  HasInitializeReshapeStack 'True numLayers shape
  where
    initializeReshapeStack = withShape @shape $ \dims ->
      let block = withoutShape @shape (initialize @(ReshapeBlock shape)) dims
          stack = withoutShape @shape (initialize @(ReshapeStack (numLayers - 1) shape)) dims
       in ReshapeStackCons @(numLayers - 1) @shape block stack

instance
  HasInitializeReshapeStack (1 <=? numLayers) numLayers shape =>
  HasInitialize (ReshapeStack numLayers shape) where
  type Initialize (ReshapeStack numLayers shape) = WithShapeF shape (ReshapeStack numLayers shape)
  initialize = initializeReshapeStack @(1 <=? numLayers) @numLayers @shape

class
  HasForwardReshapeStack
    (isCons :: Bool)
    (isSecondLayer :: Bool)
    (numLayers :: Nat)
    (shape :: Maybe [Maybe Nat])
    inputShape
  where
  type ForwardReshapeStackOutput isCons isSecondLayer numLayers shape inputShape :: Type
  forwardReshapeStack ::
    Maybe (ReshapeBlock shape -> Tensor inputShape -> Tensor inputShape) ->
    ReshapeStack numLayers shape ->
    Tensor inputShape ->
    ForwardReshapeStackOutput isCons isSecondLayer numLayers shape inputShape

instance HasForwardReshapeStack 'False isSecondLayer 0 shape inputShape where
  type
    ForwardReshapeStackOutput 'False isSecondLayer 0 shape inputShape =
      Tensor inputShape
  forwardReshapeStack _ ReshapeStackNil input = input

instance
  ( KnownShape inputShape,
    WithShapeC shape (Tensor inputShape -> Tensor (ReshapeF inputShape shape)),
    WithShapeC inputShape (Tensor (ReshapeF inputShape shape) -> Tensor outputShape),
    outputShape ~ ReshapeF (ReshapeF inputShape shape) inputShape,
    KnownShape outputShape,
    WithShapeC shape (Tensor outputShape -> Tensor (ReshapeF outputShape shape)),
    WithShapeC outputShape (Tensor (ReshapeF outputShape shape) -> Tensor outputShape),
    outputShape ~ ReshapeF (ReshapeF outputShape shape) outputShape,
    HasForwardReshapeStack (1 <=? numLayers - 1) 'True (numLayers - 1) shape outputShape
  ) =>
  HasForwardReshapeStack 'True 'False numLayers shape inputShape
  where
  type
    ForwardReshapeStackOutput 'True 'False numLayers shape inputShape =
      ForwardReshapeStackOutput (1 <=? numLayers - 1) 'True (numLayers - 1) shape (ReshapeF (ReshapeF inputShape shape) inputShape)
  forwardReshapeStack _ (ReshapeStackCons block stack) input =
    let reshaped = forward block input
     in forwardReshapeStack @(1 <=? numLayers - 1) @'True @(numLayers - 1) @shape @outputShape (Just forward) stack reshaped

instance
  ( HasForwardReshapeStack (1 <=? numLayers - 1) 'True (numLayers - 1) shape inputShape,
    ForwardReshapeStackOutput (1 <=? numLayers - 1) 'True (numLayers - 1) shape inputShape ~ Tensor inputShape
  ) =>
  HasForwardReshapeStack 'True 'True numLayers shape inputShape
  where
  type
    ForwardReshapeStackOutput 'True 'True numLayers shape inputShape = Tensor inputShape
  forwardReshapeStack (Just f) (ReshapeStackCons block stack) input =
    let reshaped = f block input
     in forwardReshapeStack @(1 <=? numLayers - 1) @'True @(numLayers - 1) @shape @inputShape (Just f) stack reshaped

instance
  HasForwardReshapeStack (1 <=? numLayers) 'False numLayers shape inputShape =>
  HasForward
    (ReshapeStack numLayers shape)
    (Tensor inputShape)
  where
  type
    ForwardOutput (ReshapeStack numLayers shape) (Tensor inputShape) =
      ForwardReshapeStackOutput (1 <=? numLayers) 'False numLayers shape inputShape
  forward = forwardReshapeStack @(1 <=? numLayers) @'False @numLayers @shape @inputShape Nothing
