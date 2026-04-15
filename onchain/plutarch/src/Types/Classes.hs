{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Classes where

import Plutarch.Internal.Term (PType)
import Plutarch.Prelude

class ScottConvertible (a :: PType) where
  type ScottOf a = (b :: PType) | b -> a
  -- | Converts a Plutarch value into its Scott-encoded representation.
  toScott :: Term s a -> Term s (ScottOf a)
  -- | Reconstructs a Plutarch value from its Scott-encoded representation.
  fromScott :: Term s (ScottOf a) -> Term s a
