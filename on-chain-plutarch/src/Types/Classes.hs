{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Classes where

import Plutarch.Prelude

class ScottConvertible (a :: PType) where
  type ScottOf a = (b :: PType) | b -> a
  toScott :: Term s a -> Term s (ScottOf a)
  fromScott :: Term s (ScottOf a) -> Term s a
