{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Constants where

import Plutarch.LedgerApi.V1 (PCurrencySymbol (..), PPubKeyHash (..), PTokenName (..))
import Plutarch.Prelude
import PlutusLedgerApi.V1 (CurrencySymbol (CurrencySymbol), TokenName (TokenName))

-- import Plutarch.Builtin (PDataNewtype(..))
import Plutarch.MerkleTree.PatriciaForestry

-- | Defines the reference Patricia-forestry root used by the test constants.
-- | Canonical empty state root used by the demo constants module.
stateRoot :: ClosedTerm PMerklePatriciaForestry
stateRoot = pfrom_root # phexByteStr "74c61b3b5584c4434f03bc9acbe31d2d2186576e257f1fd85c997916d6df5715"
-- | Defines the placeholder Midgard operator key hash constant.

-- | Placeholder operator key hash used in test and demo terms.
midgardOperator :: ClosedTerm (PAsData PPubKeyHash)
-- | Defines the placeholder fraud-token name constant.
midgardOperator = pconstant "deadbeef"

-- | Token name reserved for the fraud token in test fixtures.
fraudTokenTN :: ClosedTerm PTokenName
fraudTokenTN =
  let tn :: TokenName
-- | Defines the placeholder fraud-token currency symbol constant.
      tn = TokenName "Fraud"
   in pconstant tn

-- | Placeholder currency symbol for the fraud token in test fixtures.
fraudTokenCS :: ClosedTerm PCurrencySymbol
fraudTokenCS =
  let cs :: CurrencySymbol
      cs = CurrencySymbol "deadbeef"
   in pconstant cs
