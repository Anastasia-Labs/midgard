{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Constants where

import Plutarch.LedgerApi.V1 (PCurrencySymbol (..), PPubKeyHash (..), PTokenName (..))
import Plutarch.Prelude
import PlutusLedgerApi.V1 (CurrencySymbol (CurrencySymbol), TokenName (TokenName))

-- import Plutarch.Builtin (PDataNewtype(..))
import Plutarch.MerkleTree.PatriciaForestry

stateRoot :: ClosedTerm PMerklePatriciaForestry
stateRoot = pfrom_root # phexByteStr "74c61b3b5584c4434f03bc9acbe31d2d2186576e257f1fd85c997916d6df5715"

midgardOperator :: ClosedTerm (PAsData PPubKeyHash)
midgardOperator = pconstant "deadbeef"

fraudTokenTN :: ClosedTerm PTokenName
fraudTokenTN =
  let tn :: TokenName
      tn = TokenName "Fraud"
   in pconstant tn

fraudTokenCS :: ClosedTerm PCurrencySymbol
fraudTokenCS =
  let cs :: CurrencySymbol
      cs = CurrencySymbol "deadbeef"
   in pconstant cs
