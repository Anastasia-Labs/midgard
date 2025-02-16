{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Constants where

import Plutarch
import Plutarch.LedgerApi.V1 (PTokenName (..), PCurrencySymbol(..), PPubKeyHash(..))
import Plutarch.Prelude
import PlutusLedgerApi.V1 (TokenName, CurrencySymbol)
--import Plutarch.Builtin (PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry

stateRoot :: ClosedTerm PMerklePatriciaForestry
stateRoot = pfrom_root # phexByteStr "74c61b3b5584c4434f03bc9acbe31d2d2186576e257f1fd85c997916d6df5715"

midgardOperator :: ClosedTerm (PAsData PPubKeyHash)
midgardOperator = pconstantData "deadbeef"

fraudTokenTN :: ClosedTerm PTokenName
fraudTokenTN =
  let tn :: TokenName
      tn = "Fraud"
   in pconstant tn

fraudTokenCS :: ClosedTerm PCurrencySymbol
fraudTokenCS =
  let cs :: CurrencySymbol
      cs = "deadbeef"
   in pconstant cs

