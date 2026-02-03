{-# LANGUAGE TypeFamilyDependencies #-}

module Midgard.ScriptUtils (mintingPolicyId, validatorHash, policyIdBytes, scriptHashBytes, toMintingPolicy, toValidator) where

import Data.ByteString (ByteString)
import Data.Kind (Type)

import Cardano.Api qualified as C
import PlutusLedgerApi.Common (serialiseUPLC)
import Ply

mintingPolicyId ::
  forall version any.
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  TypedScript version '[AsRedeemer any] -> C.PolicyId
mintingPolicyId = C.scriptPolicyId . C.PlutusScript C.plutusScriptVersion . toMintingPolicy

validatorHash ::
  forall version any0 any.
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  TypedScript version '[AsDatum any0, AsRedeemer any] -> C.ScriptHash
validatorHash = C.hashScript . C.PlutusScript C.plutusScriptVersion . toValidator

policyIdBytes :: C.PolicyId -> ByteString
policyIdBytes = C.serialiseToRawBytes

scriptHashBytes :: C.ScriptHash -> ByteString
scriptHashBytes = C.serialiseToRawBytes

toMintingPolicy ::
  forall version any.
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  TypedScript version '[AsRedeemer any] -> C.PlutusScript (TransPlutusVersion version)
toMintingPolicy (TypedScript' script) = C.PlutusScriptSerialised $ serialiseUPLC script

toValidator ::
  forall version any0 any.
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  TypedScript version '[AsDatum any0, AsRedeemer any] -> C.PlutusScript (TransPlutusVersion version)
toValidator (TypedScript script) = C.PlutusScriptSerialised $ serialiseUPLC script

type TransPlutusVersion :: Ply.PlutusVersion -> Type
type family TransPlutusVersion x = r | r -> x where
  TransPlutusVersion Ply.PlutusV1 = C.PlutusScriptV1
  TransPlutusVersion Ply.PlutusV2 = C.PlutusScriptV2
  TransPlutusVersion Ply.PlutusV3 = C.PlutusScriptV3
