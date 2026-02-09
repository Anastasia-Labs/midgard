module Midgard.Constants (
  hubOracleMintingScript,
  hubOracleMintingPolicyId,
  hubOracleAssetName,
  hubOracleValidator,
  operatorRequiredBond,
  operatorSlashingPenalty,
  hubOracleMintingPolicyId',
) where

import Data.Coerce (coerce)

import Cardano.Api qualified as C
import Convex.Utils.String (unsafeAssetName)
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.V3 (serialiseUPLC)
import UntypedPlutusCore qualified as UPLC

{- | A dummy script at the moment. The real hub oracle must be parameterized by a nonce UTxO
that is meant to be consumed by the same transaction.
-}
hubOracleMintingScript :: C.PlutusScript C.PlutusScriptV3
hubOracleMintingScript = C.PlutusScriptSerialised $ serialiseUPLC alwaysSucceedsUPLC

-- Script to lock the hub oracle token at. Temporarily set to an "always fails" validator.s
hubOracleValidator :: C.PlutusScript C.PlutusScriptV3
hubOracleValidator = C.PlutusScriptSerialised $ serialiseUPLC alwaysFailsUPLC

hubOracleMintingPolicyId :: C.PolicyId
hubOracleMintingPolicyId = C.PolicyId . C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleMintingScript

hubOracleMintingPolicyId' :: C.ScriptHash
hubOracleMintingPolicyId' = coerce hubOracleMintingPolicyId

hubOracleAssetName :: C.AssetName
hubOracleAssetName = unsafeAssetName "cafe"

operatorRequiredBond :: C.Lovelace
operatorRequiredBond = 5_000_000

operatorSlashingPenalty :: C.Lovelace
operatorSlashingPenalty = 3_000_000

alwaysSucceedsUPLC :: UPLC.Program UPLC.DeBruijn uni fun ()
alwaysSucceedsUPLC =
  UPLC.Program () PLC.plcVersion110 $
    UPLC.LamAbs () (UPLC.DeBruijn 0) $
      UPLC.Var () (UPLC.DeBruijn 1)

alwaysFailsUPLC :: UPLC.Program UPLC.DeBruijn uni fun ()
alwaysFailsUPLC =
  UPLC.Program () PLC.plcVersion110 $
    UPLC.LamAbs () (UPLC.DeBruijn 0) $
      UPLC.Error ()
