module Midgard.Constants (
  hubOracleMintingScript,
  hubOracleMintingPolicyId,
  hubOracleAssetName,
  operatorRequiredBond,
  operatorSlashingPenalty,
) where

import Cardano.Api qualified as C
import Convex.Utils.String (unsafeAssetName)
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.V3 (serialiseUPLC)
import UntypedPlutusCore qualified as UPLC

hubOracleMintingScript :: C.PlutusScript C.PlutusScriptV3
hubOracleMintingScript = C.PlutusScriptSerialised $ serialiseUPLC alwaysSucceedsUPLC

hubOracleMintingPolicyId :: C.PolicyId
hubOracleMintingPolicyId = C.PolicyId . C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleMintingScript

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
