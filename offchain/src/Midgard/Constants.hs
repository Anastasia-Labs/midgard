module Midgard.Constants (
  hubOracleMintingScript,
  hubOracleMintingPolicyId,
  hubOracleAssetName,
  operatorRequiredBond,
  operatorSlashingPenalty,
) where

import Cardano.Api qualified as C
import Convex.Utils.String (unsafeAssetName)

hubOracleMintingScript :: C.PlutusScript C.PlutusScriptV1
hubOracleMintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint

hubOracleMintingPolicyId :: C.PolicyId
hubOracleMintingPolicyId = C.PolicyId . C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleMintingScript

hubOracleAssetName :: C.AssetName
hubOracleAssetName = unsafeAssetName "cafe"

operatorRequiredBond :: C.Lovelace
operatorRequiredBond = 5_000_000

operatorSlashingPenalty :: C.Lovelace
operatorSlashingPenalty = 3_000_000
