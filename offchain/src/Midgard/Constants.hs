module Midgard.Constants (
  hubOracleMintingScript,
  hubOracleScriptHash,
  hubOracleMintingPolicyId,
  hubOracleAssetName,
  shiftDuration,
  maturityDuration,
  maxValidityRangeLength,
  registrationDuration,
  operatorRequiredBond,
  operatorSlashingPenalty,
  refScriptStorage,
  emptyMerkleTreeRoot,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time.Clock (NominalDiffTime)

import Cardano.Api qualified as C
import Convex.Utils (scriptAddress)
import PlutusCore qualified as PLC
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.V1 (fromHex)
import PlutusLedgerApi.V3 (LedgerBytes (getLedgerBytes), serialiseUPLC)
import PlutusTx.Builtins qualified as PlutusTx
import UntypedPlutusCore qualified as UPLC

{- | A dummy script at the moment. The real hub oracle must be parameterized by a nonce UTxO
that is meant to be consumed by the same transaction.
-}
hubOracleMintingScript :: C.PlutusScript C.PlutusScriptV3
hubOracleMintingScript = C.PlutusScriptSerialised $ serialiseUPLC alwaysSucceedsUPLC

-- TODO (chase): Perhaps the storage address should be elsewhere.
refScriptStorage :: C.NetworkId -> C.AddressInEra C.ConwayEra
refScriptStorage netId = scriptAddress netId . C.PlutusScriptSerialised $ serialiseUPLC alwaysFailsUPLC

hubOracleScriptHash :: C.ScriptHash
hubOracleScriptHash = C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleMintingScript

hubOracleMintingPolicyId :: C.PolicyId
hubOracleMintingPolicyId = C.PolicyId . C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleMintingScript

hubOracleAssetName :: C.AssetName
hubOracleAssetName = C.UnsafeAssetName $ BS8.pack "MIDGARD_HUB_ORACLE"

-- | Mimicking aiken (testnet). 1 hour.
shiftDuration :: NominalDiffTime
shiftDuration = 60 * 60

-- | Mimicking aiken. 30 milliseconds.
maturityDuration :: NominalDiffTime
maturityDuration = 0.030

-- | Mimicking aiken. 30 milliseconds.
registrationDuration :: NominalDiffTime
registrationDuration = 0.030

-- | Mimicking aiken. 5 minutes.
maxValidityRangeLength :: NominalDiffTime
maxValidityRangeLength = 5 * 60

-- | Mimicking aiken.
operatorRequiredBond :: C.Lovelace
operatorRequiredBond = 0

operatorSlashingPenalty :: C.Lovelace
operatorSlashingPenalty = 3_000_000

-- | Mimicking aiken.
emptyMerkleTreeRoot :: ByteString
emptyMerkleTreeRoot = unsafeFromHex "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  where
    unsafeFromHex = PlutusTx.fromBuiltin . getLedgerBytes . either (error . show) id . fromHex

alwaysSucceedsUPLC :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni fun ()
alwaysSucceedsUPLC =
  UPLC.Program () PLC.plcVersion110 $
    UPLC.LamAbs () (UPLC.DeBruijn 0) $
      UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniUnit ()))

alwaysFailsUPLC :: UPLC.Program UPLC.DeBruijn uni fun ()
alwaysFailsUPLC =
  UPLC.Program () PLC.plcVersion110 $
    UPLC.LamAbs () (UPLC.DeBruijn 0) $
      UPLC.Error ()
