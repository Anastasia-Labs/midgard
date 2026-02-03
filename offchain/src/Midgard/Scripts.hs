module Midgard.Scripts (MidgardScripts (..), readAikenScripts) where

import PlutusLedgerApi.Data.V3 (BuiltinData)
import Ply

import Cardano.Api qualified as C
import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId, operatorRequiredBond, operatorSlashingPenalty)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes)
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators
import PlutusTx.Builtins qualified as PlutusTx

data MidgardScripts = MidgardScripts
  { registeredOperatorsValidator ::
      TypedScript
        PlutusV3
        '[ AsDatum BuiltinData
         , AsRedeemer BuiltinData
         ]
  , registeredOperatorsPolicy ::
      TypedScript
        PlutusV3
        '[ AsRedeemer RegisteredOperators.MintRedeemer
         ]
  }

readAikenScripts :: IO MidgardScripts
readAikenScripts = do
  aikenBp <- readBlueprint "../onchain/aiken/plutus.json"
  registeredOperatorsValidator' <- getTypedScript aikenBp "registered_operators.spend.spend"
  registeredOperatorsPolicy' <- getTypedScript aikenBp "registered_operators.mint.mint"
  let registeredOperatorsPolicy =
        registeredOperatorsPolicy'
          #! toInteger operatorRequiredBond
          #! toInteger operatorSlashingPenalty
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
          #! PlutusTx.toBuiltin (C.serialiseToRawBytes hubOracleAssetName)
  let registeredOperatorsValidator =
        registeredOperatorsValidator'
          #$! PlutusTx.toBuiltin
          . policyIdBytes
          $ mintingPolicyId registeredOperatorsPolicy
  pure MidgardScripts {registeredOperatorsValidator, registeredOperatorsPolicy}
