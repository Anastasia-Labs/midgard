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
  , activeOperatorsValidator ::
      TypedScript
        PlutusV3
        '[ AsDatum BuiltinData
         , AsRedeemer BuiltinData
         ]
  , activeOperatorsPolicy ::
      TypedScript
        PlutusV3
        '[ AsRedeemer RegisteredOperators.MintRedeemer
         ]
  , retiredOperatorsValidator ::
      TypedScript
        PlutusV3
        '[ AsDatum BuiltinData
         , AsRedeemer BuiltinData
         ]
  , retiredOperatorsPolicy ::
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
  activeOperatorsValidator' <- getTypedScript aikenBp "active_operators.spend.spend"
  activeOperatorsPolicy' <- getTypedScript aikenBp "active_operators.mint.mint"
  retiredOperatorsValidator' <- getTypedScript aikenBp "retired_operators.spend.spend"
  retiredOperatorsPolicy' <- getTypedScript aikenBp "retired_operators.mint.mint"
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
  let activeOperatorsPolicy =
        activeOperatorsPolicy'
          #! toInteger operatorSlashingPenalty
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
          #! PlutusTx.toBuiltin (C.serialiseToRawBytes hubOracleAssetName)
  let activeOperatorsValidator =
        activeOperatorsValidator'
          #! ( PlutusTx.toBuiltin
                 . policyIdBytes
                 $ mintingPolicyId registeredOperatorsPolicy
             )
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
          #! PlutusTx.toBuiltin (C.serialiseToRawBytes hubOracleAssetName)
  let retiredOperatorsPolicy =
        retiredOperatorsPolicy'
          #! toInteger operatorSlashingPenalty
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
          #! PlutusTx.toBuiltin (C.serialiseToRawBytes hubOracleAssetName)
  let retiredOperatorsValidator =
        retiredOperatorsValidator'
          #$! PlutusTx.toBuiltin
          . policyIdBytes
          $ mintingPolicyId registeredOperatorsPolicy
  pure
    MidgardScripts
      { registeredOperatorsValidator
      , registeredOperatorsPolicy
      , activeOperatorsValidator
      , activeOperatorsPolicy
      , retiredOperatorsValidator
      , retiredOperatorsPolicy
      }
