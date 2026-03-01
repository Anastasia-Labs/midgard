module Midgard.Scripts (MidgardScripts (..), readAikenScripts) where

import PlutusLedgerApi.Data.V3 (BuiltinData)
import PlutusTx.Builtins qualified as PlutusTx
import Ply

import Midgard.Constants (hubOracleMintingPolicyId)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes)
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators
import Midgard.Types.RetiredOperators qualified as RetiredOperators

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
        '[ AsDatum ActiveOperators.Datum
         , AsRedeemer ActiveOperators.SpendRedeemer
         ]
  , activeOperatorsPolicy ::
      TypedScript
        PlutusV3
        '[ AsRedeemer ActiveOperators.MintRedeemer
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
        '[ AsRedeemer RetiredOperators.MintRedeemer
         ]
  }

readAikenScripts :: IO MidgardScripts
readAikenScripts = do
  aikenBp <- readBlueprint "../onchain/aiken/plutus.json"
  registeredOperatorsValidator' <- getTypedScript aikenBp "operator_directory/registered_operators.spend.spend"
  registeredOperatorsPolicy' <- getTypedScript aikenBp "operator_directory/registered_operators.mint.mint"
  activeOperatorsValidator' <- getTypedScript aikenBp "operator_directory/active_operators.spend.spend"
  activeOperatorsPolicy' <- getTypedScript aikenBp "operator_directory/active_operators.mint.mint"
  retiredOperatorsValidator' <- getTypedScript aikenBp "operator_directory/retired_operators.spend.spend"
  retiredOperatorsPolicy' <- getTypedScript aikenBp "operator_directory/retired_operators.mint.mint"
  let retiredOperatorsPolicy =
        retiredOperatorsPolicy'
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
  let registeredOperatorsPolicy =
        registeredOperatorsPolicy'
          #! PlutusTx.toBuiltin (policyIdBytes $ mintingPolicyId retiredOperatorsPolicy)
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
  let activeOperatorsPolicy =
        activeOperatorsPolicy'
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
          #! PlutusTx.toBuiltin (policyIdBytes $ mintingPolicyId registeredOperatorsPolicy)
          #! PlutusTx.toBuiltin (policyIdBytes $ mintingPolicyId retiredOperatorsPolicy)
  let registeredOperatorsValidator =
        registeredOperatorsValidator'
          #$! PlutusTx.toBuiltin
          . policyIdBytes
          $ mintingPolicyId registeredOperatorsPolicy
  let activeOperatorsValidator =
        activeOperatorsValidator'
          #! ( PlutusTx.toBuiltin
                 . policyIdBytes
                 $ mintingPolicyId activeOperatorsPolicy
             )
          #! PlutusTx.toBuiltin (policyIdBytes hubOracleMintingPolicyId)
  let retiredOperatorsValidator =
        retiredOperatorsValidator'
          #$! PlutusTx.toBuiltin
          . policyIdBytes
          $ mintingPolicyId retiredOperatorsPolicy
  pure
    MidgardScripts
      { registeredOperatorsValidator
      , registeredOperatorsPolicy
      , activeOperatorsValidator
      , activeOperatorsPolicy
      , retiredOperatorsValidator
      , retiredOperatorsPolicy
      }
