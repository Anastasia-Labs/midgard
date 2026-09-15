{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTraceScriptSourcesStageZero (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageZero (
  scriptSourcesV1Validator,
 )
import Testing.Eval (psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace ScriptSources Stage Zero Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ scriptSourcesV1Validator
          # resolverHashes
          # pdata (pconstant ctPolicy)
          # pconstant cancelContext
    ]

cancelContext :: ScriptContext
cancelContext =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    [cancelMintEntry threadName]
    mempty

resolverHashes :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
resolverHashes = pdata $ go 29
  where
    go :: forall t. Integer -> Term t (PBuiltinList (PAsData PScriptHash))
    go 0 = pnil
    go count =
      pcons
        # pdata (pconstant $ ScriptHash $ toBuiltin $ BS.replicate 28 $ fromIntegral count)
        # go (count - 1)
