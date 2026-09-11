module Testing.ValidationTraceCekSplit (tests) where

import Data.ByteString qualified as BS
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekMachine (PCoreStepEvidenceV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
 )
import Midgard.ValidationResolver (PCekMaterialRouteV1, pselectSemanticResolver)
import Midgard.Validators.FraudProofs.ValidationTrace.Cek (cekV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (PPrepareSelectedActionV1 (..))
import Testing.Eval (
  passertEvalNoTrace,
  pfailsNoTraceWithoutHoistChecks,
  psucceedsNoTraceWithoutHoistChecks,
 )
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace CEK Split Validators"
    [ testCase "split action wire layouts match Aiken" $
        passertEvalNoTrace splitActionWireLayouts
    , testCase "CEK preparation group routes exactly four semantic resolvers" $
        passertEvalNoTrace resolverSelectionMatrix
    , testCase "core-step auxiliary wraps one evidence value" $
        passertEvalNoTrace coreStepAuxiliaryWireLayout
    , testCase "CEK preparation validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledPrepareWith threadName
    , testCase "finish validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledFinishWith threadName
    , testCase "execution-selection validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledSelectionWith threadName
    , testCase "context-step validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledContextWith threadName
    , testCase "core-step validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledCoreWith threadName
    , testCase "split validators reject cancellation of another thread" $
        pfailsNoTraceWithoutHoistChecks $
          cancelledFinishWith otherThreadName
    ]

splitActionWireLayouts :: forall s. Term s PBool
splitActionWireLayouts =
  let transition :: Term s (PAsData PValidationOneStepWitnessV1)
      transition = punsafeCoerce (pdata (pconstant 3 :: Term s PInteger))
      auxiliary :: Term s (PAsData PValidationAuxiliaryWitnessV1)
      auxiliary = punsafeCoerce (pdata (pconstant 4 :: Term s PInteger))
      materialRoute :: Term s (PAsData PCekMaterialRouteV1)
      materialRoute = punsafeCoerce (pdata (pconstant 5 :: Term s PInteger))
      coreStep :: Term s (PAsData PCoreStepEvidenceV1)
      coreStep = punsafeCoerce (pdata (pconstant 4 :: Term s PInteger))
   in pand'List
        [ pforgetData (pdata $ pcon $ PPrepareSelected (pdata 1) (pdata 2) (pdata 3) transition (pforgetData auxiliary))
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 3, PD.I 4])
        , pforgetData (pdata $ pcon $ PVerifyFinish (pdata 1) (pdata 2) transition)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3])
        , pforgetData
            ( pdata $
                pcon $
                  PVerifyExecutionSelection
                    (pdata 1)
                    (pdata 2)
                    transition
                    auxiliary
                    materialRoute
            )
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.I 5])
        , pforgetData
            (pdata $ pcon $ PVerifyContextStep (pdata 1) (pdata 2) transition auxiliary)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 4])
        , pforgetData
            (pdata $ pcon $ PVerifyCoreStep (pdata 1) (pdata 2) transition coreStep)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 4])
        ]

coreStepAuxiliaryWireLayout :: forall s. Term s PBool
coreStepAuxiliaryWireLayout =
  let coreStep :: Term s (PAsData PCoreStepEvidenceV1)
      coreStep = punsafeCoerce (pdata (pconstant 4 :: Term s PInteger))
   in pforgetData (pdata $ pcon $ PCekCoreStepWitness coreStep)
        #== pconstant (PD.Constr 12 [PD.I 4])

cancelledFinishWith :: forall s. BS.ByteString -> Term s PUnit
cancelledFinishWith cancellationName =
  cekFinishSemanticV1Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledPrepareWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPrepareWith cancellationName =
  cekV1Validator
    # resolverHashes
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledSelectionWith :: forall s. BS.ByteString -> Term s PUnit
cancelledSelectionWith cancellationName =
  cekExecutionSelectionSemanticV1Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant $ ScriptHash $ toBuiltin materialScript)
    # pconstant (cancelContext cancellationName)

cancelledContextWith :: forall s. BS.ByteString -> Term s PUnit
cancelledContextWith cancellationName =
  cekContextStepSemanticV1Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledCoreWith :: forall s. BS.ByteString -> Term s PUnit
cancelledCoreWith cancellationName =
  cekCoreStepSemanticV1Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelContext :: BS.ByteString -> ScriptContext
cancelContext cancellationName =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    [cancelMintEntry cancellationName]
    mempty

materialScript :: BS.ByteString
materialScript = BS.replicate 28 0x81

resolverHashes :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
resolverHashes = pdata resolverHashList

resolverHashList :: forall s. Term s (PBuiltinList (PAsData PScriptHash))
resolverHashList =
  pcons
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # ( pcons
          # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
          # ( pcons
                # pdata (pconstant $ ScriptHash $ toBuiltin materialScript)
                # ( pcons
                      # pdata (pconstant $ ScriptHash $ toBuiltin (BS.replicate 28 0x82))
                      # pnil
                  )
            )
      )

resolverSelectionMatrix :: forall s. Term s PBool
resolverSelectionMatrix =
  pand'List
    [ selectedResolverIs 0 nextScript
    , selectedResolverIs 1 otherScript
    , selectedResolverIs 2 materialScript
    , selectedResolverIs 3 (BS.replicate 28 0x82)
    , resolverSelectionFails resolverHashList 1 0
    , resolverSelectionFails resolverHashList 4 (-1)
    , resolverSelectionFails resolverHashList 4 4
    , resolverSelectionFails
        (pcons # pdata (pconstant $ ScriptHash $ toBuiltin (BS.replicate 28 0x83)) # resolverHashList)
        4
        0
    ]
  where
    selectedResolverIs :: Integer -> BS.ByteString -> Term s PBool
    selectedResolverIs index expected =
      pmatch (pselectSemanticResolver # resolverHashList # 4 # pconstant index) $ \case
        PJust scriptHash -> scriptHash #== pconstant (ScriptHash $ toBuiltin expected)
        PNothing -> pconstant False

resolverSelectionFails ::
  forall s.
  Term s (PBuiltinList (PAsData PScriptHash)) ->
  Integer ->
  Integer ->
  Term s PBool
resolverSelectionFails hashes expectedCount selectedIndex =
  pmatch
    (pselectSemanticResolver # hashes # pconstant expectedCount # pconstant selectedIndex)
    $ \case
      PNothing -> pconstant True
      PJust _ -> pconstant False
