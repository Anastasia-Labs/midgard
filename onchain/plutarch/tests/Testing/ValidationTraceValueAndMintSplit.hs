module Testing.ValidationTraceValueAndMintSplit (tests) where

import Data.ByteString qualified as BS
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationResolver (pselectSemanticResolver)
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint (valueAndMintV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMintSemantics
import Midgard.ValueAssetFold qualified as Fold
import Testing.Eval (passertEvalNoTrace, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace ValueAndMint Split Validators"
    [ testCase "split action wire layouts match Aiken" $
        passertEvalNoTrace splitActionWireLayouts
    , testCase "ValueAndMint preparation group routes exactly eleven semantic resolvers" $
        passertEvalNoTrace resolverSelectionMatrix
    , testCase "ValueAndMint preparation validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledPrepareWith threadName
    , testCase "begin semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledSemanticWith valueAndMintBeginSemanticV1Validator threadName
    , testCase "replay-input semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledSemanticWith valueAndMintReplayInputSemanticV1Validator threadName
    , testCase "replay-asset semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledYieldingSemanticWith valueAndMintReplayAssetSemanticV1Validator threadName
    , testCase "output-descriptor semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledSemanticWith valueAndMintOutputDescriptorSemanticV1Validator threadName
    , testCase "output-asset semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledYieldingSemanticWith valueAndMintOutputAssetSemanticV1Validator threadName
    , testCase "mint-asset semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledYieldingSemanticWith valueAndMintMintAssetSemanticV1Validator threadName
    , testCase "finalize semantic validator cancels its own computation thread" $
        psucceedsNoTraceWithoutHoistChecks $
          cancelledSemanticWith valueAndMintFinalizeSemanticV1Validator threadName
    ]

splitActionWireLayouts :: forall s. Term s PBool
splitActionWireLayouts =
  let transition :: Term s (PAsData PValidationOneStepWitnessV1)
      transition = punsafeCoerce (pdata (pconstant 3 :: Term s PInteger))
      intD :: Term s (PAsData PInteger)
      intD = pdata 4
      bytesD :: Term s (PAsData PByteString)
      bytesD = pdata (pconstant "x")
      siblingsD :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      siblingsD = punsafeCoerce (pdata (pconstant 6 :: Term s PInteger))
      claimD :: Term s (PAsData Fold.PClaim)
      claimD = punsafeCoerce (pdata (pconstant 8 :: Term s PInteger))
   in pand'List
        [ pforgetData (pdata $ pcon $ PVerifyValueAndMintSimple (pdata 1) (pdata 2) transition)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3])
        , pforgetData
            (pdata $ pcon $ PVerifyValueAndMintReplayInput (pdata 1) (pdata 2) transition intD bytesD bytesD bytesD)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.B "x", PD.B "x", PD.B "x"])
        , pforgetData
            (pdata $ pcon $ PVerifyValueAndMintReplayAsset claimD (pdata 1) (pdata 2) transition intD bytesD bytesD intD)
            #== pconstant (PD.Constr 0 [PD.I 8, PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.B "x", PD.B "x", PD.I 4])
        , pforgetData
            (pdata $ pcon $ PVerifyValueAndMintOutputDescriptor (pdata 1) (pdata 2) transition intD bytesD siblingsD)
            #== pconstant (PD.Constr 0 [PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.B "x", PD.I 6])
        , pforgetData
            (pdata $ pcon $ PVerifyValueAndMintOutputAsset claimD (pdata 1) (pdata 2) transition intD intD)
            #== pconstant (PD.Constr 0 [PD.I 8, PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.I 4])
        , pforgetData
            (pdata $ pcon $ PVerifyValueAndMintMintAsset claimD (pdata 1) (pdata 2) transition intD siblingsD intD)
            #== pconstant (PD.Constr 0 [PD.I 8, PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.I 6, PD.I 4])
        ]

cancelledPrepareWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPrepareWith cancellationName =
  valueAndMintV1Validator
    # resolverHashes
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledSemanticWith ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit) ->
  BS.ByteString ->
  Term s PUnit
cancelledSemanticWith validator cancellationName =
  validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledYieldingSemanticWith ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit) ->
  BS.ByteString ->
  Term s PUnit
cancelledYieldingSemanticWith validator cancellationName =
  validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
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

resolverHashes :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
resolverHashes = pdata resolverHashList

resolverHashList :: forall s. Term s (PBuiltinList (PAsData PScriptHash))
resolverHashList = go 0
 where
  go :: Integer -> Term s (PBuiltinList (PAsData PScriptHash))
  go index
    | index == 11 = pnil
    | otherwise =
        pcons
          # pdata (pconstant $ ScriptHash $ toBuiltin $ resolverScriptAt index)
          # go (index + 1)

resolverScriptAt :: Integer -> BS.ByteString
resolverScriptAt 0 = nextScript
resolverScriptAt 1 = otherScript
resolverScriptAt 2 = materialScript
resolverScriptAt index = BS.replicate 28 (fromInteger $ 0x7f + index)

resolverSelectionMatrix :: forall s. Term s PBool
resolverSelectionMatrix =
  pand'List
    ( [ selectedResolverIs index
      | index <- [0 .. 10]
      ]
        <> [ pnot # resolverSelectionFails resolverHashList 1 0
           , resolverSelectionFails resolverHashList 11 (-1)
           , resolverSelectionFails resolverHashList 11 11
           , pnot
               # resolverSelectionFails
                 (pcons # pdata (pconstant $ ScriptHash $ toBuiltin $ BS.replicate 28 0x8a) # resolverHashList)
                 11
                 0
           ]
    )
 where
  selectedResolverIs :: Integer -> Term s PBool
  selectedResolverIs index =
    pmatch (pselectSemanticResolver # resolverHashList # 11 # pconstant index) $ \case
      PJust scriptHash -> scriptHash #== pconstant (ScriptHash $ toBuiltin $ resolverScriptAt index)
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

materialScript :: BS.ByteString
materialScript = BS.replicate 28 0x81
