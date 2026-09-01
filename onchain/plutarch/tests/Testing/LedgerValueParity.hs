{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerValueParity (tests) where

import Data.ByteString qualified as BS
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compile, compileWithInternalConfig)
import Plutarch.MerkleTree.Merkling (pnull_hash)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as PD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
 )
import Midgard.MpfProof qualified as MpfProof
import Midgard.MpfProof.Types (PProof (..))
import Midgard.MpfProofFold qualified as ProofFold
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ValidationMachine
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  phashRejectionCode,
  phashValidationContext,
  phashWorkWitness,
  pmachineVersion,
 )
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
  testGroup
    "Ledger and Value Aiken parity"
    [ testGroup
        "parameterized_min_fee_boundary_matches_target_snapshot"
        [ testCase "canonical size is stable across the adjacent fee boundary" $
            passertEvalNoTraceWithoutHoistChecks minimumFeeCanonicalSizeIsStable
        , testCase "the exact boundary advances" $
            assertMinimumFeeRoute 0
        , testCase "one below the boundary rejects" $
            assertMinimumFeeRoute 1
        , testCase "the exact boundary cannot claim rejection" $
            assertMinimumFeeRoute 2
        , testCase "one below the boundary cannot advance" $
            assertMinimumFeeRoute 3
        ]
    , testCase "parameterized_ada_and_multi_asset_conservation_matches_typescript" $
        passertEvalNoTraceWithoutHoistChecks parameterizedAdaAndMultiAssetConservationMatchesTypescript
    , testCase "mint_and_burn_authorization_rejects_unbacked_policy" $
        passertEvalNoTraceWithoutHoistChecks mintAndBurnAuthorizationRejectsUnbackedPolicy
    ]

--------------------------------------------------------------------------------
-- Canonical empty V1 proof source
--------------------------------------------------------------------------------

emptyBody :: forall s. Term s PInteger -> Term s PNativeTxBodyCompact
emptyBody fee =
  pcon $ PNativeTxBodyCompact
    NativeField.pemptyFieldCommitment
    NativeField.pemptyFieldCommitment
    NativeField.pemptyFieldCommitment
    fee
    (-1)
    (-1)
    NativeField.pemptyFieldCommitment
    NativeField.pemptyFieldCommitment
    NativeField.pemptyFieldCommitment
    zero32
    zero32
    255

emptyWitnessSetCbor :: forall s. Term s PByteString
emptyWitnessSetCbor =
  NativeCompact.pencodeNativeTxWitnessSetCompact
    # pcon (PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment))

emptyFieldLengths :: forall s. Term s PNativeTxFieldPreimageLengthsV1
emptyFieldLengths = pcon $ PNativeTxFieldPreimageLengthsV1 1 1 1 1 1 1 1 1 1

emptyFieldLengthsCbor :: forall s. Term s PByteString
emptyFieldLengthsCbor =
  NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # emptyFieldLengths

emptyCompact :: forall s. Term s PInteger -> Term s PNativeTxCompact
emptyCompact fee =
  pcon $ PNativeTxCompact
    (emptyBody fee)
    (pblake2b_256 # emptyWitnessSetCbor)
    0

emptyCompactCbor :: forall s. Term s PInteger -> Term s PByteString
emptyCompactCbor fee = NativeCompact.pencodeNativeTxCompactV1 # emptyCompact fee

emptyTransactionId :: forall s. Term s PInteger -> Term s PByteString
emptyTransactionId fee =
  NativeCompact.pnativeTxIdForVersion
    # 1
    # (NativeCompact.pencodeNativeTxBodyCompact # emptyBody fee)

emptyProofCommitment :: forall s. Term s PInteger -> Term s PByteString
emptyProofCommitment fee =
  NativeCompact.pnativeTxProofCommitmentV1
    # emptyCompactCbor fee
    # emptyWitnessSetCbor
    # emptyFieldLengthsCbor

canonicalSize :: forall s. Term s PInteger -> Term s PInteger
canonicalSize fee =
  NativeCompact.pnativeTxCanonicalSizeV1 # emptyCompact fee # emptyFieldLengths

targetContextCbor, valueContextCbor :: forall s. Term s PByteString
targetContextCbor =
  phexByteStr "8701546d6964676172642d636f6e73656e7375732d7631186400182c1a00025ef51864"
valueContextCbor =
  phexByteStr "8701546d6964676172642d636f6e73656e7375732d763118640000001864"

zero32, hA, hC, h28A, h28B :: forall s. Term s PByteString
zero32 = preplicateBS # 32 # (pintegerToByte # 0)
hA = preplicateBS # 32 # (pintegerToByte # 0xaa)
hC = preplicateBS # 32 # (pintegerToByte # 0xcc)
h28A = preplicateBS # 28 # (pintegerToByte # 0xaa)
h28B = preplicateBS # 28 # (pintegerToByte # 0xbb)

--------------------------------------------------------------------------------
-- Minimum fee
--------------------------------------------------------------------------------

withMinimumFeeBoundary :: forall s. (Term s PInteger -> Term s PInteger -> Term s PBool) -> Term s PBool
withMinimumFeeBoundary assertion =
  plet (canonicalSize 155_381) $ \probeSize ->
  plet (44 * probeSize + 155_381) $ \boundaryFee ->
    assertion probeSize boundaryFee

minimumFeeCanonicalSizeIsStable :: forall s. Term s PBool
minimumFeeCanonicalSizeIsStable = withMinimumFeeBoundary $ \probeSize boundaryFee ->
  pand'List
    [ canonicalSize boundaryFee #== probeSize
    , canonicalSize (boundaryFee - 1) #== probeSize
    ]

minimumFeeRouteHarness :: forall s. Term s (PData :--> PBool)
minimumFeeRouteHarness = plam $ \scenarioData ->
  plet (pasInt # scenarioData) $ \scenario ->
  withMinimumFeeBoundary $ \_ boundaryFee ->
    pif (0 #<= scenario #&& scenario #<= 3)
      ( plet (scenario #== 0 #|| scenario #== 2) $ \usesBoundaryFee ->
        plet (scenario #== 0 #|| scenario #== 3) $ \claimsAdvance ->
        plet (minFeeStepIsProvable (pif usesBoundaryFee boundaryFee (boundaryFee - 1)) claimsAdvance) $ \isProvable ->
          pif (scenario #< 2) isProvable (pnot # isProvable)
      )
      perror

{-# NOINLINE compiledMinimumFeeRouteHarness #-}
compiledMinimumFeeRouteHarness :: Script
compiledMinimumFeeRouteHarness =
  either (error . show) id $
    compileWithInternalConfig (InternalConfig False False) NoTracing minimumFeeRouteHarness

{-# NOINLINE compiledTrue #-}
compiledTrue :: Script
compiledTrue = either (error . show) id $ compile NoTracing (pconstant @PBool True)

assertMinimumFeeRoute :: Integer -> Assertion
assertMinimumFeeRoute scenario =
  case evalScriptHuge (applyArguments compiledMinimumFeeRouteHarness [PD.I scenario]) of
    (Left err, _, traces) -> assertFailure ("minimum-fee route failed: " <> show err <> " " <> show traces)
    (Right result, _, _) -> assertEqual "minimum-fee route result" (printScript compiledTrue) (printScript result)

minFeeStepIsProvable :: forall s. Term s PInteger -> Term s PBool -> Term s PBool
minFeeStepIsProvable fee claimsAdvance =
  plet
    ( pencodeStaticRulesWitness
        # emptyCompactCbor fee
        # emptyWitnessSetCbor
        # emptyFieldLengthsCbor
        # targetContextCbor
    )
    $ \workCbor ->
  plet
    ( machineState
        (emptyTransactionId fee)
        (emptyProofCommitment fee)
        targetContextCbor
        (pcon PStaticLedgerRules)
        2
        workCbor
        nonEmptyClaimedDeltaRoot
    )
    $ \pre ->
  plet
    (pif claimsAdvance
      (inputSetsSuccessor pre fee)
      (rejectionSuccessor pre (pconstant "E_MIN_FEE")))
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pverifyOneStep # pre # witness

inputSetsSuccessor :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
inputSetsSuccessor pre fee = pmatch pre $ \p ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion p)
    (pmachineState'eventKeyHash p)
    (pmachineState'transactionId p)
    (pmachineState'transactionCommitment p)
    (pmachineState'validationContextHash p)
    (pmachineState'sourceKind p)
    (pmachineState'priorLedgerRoot p)
    (pdata $ pcon PInputSets)
    (pdata 3)
    ( pdata $
        phashWorkWitness
          # pcon PInputSets
          # 3
          # ( pencodeInputSetsScanWitness
                # emptyCompactCbor fee
                # emptyWitnessSetCbor
                # emptyFieldLengthsCbor
                # targetContextCbor
                # 0 # 0 # 0 # 0 # pconstant "" # pemptyResolutionScheduleHash
            )
    )
    (pmachineState'executionCpu p)
    (pmachineState'executionMemory p)
    (pmachineState'verdict p)
    (pmachineState'rejectionCodeHash p)
    (pmachineState'ledgerDeltaRoot p)

--------------------------------------------------------------------------------
-- Value conservation
--------------------------------------------------------------------------------

parameterizedAdaAndMultiAssetConservationMatchesTypescript :: forall s. Term s PBool
parameterizedAdaAndMultiAssetConservationMatchesTypescript =
  pand'List
    [ conservationMintStepIsExact 3 (-3)
    , conservationMintStepIsExact (-2) 2
    , conservationFinalStepIsProvable 1 0 True
    , pnot #$ conservationFinalStepIsProvable 1 0 False
    , conservationFinalStepIsProvable 0 0 False
    , pnot #$ conservationFinalStepIsProvable 0 0 True
    , conservationFinalStepIsProvable 1 (-1) False
    , pnot #$ conservationFinalStepIsProvable 1 (-1) True
    ]

conservationMintStepIsExact :: forall s. Integer -> Integer -> Term s PBool
conservationMintStepIsExact mintQuantity preAssetDelta =
  plet (pconstant $ BS.pack [0xbe, 0xef]) $ \assetName ->
  plet (h28A <> assetName) $ \unit ->
  plet (singletonRoot unit (pconstant preAssetDelta)) $ \oldRoot ->
  plet (singletonRoot unit (pconstant $ preAssetDelta + mintQuantity)) $ \nextRoot ->
  plet (mintFrontier h28A assetName (pconstant mintQuantity)) $ \frontier ->
  pmatch frontier $ \built ->
  plet
    ( nativeControl
        1
        (Merkle.pbuiltFrontier'peaks built)
    )
    $ \native ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata 1) (pdata oldRoot) (pdata 1) (pdata 1)
    )
    $ \accumulator ->
  plet (valueControl native 4 0 accumulator) $ \control ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata 1)
        (pdata nextRoot)
        (pdata 1)
        (pdata $ pconstant $ if preAssetDelta + mintQuantity == 0 then 0 else 1)
    )
    $ \nextAccumulator ->
  plet (valueControl native 4 1 nextAccumulator) $ \nextControl ->
  plet (pencodeValueAndMintControlV1 # control) $ \workCbor ->
  plet (valueState 60 workCbor) $ \pre ->
  plet (continuingValueSuccessor pre 61 nextControl) $ \post ->
  plet
    ( pcon $ PValueAssetMutationWitnessV1
        (pdata $ pconstant True)
        (pdata $ pconstant preAssetDelta)
        (pdata pnil)
    )
    $ \mutation ->
  plet
    ( pcon $ PValueMintAssetWitness
        (pdata 0)
        (pdata h28A)
        (pdata assetName)
        (pdata $ pconstant mintQuantity)
        (pdata pnil)
        (pdata mutation)
    )
    $ \auxiliary ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata workCbor)
        (pdata post)
    )
    $ \transition ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata transition)
        (pdata auxiliary)
    )
    $ \evidence ->
    pverifyValueAndMintOneStepV1 # pre # evidence

conservationFinalStepIsProvable :: forall s. Integer -> Integer -> Bool -> Term s PBool
conservationFinalStepIsProvable lovelaceDelta assetDelta claimsAdvance =
  plet (pconstant $ BS.pack [0xbe, 0xef]) $ \assetName ->
  plet (h28A <> assetName) $ \unit ->
  plet (singletonRoot unit (pconstant assetDelta)) $ \assetRoot ->
  plet (mintFrontier h28A assetName 3) $ \frontier ->
  pmatch frontier $ \built ->
  plet (nativeControl 1 (Merkle.pbuiltFrontier'peaks built)) $ \native ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata $ pconstant lovelaceDelta)
        (pdata assetRoot)
        (pdata 1)
        (pdata $ pconstant $ if assetDelta == 0 then 0 else 1)
    )
    $ \accumulator ->
  plet (valueControl native 5 1 accumulator) $ \control ->
  plet (pencodeValueAndMintControlV1 # control) $ \workCbor ->
  plet (valueState 62 workCbor) $ \pre ->
  plet
    ( if claimsAdvance
        then ledgerDeltaSuccessor pre native
        else rejectionSuccessor pre (pconstant "E_VALUE_NOT_PRESERVED")
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata transition)
        (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \evidence ->
    pverifyValueAndMintOneStepV1 # pre # evidence

--------------------------------------------------------------------------------
-- Mint authorization
--------------------------------------------------------------------------------

mintAndBurnAuthorizationRejectsUnbackedPolicy :: forall s. Term s PBool
mintAndBurnAuthorizationRejectsUnbackedPolicy =
  plet (pconstant $ BS.pack [0xbe, 0xef]) $ \assetName ->
  plet (mintFrontier h28A assetName 3) $ \frontier ->
  pmatch frontier $ \built ->
    pand'List
      [ mintAuthorizationStepIsProvable h28A h28A 3
      , mintAuthorizationStepIsProvable h28A h28A (-3)
      , pnot #$ mintAuthorizationStepIsProvable h28A h28B 3
      , pnot #$ mintAuthorizationStepIsProvable h28A h28B (-3)
      , Merkle.pverifyMembership
          # Merkle.pbuiltFrontier'count built
          # Merkle.pbuiltFrontier'peaks built
          # 0
          # (pmintAssetLeafHash # h28A # assetName # 3)
          # pnil
      , pnot
          #$ Merkle.pverifyMembership
          # Merkle.pbuiltFrontier'count built
          # Merkle.pbuiltFrontier'peaks built
          # 0
          # (pmintAssetLeafHash # h28B # assetName # 3)
          # pnil
      ]

mintAuthorizationStepIsProvable :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Integer ->
  Term s PBool
mintAuthorizationStepIsProvable authenticatedPolicy witnessedPolicy quantity =
  plet (pconstant $ BS.pack [0xbe, 0xef]) $ \assetName ->
  plet (mintFrontier authenticatedPolicy assetName (pconstant quantity)) $ \frontier ->
  pmatch frontier $ \built ->
  plet (nativeControl 1 (Merkle.pbuiltFrontier'peaks built)) $ \native ->
  plet pinitialValueAccumulator $ \accumulator ->
  plet (valueControl native 4 0 accumulator) $ \control ->
  plet (witnessedPolicy <> assetName) $ \unit ->
  plet (singletonRoot unit (pconstant quantity)) $ \nextRoot ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata 0) (pdata nextRoot) (pdata 1) (pdata 1)
    )
    $ \nextAccumulator ->
  plet (valueControl native 4 1 nextAccumulator) $ \nextControl ->
  plet (pencodeValueAndMintControlV1 # control) $ \workCbor ->
  plet (valueState 64 workCbor) $ \pre ->
  plet (continuingValueSuccessor pre 65 nextControl) $ \post ->
  plet
    ( pcon $ PValueAssetMutationWitnessV1
        (pdata $ pconstant False) (pdata 0) (pdata pnil)
    )
    $ \mutation ->
  plet
    ( pcon $ PValueMintAssetWitness
        (pdata 0)
        (pdata witnessedPolicy)
        (pdata assetName)
        (pdata $ pconstant quantity)
        (pdata pnil)
        (pdata mutation)
    )
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata transition) (pdata auxiliary)
    )
    $ \evidence ->
    pverifyValueAndMintOneStepV1 # pre # evidence

--------------------------------------------------------------------------------
-- Shared ValueAndMint fixtures
--------------------------------------------------------------------------------

nativeControl :: forall s.
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PNativeScriptsControlV1
nativeControl mintCount mintPeaks =
  pcon $ PNativeScriptsControlV1
    (pdata $ emptyCompactCbor 1)
    (pdata emptyWitnessSetCbor)
    (pdata emptyFieldLengthsCbor)
    (pdata valueContextCbor)
    (pdata 0)
    (pdata pinitialResolutionAccumulator)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata pnil)
    (pdata mintCount)
    (pdata mintPeaks)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata 0)
    (pdata pemptyResolutionScheduleHash)

valueControl :: forall s.
  Term s PNativeScriptsControlV1 ->
  Integer ->
  Integer ->
  Term s PValueAccumulatorV1 ->
  Term s PValueAndMintControlV1
valueControl native stage mintCursor accumulator =
  pcon $ PValueAndMintControlV1
    (pdata native)
    (pdata $ pconstant stage)
    (pdata pemptyResolutionScheduleHash)
    (pdata 0)
    (pdata 0)
    (pdata zero32)
    (pdata pinitialResolutionAccumulator)
    (pdata pemptyResolutionScheduleHash)
    (pdata 0)
    (pdata 0)
    (pdata $ pconstant mintCursor)
    (pdata accumulator)

valueState :: forall s. Integer -> Term s PByteString -> Term s PValidationMachineStateV1
valueState programCounter workCbor =
  machineState
    (emptyTransactionId 1)
    (emptyProofCommitment 1)
    valueContextCbor
    (pcon PValueAndMint)
    (pconstant programCounter)
    workCbor
    nonEmptyClaimedDeltaRoot

continuingValueSuccessor :: forall s.
  Term s PValidationMachineStateV1 ->
  Integer ->
  Term s PValueAndMintControlV1 ->
  Term s PValidationMachineStateV1
continuingValueSuccessor pre programCounter control = pmatch pre $ \p ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion p)
    (pmachineState'eventKeyHash p)
    (pmachineState'transactionId p)
    (pmachineState'transactionCommitment p)
    (pmachineState'validationContextHash p)
    (pmachineState'sourceKind p)
    (pmachineState'priorLedgerRoot p)
    (pdata $ pcon PValueAndMint)
    (pdata $ pconstant programCounter)
    ( pdata $
        phashWorkWitness
          # pcon PValueAndMint
          # pconstant programCounter
          # (pencodeValueAndMintControlV1 # control)
    )
    (pmachineState'executionCpu p)
    (pmachineState'executionMemory p)
    (pmachineState'verdict p)
    (pmachineState'rejectionCodeHash p)
    (pmachineState'ledgerDeltaRoot p)

ledgerDeltaSuccessor :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PNativeScriptsControlV1 ->
  Term s PValidationMachineStateV1
ledgerDeltaSuccessor pre native = pmatch pre $ \p ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion p)
    (pmachineState'eventKeyHash p)
    (pmachineState'transactionId p)
    (pmachineState'transactionCommitment p)
    (pmachineState'validationContextHash p)
    (pmachineState'sourceKind p)
    (pmachineState'priorLedgerRoot p)
    (pdata $ pcon PLedgerDelta)
    (pdata 63)
    ( pdata $
        phashWorkWitness
          # pcon PLedgerDelta
          # 63
          # ( pencodeLedgerDeltaWitnessV1
                # native
                # pfromData (pmachineState'priorLedgerRoot p)
                # pemptyResolutionScheduleHash
            )
    )
    (pmachineState'executionCpu p)
    (pmachineState'executionMemory p)
    (pmachineState'verdict p)
    (pmachineState'rejectionCodeHash p)
    (pmachineState'ledgerDeltaRoot p)

mintFrontier :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s Merkle.PBuiltFrontier
mintFrontier policyId assetName quantity =
  Merkle.pbuildFrontier
    # (pcons # pdata (pmintAssetLeafHash # policyId # assetName # quantity) # pnil)

singletonRoot :: forall s. Term s PByteString -> Term s PInteger -> Term s PByteString
singletonRoot unit quantity =
  pmatch
    ( MpfProof.pinsertRoot
        # pnull_hash
        # unit
        # pcborInt quantity
        # pcon (PProof pnil)
    )
    $ \case
      PNothing -> perror
      PJust root -> root

--------------------------------------------------------------------------------
-- Shared state fixtures
--------------------------------------------------------------------------------

machineState :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
machineState transactionId commitment contextCbor phase programCounter workCbor deltaRoot =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion)
    (pdata hA)
    (pdata transactionId)
    (pdata commitment)
    (pdata $ phashValidationContext # contextCbor)
    (pdata $ pcon PForced)
    (pdata hC)
    (pdata phase)
    (pdata programCounter)
    (pdata $ phashWorkWitness # phase # programCounter # workCbor)
    (pdata 0)
    (pdata 0)
    (pdata $ pcon PPending)
    (pdata zero32)
    (pdata deltaRoot)

rejectionSuccessor :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
rejectionSuccessor pre rejectionCode = pmatch pre $ \p ->
  plet (pfromData (pmachineState'programCounter p) + 1) $ \nextCounter ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion p)
    (pmachineState'eventKeyHash p)
    (pmachineState'transactionId p)
    (pmachineState'transactionCommitment p)
    (pmachineState'validationContextHash p)
    (pmachineState'sourceKind p)
    (pmachineState'priorLedgerRoot p)
    (pdata $ pcon PTerminal)
    (pdata nextCounter)
    ( pdata $
        phashWorkWitness
          # pcon PTerminal
          # nextCounter
          # ( pencodeTerminalRejectionWitness
                # rejectionCode
                # pfromData (pmachineState'priorLedgerRoot p)
            )
    )
    (pmachineState'executionCpu p)
    (pmachineState'executionMemory p)
    (pdata $ pcon PRejected)
    (pdata $ phashRejectionCode # rejectionCode)
    (pmachineState'ledgerDeltaRoot p)

nonEmptyClaimedDeltaRoot :: forall s. Term s PByteString
nonEmptyClaimedDeltaRoot =
  plet emptyProofDescriptor $ \descriptor ->
  plet
    (pledgerDeltaOperationLeafHash # 0 # phexByteStr "010203" # pconstant "" # descriptor)
    $ \deletion ->
  plet
    (pledgerDeltaOperationLeafHash # 1 # phexByteStr "0405" # phexByteStr "060708" # descriptor)
    $ \insertion ->
  pmatch
    (Merkle.pbuildFrontier #$ pcons # pdata deletion #$ pcons # pdata insertion # pnil)
    $ \frontier ->
      Merkle.pfrontierCommitment
        # Merkle.pbuiltFrontier'count frontier
        # Merkle.pbuiltFrontier'peaks frontier

emptyProofDescriptor :: forall s. Term s ProofFold.PProofDescriptorV1
emptyProofDescriptor =
  pcon $ ProofFold.PProofDescriptorV1
    (pdata 1)
    (pdata 0)
    (pdata 0)
    (pdata pnil)
