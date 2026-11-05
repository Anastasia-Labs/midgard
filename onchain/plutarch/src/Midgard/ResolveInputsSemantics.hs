{-# LANGUAGE OverloadedStrings #-}

-- | Target resolvers whose witness has no pending output proof.
module Midgard.ResolveInputsSemantics (pinitial, pfinish, pmembershipBegin, pnonMembership) where

import Midgard.FraudProofs.NativeTx.Compact qualified as Native
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor)
import Midgard.FraudProofs.NativeTx.Types
import Midgard.LedgerOutputCommitment qualified as Descriptor
import Midgard.LedgerOutputProof qualified as OutputProof
import Midgard.MpfProof qualified as Mpf
import Midgard.MpfProof.Types (PProof)
import Midgard.ResolveInputsControl qualified as Raw
import Midgard.ValidationMachine
import Midgard.ValidationTrace
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

pinitial, pfinish :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pinitial = phoistAcyclic $ plam $ \pre witness -> pmatch pre $ \p -> pmatch witness $ \w ->
  plet (Raw.pcontrolNoPendingFromWitness # pfromData (poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
  pmatch (Native.pverifyNativeTxProofSourceV1 # pfromData (pmachineState'transactionId p)
    # Raw.pcontrol'compactCbor c # Raw.pcontrol'witnessSetCompactCbor c # Raw.pcontrol'fieldPreimageLengthsCbor c) $ \(PPair verified _) ->
  pmatch verified $ \v -> pmatch (pverified'txCompact v) $ \tx -> pmatch (pcompact'body tx) $ \body ->
  pmatch (pdecodeValidationContext # Raw.pcontrol'contextCbor c) $ \context ->
    pand'List
      [ pverified'version v #== 1
      , Raw.pcontrolNoPendingIsBound # pre # witness # control
      , Raw.pcontrol'cursor c #== 0
      , pif
          ((pbodyCompact'validityIntervalStart body #< 0 #|| pfromData (pvalidationContext'blockSlot context) #>= pbodyCompact'validityIntervalStart body)
            #&& (pbodyCompact'validityIntervalEnd body #< 0 #|| pfromData (pvalidationContext'blockSlot context) #<= pbodyCompact'validityIntervalEnd body))
          (successor pre witness (pcon PResolveInputs) $ Raw.pencodeControlRaw # pcon c {Raw.pcontrol'cursor = 1})
          (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor w) # pconstant "E_VALIDITY_INTERVAL_MISMATCH")
      ]

pfinish = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.pcontrolNoPendingFromWitness # pfromData (poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
    pand'List
      [ Raw.pcontrolNoPendingIsBound # pre # witness # control
      , Raw.pcontrol'cursor c #> 0
      , Raw.pcontrol'remainingScheduleHash c #== pemptyResolutionScheduleHash
      , successor pre witness (pcon PScriptSources) $
          pencodeScriptSourcesWitness
            # Raw.pcontrol'compactCbor c # Raw.pcontrol'witnessSetCompactCbor c # Raw.pcontrol'fieldPreimageLengthsCbor c
            # Raw.pcontrol'contextCbor c # (Raw.pcontrol'cursor c - 1) # Raw.pcontrol'accumulator c
            # Raw.pcontrol'signerCount c # Raw.pcontrol'signerFrontierCommitment c
            # pnil # 0 # 0 # pnil # 0 # pnil # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
            # 0 # 0 # pnil # 0 # 0 # pnil # 0 # pemptyReceivePurposeScanControl
            # 0 # 0 # pemptyObserverPurposeScanControl # pemptyMintFoldControl # Raw.pcontrol'resolutionScheduleHash c
      ]

pmembershipBegin :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PProof :--> PSignerSetProofV1 :--> PBool)
pmembershipBegin = phoistAcyclic $ plam $ \pre witness sourceKind key nextSchedule value proof signerProof ->
  pmatch pre $ \p -> pmatch witness $ \w ->
  plet (Raw.pcontrolNoPendingFromWitness # pfromData (poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
  pmatch (Descriptor.pdecodeLedgerOutputCommitment # value) $ \descriptor ->
  pmatch (pdecodeMidgardTxInputCbor # key) $ \input ->
    pand'List
      [ Raw.pcontrolNoPendingIsBound # pre # witness # control
      , Raw.pcontrol'cursor c #> 0
      , Raw.pcontrol'remainingScheduleHash c #/= pemptyResolutionScheduleHash
      , signerProof #== pcon PNoSignerSetProof
      , sourceKind #== 0 #|| sourceKind #== 1
      , presolutionScheduleNodeHash # sourceKind # key # nextSchedule #== Raw.pcontrol'remainingScheduleHash c
      , pfromData (ptxInput'outputIndex input) #== pfromData (Descriptor.poutputCommitment'outputIndex descriptor)
      , pfromData (Descriptor.poutputCommitment'totalLength descriptor) #> 0
      , Mpf.phasV1 # pfromData (pmachineState'priorLedgerRoot p) # key # value # proof
      , successor pre witness (pcon PResolveInputs) $ Raw.pencodeControlRaw # pcon c
          { Raw.pcontrol'pendingCbor = Raw.pencodePendingRaw # sourceKind # key # nextSchedule # value
              # (OutputProof.pencodeInitialControlV1 # pfromData (Descriptor.poutputCommitment'outputIndex descriptor)
                  # pfromData (Descriptor.poutputCommitment'totalLength descriptor) # pfromData (Descriptor.poutputCommitment'itemCommitment descriptor))
          }
      ]

pnonMembership :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PByteString :--> PByteString :--> PProof :--> PBool)
pnonMembership = phoistAcyclic $ plam $ \pre witness sourceKind key nextSchedule proof ->
  pmatch pre $ \p -> pmatch witness $ \w ->
  plet (Raw.pcontrolNoPendingFromWitness # pfromData (poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
    pand'List
      [ Raw.pcontrolNoPendingIsBound # pre # witness # control
      , Raw.pcontrol'cursor c #> 0
      , Raw.pcontrol'remainingScheduleHash c #/= pemptyResolutionScheduleHash
      , presolutionScheduleNodeHash # sourceKind # key # nextSchedule #== Raw.pcontrol'remainingScheduleHash c
      , Mpf.pdoesNotHave # pfromData (pmachineState'priorLedgerRoot p) # key # proof
      , prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor w) # pconstant "E_INPUT_NOT_FOUND"
      ]

successor :: forall s. Term s PValidationMachineStateV1 -> Term s PValidationOneStepWitnessV1 -> Term s PValidationPhase -> Term s PByteString -> Term s PBool
successor pre witness phase work = pmatch pre $ \p -> pmatch witness $ \w -> pmatch (pfromData $ poneStep'claimedSuccessor w) $ \post ->
  pfromData (pmachineState'phase post) #== phase
    #&& pfromData (pmachineState'workRoot post) #== phashWorkWitness # phase # (pfromData (pmachineState'programCounter p) + 1) # work
