{-# LANGUAGE OverloadedStrings #-}

-- | Target Phase-A native item predicates split between dispatcher and yields.
module Midgard.PhaseANativeItem (pdispatch, pnative, pforeign) where

import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.FraudProofs.NativeTx.Compact qualified as Native
import Midgard.NativeScriptScan qualified as Scan
import Midgard.NativeTxFieldAccess qualified as Field
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMachineFieldDoor qualified as Door
import Midgard.ValidationTrace qualified as Trace
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

pdispatch :: forall s. Term s (Trace.PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> PInteger :--> PInteger :--> PBool)
pdispatch = phoistAcyclic $ plam $ \pre witness fieldIndex itemIndex ->
  pmatch pre $ \p -> pmatch witness $ \w ->
  plet (VM.pphaseANativeControlFromWitness # pfromData (VM.poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
  pmatch (Native.pverifyNativeTxProofSourceV1 # pfromData (Trace.pmachineState'transactionId p)
    # pfromData (VM.pphaseANative'compactCbor c) # pfromData (VM.pphaseANative'witnessSetCompactCbor c)
    # pfromData (VM.pphaseANative'fieldPreimageLengthsCbor c)) $ \(PPair verified witnessSet) ->
    VM.pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
      #&& pfromData (VM.pphaseANative'stage c) #== 0
      #&& pfromData (VM.pphaseANative'scriptCount c) #/= 0
      #&& fieldIndex #== 6 #&& itemIndex #== pfromData (VM.pphaseANative'scriptSeen c)

pnative, pforeign :: forall s. Term s (Trace.PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> Door.PMachineFieldDoorV1 :--> Field.PFieldCarriageV1 :--> PBool)
pnative = itemPayload True
pforeign = itemPayload False

itemPayload :: forall s. Bool -> Term s (Trace.PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> Door.PMachineFieldDoorV1 :--> Field.PFieldCarriageV1 :--> PBool)
itemPayload native = phoistAcyclic $ plam $ \pre witness door carriage ->
  pmatch pre $ \p -> pmatch witness $ \w ->
  plet (VM.pphaseANativeControlFromWitness # pfromData (VM.poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
  pmatch (Native.pverifyNativeTxProofSourceV1 # pfromData (Trace.pmachineState'transactionId p)
    # pfromData (VM.pphaseANative'compactCbor c) # pfromData (VM.pphaseANative'witnessSetCompactCbor c)
    # pfromData (VM.pphaseANative'fieldPreimageLengthsCbor c)) $ \(PPair verified witnessSet) ->
  plet (Door.popenMachineFieldItem # door # verified # witnessSet # 6 # pfromData (VM.pphaseANative'scriptSeen c) # carriage) $ \item ->
  plet (Door.pmachineFieldItemCount # item) $ \count -> plet (Door.pmachineFieldItemLength # item) $ \length ->
  plet (pif (pfromData (VM.pphaseANative'scriptCount c) #== (-1)) count (pfromData $ VM.pphaseANative'scriptCount c)) $ \active ->
  plet (Scan.pversionedScriptHeaderV1 # (Door.pmachineFieldItemChunk # item # 0) # length) $ \header ->
    pand'List
      [ pfromData (VM.pphaseANative'scriptCount c) #== (-1) #|| pfromData (VM.pphaseANative'scriptCount c) #== count
      , active #> 0
      , active #<= pmaxTxSizeDerivedItemCount
      , count #== active
      , length #> 0
      , length #<= VM.pmaxAggregateFieldPreimageBytes
      , pmatch header $ \case
          PNothing -> if native
            then VM.prejectedSuccessorIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w) # pconstant "E_INVALID_FIELD_TYPE"
            else pconstant False
          PJust itemHeader -> pmatch itemHeader $ \h ->
            if native
              then pfromData (Scan.pheader'languageTag h) #== 0
                #&& VM.pphaseANativeSuccessorIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w)
                  # (pcon c
                    { VM.pphaseANative'stage = pdata 1
                    , VM.pphaseANative'scriptCount = pdata active
                    , VM.pphaseANative'itemLength = pdata length
                    , VM.pphaseANative'itemCommitment = pdata $ Door.pmachineFieldItemCommitment # item
                    , VM.pphaseANative'cursor = Scan.pheader'payloadOffset h
                    })
              else pfromData (Scan.pheader'languageTag h) #/= 0
                #&& VM.pphaseANativeCompleteScriptIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w)
                  # control # active # (pfromData (VM.pphaseANative'scriptSeen c) + 1) # 1
      ]
