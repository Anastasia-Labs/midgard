{-# LANGUAGE OverloadedStrings #-}

-- | Target stage-zero inline-source hash transitions over the pending extension.
module Midgard.ScriptSourcesStageZeroHashSemantics (padvance, pblock, pterminal) where

import Midgard.Blake2b224Trace qualified as Blake2b224
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ScriptSourcesStageZeroFinishSemantics qualified as StageZeroFinish
import Midgard.ValidationMachine (
  PInlineSourceHashControlV1 (..),
  PValidationOneStepWitnessV1 (..),
  pdecodeInlineSourceHashControlV1,
  pencodeInlineSourceHashControlV1,
  pinlineSourceHashBlockV1,
 )
import Midgard.ValidationMerkle (pappendLeaf, pencodeFrontier)
import Midgard.ValidationTrace (PValidationMachineStateV1)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude

pwithHash ::
  forall s.
  Term
    s
    ( PInlineSourceHashControlV1
        :--> Blake2b224.PBlake2b224TraceControlV1
        :--> PInlineSourceHashControlV1
    )
pwithHash = phoistAcyclic $ plam $ \pending hashControl ->
  pmatch pending $ \active ->
    pcon $
      PInlineSourceHashControlV1
        (pinlineSource'version active)
        (pinlineSource'sourceIndex active)
        (pinlineSource'sourceTotalCount active)
        (pinlineSource'languageTag active)
        (pinlineSource'payloadOffset active)
        (pinlineSource'payloadLength active)
        (pinlineSource'itemLength active)
        (pinlineSource'itemCommitment active)
        (pdata hashControl)

pblock ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pblock = phoistAcyclic $ plam $ \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    plet (Raw.popenFrameV1 # pre # witness # 31 # 0) $ \frame ->
      plet (Raw.pitemBytesV1 # frame # 30) $ \pendingCbor ->
        plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
          pmatch pending $ \active ->
            pif
              ( pfromData (pinlineSource'sourceIndex active)
                  #== Raw.pitemIntV1
                  # frame
                  # 10
                  #&& pfromData (pinlineSource'sourceTotalCount active)
                  #== Raw.pitemIntV1
                  # frame
                  # 25
              )
              ( pmatch (pinlineSourceHashBlockV1 # pending # chunkProof # nextChunkProof) $ \case
                  PNothing -> pconstant False
                  PJust block ->
                    pmatch
                      ( Blake2b224.pstepV1
                          # pfromData (pinlineSource'hashControl active)
                          # (pcon $ PJust block)
                      )
                      $ \case
                        PNothing -> perror
                        PJust nextHashControl ->
                          Raw.psuccessorIsExactV1
                            # pre
                            # witness
                            # ( Raw.preplaceExtensionV1
                                  # pfromData (poneStep'workWitnessCbor stepWitness)
                                  # pendingCbor
                                  # ( pencodeInlineSourceHashControlV1
                                        # (pwithHash # pending # nextHashControl)
                                    )
                              )
              )
              perror

padvance ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
padvance = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    plet (Raw.popenFrameV1 # pre # witness # 31 # 0) $ \frame ->
      plet (Raw.pitemBytesV1 # frame # 30) $ \pendingCbor ->
        plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
          pmatch pending $ \active ->
            plet (pfromData $ pinlineSource'hashControl active) $ \hashControl ->
              pmatch hashControl $ \hash ->
                pif
                  ( pfromData (pinlineSource'sourceIndex active)
                      #== Raw.pitemIntV1
                      # frame
                      # 10
                      #&& pfromData (pinlineSource'sourceTotalCount active)
                      #== Raw.pitemIntV1
                      # frame
                      # 25
                      #&& pfromData (Blake2b224.pctl'stage hash)
                      #/= Blake2b224.pstageReady
                      #&& pfromData (Blake2b224.pctl'stage hash)
                      #/= Blake2b224.pstageTerminal
                  )
                  ( pmatch (Blake2b224.pstepV1 # hashControl # pcon PNothing) $ \case
                      PNothing -> perror
                      PJust nextHashControl ->
                        StageZeroFinish.pbaseIsBound
                          # frame
                          #&& Raw.psuccessorIsExactV1
                          # pre
                          # witness
                          # ( Raw.preplaceExtensionV1
                                # pfromData (poneStep'workWitnessCbor stepWitness)
                                # pendingCbor
                                # ( pencodeInlineSourceHashControlV1
                                      # (pwithHash # pending # nextHashControl)
                                  )
                            )
                  )
                  perror

pterminal ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pterminal = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    plet (Raw.popenFrameV1 # pre # witness # 31 # 0) $ \frame ->
      plet (Raw.pitemBytesV1 # frame # 30) $ \pendingCbor ->
        plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
          pmatch pending $ \active ->
            pmatch (Blake2b224.pdigestV1 # pfromData (pinlineSource'hashControl active)) $ \case
              PNothing -> perror
              PJust scriptHash ->
                plet (Raw.pitemIntV1 # frame # 10) $ \sourceCount ->
                  plet (sourceCount + 1) $ \nextSourceCount ->
                    plet
                      ( pappendLeaf
                          # sourceCount
                          # (Raw.pitemFrontierV1 # frame # 11)
                          # ( ScriptProof.pinlineSourceLeafHash
                                # sourceCount
                                # pfromData (pinlineSource'languageTag active)
                                # scriptHash
                                # pfromData (pinlineSource'itemLength active)
                                # pfromData (pinlineSource'itemCommitment active)
                            )
                      )
                      $ \nextSourcePeaks ->
                        pif
                          ( pfromData (pinlineSource'sourceIndex active)
                              #== sourceCount
                              #&& pfromData (pinlineSource'sourceTotalCount active)
                              #== Raw.pitemIntV1
                              # frame
                              # 25
                          )
                          ( StageZeroFinish.pbaseIsBound
                              # frame
                              #&& Raw.psuccessorIsExactV1
                              # pre
                              # witness
                              # ( Raw.pdropExtensionV1
                                    # ( Raw.preplaceItemsV1
                                          # frame
                                          # pfromData (poneStep'workWitnessCbor stepWitness)
                                          # 10
                                          # 2
                                          # (pcborInt nextSourceCount <> (pencodeFrontier # nextSourcePeaks))
                                      )
                                    # pendingCbor
                                )
                          )
                          perror
