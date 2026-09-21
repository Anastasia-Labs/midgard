{-# LANGUAGE OverloadedStrings #-}

-- | The two fixed-target native execution descriptor families.
module Midgard.NativeExecutionDescriptor (pnative, peffectful) where

import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ValidationMachine
import Midgard.ValidationMerkle
import Midgard.ValidationTrace
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

pnative, peffectful :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PValidationAuxiliaryWitnessV1 :--> PBool)
pnative = descriptor True
peffectful = descriptor False

descriptor :: forall s. Bool ->
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PValidationAuxiliaryWitnessV1 :--> PBool)
descriptor native = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch auxiliary $ \case
    PNativeExecutionDescriptorWitness{} ->
      pmatch pre $ \p -> pmatch witness $ \w ->
      plet (pnativeScriptsControlFromWitness # pfromData (poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
      pmatch (NativeCompact.pverifyNativeTxProofSourceV1 # pfromData (pmachineState'transactionId p)
        # pfromData (pnativeControl'compactCbor c) # pfromData (pnativeControl'witnessSetCompactCbor c)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor c)) $ \(PPair source _) -> pmatch source $ \verified ->
          pverified'version verified #== 1
            #&& pnativeScriptsControlIsBound # pre # witness # control
            #&& pfromData (pnativeControl'executionCursor c) #/= pfromData (pnativeControl'executionCount c)
            #&& descriptorStep native # pre # witness # auxiliary # control
    _ -> perror

pnativeScriptsSuccessorIsExact :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PNativeScriptsControlV1 :--> PInteger :--> PBool)
pnativeScriptsSuccessorIsExact = phoistAcyclic $ plam $ \pre witness control bitmap ->
  pmatch pre $ \p -> pmatch witness $ \w -> pmatch control $ \c ->
  pmatch (pfromData $ poneStep'claimedSuccessor w) $ \post ->
    pfromData (pmachineState'phase post) #== pcon PNativeScripts
      #&& pfromData (pmachineState'workRoot post) #== phashWorkWitness # pcon PNativeScripts
        # (pfromData (pmachineState'programCounter p) + 1)
        # (pencodeNativeScriptsControlV1 # pcon c { pnativeControl'executionCursor = pdata $ pfromData (pnativeControl'executionCursor c) + 1, pnativeControl'languageBitmap = pdata bitmap })

descriptorStep ::
  forall s. Bool ->
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1
        :--> PNativeScriptsControlV1
        :--> PBool
    )
descriptorStep native = phoistAcyclic $
  plam $
    \pre witness auxiliary control ->
      pmatch auxiliary $ \case
        PNativeExecutionDescriptorWitness
          executionIndexD
          languageTagD
          purposeKindD
          purposeIndexD
          scriptHashD
          subjectD
          purposeSiblingsD
          sourceIndexD
          originKindD
          sourceKeyD
          scriptTotalLengthD
          scriptItemCommitmentD
          sourceSiblingsD
          redeemerLeafD
          executionSiblingsD
          firstChunkProofD
          signerPeaksD ->
            pmatch pre $ \preState ->
              pmatch witness $ \stepWitness ->
                pmatch control $ \c ->
                  plet (pfromData executionIndexD) $ \executionIndex ->
                    plet (pfromData languageTagD) $ \languageTag ->
                      plet (pfromData purposeKindD) $ \purposeKind ->
                        plet (pfromData purposeIndexD) $ \purposeIndex ->
                          plet (pfromData scriptHashD) $ \scriptHash ->
                            plet (pfromData subjectD) $ \subject ->
                              plet (pfromData sourceIndexD) $ \sourceIndex ->
                                plet (pfromData originKindD) $ \originKind ->
                                  plet (pfromData sourceKeyD) $ \sourceKey ->
                                    plet (pfromData scriptTotalLengthD) $ \scriptTotalLength ->
                                      plet (pfromData scriptItemCommitmentD) $ \scriptItemCommitment ->
                                        plet (pfromData redeemerLeafD) $ \redeemerLeaf ->
                                          plet
                                            (ScriptProof.ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # subject)
                                            $ \purposeLeaf ->
                                              plet
                                                ( ScriptProof.psourceDescriptorLeafHash
                                                    # originKind
                                                    # sourceKey
                                                    # languageTag
                                                    # scriptHash
                                                    # scriptTotalLength
                                                    # scriptItemCommitment
                                                )
                                                $ \sourceLeaf ->
                                                  plet
                                                    (ScriptProof.pexecutionLeafHash # languageTag # purposeLeaf # sourceLeaf # redeemerLeaf)
                                                    $ \executionLeaf ->
                                                      pif
                                                        ( pand'List
                                                            [ executionIndex #== pfromData (pnativeControl'executionCursor c)
                                                            , pfromData (pnativeControl'executionCount c)
                                                                #== pfromData (pnativeControl'purposeCount c)
                                                            , scriptTotalLength #> 0
                                                            , scriptTotalLength #<= pmaxAggregateFieldPreimageBytes
                                                            , plengthBS # scriptItemCommitment #== 32
                                                            , pverifyMembership
                                                                # pfromData (pnativeControl'purposeCount c)
                                                                # pfromData (pnativeControl'purposePeaks c)
                                                                # executionIndex
                                                                # purposeLeaf
                                                                # pfromData purposeSiblingsD
                                                            , pverifyMembership
                                                                # pfromData (pnativeControl'sourceCount c)
                                                                # pfromData (pnativeControl'sourcePeaks c)
                                                                # sourceIndex
                                                                # sourceLeaf
                                                                # pfromData sourceSiblingsD
                                                            , pverifyMembership
                                                                # pfromData (pnativeControl'executionCount c)
                                                                # pfromData (pnativeControl'executionPeaks c)
                                                                # executionIndex
                                                                # executionLeaf
                                                                # pfromData executionSiblingsD
                                                            ]
                                                        )
                                                        ( if native
                                                            then languageTag #== 0 #&& ( pmatch (pfromData firstChunkProofD) $ \case
                                                                PDNothing -> perror
                                                                PDJust chunkProofD ->
                                                                  plet (pfromData chunkProofD) $ \chunkProof ->
                                                                    pmatch chunkProof $ \proof ->
                                                                      pmatch
                                                                        ( NativeScriptScan.pversionedScriptHeaderV1
                                                                            # pfromData (BoundedItem.pchunkProof'chunk proof)
                                                                            # scriptTotalLength
                                                                        )
                                                                        $ \case
                                                                          PNothing -> perror
                                                                          PJust header -> pmatch header $ \headerFields ->
                                                                            plet (pencodeNativeScriptsControlV1 # control) $ \continuationCbor ->
                                                                              plet (pfromData signerPeaksD) $ \signerPeaks ->
                                                                                plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
                                                                                  pmatch post $ \postState ->
                                                                                    pand'List
                                                                                      [ redeemerLeaf #== pconstant ""
                                                                                      , pfromData (BoundedItem.pchunkProof'chunkIndex proof) #== 0
                                                                                      , pfromData (BoundedItem.pchunkProof'totalLength proof)
                                                                                          #== scriptTotalLength
                                                                                      , BoundedItem.pverifyChunk # scriptItemCommitment # chunkProof
                                                                                      , pfromData (NativeScriptScan.pheader'languageTag headerFields) #== 0
                                                                                      , pfrontierIsWellFormed
                                                                                          # pfromData (pnativeControl'signerCount c)
                                                                                          # signerPeaks
                                                                                      , pfrontierCommitment
                                                                                          # pfromData (pnativeControl'signerCount c)
                                                                                          # signerPeaks
                                                                                          #== pfromData (pnativeControl'signerFrontierCommitment c)
                                                                                      , pfromData (pmachineState'phase postState) #== pcon PPhaseANativeScripts
                                                                                      , pfromData (pmachineState'workRoot postState)
                                                                                          #== phashWorkWitness
                                                                                          # pcon PPhaseANativeScripts
                                                                                          # (pfromData (pmachineState'programCounter preState) + 1)
                                                                                          # ( pencodePhaseANativeScriptsScanWitness
                                                                                                # pfromData (pnativeControl'compactCbor c)
                                                                                                # pfromData (pnativeControl'witnessSetCompactCbor c)
                                                                                                # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
                                                                                                # pfromData (pnativeControl'contextCbor c)
                                                                                                # (pblake2b_256 # continuationCbor)
                                                                                                # 1
                                                                                                # 1
                                                                                                # 0
                                                                                                # 0
                                                                                                # scriptTotalLength
                                                                                                # scriptItemCommitment
                                                                                                # pfromData (NativeScriptScan.pheader'payloadOffset headerFields)
                                                                                                # pconstant ""
                                                                                                # 0
                                                                                                # 0
                                                                                                # (-1)
                                                                                                # pfromData (pnativeControl'signerCount c)
                                                                                                # signerPeaks
                                                                                                # continuationCbor
                                                                                            )
                                                                                      ]
                                                            )
                                                            else ( pif
                                                                ( pand'List
                                                                    [ languageTag #== 3 #|| languageTag #== 128
                                                                    , plengthBS # redeemerLeaf #== 32
                                                                    , pfromData firstChunkProofD #== pcon PDNothing
                                                                    , pfromData signerPeaksD #== pnil
                                                                    ]
                                                                )
                                                                ( plet (pfromData $ pnativeControl'languageBitmap c) $ \bitmap ->
                                                                    plet (pnativeScriptsNextLanguageBitmap # bitmap # languageTag) $
                                                                      \nextBitmap ->
                                                                        pnativeScriptsSuccessorIsExact
                                                                          # pre
                                                                          # witness
                                                                          # control
                                                                          # nextBitmap
                                                                )
                                                                (pconstant False)
                                                            )
                                                        )
                                                        (pconstant False)
        _ -> perror

