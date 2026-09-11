{- |
Module      : Midgard.ValidationMachine.ValueAndMintSemantics
Description : Branch-local ValueAndMint semantic resolvers.
-}
module Midgard.ValidationMachine.ValueAndMintSemantics (
  pverifyValueAndMintBeginSemanticsV1,
  pverifyValueAndMintReplayBeginSemanticsV1,
  pverifyValueAndMintReplayInputSemanticsV1,
  pverifyValueAndMintReplayAssetSemanticsV1,
  pverifyValueAndMintReplayFinishSemanticsV1,
  pverifyValueAndMintOutputDescriptorSemanticsV1,
  pverifyValueAndMintOutputAssetSemanticsV1,
  pverifyValueAndMintOutputFinishSemanticsV1,
  pverifyValueAndMintMintAssetSemanticsV1,
  pverifyValueAndMintMintFinishSemanticsV1,
  pverifyValueAndMintFinalizeSemanticsV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact,
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.LedgerOutput qualified as LedgerOutput
import Midgard.ValidationMachine
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), phashValidationContext)

data PVerifiedValueAndMintV1 (s :: S)
  = PVerifiedValueAndMintV1
      (Term s PValueAndMintControlV1)
      (Term s PNativeTxBodyCompact)
      (Term s PBool)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PVerifiedValueAndMintV1)

pverifiedValueAndMintV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PVerifiedValueAndMintV1
    )
pverifiedValueAndMintV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
    pmatch witness $ \stepWitness ->
      plet
        (pvalueAndMintControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
        $ \control ->
          pmatch control $ \c ->
            plet (pfromData $ pvalueAndMint'nativeControl c) $ \nativeControl ->
              pmatch nativeControl $ \native ->
                pmatch
                  ( NativeCompact.pverifyNativeTxProofSourceV1
                      # pfromData (pmachineState'transactionId preState)
                      # pfromData (pnativeControl'compactCbor native)
                      # pfromData (pnativeControl'witnessSetCompactCbor native)
                      # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
                  )
                  $ \(PPair verifiedSource _) ->
                    pmatch verifiedSource $ \verified ->
                      pmatch (pverified'txCompact verified) $ \compact ->
                        plet (pfromData $ pvalueAndMint'valueAccumulator c) $ \accumulator ->
                          pmatch accumulator $ \value ->
                            plet (pfromData $ pvalueAndMint'stage c) $ \stage ->
                              pcon $
                                PVerifiedValueAndMintV1
                                  control
                                  (pcompact'body compact)
                                  ( pand'List
                                      [ NativeCompact.pnativeTxProofCommitmentV1
                                          # pfromData (pnativeControl'compactCbor native)
                                          # pfromData (pnativeControl'witnessSetCompactCbor native)
                                          # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
                                          #== pfromData (pmachineState'transactionCommitment preState)
                                      , phashValidationContext
                                          # pfromData (pnativeControl'contextCbor native)
                                          #== pfromData (pmachineState'validationContextHash preState)
                                      , pnativeScriptsControlIsWellFormed # nativeControl
                                      , pfromData (pnativeControl'executionCursor native)
                                          #== pfromData (pnativeControl'executionCount native)
                                      , plengthBS # pfromData (pvalueAndMint'replayScheduleHash c) #== 32
                                      , plengthBS # pfromData (pvalueAndMint'replayValueHash c) #== 32
                                      , plengthBS # pfromData (pvalueAndMint'replayAccumulator c) #== 32
                                      , plengthBS # pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== 32
                                      , pfromData (pvalueAndMint'replayCursor c) #>= 0
                                      , pfromData (pvalueAndMint'replayCursor c)
                                          #<= pfromData (pnativeControl'resolvedInputCount native)
                                      , pfromData (pvalueAndMint'replayAssetCursor c) #>= 0
                                      , pfromData (pvalueAndMint'replayAssetCursor c) #<= LedgerOutput.pmaxDistinctAssetCount
                                      , pif
                                          ( stage
                                              #== 2
                                              #&& pfromData (pvalueAndMint'replayAssetCursor c)
                                              #> 0
                                              #|| stage
                                              #== 3
                                              #&& pfromData (pvalueAndMint'outputAssetCursor c)
                                              #> 0
                                          )
                                          (pconstant True)
                                          ( pfromData (pvalueAndMint'replayValueHash c)
                                              #== preplicateBS
                                              # 32
                                              # (pintegerToByte # 0)
                                          )
                                      , pfromData (pvalueAndMint'outputCursor c) #>= 0
                                      , pfromData (pvalueAndMint'outputCursor c)
                                          #<= pfromData (pnativeControl'outputCount native)
                                      , pfromData (pvalueAndMint'outputAssetCursor c) #>= 0
                                      , pfromData (pvalueAndMint'outputAssetCursor c) #<= LedgerOutput.pmaxDistinctAssetCount
                                      , plengthBS # pfromData (pvalueAccumulator'assetRoot value) #== 32
                                      , pfromData (pvalueAccumulator'seenAssetCount value) #>= 0
                                      , pfromData (pvalueAccumulator'seenAssetCount value) #<= LedgerOutput.pmaxDistinctAssetCount
                                      , pfromData (pvalueAccumulator'nonzeroAssetCount value) #>= 0
                                      , pfromData (pvalueAccumulator'nonzeroAssetCount value)
                                          #<= pfromData (pvalueAccumulator'seenAssetCount value)
                                      , pfromData (pvalueAndMint'mintCursor c) #>= 0
                                      , pfromData (pvalueAndMint'mintCursor c)
                                          #<= pfromData (pnativeControl'mintCount native)
                                      , stage #>= 0
                                      , stage #<= 5
                                      , pfromData (poneStep'workWitnessCbor stepWitness)
                                          #== pencodeValueAndMintControlV1
                                          # control
                                      ]
                                  )

pwithValueAndMint ::
  forall s.
  Term s PInteger ->
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1
        :--> PNativeTxBodyCompact
        :--> PBool
    ) ->
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pwithValueAndMint expectedStage verifyBranch = plam $ \pre witness ->
  pmatch (pverifiedValueAndMintV1 # pre # witness) $ \(PVerifiedValueAndMintV1 control body valid) ->
    pmatch control $ \c ->
      valid
        #&& pfromData (pvalueAndMint'stage c)
        #== expectedStage
        #&& verifyBranch
        # pre
        # witness
        # control
        # body

pverifyValueAndMintBeginSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintBeginSemanticsV1 = pwithValueAndMint 0 $ plam $ \pre witness control _ ->
  pvalueAndMintStageZero # pre # witness # pcon PNoAuxiliaryWitness # control

pverifyValueAndMintReplayBeginSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintReplayBeginSemanticsV1 = pwithValueAndMint 1 $ plam $ \pre witness control _ ->
  pvalueAndMintStageOne # pre # witness # pcon PNoAuxiliaryWitness # control

pverifyValueAndMintReplayInputSemanticsV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pverifyValueAndMintReplayInputSemanticsV1 = phoistAcyclic $ plam $ \pre witness sourceKind key nextScheduleHash value ->
  pwithValueAndMint
    2
    ( plam $ \pre' witness' control _ ->
        pmatch control $ \c ->
          pfromData (pvalueAndMint'replayRemainingScheduleHash c)
            #/= pemptyResolutionScheduleHash
            #&& pfromData (pvalueAndMint'replayAssetCursor c)
            #== 0
            #&& pvalueAndMintStageTwoReplay
            # pre'
            # witness'
            # control
            # sourceKind
            # key
            # nextScheduleHash
            # value
    )
    # pre
    # witness

pverifyValueAndMintReplayAssetSemanticsV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1
        :--> PBool
    )
pverifyValueAndMintReplayAssetSemanticsV1 = phoistAcyclic $ plam $ \pre witness sourceKind key nextScheduleHash descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
  pwithValueAndMint
    2
    ( plam $ \pre' witness' control _ ->
        pmatch control $ \c ->
          pfromData (pvalueAndMint'replayRemainingScheduleHash c)
            #/= pemptyResolutionScheduleHash
            #&& pfromData (pvalueAndMint'replayAssetCursor c)
            #> 0
            #&& pvalueAndMintStageTwoAsset
            # pre'
            # witness'
            # control
            # sourceKind
            # key
            # nextScheduleHash
            # descriptorCbor
            # assetIndex
            # policyId
            # assetName
            # quantity
            # assetPeaks
            # assetSiblings
            # mutation
    )
    # pre
    # witness

pverifyValueAndMintReplayFinishSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintReplayFinishSemanticsV1 = pwithValueAndMint 2 $ plam $ \pre witness control _ ->
  pmatch control $ \c ->
    pfromData (pvalueAndMint'replayRemainingScheduleHash c)
      #== pemptyResolutionScheduleHash
      #&& pvalueAndMintStageTwoFinish
      # pre
      # witness
      # pcon PNoAuxiliaryWitness
      # control

pverifyValueAndMintOutputDescriptorSemanticsV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
pverifyValueAndMintOutputDescriptorSemanticsV1 = phoistAcyclic $ plam $ \pre witness outputIndex descriptorCbor siblings ->
  pwithValueAndMint
    3
    ( plam $ \pre' witness' control _ ->
        pmatch control $ \c ->
          pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
            pfromData (pvalueAndMint'outputCursor c)
              #< pfromData (pnativeControl'outputCount native)
              #&& pfromData (pvalueAndMint'outputAssetCursor c)
              #== 0
              #&& pvalueAndMintStageThreeDescriptor
              # pre'
              # witness'
              # control
              # outputIndex
              # descriptorCbor
              # siblings
    )
    # pre
    # witness

pverifyValueAndMintOutputAssetSemanticsV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1
        :--> PBool
    )
pverifyValueAndMintOutputAssetSemanticsV1 = phoistAcyclic $ plam $ \pre witness outputIndex descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
  pwithValueAndMint
    3
    ( plam $ \pre' witness' control _ ->
        pmatch control $ \c ->
          pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
            pfromData (pvalueAndMint'outputCursor c)
              #< pfromData (pnativeControl'outputCount native)
              #&& pfromData (pvalueAndMint'outputAssetCursor c)
              #> 0
              #&& pvalueAndMintStageThreeAsset
              # pre'
              # witness'
              # control
              # outputIndex
              # descriptorCbor
              # assetIndex
              # policyId
              # assetName
              # quantity
              # assetPeaks
              # assetSiblings
              # mutation
    )
    # pre
    # witness

pverifyValueAndMintOutputFinishSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintOutputFinishSemanticsV1 = pwithValueAndMint 3 $ plam $ \pre witness control _ ->
  pmatch control $ \c ->
    pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
      pfromData (pvalueAndMint'outputCursor c)
        #== pfromData (pnativeControl'outputCount native)
        #&& pvalueAndMintStageThreeFinish
        # pre
        # witness
        # pcon PNoAuxiliaryWitness
        # control

pverifyValueAndMintMintAssetSemanticsV1 ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1
        :--> PBool
    )
pverifyValueAndMintMintAssetSemanticsV1 = phoistAcyclic $ plam $ \pre witness mintIndex policyId assetName quantity siblings mutation ->
  pwithValueAndMint
    4
    ( plam $ \pre' witness' control _ ->
        pmatch control $ \c ->
          pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
            pfromData (pvalueAndMint'mintCursor c)
              #< pfromData (pnativeControl'mintCount native)
              #&& pvalueAndMintStageFourAsset
              # pre'
              # witness'
              # control
              # mintIndex
              # policyId
              # assetName
              # quantity
              # siblings
              # mutation
    )
    # pre
    # witness

pverifyValueAndMintMintFinishSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintMintFinishSemanticsV1 = pwithValueAndMint 4 $ plam $ \pre witness control _ ->
  pmatch control $ \c ->
    pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
      pfromData (pvalueAndMint'mintCursor c)
        #== pfromData (pnativeControl'mintCount native)
        #&& pvalueAndMintStageFourFinish
        # pre
        # witness
        # pcon PNoAuxiliaryWitness
        # control

pverifyValueAndMintFinalizeSemanticsV1 :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyValueAndMintFinalizeSemanticsV1 = pwithValueAndMint 5 $ plam $ \pre witness control body ->
  pvalueAndMintStageFive # pre # witness # pcon PNoAuxiliaryWitness # control # body
