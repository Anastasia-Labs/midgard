{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMintSemantics
Description : Split ValueAndMint semantic resolvers.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMintSemantics (
  PValueAndMintSimpleActionV1 (..),
  PValueAndMintReplayInputActionV1 (..),
  PValueAndMintReplayAssetActionV1 (..),
  PValueAndMintOutputDescriptorActionV1 (..),
  PValueAndMintOutputAssetActionV1 (..),
  PValueAndMintMintAssetActionV1 (..),
  valueAndMintBeginSemanticV1Validator,
  valueAndMintReplayBeginSemanticV1Validator,
  valueAndMintReplayInputSemanticV1Validator,
  valueAndMintReplayAssetSemanticV1Validator,
  valueAndMintReplayFinishSemanticV1Validator,
  valueAndMintOutputDescriptorSemanticV1Validator,
  valueAndMintOutputAssetSemanticV1Validator,
  valueAndMintOutputFinishSemanticV1Validator,
  valueAndMintMintAssetSemanticV1Validator,
  valueAndMintMintFinishSemanticV1Validator,
  valueAndMintFinalizeSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo, PTxOutRef)
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.StateQueueYield qualified as Yield
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
 )
import Midgard.ValidationMachine.ValueAndMintSemantics
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationMachineStateV1, PValidationPhase (PValueAndMint))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.ValueAssetFold qualified as Fold
import Midgard.ValueAssetFoldYield qualified as FoldYield

data PValueAndMintSimpleActionV1 (s :: S)
  = PVerifyValueAndMintSimple
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintSimpleActionV1)

data PValueAndMintReplayInputActionV1 (s :: S)
  = PVerifyValueAndMintReplayInput
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintReplayInputActionV1)

data PValueAndMintReplayAssetActionV1 (s :: S)
  = PVerifyValueAndMintReplayAsset
      (Term s (PAsData Fold.PClaim))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintReplayAssetActionV1)

data PValueAndMintOutputDescriptorActionV1 (s :: S)
  = PVerifyValueAndMintOutputDescriptor
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintOutputDescriptorActionV1)

data PValueAndMintOutputAssetActionV1 (s :: S)
  = PVerifyValueAndMintOutputAsset
      (Term s (PAsData Fold.PClaim))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintOutputAssetActionV1)

data PValueAndMintMintAssetActionV1 (s :: S)
  = PVerifyValueAndMintMintAsset
      (Term s (PAsData Fold.PClaim))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintMintAssetActionV1)

pcontinueValueAndMint ::
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PInteger) ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PData ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueValueAndMint awardScriptHash policyId datum inputIndex outputIndex transition auxiliary valid ownOutRef txInfo =
  pcontinueWinning
    (pcon PValueAndMint)
    awardScriptHash
    policyId
    datum
    (pfromData inputIndex)
    (pfromData outputIndex)
    transition
    auxiliary
    valid
    ownOutRef
    txInfo

psimpleValueAndMintValidator ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool) ->
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
psimpleValueAndMintValidator verify = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintSimpleActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintSimple inputIndex outputIndex transitionD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
            pcontinueValueAndMint
              awardScriptHash
              policyId
              datum
              inputIndex
              outputIndex
              transition
              (pforgetData $ pdata auxiliary)
              (verify # pvalidationSemanticPreState datum # transition)
              ownOutRef
              txInfo

valueAndMintBeginSemanticV1Validator
  , valueAndMintReplayBeginSemanticV1Validator
  , valueAndMintReplayInputSemanticV1Validator
  , valueAndMintReplayFinishSemanticV1Validator
  , valueAndMintOutputDescriptorSemanticV1Validator
  , valueAndMintOutputFinishSemanticV1Validator
  , valueAndMintMintFinishSemanticV1Validator
  , valueAndMintFinalizeSemanticV1Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueAndMintBeginSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintBeginSemanticsV1
valueAndMintReplayBeginSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintReplayBeginSemanticsV1
valueAndMintReplayFinishSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintReplayFinishSemanticsV1
valueAndMintOutputFinishSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintOutputFinishSemanticsV1
valueAndMintMintFinishSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintMintFinishSemanticsV1
valueAndMintFinalizeSemanticV1Validator = psimpleValueAndMintValidator pverifyValueAndMintFinalizeSemanticsV1
valueAndMintReplayInputSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintReplayInputActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintReplayInput inputIndex outputIndex transitionD sourceKindD keyD nextScheduleHashD valueD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pcon $ PResolvedInputReplayWitness sourceKindD keyD nextScheduleHashD valueD) $ \auxiliary ->
            pcontinueValueAndMint
              awardScriptHash
              policyId
              datum
              inputIndex
              outputIndex
              transition
              (pforgetData $ pdata auxiliary)
              ( pverifyValueAndMintReplayInputSemanticsV1
                  # pvalidationSemanticPreState datum
                  # transition
                  # pfromData sourceKindD
                  # pfromData keyD
                  # pfromData nextScheduleHashD
                  # pfromData valueD
              )
              ownOutRef
              txInfo
valueAndMintReplayAssetSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueAndMintReplayAssetSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintReplayAssetActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintReplayAsset claimD inputIndex outputIndex transitionD sourceKindD keyD nextScheduleHashD yieldIndexD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData claimD) $ \claim ->
            pmatch claim $ \c ->
              pmatch (pfromData $ Fold.pclaim'descriptor c) $ \case
                PDNothing -> perror
                PDJust descriptorD -> pmatch (pfromData descriptorD) $ \descriptor ->
                  plet
                    ( pcon $
                        PValueInputAssetWitness
                          sourceKindD
                          keyD
                          nextScheduleHashD
                          (Fold.pdescriptor'cbor descriptor)
                          (Fold.pdescriptor'assetIndex descriptor)
                          (Fold.pclaim'policy c)
                          (Fold.pclaim'asset c)
                          (Fold.pclaim'quantity c)
                          (Fold.pdescriptor'peaks descriptor)
                          (Fold.pdescriptor'siblings descriptor)
                          (Fold.pclaim'mutation c)
                    )
                    $ \auxiliary ->
                      plet
                        ( Yield.prequireAuthenticatedZeroYield
                            # txInfo
                            # pfromData authPolicy
                            # FoldYield.prole
                            # pfromData yieldIndexD
                        )
                        $ \yieldHash ->
                          pcontinueValueAndMint
                            awardScriptHash
                            policyId
                            datum
                            inputIndex
                            outputIndex
                            transition
                            (pforgetData $ pdata auxiliary)
                            ( plengthBS
                                # pto yieldHash
                                #> 0
                                #&& Fold.preplay
                                # pvalidationSemanticPreState datum
                                # transition
                                # pfromData sourceKindD
                                # pfromData keyD
                                # pfromData nextScheduleHashD
                                # claim
                            )
                            ownOutRef
                            txInfo
valueAndMintOutputDescriptorSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintOutputDescriptorActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintOutputDescriptor inputIndex outputIndex transitionD ledgerOutputIndexD descriptorCborD siblingsD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pcon $ PValueOutputDescriptorWitness ledgerOutputIndexD descriptorCborD siblingsD) $ \auxiliary ->
            pcontinueValueAndMint
              awardScriptHash
              policyId
              datum
              inputIndex
              outputIndex
              transition
              (pforgetData $ pdata auxiliary)
              ( pverifyValueAndMintOutputDescriptorSemanticsV1
                  # pvalidationSemanticPreState datum
                  # transition
                  # pfromData ledgerOutputIndexD
                  # pfromData descriptorCborD
                  # pfromData siblingsD
              )
              ownOutRef
              txInfo
valueAndMintOutputAssetSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueAndMintOutputAssetSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintOutputAssetActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintOutputAsset claimD inputIndex outputIndex transitionD ledgerOutputIndexD yieldIndexD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData claimD) $ \claim ->
            pmatch claim $ \c ->
              pmatch (pfromData $ Fold.pclaim'descriptor c) $ \case
                PDNothing -> perror
                PDJust descriptorD -> pmatch (pfromData descriptorD) $ \descriptor ->
                  plet
                    ( pcon $
                        PValueOutputAssetWitness
                          ledgerOutputIndexD
                          (Fold.pdescriptor'cbor descriptor)
                          (Fold.pdescriptor'assetIndex descriptor)
                          (Fold.pclaim'policy c)
                          (Fold.pclaim'asset c)
                          (Fold.pclaim'quantity c)
                          (Fold.pdescriptor'peaks descriptor)
                          (Fold.pdescriptor'siblings descriptor)
                          (Fold.pclaim'mutation c)
                    )
                    $ \auxiliary ->
                      plet
                        ( Yield.prequireAuthenticatedZeroYield
                            # txInfo
                            # pfromData authPolicy
                            # FoldYield.prole
                            # pfromData yieldIndexD
                        )
                        $ \yieldHash ->
                          pcontinueValueAndMint
                            awardScriptHash
                            policyId
                            datum
                            inputIndex
                            outputIndex
                            transition
                            (pforgetData $ pdata auxiliary)
                            ( plengthBS
                                # pto yieldHash
                                #> 0
                                #&& Fold.poutput
                                # pvalidationSemanticPreState datum
                                # transition
                                # pfromData ledgerOutputIndexD
                                # claim
                            )
                            ownOutRef
                            txInfo
valueAndMintMintAssetSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueAndMintMintAssetSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PValueAndMintMintAssetActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyValueAndMintMintAsset claimD inputIndex outputIndex transitionD mintIndexD siblingsD yieldIndexD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData claimD) $ \claim ->
            pmatch claim $ \c ->
              plet
                ( pcon $
                    PValueMintAssetWitness
                      mintIndexD
                      (Fold.pclaim'policy c)
                      (Fold.pclaim'asset c)
                      (Fold.pclaim'quantity c)
                      siblingsD
                      (Fold.pclaim'mutation c)
                )
                $ \auxiliary ->
                  plet
                    ( Yield.prequireAuthenticatedZeroYield
                        # txInfo
                        # pfromData authPolicy
                        # FoldYield.prole
                        # pfromData yieldIndexD
                    )
                    $ \yieldHash ->
                      pcontinueValueAndMint
                        awardScriptHash
                        policyId
                        datum
                        inputIndex
                        outputIndex
                        transition
                        (pforgetData $ pdata auxiliary)
                        ( plengthBS
                            # pto yieldHash
                            #> 0
                            #&& Fold.pmint
                            # pvalidationSemanticPreState datum
                            # transition
                            # pfromData mintIndexD
                            # pfromData siblingsD
                            # claim
                        )
                        ownOutRef
                        txInfo
