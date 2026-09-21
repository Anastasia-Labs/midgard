{-# LANGUAGE OverloadedStrings #-}

-- | The target's common asset-fold claim and three authenticated dispatchers.
module Midgard.ValueAssetFold (PDescriptorClaim (..), PClaim (..), pverifyClaim, preplay, poutput, pmint) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.LedgerOutputCommitment qualified as Descriptor
import Midgard.ValidationMachine
import Midgard.ValidationMachine.ValueAndMintSemantics (PVerifiedValueAndMintV1 (..), pverifiedValueAndMintV1)
import Midgard.ValidationMerkle (PFrontierPeak, pverifyMembership)
import Midgard.ValidationTrace (PValidationMachineStateV1)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

data PDescriptorClaim s = PDescriptorClaim
  { pdescriptor'cbor :: Term s (PAsData PByteString)
  , pdescriptor'assetIndex :: Term s (PAsData PInteger)
  , pdescriptor'peaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pdescriptor'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pdescriptor'assetCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDescriptorClaim)

data PClaim s = PClaim
  { pclaim'policy :: Term s (PAsData PByteString)
  , pclaim'asset :: Term s (PAsData PByteString)
  , pclaim'quantity :: Term s (PAsData PInteger)
  , pclaim'mutation :: Term s (PAsData PValueAssetMutationWitnessV1)
  , pclaim'preValue :: Term s (PAsData PValueAccumulatorV1)
  , pclaim'outcome :: Term s (PAsData PValueAccumulatorUpdateV1)
  , pclaim'descriptor :: Term s (PAsData (PMaybeData PDescriptorClaim))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PClaim)

pverifyClaim :: forall s. Term s (PClaim :--> PBool :--> PBool)
pverifyClaim = phoistAcyclic $ plam $ \claim negateQuantity -> pmatch claim $ \c ->
  pmatch (pfromData $ pclaim'outcome c) $ \case
    PValueAccumulatorMutationInvalid -> pconstant False
    _ ->
      ( pmatch (pfromData $ pclaim'descriptor c) $ \case
          PDNothing -> pconstant True
          PDJust d -> pmatch (pfromData d) $ \details ->
            plet (Descriptor.pdecodeLedgerOutputCommitment # pfromData (pdescriptor'cbor details)) $ \descriptor -> pmatch descriptor $ \parsed ->
              pfromData (Descriptor.poutputCommitment'assetCount parsed)
                #== pfromData (pdescriptor'assetCount details)
                #&& Descriptor.pverifyOutputAssetMembership
                # descriptor
                # pfromData (pdescriptor'assetIndex details)
                # pfromData (pclaim'policy c)
                # pfromData (pclaim'asset c)
                # pfromData (pclaim'quantity c)
                # pfromData (pdescriptor'peaks details)
                # pfromData (pdescriptor'siblings details)
      )
        #&& papplyValueAssetMutation
        # pfromData (pclaim'preValue c)
        # (pfromData (pclaim'policy c) <> pfromData (pclaim'asset c))
        # (pif negateQuantity (0 - pfromData (pclaim'quantity c)) (pfromData $ pclaim'quantity c))
        # pfromData (pclaim'mutation c)
        #== pfromData (pclaim'outcome c)

preplay :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PByteString :--> PByteString :--> PClaim :--> PBool)
preplay = phoistAcyclic $ plam $ \pre witness kind key next claim ->
  pmatch (pverifiedValueAndMintV1 # pre # witness) $ \(PVerifiedValueAndMintV1 control _ valid) ->
    pmatch control $ \c -> pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native -> pmatch claim $ \q ->
      pmatch (pfromData $ pclaim'descriptor q) $ \case
        PDNothing -> perror
        PDJust descriptor -> pmatch (pfromData descriptor) $ \d ->
          pand'List
            [ valid
            , pfromData (pvalueAndMint'stage c) #== 2
            , pfromData (pvalueAndMint'replayRemainingScheduleHash c) #/= pemptyResolutionScheduleHash
            , pfromData (pvalueAndMint'replayAssetCursor c) #> 0
            , pfromData (pvalueAndMint'valueAccumulator c) #== pfromData (pclaim'preValue q)
            , kind #== 0
            , presolutionScheduleNodeHash # kind # key # next #== pfromData (pvalueAndMint'replayRemainingScheduleHash c)
            , pfromData (pvalueAndMint'replayCursor c) #< pfromData (pnativeControl'resolvedInputCount native)
            , pblake2b_256 # pfromData (pdescriptor'cbor d) #== pfromData (pvalueAndMint'replayValueHash c)
            , pfromData (pdescriptor'assetIndex d) #== pfromData (pvalueAndMint'replayAssetCursor c) - 1
            , withOutcome pre witness (pfromData $ pclaim'outcome q) $ \value ->
                pvalueAndMintSuccessorIsExact
                  # pre
                  # witness
                  # ( pif
                        (pfromData (pvalueAndMint'replayAssetCursor c) #== pfromData (pdescriptor'assetCount d))
                        (pcompleteValueInputReplay # control # kind # key # pfromData (pdescriptor'cbor d) # next # value)
                        (pcon c{pvalueAndMint'replayAssetCursor = pdata $ pfromData (pvalueAndMint'replayAssetCursor c) + 1, pvalueAndMint'valueAccumulator = pdata value})
                    )
            ]

poutput :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PClaim :--> PBool)
poutput = phoistAcyclic $ plam $ \pre witness index claim ->
  pmatch (pverifiedValueAndMintV1 # pre # witness) $ \(PVerifiedValueAndMintV1 control _ valid) ->
    pmatch control $ \c -> pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native -> pmatch claim $ \q ->
      pmatch (pfromData $ pclaim'descriptor q) $ \case
        PDNothing -> perror
        PDJust descriptor -> pmatch (pfromData descriptor) $ \d ->
          pand'List
            [ valid
            , pfromData (pvalueAndMint'stage c) #== 3
            , pfromData (pvalueAndMint'outputCursor c) #< pfromData (pnativeControl'outputCount native)
            , pfromData (pvalueAndMint'outputAssetCursor c) #> 0
            , pfromData (pvalueAndMint'valueAccumulator c) #== pfromData (pclaim'preValue q)
            , index #== pfromData (pvalueAndMint'outputCursor c)
            , pfromData (pdescriptor'assetIndex d) #== pfromData (pvalueAndMint'outputAssetCursor c) - 1
            , pblake2b_256 # pfromData (pdescriptor'cbor d) #== pfromData (pvalueAndMint'replayValueHash c)
            , withOutcome pre witness (pfromData $ pclaim'outcome q) $ \value ->
                pvalueAndMintSuccessorIsExact
                  # pre
                  # witness
                  # ( pif
                        (pfromData (pvalueAndMint'outputAssetCursor c) #== pfromData (pdescriptor'assetCount d))
                        ( pcon
                            c
                              { pvalueAndMint'outputCursor = pdata $ pfromData (pvalueAndMint'outputCursor c) + 1
                              , pvalueAndMint'outputAssetCursor = pdata 0
                              , pvalueAndMint'replayValueHash = pdata $ preplicateBS # 32 # (pintegerToByte # 0)
                              , pvalueAndMint'valueAccumulator = pdata value
                              }
                        )
                        ( pcon
                            c
                              { pvalueAndMint'outputAssetCursor = pdata $ pfromData (pvalueAndMint'outputAssetCursor c) + 1
                              , pvalueAndMint'valueAccumulator = pdata value
                              }
                        )
                    )
            ]

pmint :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PBuiltinList (PAsData PByteString) :--> PClaim :--> PBool)
pmint = phoistAcyclic $ plam $ \pre witness index siblings claim ->
  pmatch (pverifiedValueAndMintV1 # pre # witness) $ \(PVerifiedValueAndMintV1 control _ valid) ->
    pmatch control $ \c -> pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native -> pmatch claim $ \q ->
      pand'List
        [ valid
        , pfromData (pvalueAndMint'stage c) #== 4
        , pfromData (pvalueAndMint'mintCursor c) #< pfromData (pnativeControl'mintCount native)
        , pfromData (pvalueAndMint'valueAccumulator c) #== pfromData (pclaim'preValue q)
        , pmatch (pfromData $ pclaim'descriptor q) $ \case PDNothing -> pconstant True; PDJust _ -> pconstant False
        , index #== pfromData (pvalueAndMint'mintCursor c)
        , plengthBS # pfromData (pclaim'policy q) #== 28
        , plengthBS # pfromData (pclaim'asset q) #<= 32
        , pfromData (pclaim'quantity q) #/= 0
        , pverifyMembership
            # pfromData (pnativeControl'mintCount native)
            # pfromData (pnativeControl'mintPeaks native)
            # index
            # (pmintAssetLeafHash # pfromData (pclaim'policy q) # pfromData (pclaim'asset q) # pfromData (pclaim'quantity q))
            # siblings
        , withOutcome pre witness (pfromData $ pclaim'outcome q) $ \value ->
            pvalueAndMintSuccessorIsExact
              # pre
              # witness
              # (pcon c{pvalueAndMint'mintCursor = pdata $ pfromData (pvalueAndMint'mintCursor c) + 1, pvalueAndMint'valueAccumulator = pdata value})
        ]

withOutcome :: forall s. Term s PValidationMachineStateV1 -> Term s PValidationOneStepWitnessV1 -> Term s PValueAccumulatorUpdateV1 -> (Term s PValueAccumulatorV1 -> Term s PBool) -> Term s PBool
withOutcome pre witness outcome next = pmatch witness $ \w -> pmatch outcome $ \case
  PValueAccumulatorAssetLimitExceeded -> prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor w) # pconstant "E_ASSET_COUNT"
  PValueAccumulatorMutationInvalid -> pconstant False
  PValueAccumulatorUpdated value -> next $ pfromData value
