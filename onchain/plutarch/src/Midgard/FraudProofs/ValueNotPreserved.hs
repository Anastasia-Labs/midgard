{-# LANGUAGE OverloadedStrings #-}

module Midgard.FraudProofs.ValueNotPreserved (
    PClaimedAssetV1 (..),
    PClaimedImbalanceDirectionV1 (..),
    PStep01Args (..),
    PStep02State (..),
    PAssetLeafOpeningV1 (..),
    PSpentInputValueWitnessV1 (..),
    PStep02FoldArgs (..),
    PStep02Args (..),
    PStep03State (..),
    PStep03Args (..),
    PStep04State (..),
    PStep04Args (..),
    pvalueNotPreservedFraudCategoryId,
    pfraudCategoryIsValueNotPreservedV1,
    pclaimedAssetIsWellFormedV1,
    punitMatchesClaimV1,
    pspentInputClaimedQuantityV1,
    poutputClaimedQuantityV1,
    pmintItemClaimedQuantityV1,
    pvalueNotPreservedFaultIsEstablishedV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PTokenName)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.Common.Types (PProof)
import Midgard.FraudProofs.Common (PNativeTxInclusionArgs)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Components (
    pdecodeMidgardTxOutputCbor,
    pencodeMidgardTxInput,
 )
import Midgard.FraudProofs.NativeTx.Preimages (pdecodeMintPolicyItemCbor)
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardTxInput,
    PMidgardTxOutput (..),
    PMidgardValue (..),
 )
import Midgard.FraudProofs.TransitionTrace.Proof (pverifyLedgerMembership)
import Midgard.LedgerOutputCommitment (
    PLedgerOutputCommitmentV1 (..),
    pdecodeLedgerOutputCommitment,
    pverifyOutputAssetMembership,
 )
import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.Validators.FraudProofs.Step (pexpecting)

data PClaimedAssetV1 (s :: S)
    = PAdaAsset
    | PTokenAsset (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PClaimedAssetV1)

data PClaimedImbalanceDirectionV1 (s :: S)
    = PClaimedAssetInflated
    | PClaimedAssetDeflated
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PClaimedImbalanceDirectionV1)

data PStep01Args (s :: S)
    = PStep01Args
        { pstep01Args'txInclusion :: Term s (PAsData PNativeTxInclusionArgs)
        , pstep01Args'claimedAsset :: Term s (PAsData PClaimedAssetV1)
        , pstep01Args'claimedDirection :: Term s (PAsData PClaimedImbalanceDirectionV1)
        }
    | PLaunchUnion (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s PData)
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
    { pstep02State'badTxId :: Term s (PAsData PByteString)
    , pstep02State'claimedAsset :: Term s (PAsData PClaimedAssetV1)
    , pstep02State'claimedDirection :: Term s (PAsData PClaimedImbalanceDirectionV1)
    , pstep02State'committedFee :: Term s (PAsData PInteger)
    , pstep02State'prevUtxosRoot :: Term s (PAsData PByteString)
    , pstep02State'inputCursor :: Term s (PAsData PInteger)
    , pstep02State'claimedDelta :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PAssetLeafOpeningV1 (s :: S) = PAssetLeafOpeningV1
    { passetOpening'policyId :: Term s (PAsData PByteString)
    , passetOpening'assetName :: Term s (PAsData PByteString)
    , passetOpening'quantity :: Term s (PAsData PInteger)
    , passetOpening'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAssetLeafOpeningV1)

data PSpentInputValueWitnessV1 (s :: S) = PSpentInputValueWitnessV1
    { pvalueWitness'descriptorCbor :: Term s (PAsData PByteString)
    , pvalueWitness'ledgerMembershipProof :: Term s (PAsData PProof)
    , pvalueWitness'assetPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pvalueWitness'assetOpenings :: Term s (PAsData (PBuiltinList (PAsData PAssetLeafOpeningV1)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSpentInputValueWitnessV1)

data PStep02FoldArgs (s :: S) = PStep02FoldArgs
    { pstep02FoldArgs'inputIndex :: Term s (PAsData PInteger)
    , pstep02FoldArgs'outputIndex :: Term s (PAsData PInteger)
    , pstep02FoldArgs'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
    , pstep02FoldArgs'valueWitness :: Term s (PAsData PSpentInputValueWitnessV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02FoldArgs)

data PStep02Args (s :: S)
    = PFoldInput (Term s (PAsData PStep02FoldArgs))
    | PFinishInputs
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S) = PStep03State
    { pstep03State'badTxId :: Term s (PAsData PByteString)
    , pstep03State'claimedAsset :: Term s (PAsData PClaimedAssetV1)
    , pstep03State'claimedDirection :: Term s (PAsData PClaimedImbalanceDirectionV1)
    , pstep03State'committedFee :: Term s (PAsData PInteger)
    , pstep03State'claimedDelta :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args (s :: S) = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'nativeTxCompactCbor :: Term s (PAsData PByteString)
    , pstep03Args'outputsCarriage :: Term s (PAsData PFieldCarriageV1)
    , pstep03Args'mintCarriage :: Term s (PMaybeData PFieldCarriageV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State (s :: S) = PStep04State
    { pstep04State'badTxId :: Term s (PAsData PByteString)
    , pstep04State'claimedAsset :: Term s (PAsData PClaimedAssetV1)
    , pstep04State'claimedDirection :: Term s (PAsData PClaimedImbalanceDirectionV1)
    , pstep04State'finalDelta :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args (s :: S) = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

pvalueNotPreservedFraudCategoryId :: forall s. Term s PByteString
pvalueNotPreservedFraudCategoryId = pconstant "\x00\x00\x00\x19"

pfraudCategoryIsValueNotPreservedV1 :: forall s. Term s (PTokenName :--> PBool)
pfraudCategoryIsValueNotPreservedV1 = phoistAcyclic $ plam $ \assetName ->
    plet (pto assetName) $ \bytes ->
        plengthBS
            # bytes
            #== 32
            #&& psliceBS
            # 0
            # 4
            # bytes
            #== pvalueNotPreservedFraudCategoryId

pclaimedAssetIsWellFormedV1 :: forall s. Term s (PClaimedAssetV1 :--> PBool)
pclaimedAssetIsWellFormedV1 = phoistAcyclic $ plam $ \claimed -> pmatch claimed $ \case
    PAdaAsset -> pconstant True
    PTokenAsset policyId assetName ->
        plengthBS
            # pfromData policyId
            #== 28
            #&& plengthBS
            # pfromData assetName
            #<= 32

punitMatchesClaimV1 :: forall s. Term s (PClaimedAssetV1 :--> PByteString :--> PBool)
punitMatchesClaimV1 = phoistAcyclic $ plam $ \claimed unit -> pmatch claimed $ \case
    PAdaAsset -> pconstant False
    PTokenAsset policyId assetName ->
        psliceBS
            # 0
            # 28
            # unit
            #== pfromData policyId
            #&& psliceBS
            # 28
            # (plengthBS # unit - 28)
            # unit
            #== pfromData assetName

pspentInputClaimedQuantityV1 ::
    forall s.
    Term s (PByteString :--> PMidgardTxInput :--> PSpentInputValueWitnessV1 :--> PClaimedAssetV1 :--> PInteger)
pspentInputClaimedQuantityV1 = phoistAcyclic $ plam $ \prevRoot spendInput witness claimed -> P.do
    PSpentInputValueWitnessV1
        { pvalueWitness'descriptorCbor
        , pvalueWitness'ledgerMembershipProof
        , pvalueWitness'assetPeaks
        , pvalueWitness'assetOpenings
        } <-
        pmatch witness
    descriptorCbor <- plet $ pfromData pvalueWitness'descriptorCbor
    descriptor <- plet $ pdecodeLedgerOutputCommitment # descriptorCbor
    pexpecting
        ( pverifyLedgerMembership
            prevRoot
            (pencodeMidgardTxInput # spendInput)
            descriptorCbor
            (pfromData pvalueWitness'ledgerMembershipProof)
        )
        $ pmatch claimed
        $ \case
            PAdaAsset ->
                pexpecting (pnull # pfromData pvalueWitness'assetOpenings) $
                    pexpecting (pnull # pfromData pvalueWitness'assetPeaks) $
                        pmatch descriptor $
                            \fields -> pfromData $ poutputCommitment'lovelace fields
            PTokenAsset _ _ ->
                pmatch descriptor $ \fields ->
                    pexpecting
                        (plength # pfromData pvalueWitness'assetOpenings #== pfromData (poutputCommitment'assetCount fields))
                        $ psumClaimedOpenings
                            # descriptor
                            # claimed
                            # pfromData pvalueWitness'assetPeaks
                            # pfromData pvalueWitness'assetOpenings
                            # 0
                            # 0

psumClaimedOpenings ::
    forall s.
    Term
        s
        ( PLedgerOutputCommitmentV1
            :--> PClaimedAssetV1
            :--> PBuiltinList (PAsData PFrontierPeak)
            :--> PBuiltinList (PAsData PAssetLeafOpeningV1)
            :--> PInteger
            :--> PInteger
            :--> PInteger
        )
psumClaimedOpenings = phoistAcyclic $ pfix $ \self -> plam $ \descriptor claimed peaks openings index accumulated ->
    pmatch openings $ \case
        PNil -> accumulated
        PCons openingD rest -> P.do
            PAssetLeafOpeningV1
                { passetOpening'policyId
                , passetOpening'assetName
                , passetOpening'quantity
                , passetOpening'siblings
                } <-
                pmatch $ pfromData openingD
            policyId <- plet $ pfromData passetOpening'policyId
            assetName <- plet $ pfromData passetOpening'assetName
            quantity <- plet $ pfromData passetOpening'quantity
            contribution <- plet $ pif (punitMatchesClaimV1 # claimed # (policyId <> assetName)) quantity 0
            pexpecting
                ( pverifyOutputAssetMembership
                    # descriptor
                    # index
                    # policyId
                    # assetName
                    # quantity
                    # peaks
                    # pfromData passetOpening'siblings
                )
                (self # descriptor # claimed # peaks # rest # (index + 1) # (accumulated + contribution))

poutputClaimedQuantityV1 :: forall s. Term s (PByteString :--> PClaimedAssetV1 :--> PInteger)
poutputClaimedQuantityV1 = phoistAcyclic $ plam $ \outputCbor claimed -> P.do
    PMidgardTxOutput{ptxOutput'value} <- pmatch $ pdecodeMidgardTxOutputCbor # outputCbor
    PMidgardValue{pvalue'lovelace, pvalue'assets} <- pmatch $ pfromData ptxOutput'value
    pmatch claimed $ \case
        PAdaAsset -> pfromData pvalue'lovelace
        PTokenAsset _ _ -> psumOutputAssets # claimed # pto (pfromData pvalue'assets) # 0

psumOutputAssets ::
    forall s.
    Term s (PClaimedAssetV1 :--> PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger)) :--> PInteger :--> PInteger)
psumOutputAssets = phoistAcyclic $ pfix $ \self -> plam $ \claimed assets accumulated ->
    pmatch assets $ \case
        PNil -> accumulated
        PCons pair rest ->
            pmatch pair $ \(PBuiltinPair unit quantity) ->
                self
                    # claimed
                    # rest
                    # ( accumulated
                            + pif
                                (punitMatchesClaimV1 # claimed # pfromData unit)
                                (pfromData quantity)
                                0
                      )

pmintItemClaimedQuantityV1 :: forall s. Term s (PByteString :--> PClaimedAssetV1 :--> PInteger)
pmintItemClaimedQuantityV1 = phoistAcyclic $ plam $ \itemCbor claimed ->
    pmatch claimed $ \case
        PAdaAsset -> perror
        PTokenAsset policyId assetName ->
            pmatch (pdecodeMintPolicyItemCbor # itemCbor) $ \(PPair itemPolicyId assets) ->
                pif
                    (itemPolicyId #== pfromData policyId)
                    (psumMintAssets # pfromData assetName # assets # 0)
                    0

psumMintAssets ::
    forall s.
    Term s (PByteString :--> PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger)) :--> PInteger :--> PInteger)
psumMintAssets = phoistAcyclic $ pfix $ \self -> plam $ \claimedName assets accumulated ->
    pmatch assets $ \case
        PNil -> accumulated
        PCons pair rest ->
            pmatch pair $ \(PBuiltinPair name quantity) ->
                self
                    # claimedName
                    # rest
                    # ( accumulated
                            + pif
                                (pfromData name #== claimedName)
                                (pfromData quantity)
                                0
                      )

pvalueNotPreservedFaultIsEstablishedV1 :: forall s. Term s (PStep04State :--> PBool)
pvalueNotPreservedFaultIsEstablishedV1 = phoistAcyclic $ plam $ \state ->
    pmatch state $ \PStep04State{pstep04State'claimedDirection, pstep04State'finalDelta} ->
        pmatch (pfromData pstep04State'claimedDirection) $ \case
            PClaimedAssetInflated -> pfromData pstep04State'finalDelta #< 0
            PClaimedAssetDeflated -> pfromData pstep04State'finalDelta #> 0
