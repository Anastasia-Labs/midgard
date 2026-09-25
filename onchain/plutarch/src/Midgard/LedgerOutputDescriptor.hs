{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.LedgerOutputDescriptor
Description : Plutarch port of @lib/midgard/ledger-output-descriptor-v1.ak@.

The one-shot builder for the compact value stored in @utxos_root@.  It is the
in-memory twin of the staged ledger-output admission machine: both derive the
same sixteen-field descriptor, but this route is used where an authenticated
one-step rule already has the complete canonical output bytes.
-}
module Midgard.LedgerOutputDescriptor (
    pbuildV1,
    passemble,
    pledgerValueV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.BoundedItem qualified as BoundedItem
import Midgard.CekData (PDataSummaryV1)
import Midgard.FraudProofs.NativeTx.Codec (
    pcborInt,
    pencodeDefiniteBytes,
    pencodeDefiniteMapHeader,
 )
import Midgard.FraudProofs.NativeTx.Components (
    pencodeMidgardAddress,
    pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardTxOutput (..),
    PMidgardValue (..),
    PMidgardVersionedScript (..),
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.LedgerOutputCommitment (
    PLedgerOutputCommitmentV1 (..),
    passetLeafHash,
    pdescriptorIsWellFormed,
    pencodeLedgerOutputCommitment,
    pledgerOutputCommitmentVersion,
    poutputFieldIndex,
    poutputItemCommitment,
 )
import Midgard.ScriptContext (pledgerOutputSummariesV1)
import Midgard.ScriptProof (planguageTag, pversionedScriptHash)
import Midgard.ValidationMerkle (
    PFrontierPeak,
    pappendLeaf,
    pemptyFrontier,
    pfrontierCommitment,
 )

type PAssetList =
    PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger))

data PAssetFoldV1 (s :: S) = PAssetFoldV1
    { passetFold'count :: Term s PInteger
    , passetFold'peaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
    , passetFold'policyCount :: Term s PInteger
    , passetFold'size :: Term s PInteger
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PAssetFoldV1)

data PReferenceScriptFactsV1 (s :: S) = PReferenceScriptFactsV1
    { preferenceFacts'language :: Term s PInteger
    , preferenceFacts'hash :: Term s PByteString
    , preferenceFacts'totalLength :: Term s PInteger
    , preferenceFacts'itemCommitment :: Term s PByteString
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PReferenceScriptFactsV1)

ppolicyId :: forall s. Term s PByteString -> Term s PByteString
ppolicyId unit = psliceBS # 0 # 28 # unit

passetName :: forall s. Term s PByteString -> Term s PByteString
passetName unit = psliceBS # 28 # (plengthBS # unit - 28) # unit

ppolicyRunLength :: forall s. Term s (PByteString :--> PAssetList :--> PInteger)
ppolicyRunLength = phoistAcyclic $ pfix $ \self -> plam $ \policyId assets ->
    pelimList
        ( \asset rest ->
            pmatch asset $ \(PBuiltinPair unitData _) ->
                pif
                    (ppolicyId (pfromData unitData) #== policyId)
                    (1 + self # policyId # rest)
                    0
        )
        0
        assets

pfoldAssets :: forall s. Term s (PAssetList :--> PByteString :--> PAssetFoldV1 :--> PAssetFoldV1)
pfoldAssets = phoistAcyclic $ pfix $ \self -> plam $ \assets previousPolicy accumulator ->
    pelimList
        ( \asset rest ->
            pmatch asset $ \(PBuiltinPair unitData quantityData) ->
                plet (pfromData unitData) $ \unit ->
                    plet (ppolicyId unit) $ \policyId ->
                        plet (passetName unit) $ \assetName ->
                            plet (policyId #/= previousPolicy) $ \opensGroup ->
                                pmatch accumulator $ \folded ->
                                    plet
                                        ( pif
                                            opensGroup
                                            ( (plengthBS #$ pencodeDefiniteBytes # policyId)
                                                + ( plengthBS
                                                        #$ pencodeDefiniteMapHeader
                                                        # (ppolicyRunLength # policyId # assets)
                                                  )
                                            )
                                            0
                                        )
                                        $ \groupBytes ->
                                            self
                                                # rest
                                                # policyId
                                                # ( pcon $
                                                        PAssetFoldV1
                                                            (passetFold'count folded + 1)
                                                            ( pappendLeaf
                                                                # passetFold'count folded
                                                                # passetFold'peaks folded
                                                                # ( passetLeafHash
                                                                        # policyId
                                                                        # assetName
                                                                        # pfromData quantityData
                                                                  )
                                                            )
                                                            ( passetFold'policyCount folded
                                                                + pif opensGroup 1 0
                                                            )
                                                            ( passetFold'size folded
                                                                + groupBytes
                                                                + (plengthBS #$ pencodeDefiniteBytes # assetName)
                                                                + (plengthBS #$ pcborInt (pfromData quantityData))
                                                            )
                                                  )
        )
        accumulator
        assets

pcardanoValueSize :: forall s. Term s PInteger -> Term s PAssetFoldV1 -> Term s PInteger
pcardanoValueSize lovelace assets = pmatch assets $ \folded ->
    pif
        (passetFold'count folded #== 0)
        (plengthBS # pcborInt lovelace)
        ( 1
            + (plengthBS # pcborInt lovelace)
            + (plengthBS #$ pencodeDefiniteMapHeader # passetFold'policyCount folded)
            + passetFold'size folded
        )

preferenceScriptFacts ::
    forall s.
    Term s PInteger ->
    Term s PMidgardTxOutput ->
    Term s PReferenceScriptFactsV1
preferenceScriptFacts outputIndex output = pmatch output $ \fields ->
    pmatch (pfromData $ ptxOutput'scriptRef fields) $ \case
        PDNothing ->
            pcon $
                PReferenceScriptFactsV1
                    (-1)
                    (pconstant "")
                    0
                    (pconstant "")
        PDJust scriptData ->
            plet (pfromData scriptData :: Term s PMidgardVersionedScript) $ \script ->
                plet (pencodeMidgardVersionedScript # script) $ \scriptCbor ->
                    pmatch script $ \scriptFields ->
                        pcon $
                            PReferenceScriptFactsV1
                                (planguageTag # pfromData (pversionedScript'language scriptFields))
                                (pversionedScriptHash # script)
                                (plengthBS # scriptCbor)
                                (BoundedItem.pfromBytes # poutputFieldIndex # outputIndex # scriptCbor)

{- | Aiken @ledger_output_descriptor_v1.build_v1@.

Returns 'PNothing' for an out-of-domain index, non-canonical output, or datum
that cannot be materialised into the three required context summaries.
-}
pbuildV1 ::
    forall s.
    Term s (PInteger :--> PByteString :--> PMaybe PLedgerOutputCommitmentV1)
pbuildV1 = phoistAcyclic $ plam $ \outputIndex outputCbor ->
    pif
        (outputIndex #< 0 #|| outputIndex #> 65_535)
        (pcon PNothing)
        ( pmatch (pdecodeCanonicalOutput # outputCbor) $ \case
            PNothing -> pcon PNothing
            PJust output ->
                pmatch (pledgerOutputSummariesV1 # outputCbor) $ \case
                    PNothing -> pcon PNothing
                    PJust summaries ->
                        pmatch summaries $ \(PPair cardanoTxOut remainingSummaries) ->
                            pmatch remainingSummaries $ \(PPair midgardTxOut cardanoSpendDatum) ->
                                passemble # outputIndex # output # outputCbor # cardanoTxOut # midgardTxOut # cardanoSpendDatum
        )

-- | Assemble after canonical decoding and independent semantic authentication.
passemble :: forall s. Term s (PInteger :--> PMidgardTxOutput :--> PByteString :--> PDataSummaryV1 :--> PDataSummaryV1 :--> PDataSummaryV1 :--> PMaybe PLedgerOutputCommitmentV1)
passemble = phoistAcyclic $ plam $ \outputIndex output outputCbor cardanoTxOut midgardTxOut cardanoSpendDatum ->
  pif (outputIndex #< 0 #|| outputIndex #> 65_535) (pcon PNothing) $
    pmatch output $ \outputFields ->
        pmatch (pfromData $ ptxOutput'value outputFields) $ \valueFields ->
            plet
                ( pfoldAssets
                    # pto (pfromData $ pvalue'assets valueFields)
                    # pconstant ""
                    # pcon (PAssetFoldV1 0 pemptyFrontier 0 0)
                )
                $ \assets ->
                    plet (pfromData $ pvalue'lovelace valueFields) $ \lovelace ->
                        plet (preferenceScriptFacts outputIndex output) $ \referenceFacts ->
                            pmatch referenceFacts $ \reference ->
                                pmatch assets $ \folded ->
                                    plet
                                        ( pcon $
                                            PLedgerOutputCommitmentV1
                                                (pdata pledgerOutputCommitmentVersion)
                                                (pdata outputIndex)
                                                (pdata $ plengthBS # outputCbor)
                                                (pdata $ poutputItemCommitment # outputIndex # outputCbor)
                                                (pdata $ pencodeMidgardAddress # pfromData (ptxOutput'address outputFields))
                                                (pdata lovelace)
                                                (pdata $ passetFold'count folded)
                                                ( pdata $
                                                    pfrontierCommitment
                                                        # passetFold'count folded
                                                        # passetFold'peaks folded
                                                )
                                                (pdata $ pcardanoValueSize lovelace assets)
                                                (pdata $ preferenceFacts'language reference)
                                                (pdata $ preferenceFacts'hash reference)
                                                (pdata $ preferenceFacts'totalLength reference)
                                                (pdata $ preferenceFacts'itemCommitment reference)
                                                (pdata cardanoTxOut)
                                                (pdata midgardTxOut)
                                                (pdata cardanoSpendDatum)
                                        )
                                        $ \descriptor ->
                                            pif
                                                (pdescriptorIsWellFormed descriptor)
                                                (pcon $ PJust descriptor)
                                                (pcon PNothing)

-- | The exact bytes one canonical output occupies in @utxos_root@.
pledgerValueV1 :: forall s. Term s (PInteger :--> PByteString :--> PMaybe PByteString)
pledgerValueV1 = phoistAcyclic $ plam $ \outputIndex outputCbor ->
    pmatch (pbuildV1 # outputIndex # outputCbor) $ \case
        PNothing -> pcon PNothing
        PJust descriptor -> pcon $ PJust $ pencodeLedgerOutputCommitment # descriptor
