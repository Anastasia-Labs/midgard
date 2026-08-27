{-# LANGUAGE OverloadedStrings #-}

module Midgard.ScriptContext (
  ptxOutDataV1, ptxOutSummaryV1, ptxInInfoDataV1, ptxInInfoSummaryV1,
  pprependAuthenticatedListItemV1, pprependResolvedTxInInfoV1,
  pprependResolvedDescriptorTxInInfoV1, pprependOutputV1, pprependOutputDescriptorV1,
  pprependSignerV1,
  pobserverCollectionSummaryV1, pvalidityIntervalSummaryV1, pfeeSummaryV1,
  ptransactionIdSummaryV1, pscriptPurposeSummaryV1, ptxInfoTailFieldsSummaryV1,
  ptxInfoFromTailSummaryV1, ptxInfoSummaryV1, pspendDatumSummaryV1,
  pcardanoScriptInfoSummaryV1, pcardanoSpendScriptInfoFromDescriptorV1,
  pscriptContextSummaryV1,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Core.Utils ((#/=))
import Plutarch.Internal.Term (punsafeBuiltin, punsafeCoerce)
import Plutarch.Prelude
import PlutusCore qualified as PLC

import Aiken.Cbor (pdeserialise)
import Midgard.CanonicalPlutusData (pisMaterialisablePlutusDataV1)
import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.CekData (
  PDataSequenceSummaryV1 (..), PDataSummaryV1 (..), pemptyDataListSummaryV1,
  pemptyDataPairSummaryV1, plistDataSummaryV1, pmapDataSummaryV1,
  pprependDataListSummaryV1, psemanticDataSummaryV1, psmallConstrDataSummaryV1,
 )
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor, pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..), PMidgardCredential (..), PMidgardTxInput (..),
  PMidgardTxOutput (..), PMidgardVersionedScript,
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pdecodeLedgerOutputCommitment)
import Midgard.ScriptProof (pversionedScriptHash)
import Midgard.ScriptProof qualified as Proof
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierCommitment, pfrontierIsWellFormed, pverifyMembership)

pconstrData :: forall s. Term s PInteger -> Term s (PBuiltinList PData) -> Term s PData
pconstrData tag fields = pforgetData $ pconstrBuiltin # tag # fields

pbytesData :: forall s. Term s PByteString -> Term s PData
pbytesData value = pforgetData $ pdata value

pintData :: forall s. Term s PInteger -> Term s PData
pintData value = pforgetData $ pdata value

pmapData :: forall s. Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
pmapData = punsafeBuiltin PLC.MapData

pfields2 :: forall s. Term s PData -> Term s PData -> Term s (PBuiltinList PData)
pfields2 a b = pcons # a #$ pcons # b # pnil

pcredentialData :: forall s. Term s PMidgardCredential -> Term s PData
pcredentialData credential = pmatch credential $ \case
  PMidgardPubKeyCredential hashValue -> pconstrData 0 (pcons # pbytesData (pfromData hashValue) # pnil)
  PMidgardScriptCredential hashValue -> pconstrData 1 (pcons # pbytesData (pfromData hashValue) # pnil)

pstakeCredentialData :: forall s. Term s (PMaybeData PMidgardCredential) -> Term s PData
pstakeCredentialData credential = pmatch credential $ \case
  PDNothing -> pconstrData 1 pnil
  PDJust value -> pconstrData 0 $ pcons
    # pconstrData 0 (pcons # pcredentialData (pfromData value) # pnil)
    # pnil

paddressData :: forall s. Term s PMidgardTxOutput -> Term s PBool -> Term s PData
paddressData output midgardEncoding = pmatch output $ \o ->
  pmatch (pfromData $ ptxOutput'address o) $ \a ->
    pconstrData
      (pif (midgardEncoding #&& pfromData (paddress'protected a)) 1 0)
      (pfields2
        (pcredentialData $ pfromData $ paddress'paymentCredential a)
        (pstakeCredentialData $ pfromData $ paddress'stakeCredential a))

psndDataPair :: forall s. Term s (PBuiltinPair PData PData) -> Term s PData
psndDataPair pair = pmatch pair $ \(PBuiltinPair _ value) -> value

poutputValueData :: forall s. Term s PData -> Term s (PMaybe PData)
poutputValueData outputData =
  plet (pasMap # outputData) $ \entries ->
  plet (plength # entries) $ \count ->
    pif (count #>= 2 #&& count #<= 4)
      (pcon $ PJust $ psndDataPair $ pelemAt # 1 # entries)
      (pcon PNothing)

pscriptValueData :: forall s. Term s PData -> Term s PData
pscriptValueData valueData =
  plet (pasList # valueData) $ \fields ->
  plet (pasInt # (pelemAt # 0 # fields)) $ \lovelace ->
  plet (pasMap # (pelemAt # 1 # fields)) $ \assets ->
    pif (lovelace #== 0)
      (pmapData # assets)
      (pmapData #$ pcons
        # (punsafeCoerce $ ppairDataBuiltin # pdata (pconstant @PByteString "")
          # pdata (pmapData #$ pcons
            # (punsafeCoerce $ ppairDataBuiltin # pdata (pconstant @PByteString "") # pdata lovelace)
            # pnil))
        # assets)

pdatumData :: forall s. Term s PMidgardTxOutput -> Term s (PMaybe PData)
pdatumData output = pmatch output $ \o -> pmatch (pfromData $ ptxOutput'datumCbor o) $ \case
  PDNothing -> pcon $ PJust $ pconstrData 0 pnil
  PDJust datumBytes -> plet (pfromData datumBytes) $ \datumCbor ->
    pif (pisMaterialisablePlutusDataV1 # datumCbor)
      (pmatch (pdeserialise # datumCbor) $ \case
        PNothing -> pcon PNothing
        PJust datum -> pcon $ PJust $ pconstrData 2 (pcons # datum # pnil))
      (pcon PNothing)

preferenceScriptData :: forall s. Term s PMidgardTxOutput -> Term s PData
preferenceScriptData output = pmatch output $ \o -> pmatch (pfromData $ ptxOutput'scriptRef o) $ \case
  PDNothing -> pconstrData 1 pnil
  PDJust script -> pconstrData 0 $ pcons
    # pbytesData (pversionedScriptHash # (pfromData script :: Term s PMidgardVersionedScript))
    # pnil

ptxOutDataV1 :: forall s. Term s (PByteString :--> PBool :--> PMaybe PData)
ptxOutDataV1 = phoistAcyclic $ plam $ \outputCbor midgardEncoding ->
  pmatch (pdecodeCanonicalOutput # outputCbor) $ \case
    PNothing -> pcon PNothing
    PJust output -> pmatch (pdeserialise # outputCbor) $ \case
      PNothing -> pcon PNothing
      PJust outputData -> pmatch (poutputValueData outputData) $ \case
        PNothing -> pcon PNothing
        PJust valueData -> pmatch (pdatumData output) $ \case
          PNothing -> pcon PNothing
          PJust datum -> pcon $ PJust $ pconstrData 0 $
            pcons # paddressData output midgardEncoding
              #$ pcons # pscriptValueData valueData
              #$ pcons # datum
              #$ pcons # preferenceScriptData output
              # pnil

ptxOutSummaryV1 :: forall s. Term s (PByteString :--> PBool :--> PMaybe PDataSummaryV1)
ptxOutSummaryV1 = phoistAcyclic $ plam $ \outputCbor midgardEncoding ->
  pmatch (ptxOutDataV1 # outputCbor # midgardEncoding) $ \case
    PNothing -> pcon PNothing
    PJust dat -> pcon $ PJust $ psemanticDataSummaryV1 # dat

ptxOutRefData :: forall s. Term s PMidgardTxInput -> Term s PData
ptxOutRefData input = pmatch input $ \i -> pconstrData 0 $
  pfields2 (pbytesData $ pfromData $ ptxInput'txId i) (pintData $ pfromData $ ptxInput'outputIndex i)

ptxInInfoDataV1 :: forall s. Term s (PByteString :--> PByteString :--> PBool :--> PMaybe PData)
ptxInInfoDataV1 = phoistAcyclic $ plam $ \inputCbor outputCbor midgardEncoding ->
  plet (pdecodeMidgardTxInputCbor # inputCbor) $ \input ->
  pif (pencodeMidgardTxInput # input #/= inputCbor) (pcon PNothing) $
    pmatch (ptxOutDataV1 # outputCbor # midgardEncoding) $ \case
      PNothing -> pcon PNothing
      PJust output -> pcon $ PJust $ pconstrData 0 (pfields2 (ptxOutRefData input) output)

ptxInInfoSummaryV1 :: forall s. Term s (PByteString :--> PByteString :--> PBool :--> PMaybe PDataSummaryV1)
ptxInInfoSummaryV1 = phoistAcyclic $ plam $ \inputCbor outputCbor midgardEncoding ->
  pmatch (ptxInInfoDataV1 # inputCbor # outputCbor # midgardEncoding) $ \case
    PNothing -> pcon PNothing
    PJust dat -> pcon $ PJust $ psemanticDataSummaryV1 # dat

ptxInInfoFromOutputSummaryV1 :: forall s. Term s PByteString -> Term s PDataSummaryV1 -> Term s (PMaybe PDataSummaryV1)
ptxInInfoFromOutputSummaryV1 inputCbor output =
  plet (pdecodeMidgardTxInputCbor # inputCbor) $ \input ->
  pif (pencodeMidgardTxInput # input #/= inputCbor) (pcon PNothing) $
    plet (psemanticDataSummaryV1 # ptxOutRefData input) $ \outRef ->
    plet (pprependDataListSummaryV1 # output # pemptyDataListSummaryV1) $ \withOutput ->
    plet (pprependDataListSummaryV1 # outRef # withOutput) $ \fields ->
      pcon $ PJust $ psmallConstrDataSummaryV1 # 0 # fields

pprependAuthenticatedListItemV1 :: forall s. Term s
  (PInteger :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :-->
    PDataSummaryV1 :--> PBuiltinList (PAsData PByteString) :--> PDataSequenceSummaryV1 :-->
    PMaybe PDataSequenceSummaryV1)
pprependAuthenticatedListItemV1 = phoistAcyclic $ plam $
  \collectionKind itemCount peaks itemIndex item siblings tailSummary ->
    pmatch item $ \i -> pmatch tailSummary $ \tailFields ->
    pif (itemCount #>= 0
        #&& pfrontierIsWellFormed # itemCount # peaks
        #&& itemIndex #== itemCount - pfromData (pseq'length tailFields) - 1
        #&& pverifyMembership # itemCount # peaks # itemIndex
          # (Proof.pcontextItemLeafHash # collectionKind # itemIndex # pfromData (psummary'root i)
            # pfromData (psummary'cborLength i) # pfromData (psummary'memory i)) # siblings)
      (pcon $ PJust $ pprependDataListSummaryV1 # item # tailSummary) (pcon PNothing)

pprependResolvedTxInInfoV1 :: forall s. Term s
  (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PInteger :--> PInteger :-->
    PByteString :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PBool :-->
    PDataSequenceSummaryV1 :--> PMaybe PDataSequenceSummaryV1)
pprependResolvedTxInInfoV1 = phoistAcyclic $ plam $
  \resolvedCount peaks spendCount sourceKind itemIndex inputCbor outputCbor siblings midgardEncoding tailSummary ->
    plet (pif (sourceKind #== 0) spendCount (resolvedCount - spendCount)) $ \collectionCount ->
    plet (pif (sourceKind #== 0) 0 spendCount) $ \collectionOffset ->
    pmatch tailSummary $ \tailFields ->
    pif (resolvedCount #>= 0 #&& spendCount #>= 0 #&& spendCount #<= resolvedCount
        #&& (sourceKind #== 0 #|| sourceKind #== 1)
        #&& pfrontierIsWellFormed # resolvedCount # peaks
        #&& itemIndex #== collectionOffset + collectionCount - pfromData (pseq'length tailFields) - 1
        #&& pverifyMembership # resolvedCount # peaks # itemIndex
          # (Proof.presolvedContextItemLeafHash # sourceKind # itemIndex # inputCbor # outputCbor) # siblings)
      (pmatch (ptxInInfoSummaryV1 # inputCbor # outputCbor # midgardEncoding) $ \case
        PNothing -> pcon PNothing
        PJust item -> pcon $ PJust $ pprependDataListSummaryV1 # item # tailSummary)
      (pcon PNothing)

pprependResolvedDescriptorTxInInfoV1 :: forall s. Term s
  (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PInteger :--> PInteger :-->
    PByteString :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PBool :-->
    PDataSequenceSummaryV1 :--> PMaybe PDataSequenceSummaryV1)
pprependResolvedDescriptorTxInInfoV1 = phoistAcyclic $ plam $
  \resolvedCount peaks spendCount sourceKind itemIndex inputCbor descriptorCbor siblings midgardEncoding tailSummary ->
    plet (pif (sourceKind #== 0) spendCount (resolvedCount - spendCount)) $ \collectionCount ->
    plet (pif (sourceKind #== 0) 0 spendCount) $ \collectionOffset ->
    plet (pdecodeMidgardTxInputCbor # inputCbor) $ \input ->
    plet (pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
    plet (pif midgardEncoding
      (pmatch descriptor $ \d -> pfromData $ poutputCommitment'midgardTxOut d)
      (pmatch descriptor $ \d -> pfromData $ poutputCommitment'cardanoTxOut d)) $ \output ->
    pmatch input $ \inputFields -> pmatch descriptor $ \descriptorFields -> pmatch tailSummary $ \tailFields ->
    pif (resolvedCount #>= 0 #&& spendCount #>= 0 #&& spendCount #<= resolvedCount
        #&& (sourceKind #== 0 #|| sourceKind #== 1)
        #&& pencodeMidgardTxInput # input #== inputCbor
        #&& pfromData (poutputCommitment'outputIndex descriptorFields) #== pfromData (ptxInput'outputIndex inputFields)
        #&& pfrontierIsWellFormed # resolvedCount # peaks
        #&& itemIndex #== collectionOffset + collectionCount - pfromData (pseq'length tailFields) - 1
        #&& pverifyMembership # resolvedCount # peaks # itemIndex
          # (Proof.presolvedContextItemLeafHash # sourceKind # itemIndex # inputCbor # descriptorCbor) # siblings)
      (pmatch (ptxInInfoFromOutputSummaryV1 inputCbor output) $ \case
        PNothing -> pcon PNothing
        PJust item -> pcon $ PJust $ pprependDataListSummaryV1 # item # tailSummary)
      (pcon PNothing)

pprependOutputV1 :: forall s. Term s
  (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PByteString :-->
    PBuiltinList (PAsData PByteString) :--> PBool :--> PDataSequenceSummaryV1 :-->
    PMaybe PDataSequenceSummaryV1)
pprependOutputV1 = phoistAcyclic $ plam $ \count peaks index outputCbor siblings midgardEncoding tailSummary ->
  pmatch tailSummary $ \tailFields ->
  pif (count #>= 0 #&& pfrontierIsWellFormed # count # peaks
      #&& index #== count - pfromData (pseq'length tailFields) - 1
      #&& pverifyMembership # count # peaks # index # (Proof.poutputLeafHash # index # outputCbor) # siblings)
    (pmatch (ptxOutSummaryV1 # outputCbor # midgardEncoding) $ \case
      PNothing -> pcon PNothing
      PJust item -> pcon $ PJust $ pprependDataListSummaryV1 # item # tailSummary)
    (pcon PNothing)

pprependOutputDescriptorV1 :: forall s. Term s
  (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PByteString :-->
    PBuiltinList (PAsData PByteString) :--> PBool :--> PDataSequenceSummaryV1 :-->
    PMaybe PDataSequenceSummaryV1)
pprependOutputDescriptorV1 = phoistAcyclic $ plam $ \count peaks index descriptorCbor siblings midgardEncoding tailSummary ->
  plet (pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor -> pmatch descriptor $ \d ->
  plet (pif midgardEncoding (pfromData $ poutputCommitment'midgardTxOut d)
      (pfromData $ poutputCommitment'cardanoTxOut d)) $ \output -> pmatch tailSummary $ \tailFields ->
  pif (count #>= 0 #&& pfromData (poutputCommitment'outputIndex d) #== index
      #&& pfrontierIsWellFormed # count # peaks
      #&& index #== count - pfromData (pseq'length tailFields) - 1
      #&& pverifyMembership # count # peaks # index
        # (Proof.poutputDescriptorLeafHash # index # descriptorCbor) # siblings)
    (pcon $ PJust $ pprependDataListSummaryV1 # output # tailSummary) (pcon PNothing)

pprependSignerV1 :: forall s. Term s
  (PInteger :--> PByteString :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :-->
    PByteString :--> PBuiltinList (PAsData PByteString) :--> PDataSequenceSummaryV1 :-->
    PMaybe PDataSequenceSummaryV1)
pprependSignerV1 = phoistAcyclic $ plam $ \count commitment peaks index signer siblings tailSummary ->
  pmatch tailSummary $ \tailFields ->
  pif (plengthBS # commitment #== 32 #&& plengthBS # signer #== 28
      #&& pfrontierIsWellFormed # count # peaks
      #&& pfrontierCommitment # count # peaks #== commitment
      #&& index #== count - pfromData (pseq'length tailFields) - 1
      #&& pverifyMembership # count # peaks # index # (Proof.psignerLeafHash # signer) # siblings)
    (pcon $ PJust $ pprependDataListSummaryV1 # (psemanticDataSummaryV1 # pbytesData signer) # tailSummary)
    (pcon PNothing)

pinsertSortedUnique :: forall s. Term s (PByteString :--> PBuiltinList PByteString :--> PBuiltinList PByteString)
pinsertSortedUnique = phoistAcyclic $ pfix $ \self -> plam $ \value values -> pmatch values $ \case
  PNil -> pcons # value # pnil
  PCons headValue rest -> pif (value #== headValue) values $
    pif (value #< headValue) (pcons # value # values) (pcons # headValue # (self # value # rest))

psortUnique :: forall s. Term s (PBuiltinList PByteString :--> PBuiltinList PByteString)
psortUnique = phoistAcyclic $ pfix $ \self -> plam $ \values -> pmatch values $ \case
  PNil -> pnil
  PCons value rest -> pinsertSortedUnique # value # (self # rest)

pallHash28 :: forall s. Term s (PBuiltinList PByteString :--> PBool)
pallHash28 = phoistAcyclic $ pfix $ \self -> plam $ \values -> pmatch values $ \case
  PNil -> pconstant True
  PCons value rest -> plengthBS # value #== 28 #&& self # rest

pbyteDataItems :: forall s. Term s (PBuiltinList PByteString :--> PBuiltinList PData)
pbyteDataItems = phoistAcyclic $ pfix $ \self -> plam $ \values -> pmatch values $ \case
  PNil -> pnil
  PCons value rest -> pcons # pbytesData value # (self # rest)

pobserverWithdrawalPairs :: forall s. Term s (PBuiltinList PByteString :--> PBuiltinList (PBuiltinPair PData PData))
pobserverWithdrawalPairs = phoistAcyclic $ pfix $ \self -> plam $ \values -> pmatch values $ \case
  PNil -> pnil
  PCons value rest -> pcons
    # (punsafeCoerce $ ppairDataBuiltin
      # pdata (pconstrData 1 $ pcons # pbytesData value # pnil)
      # pdata (pintData 0))
    # (self # rest)

pobserverCollectionSummaryV1 :: forall s. Term s (PBuiltinList PByteString :--> PBool :--> PMaybe PDataSummaryV1)
pobserverCollectionSummaryV1 = phoistAcyclic $ plam $ \observers midgardEncoding ->
  plet (psortUnique # observers) $ \canonical ->
  pif (plength # canonical #<= pmaxTxSizeDerivedItemCount #&& pallHash28 # canonical)
    (pcon $ PJust $ psemanticDataSummaryV1 # pif midgardEncoding
      (plistData # (pbyteDataItems # canonical))
      (pmapData # (pobserverWithdrawalPairs # canonical)))
    (pcon PNothing)

pboolData :: forall s. Term s PBool -> Term s PData
pboolData value = pconstrData (pif value 1 0) pnil

pintervalBoundData :: forall s. Term s PInteger -> Term s PData
pintervalBoundData value = pif (value #< 0) (pconstrData 0 pnil)
  (pconstrData 1 $ pcons # pintData value # pnil)

pvalidityIntervalSummaryV1 :: forall s. Term s (PInteger :--> PInteger :--> PDataSummaryV1)
pvalidityIntervalSummaryV1 = phoistAcyclic $ plam $ \start end -> psemanticDataSummaryV1 # pconstrData 0
  (pfields2
    (pconstrData 0 $ pfields2 (pintervalBoundData start) (pboolData $ pconstant True))
    (pconstrData 0 $ pfields2 (pintervalBoundData end) (pboolData $ pconstant False)))

pfeeSummaryV1 :: forall s. Term s (PInteger :--> PDataSummaryV1)
pfeeSummaryV1 = phoistAcyclic $ plam $ \fee -> psemanticDataSummaryV1 # pintData fee

ptransactionIdSummaryV1 :: forall s. Term s (PByteString :--> PDataSummaryV1)
ptransactionIdSummaryV1 = phoistAcyclic $ plam $ \txId -> pif (plengthBS # txId #== 32)
  (psemanticDataSummaryV1 # pbytesData txId) perror

pscriptPurposeSummaryV1 :: forall s. Term s (PInteger :--> PByteString :--> PByteString :--> PBool :--> PMaybe PDataSummaryV1)
pscriptPurposeSummaryV1 = phoistAcyclic $ plam $ \purposeKind scriptHash subject midgardEncoding ->
  pif (plengthBS # scriptHash #/= 28) (pcon PNothing) $
  plet
    (pif (purposeKind #== 0)
      (plet (pdecodeMidgardTxInputCbor # subject) $ \input ->
        pif (pencodeMidgardTxInput # input #/= subject) (pcon PNothing)
          (pcon $ PJust $ pconstrData 1 $ pif midgardEncoding
            (pfields2 (pbytesData scriptHash) (ptxOutRefData input))
            (pcons # ptxOutRefData input # pnil))) $
    pif (purposeKind #== 1)
      (pcon $ PJust $ pconstrData 0 $ pcons # pbytesData scriptHash # pnil) $
    pif (purposeKind #== 2)
      (pcon $ PJust $ pconstrData 2 $ pcons
        # pif midgardEncoding (pbytesData scriptHash)
          (pcredentialData $ pcon $ PMidgardScriptCredential $ pdata scriptHash)
        # pnil) $
    pif (purposeKind #== 3 #&& midgardEncoding)
      (pcon $ PJust $ pconstrData 3 $ pcons # pbytesData scriptHash # pnil)
      (pcon PNothing)) $ \purpose -> pmatch purpose $ \case
        PNothing -> pcon PNothing
        PJust dat -> pcon $ PJust $ psemanticDataSummaryV1 # dat

psummaryFields :: forall s. [Term s PDataSummaryV1] -> Term s PDataSequenceSummaryV1
psummaryFields = foldr (\field tailSummary -> pprependDataListSummaryV1 # field # tailSummary) pemptyDataListSummaryV1

pemptyListSummary, pemptyMapSummary, pnoneSummary :: forall s. Term s PDataSummaryV1
pemptyListSummary = plistDataSummaryV1 # pemptyDataListSummaryV1
pemptyMapSummary = pmapDataSummaryV1 # pemptyDataPairSummaryV1
pnoneSummary = psmallConstrDataSummaryV1 # 1 # pemptyDataListSummaryV1

ptxInfoTailFieldsSummaryV1 :: forall s. Term s
  (PBool :--> PDataSummaryV1 :--> PDataSequenceSummaryV1 :--> PDataSummaryV1 :-->
    PDataSequenceSummaryV1 :--> PByteString :--> PDataSequenceSummaryV1)
ptxInfoTailFieldsSummaryV1 = phoistAcyclic $ plam $ \midgard observers signers mint redeemers txId ->
  plet (plistDataSummaryV1 # signers) $ \signerList ->
  plet (pmapDataSummaryV1 # redeemers) $ \redeemerMap ->
  plet (ptransactionIdSummaryV1 # txId) $ \txIdSummary ->
    pif midgard
      (psummaryFields [observers, signerList, mint, redeemerMap, txIdSummary])
      (psummaryFields [signerList, redeemerMap, pemptyMapSummary, txIdSummary,
        pemptyMapSummary, pemptyListSummary, pnoneSummary, pnoneSummary])

ptxInfoFromTailSummaryV1 :: forall s. Term s
  (PBool :--> PDataSequenceSummaryV1 :--> PDataSequenceSummaryV1 :--> PDataSequenceSummaryV1 :-->
    PInteger :--> PInteger :--> PInteger :--> PDataSummaryV1 :--> PDataSummaryV1 :-->
    PDataSequenceSummaryV1 :--> PDataSummaryV1)
ptxInfoFromTailSummaryV1 = phoistAcyclic $ plam $
  \midgard inputs references outputs fee start end observers mint tailFields ->
    plet (plistDataSummaryV1 # inputs) $ \inputList ->
    plet (plistDataSummaryV1 # references) $ \referenceList ->
    plet (plistDataSummaryV1 # outputs) $ \outputList ->
    plet (pfeeSummaryV1 # fee) $ \feeSummary ->
    plet (pvalidityIntervalSummaryV1 # start # end) $ \validity ->
    plet (ifSummary midgard
      [inputList, referenceList, outputList, feeSummary, validity]
      [inputList, referenceList, outputList, feeSummary, mint, pemptyListSummary, observers, validity]
      tailFields) $ \fields -> psmallConstrDataSummaryV1 # 0 # fields
  where
    ifSummary midgard yes no tailFields = pif midgard
      (foldr (\field rest -> pprependDataListSummaryV1 # field # rest) tailFields yes)
      (foldr (\field rest -> pprependDataListSummaryV1 # field # rest) tailFields no)

ptxInfoSummaryV1 :: forall s. Term s
  (PBool :--> PDataSequenceSummaryV1 :--> PDataSequenceSummaryV1 :--> PDataSequenceSummaryV1 :-->
    PInteger :--> PInteger :--> PInteger :--> PDataSummaryV1 :--> PDataSequenceSummaryV1 :-->
    PDataSummaryV1 :--> PDataSequenceSummaryV1 :--> PByteString :--> PDataSummaryV1)
ptxInfoSummaryV1 = phoistAcyclic $ plam $
  \midgard inputs references outputs fee start end observers signers mint redeemers txId ->
    ptxInfoFromTailSummaryV1 # midgard # inputs # references # outputs # fee # start # end # observers # mint
      # (ptxInfoTailFieldsSummaryV1 # midgard # observers # signers # mint # redeemers # txId)

pspendDatumSummaryV1 :: forall s. Term s (PByteString :--> PMaybe PDataSummaryV1)
pspendDatumSummaryV1 = phoistAcyclic $ plam $ \outputCbor ->
  pmatch (pdecodeCanonicalOutput # outputCbor) $ \case
    PNothing -> pcon PNothing
    PJust output -> pmatch output $ \o -> pmatch (pfromData $ ptxOutput'datumCbor o) $ \case
      PDNothing -> pcon $ PJust $ psemanticDataSummaryV1 # pconstrData 1 pnil
      PDJust datumBytes -> plet (pfromData datumBytes) $ \datumCbor ->
        pif (pisMaterialisablePlutusDataV1 # datumCbor)
          (pmatch (pdeserialise # datumCbor) $ \case
            PNothing -> pcon PNothing
            PJust datum -> pcon $ PJust $ psemanticDataSummaryV1
              # pconstrData 0 (pcons # datum # pnil))
          (pcon PNothing)

pcardanoScriptInfoSummaryV1 :: forall s. Term s
  (PInteger :--> PByteString :--> PByteString :--> PMaybeData PByteString :--> PMaybe PDataSummaryV1)
pcardanoScriptInfoSummaryV1 = phoistAcyclic $ plam $ \purposeKind scriptHash subject spendOutput ->
  pif (purposeKind #== 0)
    (pmatch spendOutput $ \case
      PDNothing -> perror
      PDJust outputData -> plet (pfromData outputData) $ \outputCbor ->
        plet (pdecodeMidgardTxInputCbor # subject) $ \input ->
        pif (pencodeMidgardTxInput # input #/= subject) (pcon PNothing) $
          pmatch (pspendDatumSummaryV1 # outputCbor) $ \case
            PNothing -> pcon PNothing
            PJust datum -> plet (psemanticDataSummaryV1 # ptxOutRefData input) $ \outRef ->
              pcon $ PJust $ psmallConstrDataSummaryV1 # 1 # psummaryFields [outRef, datum])
    (pmatch spendOutput $ \case
      PDJust _ -> pcon PNothing
      PDNothing -> pscriptPurposeSummaryV1 # purposeKind # scriptHash # subject # pconstant False)

pcardanoSpendScriptInfoFromDescriptorV1 :: forall s. Term s (PByteString :--> PByteString :--> PMaybe PDataSummaryV1)
pcardanoSpendScriptInfoFromDescriptorV1 = phoistAcyclic $ plam $ \subject descriptorCbor ->
  plet (pdecodeMidgardTxInputCbor # subject) $ \input ->
  plet (pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
  pmatch input $ \i -> pmatch descriptor $ \d ->
  pif (pencodeMidgardTxInput # input #== subject
      #&& pfromData (poutputCommitment'outputIndex d) #== pfromData (ptxInput'outputIndex i))
    (plet (psemanticDataSummaryV1 # ptxOutRefData input) $ \outRef ->
      pcon $ PJust $ psmallConstrDataSummaryV1 # 1
        # psummaryFields [outRef, pfromData $ poutputCommitment'cardanoSpendDatum d])
    (pcon PNothing)

pscriptContextSummaryV1 :: forall s. Term s (PDataSummaryV1 :--> PDataSummaryV1 :--> PDataSummaryV1 :--> PDataSummaryV1)
pscriptContextSummaryV1 = phoistAcyclic $ plam $ \txInfo redeemer scriptInfo ->
  psmallConstrDataSummaryV1 # 0 # psummaryFields [txInfo, redeemer, scriptInfo]
