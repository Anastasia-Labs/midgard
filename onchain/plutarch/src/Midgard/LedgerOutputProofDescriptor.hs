{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputProofDescriptor (
  pterminalScan,
  pvalueSummaryIsPinned,
  pdatumSummaryIsPinned,
  preferenceScriptIsPinned,
  pscanFactsArePinned,
  pitemCommitmentIsSized,
) where

import Midgard.Blake2b224Trace qualified as Blake
import Midgard.BoundedItem qualified as Bounded
import Midgard.CekData qualified as Summary
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.FraudProofs.NativeTx.Types (PMidgardAddress (..), PMidgardCredential (..))
import Midgard.LedgerOutput qualified as Output
import Midgard.LedgerOutputCommitment qualified as Commitment
import Midgard.LedgerOutputProof qualified as Proof
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue qualified as Value
import Midgard.NativeScriptScan qualified as Native
import Midgard.ValidationMerkle qualified as Merkle
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

pnone :: forall s. Term s PData
pnone = pforgetData $ pconstrBuiltin # 1 # pnil

pinner :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s PData
pinner control index = pmatch (pasConstr # (Raw.pitem # control # index)) $ \(PBuiltinPair tag fields) ->
  pif (tag #== 0 #&& plength # fields #== 1) (phead # fields) perror

pterminalScan :: forall s. Term s (Raw.PFrame :--> Scan.PLedgerOutputScanControlV1)
pterminalScan = phoistAcyclic $ plam $ \control ->
  plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
    pif
      ( Raw.pinteger
          # control
          # 1
          #== Proof.pstageTerminal
          #&& pfromData (Scan.pscan'cursor s)
          #<= Raw.pinteger
          # control
          # 3
          #&& Scan.pterminalIsExactV1
          # scan
          # (Raw.pinteger # control # 3)
      )
      scan
      perror

pvalueSummaryIsPinned :: forall s. Term s (Raw.PFrame :--> Summary.PDataSummaryV1 :--> PBool)
pvalueSummaryIsPinned = phoistAcyclic $ plam $ \control claimed ->
  pmatch (pterminalScan # control) $ \scan ->
    plet (Value.pcontrolFromDataV1 # pinner control 6) $ \value -> pmatch value $ \v ->
      pif
        (pfromData (Value.pvalueControl'assetRemaining v) #<= pfromData (Scan.pscan'assetCount scan))
        (Value.pfinalizeV1 # value #== pcon (PJust claimed))
        perror

pdatumSummaryIsPinned :: forall s. Term s (Raw.PFrame :--> PMaybeData Summary.PDataSummaryV1 :--> PBool)
pdatumSummaryIsPinned = phoistAcyclic $ plam $ \control claimed ->
  pmatch (pterminalScan # control) $ \scan ->
    pif
      (pfromData (Scan.pscan'datumOffset scan) #== (-1))
      (Raw.pitem # control # 7 #== pnone #&& claimed #== pcon PDNothing)
      $ plet (Traverse.pcontrolFromDataV1 # pinner control 7)
      $ \datum -> pmatch datum $ \d ->
        pif
          ( pfromData (Traverse.ptraverse'sourceStart d)
              #== pfromData (Scan.pscan'datumOffset scan)
              #&& pfromData (Traverse.ptraverse'sourceLength d)
              #== pfromData (Scan.pscan'datumLength scan)
              #&& pfromData (Scan.pscan'datumLength scan)
              #> 0
              #&& pfromData (Traverse.ptraverse'stage d)
              #== Traverse.pstageTerminal
          )
          ( pmatch (Traverse.pfinalizeV1 # datum) $ \case
              PNothing -> pconstant False
              PJust folded -> claimed #== pcon (PDJust $ pdata folded)
          )
          perror

pdecodePeaks :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
pdecodePeaks dat =
  pmap
    # plam
      ( \peak -> plet (pasList # peak) $ \fields ->
          pif
            (plength # fields #== 2)
            (pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pasInt # (phead # fields)) (pdata $ pasByteStr # (pelemAt # 1 # fields)))
            perror
      )
    # (pasList # dat)

preferenceScriptIsPinned :: forall s. Term s (Raw.PFrame :--> PByteString :--> PBool)
preferenceScriptIsPinned = phoistAcyclic $ plam $ \control descriptorCbor ->
  pmatch (Commitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
    pmatch (pterminalScan # control) $ \scan ->
      plet (Raw.pinteger # control # 3) $ \total ->
        plet (pfromData $ Scan.pscan'referenceScriptLanguage scan) $ \language ->
          plet (Raw.pinteger # control # 8) $ \count ->
            plet (pdecodePeaks $ Raw.pitem # control # 9) $ \peaks ->
              pif
                (language #== (-1))
                ( pfromData (Commitment.poutputCommitment'referenceScriptLanguage descriptor)
                    #== (-1)
                    #&& pfromData (Commitment.poutputCommitment'referenceScriptHash descriptor)
                    #== pconstant ""
                    #&& pfromData (Commitment.poutputCommitment'referenceScriptTotalLength descriptor)
                    #== 0
                    #&& pfromData (Commitment.poutputCommitment'referenceScriptItemCommitment descriptor)
                    #== pconstant ""
                    #&& count
                    #== 0
                    #&& pnull
                    # peaks
                    #&& Raw.pitem
                    # control
                    # 10
                    #== pnone
                    #&& Raw.pitem
                    # control
                    # 11
                    #== pnone
                )
                $ plet (total - pfromData (Scan.pscan'referenceScriptItemOffset scan))
                $ \length ->
                  plet (Blake.pcontrolFromDataV1 # pinner control 10) $ \hash -> pmatch hash $ \h ->
                    plet
                      ( pif
                          (language #== 0)
                          ( plet (Native.pstructureControlFromDataV1 # pinner control 11) $ \native -> pmatch native $ \n ->
                              pfromData (Native.pstructure'startOffset n)
                                #== pfromData (Scan.pscan'referenceScriptOffset scan)
                                #&& pfromData (Native.pstructure'endOffset n)
                                #== pfromData (Scan.pscan'referenceScriptOffset scan)
                                + pfromData (Scan.pscan'referenceScriptLength scan)
                                  #&& Native.pstructureTerminalIsExactV1
                                  # native
                          )
                          (Raw.pitem # control # 11 #== pnone)
                      )
                      $ \nativePinned ->
                        pmatch (Blake.pdigestV1 # hash) $ \case
                          PNothing -> pconstant False
                          PJust digest ->
                            length
                              #> 0
                              #&& count
                              #== Bounded.pchunkCount
                              # length
                              #&& Merkle.pfrontierIsWellFormed
                              # count
                              # peaks
                              #&& pfromData (Blake.pctl'totalLength h)
                              #== pfromData (Scan.pscan'referenceScriptLength scan)
                              + 1
                                #&& nativePinned
                                #&& pfromData (Commitment.poutputCommitment'referenceScriptLanguage descriptor)
                                #== language
                                #&& pfromData (Commitment.poutputCommitment'referenceScriptHash descriptor)
                                #== digest
                                #&& pfromData (Commitment.poutputCommitment'referenceScriptTotalLength descriptor)
                                #== length
                                #&& pfromData (Commitment.poutputCommitment'referenceScriptItemCommitment descriptor)
                                #== Bounded.pcommitment
                                # 2
                                # (Raw.pinteger # control # 2)
                                # length
                                # peaks

pcredentialData :: forall s. Term s PMidgardCredential -> Term s PData
pcredentialData credential = pmatch credential $ \case
  PMidgardPubKeyCredential hash -> pforgetData $ pconstrBuiltin # 0 # (pcons # pforgetData hash # pnil)
  PMidgardScriptCredential hash -> pforgetData $ pconstrBuiltin # 1 # (pcons # pforgetData hash # pnil)

pstakeData :: forall s. Term s (PMaybeData PMidgardCredential) -> Term s PData
pstakeData credential = pmatch credential $ \case
  PDNothing -> pnone
  PDJust inner ->
    pforgetData $
      pconstrBuiltin
        # 0
        # ( pcons
              # ( pforgetData $
                    pconstrBuiltin
                      # 0
                      # (pcons # pcredentialData (pfromData inner) # pnil)
                )
              # pnil
          )

paddressSummary :: forall s. Term s PMidgardAddress -> Term s PBool -> Term s Summary.PDataSummaryV1
paddressSummary address midgard = pmatch address $ \a ->
  Summary.psemanticDataSummaryV1
    # ( pforgetData $
          pconstrBuiltin
            # (pif (midgard #&& pfromData (paddress'protected a)) 1 0)
            # (pcons # pcredentialData (pfromData $ paddress'paymentCredential a) # (pcons # pstakeData (pfromData $ paddress'stakeCredential a) # pnil))
      )

ptxOutSummary :: forall s. Term s PMidgardAddress -> Term s PBool -> Term s Summary.PDataSummaryV1 -> Term s Summary.PDataSummaryV1 -> Term s Summary.PDataSummaryV1 -> Term s Summary.PDataSummaryV1
ptxOutSummary address midgard value datum reference =
  Summary.psmallConstrDataSummaryV1
    # 0
    # ( Summary.pprependDataListSummaryV1
          # paddressSummary address midgard
          # ( Summary.pprependDataListSummaryV1
                # value
                # ( Summary.pprependDataListSummaryV1
                      # datum
                      # (Summary.pprependDataListSummaryV1 # reference # Summary.pemptyDataListSummaryV1)
                  )
            )
      )

pscanFactsArePinned :: forall s. Term s (Raw.PFrame :--> PByteString :--> Summary.PDataSummaryV1 :--> PMaybeData Summary.PDataSummaryV1 :--> PBool)
pscanFactsArePinned = phoistAcyclic $ plam $ \control descriptorCbor value claimedDatum ->
  pmatch (Commitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \d ->
    pmatch (pterminalScan # control) $ \scan ->
      pmatch (Output.pdecodeCanonicalAddressBytes # pfromData (Scan.pscan'address scan)) $ \case
        PNothing -> perror
        PJust address ->
          plet
            ( pmatch claimedDatum $ \case
                PDNothing -> Summary.psmallConstrDataSummaryV1 # 0 # Summary.pemptyDataListSummaryV1
                PDJust datum -> Summary.psmallConstrDataSummaryV1 # 2 # (Summary.pprependDataListSummaryV1 # pfromData datum # Summary.pemptyDataListSummaryV1)
            )
            $ \datumField ->
              plet
                ( pmatch claimedDatum $ \case
                    PDNothing -> Summary.psmallConstrDataSummaryV1 # 1 # Summary.pemptyDataListSummaryV1
                    PDJust datum -> Summary.psmallConstrDataSummaryV1 # 0 # (Summary.pprependDataListSummaryV1 # pfromData datum # Summary.pemptyDataListSummaryV1)
                )
                $ \spend ->
                  plet
                    ( pif
                        (pfromData (Scan.pscan'referenceScriptLanguage scan) #== (-1))
                        (Summary.psmallConstrDataSummaryV1 # 1 # Summary.pemptyDataListSummaryV1)
                        (Summary.psemanticDataSummaryV1 # (pforgetData $ pconstrBuiltin # 0 # (pcons # pforgetData (Commitment.poutputCommitment'referenceScriptHash d) # pnil)))
                    )
                    $ \reference ->
                      (claimedDatum #== pcon PDNothing)
                        #== (pfromData (Scan.pscan'datumOffset scan) #== (-1))
                        #&& pfromData (Commitment.poutputCommitment'version d)
                        #== Commitment.pledgerOutputCommitmentVersion
                        #&& pfromData (Commitment.poutputCommitment'outputIndex d)
                        #== Raw.pinteger
                        # control
                        # 2
                        #&& pfromData (Commitment.poutputCommitment'totalLength d)
                        #== Raw.pinteger
                        # control
                        # 3
                        #&& pfromData (Commitment.poutputCommitment'itemCommitment d)
                        #== Raw.pbytes
                        # control
                        # 4
                        #&& Commitment.poutputCommitment'address d
                        #== Scan.pscan'address scan
                        #&& Commitment.poutputCommitment'lovelace d
                        #== Scan.pscan'lovelace scan
                        #&& Commitment.poutputCommitment'assetCount d
                        #== Scan.pscan'assetCount scan
                        #&& pfromData (Commitment.poutputCommitment'assetFrontierCommitment d)
                        #== Merkle.pfrontierCommitment
                        # pfromData (Scan.pscan'assetCount scan)
                        # pfromData (Scan.pscan'assetPeaks scan)
                        #&& Commitment.poutputCommitment'cardanoValueSize d
                        #== Scan.pscan'cardanoValueSize scan
                        #&& pfromData (Commitment.poutputCommitment'cardanoTxOut d)
                        #== ptxOutSummary address (pconstant False) value datumField reference
                        #&& pfromData (Commitment.poutputCommitment'midgardTxOut d)
                        #== ptxOutSummary address (pconstant True) value datumField reference
                        #&& pfromData (Commitment.poutputCommitment'cardanoSpendDatum d)
                        #== spend

pitemCommitmentIsSized :: forall s. Term s (Raw.PFrame :--> PBool)
pitemCommitmentIsSized = phoistAcyclic $ plam $ \control -> plengthBS # (Raw.pbytes # control # 4) #== 32
