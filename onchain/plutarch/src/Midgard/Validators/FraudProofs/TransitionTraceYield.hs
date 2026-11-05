module Midgard.Validators.FraudProofs.TransitionTraceYield (
  outputScanValidator,
  outputValueValidator,
  outputAssemblyValidator,
  claimStructureValidator,
  claimSourceValidator,
  claimEndpointsValidator,
  l2OpenValidator,
  l2ReplayValidator,
  outputSummariesValidator,
  depositSummariesValidator,
  depositProjectionValidator,
  depositValueValidator,
) where

import Midgard.Common.Utils (pheadSingleton)
import Midgard.Common.Value (pquantityOfValue)
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.FieldOpening qualified as Opening
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxOutputCbor)
import Midgard.FraudProofs.NativeTx.Components qualified as Components
import Midgard.FraudProofs.NativeTx.Types qualified as Native
import Midgard.FraudProofs.StructuredDataCarriage qualified as Structured
import Midgard.FraudProofs.TransitionTrace.DepositSource qualified as Deposit
import Midgard.FraudProofs.TransitionTrace.FinalYield qualified as Yield
import Midgard.FraudProofs.TransitionTrace.Proof qualified as Proof
import Midgard.FraudProofs.TransitionTrace.ProofCarriage qualified as Carriage
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerOutput qualified as Output
import Midgard.LedgerOutputCommitment qualified as Commitment
import Midgard.LedgerOutputDescriptor qualified as Descriptor
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue qualified as Value
import Midgard.LedgerState (PHeaderV1)
import Midgard.LedgerState qualified as Ledger
import Midgard.ScriptContext qualified as Context
import Midgard.TransitionTrace qualified as Trace
import Midgard.ValidationClaim qualified as Claim
import Midgard.ValidationMerkle (pfrontierCommitment)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value qualified as CardanoValue
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

pwithYield ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s PScriptContext ->
  (Term s PStepDatum -> Term s Yield.PState -> Term s Yield.PArgs -> Term s PTxInfo -> Term s PData -> Term s PBool) ->
  Term s PUnit
pwithYield dispatcher ctx action = P.do
  PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
      PPair datum args <- pmatch $ Yield.pdispatch # pfromData dispatcher # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)
      PStepDatum _ stateData <- pmatch datum
      state <- plet $ pmatch stateData $ \case
        PDNothing -> perror
        PDJust dat -> pfromData $ punsafeCoerce @(PAsData Yield.PState) $ pfromData dat
      pif (action datum state args pscriptContext'txInfo $ pto pscriptContext'redeemer) (pconstant ()) perror
    _ -> perror

padvance :: forall s. Term s PStepDatum -> Term s Yield.PArgs -> Term s PTxInfo -> Term s Yield.PState -> Term s PBool
padvance datum args tx next = pmatch tx $ \PTxInfo{ptxInfo'outputs} -> Yield.padvance # datum # args # pfromData ptxInfo'outputs # next

pscanBatch :: forall s. Term s (Scan.PLedgerOutputScanControlV1 :--> PByteString :--> PInteger :--> PInteger :--> PInteger :--> PPair Scan.PLedgerOutputScanControlV1 (PPair PInteger PInteger))
pscanBatch = phoistAcyclic $ pfix $ \self -> plam $ \control bytes start end remaining ->
  pif
    (remaining #== 0 #|| Scan.pterminalIsExactV1 # control # (plengthBS # bytes))
    (pcon $ PPair control $ pcon $ PPair start end)
    ( P.do
        Scan.PLedgerOutputScanControlV1{Scan.pscan'stage, Scan.pscan'cursor} <- pmatch control
        next <- plet $ pmatch (Scan.pfinishV1 # control # (plengthBS # bytes)) $ \case
          PJust finished -> finished
          PNothing -> pmatch (Scan.pstepV1 # control # (plengthBS # bytes) # bytes # pfromData pscan'cursor) $ \case
            PJust advanced -> advanced
            PNothing -> perror
        Scan.PLedgerOutputScanControlV1{Scan.pscan'stage = nextStage, Scan.pscan'cursor = nextCursor} <- pmatch next
        self
          # next
          # bytes
          # (pif (pfromData pscan'stage #== Scan.pstageRequiredFields) (pfromData nextCursor) start)
          # (pif (pfromData pscan'stage #<= Scan.pstageAsset #&& pfromData nextStage #== Scan.pstageOptionalField) (pfromData nextCursor) end)
          # (remaining - 1)
    )

outputScanValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
outputScanValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx _ -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'phase st) #== 2 #&& pnot # (pfromData (Yield.pstate'kind st) #== 2))
    ( P.do
        PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
        bytes <- plet $ Yield.poutputBytes # state # args # pfromData ptxInfo'referenceInputs
        control <- plet $ pif (pfromData (Yield.pstate'scanCbor st) #== pconstant "") Scan.pinitialControlV1 (Scan.pdecodeControlV1 # pfromData (Yield.pstate'scanCbor st))
        PPair next positions <- pmatch $ pscanBatch # control # bytes # pfromData (Yield.pstate'valueStart st) # pfromData (Yield.pstate'valueEnd st) # 4
        PPair start end <- pmatch positions
        padvance datum args tx $
          pcon
            st
              { Yield.pstate'phase = pdata $ pif (Scan.pterminalIsExactV1 # next # (plengthBS # bytes)) 7 2
              , Yield.pstate'scanCbor = pdata $ Scan.pencodeControlV1 # next
              , Yield.pstate'valueStart = pdata start
              , Yield.pstate'valueEnd = pdata end
              }
    )
    perror

pvalueBatch :: forall s. Term s (Value.PLedgerOutputValueControlV1 :--> Scan.PLedgerOutputScanControlV1 :--> PBuiltinList (PAsData Value.PLedgerOutputValueWitnessV1) :--> Value.PLedgerOutputValueControlV1)
pvalueBatch = phoistAcyclic $ pfix $ \self -> plam $ \control facts witnesses ->
  pelimList
    ( \witness rest -> P.do
        Scan.PLedgerOutputScanControlV1{Scan.pscan'assetCount, Scan.pscan'assetPeaks, Scan.pscan'lovelace} <- pmatch facts
        pmatch (Value.pstepV1 # control # pfromData pscan'assetCount # pfromData pscan'assetPeaks # pfromData pscan'lovelace # pfromData witness) $ \case
          PNothing -> perror
          PJust next -> self # next # facts # rest
    )
    control
    witnesses

outputValueValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
outputValueValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  witnesses <- plet $ pfromData $ punsafeCoerce @(PAsData (PBuiltinList (PAsData Value.PLedgerOutputValueWitnessV1))) raw
  pif
    (pfromData (Yield.pstate'phase st) #== 7 #&& pnot # (pfromData (Yield.pstate'kind st) #== 2) #&& pnot # (pnull # witnesses) #&& plength # witnesses #<= 8)
    ( P.do
        facts <- plet $ Scan.pdecodeControlV1 # pfromData (Yield.pstate'scanCbor st)
        Scan.PLedgerOutputScanControlV1{Scan.pscan'assetCount} <- pmatch facts
        control <-
          plet $
            pif
              (pfromData (Yield.pstate'valueCbor st) #== pconstant "")
              (Value.pinitialControlV1 # pfromData pscan'assetCount)
              (Value.pdecodeControlV1 # pfromData (Yield.pstate'valueCbor st))
        next <- plet $ pvalueBatch # control # facts # witnesses
        Value.PLedgerOutputValueControlV1{Value.pvalueControl'stage} <- pmatch next
        summary <- plet $ pmatch (Value.pfinalizeV1 # next) $ \case
          PNothing -> pcon PDNothing
          PJust result -> pcon $ PDJust $ pdata result
        padvance datum args tx $
          pcon
            st
              { Yield.pstate'phase = pdata $ pif (pfromData pvalueControl'stage #== Value.pstageTerminal) 8 7
              , Yield.pstate'valueCbor = pdata $ Value.pencodeControlV1 # next
              , Yield.pstate'valueSummary = summary
              }
    )
    perror

outputAssemblyValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
outputAssemblyValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx _ -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'phase st) #== 3)
    ( P.do
        index <- plet $ pif (pfromData (Yield.pstate'kind st) #== 0) (pfromData $ Yield.pstate'outputIndex st) (pfromData $ Yield.pstate'depositIndex st)
        PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
        bytes <- plet $ Yield.poutputBytes # state # args # pfromData ptxInfo'referenceInputs
        Yield.POutputSummaries summaries <- pmatch $ pfromData $ Yield.pstate'summaries st
        triple <- plet $ pfromData $ pheadSingleton # pfromData summaries
        pif
          (plength # triple #== 3)
          ( P.do
              skeleton <- plet $ Yield.poutputMetadataSkeleton # state # bytes
              facts <- pmatch $ Scan.pdecodeControlV1 # pfromData (Yield.pstate'scanCbor st)
              base <- plet
                $ pmatch
                  ( Descriptor.passemble
                      # index
                      # (pdecodeMidgardTxOutputCbor # skeleton)
                      # skeleton
                      # pfromData (pelemAt # 0 # triple)
                      # pfromData (pelemAt # 1 # triple)
                      # pfromData (pelemAt # 2 # triple)
                  )
                $ \case
                  PNothing -> perror
                  PJust result -> result
              original <- pmatch base
              descriptor <-
                plet $
                  pcon
                    original
                      { Commitment.poutputCommitment'totalLength = pdata $ plengthBS # bytes
                      , Commitment.poutputCommitment'itemCommitment = pdata $ Commitment.poutputItemCommitment # index # bytes
                      , Commitment.poutputCommitment'lovelace = Scan.pscan'lovelace facts
                      , Commitment.poutputCommitment'assetCount = Scan.pscan'assetCount facts
                      , Commitment.poutputCommitment'assetFrontierCommitment = pdata $ pfrontierCommitment # pfromData (Scan.pscan'assetCount facts) # pfromData (Scan.pscan'assetPeaks facts)
                      , Commitment.poutputCommitment'cardanoValueSize = Scan.pscan'cardanoValueSize facts
                      }
              Commitment.pdescriptorIsWellFormed descriptor
                #&& padvance
                  datum
                  args
                  tx
                  ( pcon
                      st
                        { Yield.pstate'phase = pdata 9
                        , Yield.pstate'descriptorCbor = pdata $ Commitment.pencodeLedgerOutputCommitment # descriptor
                        }
                  )
          )
          perror
    )
    perror

pclaimYield ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s PScriptContext ->
  Integer ->
  (Term s PHeaderV1 -> Term s Claim.PValidationClaimWitnessV1 -> Term s PBool) ->
  Term s PUnit
pclaimYield dispatcher ctx phase check = pwithYield dispatcher ctx $ \datum state args tx _ -> P.do
  st <- pmatch state
  PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
  Yield.PArgs{Yield.pargs'proofRefIndices} <- pmatch args
  PPair headerRaw fault <- pmatch $ Carriage.pfields # (Carriage.pread # pfromData (Yield.pstate'proofCommitment st) # pfromData pargs'proofRefIndices # pfromData ptxInfo'referenceInputs)
  PBuiltinPair tag fields <- pmatch $ pasConstr # fault
  pif
    (tag #== 9)
    ( P.do
        PBuiltinPair _ witnessFields <- pmatch $ pasConstr # (pheadSingleton # fields)
        pif
          (plength # witnessFields #== 2)
          ( check (pfromData $ punsafeCoerce @(PAsData PHeaderV1) headerRaw) (pfromData $ punsafeCoerce @(PAsData Claim.PValidationClaimWitnessV1) $ phead # witnessFields)
              #&& pfromData (Yield.pstate'kind st)
              #== 2
              #&& pfromData (Yield.pstate'phase st)
              #== pconstant phase
              #&& padvance datum args tx (pcon st{Yield.pstate'phase = pdata $ pconstant $ phase + 1})
          )
          perror
    )
    perror

claimStructureValidator, claimSourceValidator, claimEndpointsValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
claimStructureValidator = plam $ \dispatcher ctx -> pclaimYield dispatcher ctx 0 Claim.pcommittedClaimStructureIsValid
claimSourceValidator = plam $ \dispatcher ctx -> pclaimYield dispatcher ctx 1 Claim.pcommittedClaimSourceIsAuthenticated
claimEndpointsValidator = plam $ \dispatcher ctx -> pclaimYield dispatcher ctx 2 Claim.pcommittedClaimEndpointsAndSourceAreValid

l2OpenValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
l2OpenValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'kind st) #== 0 #&& pfromData (Yield.pstate'phase st) #== 0)
    ( P.do
        PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
        Yield.PArgs{Yield.pargs'proofRefIndices} <- pmatch args
        PPair _ fault <- pmatch $ Carriage.pfields # (Carriage.pread # pfromData (Yield.pstate'proofCommitment st) # pfromData pargs'proofRefIndices # pfromData ptxInfo'referenceInputs)
        fields <- plet $ Yield.poneStepWitness # fault # 4
        pif
          (plength # fields #== 7)
          ( P.do
              Trace.PRootMembershipProof{Trace.prootMembership'value = traceData} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 0 # fields
              Ledger.PTransitionStep{Ledger.ptransitionStep'preUtxosRoot} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Ledger.PTransitionStep) traceData
              Trace.PRootMembershipProof{Trace.prootMembership'key = key, Trace.prootMembership'value = encodedSource} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 2 # fields
              sourceBytes <- plet $ pasByteStr # encodedSource
              source <- plet $ Proof.pdecodeL2TransactionSource # sourceBytes
              Ledger.PL2TransactionSourceV1{Ledger.pl2Source'txId, Ledger.pl2Source'source} <- pmatch source
              Ledger.PNativeTxProofSourceV1{Ledger.pnativeSource'compactCbor} <- pmatch $ pfromData pl2Source'source
              anchored <-
                plet $
                  Opening.panchoredNativeTx
                    # pcon (Opening.PBodyTxOpening $ pfromData pnativeSource'compactCbor)
                    # pcon (Opening.PBodyAnchor $ pdata $ pasByteStr # key)
              opened <- plet $ pfromData $ punsafeCoerce @(PAsData Yield.POpenedOutputs) raw
              Yield.POpenedOutputs keys hashes <- pmatch opened
              let inputs = Proof.pdoorBodyFieldItems anchored 0 (pasByteStr # (pelemAt # 3 # fields))
                  outputs = Proof.pdoorBodyFieldItems anchored 2 (pasByteStr # (pelemAt # 4 # fields))
              Opening.punanchoredValidityCodeOf
                # anchored
                #== 0
                #&& pserialiseData
                # pforgetData (pdata source)
                #== sourceBytes
                #&& pfromData pl2Source'txId
                #== pasByteStr
                # key
                #&& Opening.panchoredNativeTxVersion
                # anchored
                #== 1
                #&& pfromData keys
                #== (pmap # plam (\bytes -> pdata $ Components.pencodeMidgardTxInput # (Components.pdecodeMidgardTxInputCbor # bytes)) # inputs)
                #&& pfromData hashes
                #== (pmap # plam (\bytes -> pdata $ pblake2b_256 # bytes) # outputs)
                #&& padvance
                  datum
                  args
                  tx
                  ( pcon
                      st
                        { Yield.pstate'opened = pdata opened
                        , Yield.pstate'phase = pdata 1
                        , Yield.pstate'currentRoot = ptransitionStep'preUtxosRoot
                        }
                  )
          )
          perror
    )
    perror

l2ReplayValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
l2ReplayValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx _ -> P.do
  st <- pmatch state
  PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
  Yield.PArgs{Yield.pargs'proofRefIndices} <- pmatch args
  PPair _ fault <- pmatch $ Carriage.pfields # (Carriage.pread # pfromData (Yield.pstate'proofCommitment st) # pfromData pargs'proofRefIndices # pfromData ptxInfo'referenceInputs)
  kind <- plet $ pfromData $ Yield.pstate'kind st
  fields <-
    plet $
      pif
        (kind #== 0)
        (Yield.poneStepWitness # fault # 4)
        (pif (kind #== 1) (Yield.poneStepWitness # fault # 3) perror)
  pif
    (plength # fields #== pif (kind #== 0) 7 6)
    ( P.do
        Trace.PRootMembershipProof{Trace.prootMembership'value = traceData} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ phead # fields
        Ledger.PTransitionStep{Ledger.ptransitionStep'postUtxosRoot} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Ledger.PTransitionStep) traceData
        let produced =
              pfromData $
                punsafeCoerce @(PAsData (PBuiltinList (PAsData Proof.PLedgerInsertWitness))) $
                  pif (kind #== 0) (pelemAt # 6 # fields) (plistData # (psingleton # (pelemAt # 5 # fields)))
        phase <- plet $ pfromData $ Yield.pstate'phase st
        index <- plet $ pfromData $ Yield.pstate'outputIndex st
        Yield.POpenedOutputs spendKeys outputHashes <- pmatch $ pfromData $ Yield.pstate'opened st
        pif
          (phase #== 9)
          ( P.do
              Trace.PRootMembershipProof{Trace.prootMembership'key} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 2 # fields
              key <-
                plet $
                  pif
                    (kind #== 0)
                    (Components.pencodeMidgardTxInput # pcon (Native.PMidgardTxInput (pdata $ pasByteStr # prootMembership'key) (pdata index)))
                    (Proof.pledgerOutrefKey prootMembership'key)
              witness <- pmatch $ pfromData $ pelemAt # index # produced
              pfromData (Proof.pledgerInsert'key witness)
                #== key
                #&& pfromData (Proof.pledgerInsert'value witness)
                #== pfromData (Yield.pstate'descriptorCbor st)
                #&& padvance datum args tx (pcon st{Yield.pstate'phase = pdata 4})
          )
          $ pif
            (phase #== 5)
            (pnot # (pfromData (Yield.pstate'currentRoot st) #== pfromData ptransitionStep'postUtxosRoot))
          $ pif
            (phase #== 1)
            ( pif
                (kind #== 0)
                ( P.do
                    spent <- plet $ pfromData $ punsafeCoerce @(PAsData (PBuiltinList (PAsData Proof.PLedgerDeleteWitness))) $ pelemAt # 5 # fields
                    inputIndex <- plet $ pfromData $ Yield.pstate'inputIndex st
                    pif
                      (inputIndex #== plength # pfromData spendKeys)
                      ( inputIndex
                          #== plength
                          # spent
                          #&& padvance
                            datum
                            args
                            tx
                            ( pcon
                                st
                                  { Yield.pstate'phase = pdata $ pif (pnull # pfromData outputHashes) 5 2
                                  }
                            )
                      )
                      ( P.do
                          key <- plet $ pfromData $ pelemAt # inputIndex # pfromData spendKeys
                          witness <- plet $ pfromData $ pelemAt # inputIndex # spent
                          Proof.PLedgerDeleteWitness{Proof.pledgerDelete'key} <- pmatch witness
                          pfromData pledgerDelete'key
                            #== key
                            #&& padvance
                              datum
                              args
                              tx
                              ( pcon
                                  st
                                    { Yield.pstate'inputIndex = pdata $ inputIndex + 1
                                    , Yield.pstate'currentRoot = pdata $ Proof.papplyDeleteWitness (pfromData $ Yield.pstate'currentRoot st) witness
                                    }
                              )
                      )
                )
                perror
            )
          $ pif
            (phase #== 4)
            ( P.do
                witness <- plet $ pfromData $ pelemAt # index # produced
                root <- plet $ Proof.papplyInsertWitness (pfromData $ Yield.pstate'currentRoot st) witness
                nextIndex <- plet $ index + 1
                done <- plet $ nextIndex #== plength # pfromData outputHashes
                (pnot # done #|| nextIndex #== plength # produced)
                  #&& padvance
                    datum
                    args
                    tx
                    ( pcon
                        st
                          { Yield.pstate'phase = pdata $ pif done 5 2
                          , Yield.pstate'outputIndex = pdata nextIndex
                          , Yield.pstate'currentRoot = pdata root
                          , Yield.pstate'summaries = pdata $ pcon $ Yield.POutputSummaries $ pdata pnil
                          , Yield.pstate'scanCbor = pdata $ pconstant ""
                          , Yield.pstate'valueCbor = pdata $ pconstant ""
                          , Yield.pstate'valueStart = pdata 0
                          , Yield.pstate'valueEnd = pdata 0
                          , Yield.pstate'valueSummary = pcon PDNothing
                          , Yield.pstate'descriptorCbor = pdata $ pconstant ""
                          }
                    )
            )
            perror
    )
    perror

pmaterialisable :: forall s. Term s (PData :--> PBool)
pmaterialisable = phoistAcyclic $ pfix $ \self -> plam $ \dat -> P.do
  tag <- plet $ pchooseData # dat # (pconstant @PInteger 0) # 1 # 2 # 3 # 4
  pif
    (tag #== 0)
    ( P.do
        PBuiltinPair alternative fields <- pmatch $ pasConstr # dat
        alternative #>= 0 #&& alternative #< 128 #&& pall # self # fields
    )
    $ pif
      (tag #== 1)
      (pall # plam (\pair -> self # (pfstBuiltin # pair) #&& self # (psndBuiltin # pair)) # (pasMap # dat))
    $ pif (tag #== 2) (pall # self # (pasList # dat))
    $ pif
      (tag #== 3)
      (plet (pasInt # dat) $ \value -> value #>= (-18446744073709551616) #&& value #<= 18446744073709551615)
      (pconstant True)

outputSummariesValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
outputSummariesValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'phase st) #== 8 #&& pnot # (pfromData (Yield.pstate'kind st) #== 2))
    ( P.do
        evidence <- plet $ pasList # raw
        pif
          (plength # evidence #== 2)
          ( P.do
              committed <- plet $ pfromData $ punsafeCoerce @(PAsData Yield.POutputSummaries) $ phead # evidence
              PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
              bytes <- plet $ Yield.poutputBytes # state # args # pfromData ptxInfo'referenceInputs
              facts <- pmatch $ Scan.pdecodeControlV1 # pfromData (Yield.pstate'scanCbor st)
              address <- plet $ pmatch (Output.pdecodeCanonicalAddressBytes # pfromData (Scan.pscan'address facts)) $ \case
                PJust value -> value
                PNothing -> perror
              script <-
                plet $
                  pif
                    (pfromData (Scan.pscan'referenceScriptItemOffset facts) #== (-1))
                    (pcon PDNothing)
                    ( P.do
                        language <- plet $ pfromData $ Scan.pscan'referenceScriptLanguage facts
                        mapped <-
                          plet $
                            pif (language #== 0) (pcon Native.PNativeCardanoScript) $
                              pif (language #== 3) (pcon Native.PPlutusV3Script) $
                                pif (language #== 128) (pcon Native.PMidgardV1Script) perror
                        pcon $
                          PDJust $
                            pdata $
                              pcon $
                                Native.PMidgardVersionedScript
                                  (pdata mapped)
                                  (pdata $ psliceBS # pfromData (Scan.pscan'referenceScriptOffset facts) # pfromData (Scan.pscan'referenceScriptLength facts) # bytes)
                    )
              let output =
                    pcon $
                      Native.PMidgardTxOutput
                        (pdata address)
                        (pdata $ pcon $ Native.PMidgardValue (pdata 0) (pdata $ pcon $ PAssocMap pnil))
                        (pdata $ pcon PDNothing)
                        (pdata script)
              datumField <-
                plet $
                  pif
                    (pfromData (Scan.pscan'datumOffset facts) #== (-1))
                    (pforgetData $ pconstrBuiltin # 0 # pnil)
                    ( P.do
                        expected <- plet $ psliceBS # pfromData (Scan.pscan'datumOffset facts) # pfromData (Scan.pscan'datumLength facts) # bytes
                        resolved <- plet $ Structured.presolve # (pelemAt # 1 # evidence) # pfromData ptxInfo'referenceInputs
                        pif
                          (pmaterialisable # resolved #&& pserialiseData # resolved #== expected)
                          (pforgetData $ pconstrBuiltin # 2 # (psingleton # resolved))
                          perror
                    )
              value <- plet $ pmatch (Yield.pstate'valueSummary st) $ \case
                PDNothing -> perror
                PDJust summary -> pfromData summary
              PPair cardano rest <- pmatch $ Context.pledgerOutputSummariesOfAuthenticatedPartsV1 # output # value # datumField
              PPair midgard spendDatum <- pmatch rest
              Yield.POutputSummaries actual <- pmatch committed
              pfromData actual
                #== (psingleton # pdata (pcons # pdata cardano # (pcons # pdata midgard # (psingleton # pdata spendDatum))))
                #&& padvance datum args tx (pcon st{Yield.pstate'phase = pdata 3, Yield.pstate'summaries = pdata committed})
          )
          perror
    )
    perror

depositSummariesValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
depositSummariesValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'kind st) #== 1 #&& pfromData (Yield.pstate'phase st) #== 8)
    ( P.do
        PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
        PPair reference _ <- pmatch $ Deposit.popen # state # args # pfromData ptxInfo'referenceInputs
        Proof.PAuthenticatedDepositReference{Proof.pauthDeposit'info} <- pmatch reference
        Ledger.PDepositInfo{Ledger.pdepositInfo'l2Address, Ledger.pdepositInfo'l2NetworkId, Ledger.pdepositInfo'l2Datum} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Ledger.PDepositInfo) pauthDeposit'info
        let output =
              pcon $
                Native.PMidgardTxOutput
                  (pdata $ Proof.pdepositAddressToMidgard (pfromData pdepositInfo'l2Address) (pfromData pdepositInfo'l2NetworkId))
                  (pdata $ pcon $ Native.PMidgardValue (pdata 0) (pdata $ pcon $ PAssocMap pnil))
                  (pdata $ pcon PDNothing)
                  (pdata $ pcon PDNothing)
            datumField = pmatch (punsafeCoerce @(PMaybeData PData) pdepositInfo'l2Datum) $ \case
              PDNothing -> pforgetData $ pconstrBuiltin # 0 # pnil
              PDJust value -> pforgetData $ pconstrBuiltin # 2 # (psingleton # pfromData value)
        value <- plet $ pmatch (Yield.pstate'valueSummary st) $ \case
          PDNothing -> perror
          PDJust summary -> pfromData summary
        PPair cardano rest <- pmatch $ Context.pledgerOutputSummariesOfAuthenticatedPartsV1 # output # value # datumField
        PPair midgard spendDatum <- pmatch rest
        committed <- plet $ pfromData $ punsafeCoerce @(PAsData Yield.POutputSummaries) raw
        Yield.POutputSummaries actual <- pmatch committed
        pfromData actual
          #== (psingleton # pdata (pcons # pdata cardano # (pcons # pdata midgard # (psingleton # pdata spendDatum))))
          #&& padvance datum args tx (pcon st{Yield.pstate'phase = pdata 3, Yield.pstate'summaries = pdata committed})
    )
    perror

depositProjectionValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
depositProjectionValidator = plam $ \dispatcher hub ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'kind st) #== 1)
    ( P.do
        PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
        pif
          (pfromData (Yield.pstate'phase st) #== 10)
          ( P.do
              PPair reference pair <- pmatch $ Deposit.popen # state # args # pfromData ptxInfo'referenceInputs
              PPair policy asset <- pmatch pair
              Proof.PAuthenticatedDepositReference{Proof.pauthDeposit'info} <- pmatch reference
              let policyId = pcon $ PCurrencySymbol policy
                  tokenName = pcon $ PTokenName asset
                  emptyProjection =
                    Proof.pprojectedDepositOutput
                      (pfromData $ punsafeCoerce @(PAsData Ledger.PDepositInfo) pauthDeposit'info)
                      (CardanoValue.psingletonSortedValue # policyId # tokenName # 1)
                      (pdata policyId)
                      (pdata tokenName)
              Yield.poutputSkeleton
                # state
                # (Yield.poutputBytes # state # args # pfromData ptxInfo'referenceInputs)
                #== Components.pencodeMidgardTxOutput
                # emptyProjection
                #&& padvance datum args tx (pcon st{Yield.pstate'phase = pdata 8})
          )
          $ pif
            (pfromData (Yield.pstate'phase st) #== 0)
            ( P.do
                Yield.PArgs{Yield.pargs'proofRefIndices, Yield.pargs'hubRefInputIndex, Yield.pargs'depositEventRefIndex} <- pmatch args
                PPair _ fault <- pmatch $ Carriage.pfields # (Carriage.pread # pfromData (Yield.pstate'proofCommitment st) # pfromData pargs'proofRefIndices # pfromData ptxInfo'referenceInputs)
                fields <- plet $ Yield.poneStepWitness # fault # 3
                pif
                  (plength # fields #== 6)
                  ( P.do
                      Trace.PRootMembershipProof{Trace.prootMembership'value = traceRaw} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ phead # fields
                      Ledger.PTransitionStep{Ledger.ptransitionStep'preUtxosRoot} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Ledger.PTransitionStep) traceRaw
                      Trace.PRootMembershipProof{Trace.prootMembership'key = sourceKey, Trace.prootMembership'value = sourceValue} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 2 # fields
                      eventAsset <- plet $ pasByteStr # (pelemAt # 4 # fields)
                      Proof.PLedgerInsertWitness{Proof.pledgerInsert'key} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Proof.PLedgerInsertWitness) $ pelemAt # 5 # fields
                      Hub.PHubOracleDatum{Hub.phubOracle'deposit} <- pmatch $ Hub.pgetDatum # pfromData ptxInfo'referenceInputs # hub # pfromData pargs'hubRefInputIndex
                      reference <- plet $ Proof.pgetAuthenticatedDepositReference # pfromData ptxInfo'referenceInputs # phubOracle'deposit # pdata (pcon $ PTokenName eventAsset) # pfromData pargs'depositEventRefIndex
                      Proof.PAuthenticatedDepositReference{Proof.pauthDeposit'outRef, Proof.pauthDeposit'id, Proof.pauthDeposit'info, Proof.pauthDeposit'value} <- pmatch reference
                      count <-
                        plet $
                          pfoldl
                            # plam
                              ( \total pair ->
                                  pif
                                    (pto (pfromData $ pfstBuiltin # pair) #== pconstant "")
                                    total
                                    (total + plength # (pto $ pto $ pfromData $ psndBuiltin # pair))
                              )
                            # 0
                            # (pto $ pto $ pto pauthDeposit'value)
                      PTxOutRef{ptxOutRef'idx} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PTxOutRef) sourceKey
                      opened <- plet $ pfromData $ punsafeCoerce @(PAsData Yield.POpenedOutputs) raw
                      Yield.POpenedOutputs keys hashes <- pmatch opened
                      count
                        #> 0
                        #&& pauthDeposit'id
                        #== sourceKey
                        #&& pauthDeposit'info
                        #== sourceValue
                        #&& pfromData pledgerInsert'key
                        #== Proof.pledgerOutrefKey sourceKey
                        #&& pnull
                        # pfromData keys
                        #&& plength
                        # pfromData hashes
                        #== 1
                        #&& padvance
                          datum
                          args
                          tx
                          ( pcon
                              st
                                { Yield.pstate'opened = pdata opened
                                , Yield.pstate'phase = pdata 2
                                , Yield.pstate'depositSourceCbor =
                                    pdata $
                                      pserialiseData
                                        # ( plistData
                                              # (pcons # pforgetData pauthDeposit'outRef # (pcons # pforgetData phubOracle'deposit # (psingleton # pforgetData (pdata eventAsset))))
                                          )
                                , Yield.pstate'depositAssetCount = pdata $ count - 1
                                , Yield.pstate'depositIndex = ptxOutRef'idx
                                , Yield.pstate'currentRoot = ptransitionStep'preUtxosRoot
                                }
                          )
                  )
                  perror
            )
            perror
    )
    perror

type PIndexedValue = PBuiltinPair (PAsData Value.PLedgerOutputValueWitnessV1) (PAsData PInteger)

type PSourceTokens = PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))

-- Aiken list.drop: advance through the already selected policy's token list.
psourceDrop :: forall s. Term s (PInteger :--> PSourceTokens :--> PSourceTokens)
psourceDrop = phoistAcyclic $ pfix $ \self -> plam $ \count tokens ->
  pif (count #<= 0) tokens $ pelimList (\_ rest -> self # (count - 1) # rest) pnil tokens

pindexedPolicy :: forall s. Term s (PIndexedValue :--> PByteString)
pindexedPolicy = phoistAcyclic $ plam $ \item -> P.do
  PBuiltinPair witnessData _ <- pmatch item
  witness <- plet $ pfromData witnessData
  pmatch witness $ \case
    Value.PLedgerOutputValueAsset _ policy _ _ _ _ -> pfromData policy
    _ -> perror

pinsertIndexed :: forall s. Term s (PIndexedValue :--> PBuiltinList PIndexedValue :--> PBuiltinList PIndexedValue)
pinsertIndexed = phoistAcyclic $ pfix $ \self -> plam $ \item items ->
  pelimList
    ( \next rest -> P.do
        PBuiltinPair _ indexData <- pmatch item
        PBuiltinPair _ nextIndexData <- pmatch next
        let index = pfromData indexData
            nextIndex = pfromData nextIndexData
        policy <- plet $ pindexedPolicy # item
        nextPolicy <- plet $ pindexedPolicy # next
        pif
          (policy #< nextPolicy #|| (policy #== nextPolicy #&& index #<= nextIndex))
          (pcons # item # items)
          (pcons # next # (self # item # rest))
    )
    (psingleton # item)
    items

psourceMatches :: forall s. Term s (PBuiltinList PIndexedValue :--> CardanoValue.PSortedValue :--> PByteString :--> PInteger :--> PSourceTokens :--> PByteString :--> PByteString :--> PBool)
psourceMatches = phoistAcyclic $ pfix $ \self -> plam $ \requested source currentPolicy nextIndex remaining eventPolicy eventAsset ->
  pelimList
    ( \item rest -> P.do
        PBuiltinPair witnessData indexData <- pmatch item
        let witness = pfromData witnessData
            index = pfromData indexData
        pmatch witness $ \case
          Value.PLedgerOutputValueNoWitness -> perror
          Value.PLedgerOutputValueAsset _ policyD assetD quantityD _ _ -> P.do
            policy <- plet $ pfromData policyD
            PPair start tokens <-
              pmatch $
                pif
                  (policy #== currentPolicy)
                  (pcon $ PPair nextIndex remaining)
                  ( pcon $ PPair 0 $ pmatch (AssocMap.plookup # pcon (PCurrencySymbol policy) # pto source) $ \case
                      PNothing -> pnil
                      PJust tokenMap -> pto $ pto tokenMap
                  )
            pif
              (index #>= start)
              ( P.do
                  selected <- plet $ psourceDrop # (index - start) # tokens
                  PBuiltinPair actualName actualQuantity <- pmatch $ phead # selected
                  (pnot # (policy #== eventPolicy) #|| pnot # (pfromData assetD #== eventAsset))
                    #&& pto (pfromData actualName)
                    #== pfromData assetD
                    #&& pfromData actualQuantity
                    #== pfromData quantityD
                    #&& self
                    # rest
                    # source
                    # policy
                    # (index + 1)
                    # (ptail # selected)
                    # eventPolicy
                    # eventAsset
              )
              perror
    )
    (pconstant True)
    requested

depositValueValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
depositValueValidator = plam $ \dispatcher ctx -> pwithYield dispatcher ctx $ \datum state args tx raw -> P.do
  st <- pmatch state
  pif
    (pfromData (Yield.pstate'phase st) #== 7 #&& pfromData (Yield.pstate'kind st) #== 1)
    ( P.do
        indexed <-
          plet $
            pmap
              # plam
                ( \dat -> P.do
                    fields <- plet $ pasList # dat
                    pif
                      (plength # fields #== 2)
                      (ppairDataBuiltin # (punsafeCoerce @(PAsData Value.PLedgerOutputValueWitnessV1) $ phead # fields) # pdata (pasInt # (pelemAt # 1 # fields)))
                      perror
                )
              # (pasList # raw)
        pif
          (pnot # (pnull # indexed) #&& plength # indexed #<= 4)
          ( P.do
              facts <- plet $ Scan.pdecodeControlV1 # pfromData (Yield.pstate'scanCbor st)
              Scan.PLedgerOutputScanControlV1{Scan.pscan'assetCount, Scan.pscan'lovelace} <- pmatch facts
              PTxInfo{ptxInfo'referenceInputs} <- pmatch tx
              PPair reference pair <- pmatch $ Deposit.popen # state # args # pfromData ptxInfo'referenceInputs
              PPair policy asset <- pmatch pair
              Proof.PAuthenticatedDepositReference{Proof.pauthDeposit'value} <- pmatch reference
              pif
                ( pscan'assetCount
                    #== Yield.pstate'depositAssetCount st
                    #&& pfromData pscan'lovelace
                    #== (pquantityOfValue # pauthDeposit'value # CardanoValue.padaSymbolData # pdata CardanoValue.padaToken)
                    #&& pall
                    # plam
                      ( \item -> P.do
                          PBuiltinPair witnessData indexData <- pmatch item
                          let witness = pfromData witnessData
                              index = pfromData indexData
                          pmatch witness $ \case
                            Value.PLedgerOutputValueNoWitness -> index #== (-1)
                            _ -> index #>= 0
                      )
                    # indexed
                )
                ( P.do
                    requested <-
                      plet $
                        pfoldr
                          # pinsertIndexed
                          # pnil
                          # ( pfilter
                                # plam
                                  ( \item -> P.do
                                      PBuiltinPair witnessData _ <- pmatch item
                                      let witness = pfromData witnessData
                                      pmatch witness $ \case Value.PLedgerOutputValueNoWitness -> pconstant False; _ -> pconstant True
                                  )
                                # indexed
                            )
                    pif
                      (psourceMatches # requested # pauthDeposit'value # pconstant "" # 0 # pnil # policy # asset)
                      ( P.do
                          control <-
                            plet $
                              pif
                                (pfromData (Yield.pstate'valueCbor st) #== pconstant "")
                                (Value.pinitialControlV1 # pfromData pscan'assetCount)
                                (Value.pdecodeControlV1 # pfromData (Yield.pstate'valueCbor st))
                          witnesses <- plet $ pmap # plam (\item -> pmatch item $ \(PBuiltinPair witness _) -> witness) # indexed
                          next <- plet $ pvalueBatch # control # facts # witnesses
                          Value.PLedgerOutputValueControlV1{Value.pvalueControl'stage} <- pmatch next
                          summary <- plet $ pmatch (Value.pfinalizeV1 # next) $ \case
                            PNothing -> pcon PDNothing
                            PJust result -> pcon $ PDJust $ pdata result
                          padvance datum args tx $
                            pcon
                              st
                                { Yield.pstate'phase = pdata $ pif (pfromData pvalueControl'stage #== Value.pstageTerminal) 10 7
                                , Yield.pstate'valueCbor = pdata $ Value.pencodeControlV1 # next
                                , Yield.pstate'valueSummary = summary
                                }
                      )
                      perror
                )
                perror
          )
          perror
    )
    perror
