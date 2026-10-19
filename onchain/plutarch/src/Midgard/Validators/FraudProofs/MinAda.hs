module Midgard.Validators.FraudProofs.MinAda (
  minAdaStep01Validator,
  minAdaStep02Validator,
  minAdaStep03Validator,
  minAdaStep04Validator,
  minAdaStep05Validator,
  minAdaTxYieldValidator,
  minAdaUtxoYieldValidator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PRedeemer (..),
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pheadSingleton)
import Midgard.ComputationThread (PStepDatum (..), PStepRedeemer (..))
import Midgard.FraudProof qualified as FraudProof
import Midgard.FraudProofs.ChunkedInclusion (ppublishedChunkMembershipFourChunks, ppublishedChunkNonMembershipFourChunks)
import Midgard.FraudProofs.Common (
  PMembershipCarriage (..),
  PNativeTxInclusionCarriage,
  PNonMembershipCarriage (..),
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
  pverifyMembershipCarried,
  pverifyNonMembershipCarried,
 )
import Midgard.FraudProofs.FieldOpening
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1)
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.TransactionOutputNonCanonical qualified as OutputScan
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.NativeTxMachineWalk qualified as Walk
import Midgard.RejectionReason (PRejectionReasonV1 (POutputBelowMinAda))
import Midgard.StateQueueYield (prequireAuthenticatedZeroYield)
import Midgard.TransitionTrace (PRootMembershipProof (..))

import Midgard.FraudProofs.MinAda
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTxInput (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..),
  pdecodeLedgerOutputCommitment,
 )
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.ValidationMachine (pcoinsPerUtxoByte, poutputMeetsMinAdaV1)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

minAdaStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
minAdaStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args{pstep01Args'forcedSource, pstep01Args'txInclusion, pstep01Args'postUtxoMembership, pstep01Args'fault} <- pmatch args
      fault <- plet $ pfromData pstep01Args'fault
      pmatch pstep01Args'forcedSource $ \case
        PDJust source -> ppassForcedClaim step02ScriptHash computationThreadPolicy datum (pfromData source) pstep01Args'txInclusion pstep01Args'postUtxoMembership fault ownOutRef txInfo
        PDNothing ->
          pmatch fault $ \case
            PMinAdaTx{pminAdaTx'outputIndex} ->
              pexpecting (pfromData pminAdaTx'outputIndex #>= 0) $
                pmatch pstep01Args'txInclusion $ \case
                  PDJust inclusion -> pmatch pstep01Args'postUtxoMembership $ \case
                    PDNothing ->
                      ppassTransactionClaim
                        step02ScriptHash
                        computationThreadPolicy
                        hubOracle
                        datum
                        (pfromData inclusion)
                        fault
                        ownOutRef
                        txInfo
                    PDJust _ -> perror
                  PDNothing -> perror
            PMinAdaUtxo ->
              pmatch pstep01Args'txInclusion $ \case
                PDNothing -> pmatch pstep01Args'postUtxoMembership $ \case
                  PDJust membership ->
                    ppassPostUtxoClaim
                      step02ScriptHash
                      computationThreadPolicy
                      hubOracle
                      datum
                      (pfromData membership)
                      fault
                      ownOutRef
                      txInfo
                  PDNothing -> perror
                PDJust _ -> perror

ppassTransactionClaim ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PNativeTxInclusionCarriage ->
  Term s PMinAdaFaultV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
ppassTransactionClaim step02ScriptHash computationThreadPolicy hubOracle datum inclusion fault ownOutRef txInfo = P.do
  PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
  ppassNativeTxToNextStepCarried
    computationThreadPolicy
    hubOracle
    datum
    inclusion
    ownOutRef
    (pfromData ptxInfo'inputs)
    (pfromData ptxInfo'referenceInputs)
    (pfromData ptxInfo'outputs)
    (pto $ pto $ pfromData ptxInfo'redeemers)
    $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
      PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch badTxView
      PNativeTxCompact{pcompact'validityCode} <- pmatch pverified'txCompact
      expected <- plet $ pcon $ PStep02State (pdata $ pconstant "") (pdata $ pconstant False) (pdata $ pconstant "") (pdata 0) (pdata badTxId) (pdata fault) (pcon PDNothing)
      pexpecting (pstateIsAbsent inputState) $
        pexpecting (pcompact'validityCode #== 0) $
          pexpecting (outputScriptHash #== step02ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

ppassPostUtxoClaim ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PPostUtxoMembershipV1 ->
  Term s PMinAdaFaultV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
ppassPostUtxoClaim step02ScriptHash computationThreadPolicy hubOracle datum membership fault ownOutRef txInfo = P.do
  PPostUtxoMembershipV1
    { ppostMembership'inputIndex
    , ppostMembership'outputIndex
    , ppostMembership'hubRefInputIndex
    , ppostMembership'stateQueueNodeRefInputIndex
    , ppostMembership'outRef
    , ppostMembership'descriptorCbor
    } <-
    pmatch membership
  PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
  pcontinue
    computationThreadPolicy
    (pexpectDatum datum)
    (pfromData ppostMembership'inputIndex)
    (pfromData ppostMembership'outputIndex)
    ownOutRef
    (pfromData ptxInfo'inputs)
    (pfromData ptxInfo'outputs)
    $ \_ownScriptHash threadName _prover inputState outputScriptHash outputStateData ->
      pmatch (Hub.pgetDatum # pfromData ptxInfo'referenceInputs # hubOracle # pfromData ppostMembership'hubRefInputIndex) $
        \PHubOracleDatum{phubOracle'stateQueue} ->
          pgetBlockDatumV1
            (pfromData ptxInfo'referenceInputs)
            phubOracle'stateQueue
            (pfromData ppostMembership'stateQueueNodeRefInputIndex)
            $ \headerD nodeKey -> P.do
              PHeaderV1{pheader'utxosRoot, pheader'prevUtxosRoot} <- pmatch $ pfromData headerD
              PTxOutRef{ptxOutRef'id} <- pmatch $ pfromData ppostMembership'outRef
              postState <-
                plet $
                  pcon $
                    PPostUtxoStateV1
                      ppostMembership'outRef
                      ppostMembership'descriptorCbor
                      pheader'utxosRoot
                      pheader'prevUtxosRoot
              expected <-
                plet $
                  pcon $
                    PStep02State
                      (pdata $ pconstant "")
                      (pdata $ pconstant False)
                      (pdata $ pconstant "")
                      (pdata 0)
                      (pdata $ pto $ pfromData ptxOutRef'id)
                      (pdata fault)
                      (pcon $ PDJust $ pdata postState)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (outputScriptHash #== step02ScriptHash) $
                  pexpecting (nodeKey #== FraudProof.passetNameToHeaderHash # threadName) $
                    pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

minAdaStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaStep02Validator = plam $ \step03Hash _step05Hash threadPolicy authPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep02Args{pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'yieldToRefInputIndex, pstep02Args'outputsOpening, pstep02Args'postMembership} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          PStep02State{pstep02State'fault, pstep02State'postUtxo} <- pmatch $ pexpectStateAs @PStep02State inputState
          pmatch (pfromData pstep02State'fault) $ \case
            PMinAdaTx{} -> P.do
              PDJust _ <- pmatch pstep02Args'outputsOpening
              PDNothing <- pmatch pstep02Args'postMembership
              PDNothing <- pmatch pstep02State'postUtxo
              pexpecting (pif (outputHash #== ownHash) (pconstrShape outputState 0 7) (outputHash #== step03Hash #&& pconstrShape outputState 0 2)) $ P.do
                _ <- plet $ prequireAuthenticatedZeroYield # txInfo # pfromData authPolicy # pcon (PTokenName $ pconstant "V1FpMinAdaS02TxYield") # pfromData pstep02Args'yieldToRefInputIndex
                pconstant True
            PMinAdaUtxo -> P.do
              PDNothing <- pmatch pstep02Args'outputsOpening
              PDJust _ <- pmatch pstep02Args'postMembership
              PDJust post <- pmatch pstep02State'postUtxo
              PPostUtxoStateV1{ppostState'outRef, ppostState'descriptorCbor, ppostState'prevUtxosRoot} <- pmatch $ pfromData post
              expected <- plet $ pcon $ PMinAdaUtxoDescriptor ppostState'descriptorCbor (pdata $ pledgerOutrefKey # pfromData ppostState'outRef) ppostState'prevUtxosRoot
              pexpecting (outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)) $ P.do
                _ <- plet $ prequireAuthenticatedZeroYield # txInfo # pfromData authPolicy # pcon (PTokenName $ pconstant "V1FpMinAdaS02UtxoYield") # pfromData pstep02Args'yieldToRefInputIndex
                pconstant True

pconstrShape :: Term s PData -> Term s PInteger -> Term s PInteger -> Term s PBool
pconstrShape value tag arity = pmatch (pasConstr # value) $ \(PBuiltinPair actual fields) -> actual #== tag #&& plength # fields #== arity

minAdaStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaStep03Validator = plam $ \step04Hash step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep03Args window inputIndex outputIndex <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState ->
          pmatch (pexpectStateAs @PStep03State inputState) $ \case
            PMinAdaTxScan direction scan -> P.do
              next <- plet $ padvanceMinAdaScan # pfromData scan # pfromData window
              OutputScan.POutputScanState{OutputScan.poutputScan'outcome = outcome, OutputScan.poutputScan'itemLength = itemLength, OutputScan.poutputScan'control = control} <- pmatch next
              Scan.PLedgerOutputScanControlV1{Scan.pscan'lovelace = lovelace} <- pmatch $ pfromData control
              expected <-
                plet $
                  pif
                    (pfromData outcome #== OutputScan.poutcomeCanonical)
                    (pcon $ PMinAdaTxDescriptor direction itemLength lovelace)
                    (pcon $ PMinAdaTxScan direction (pdata next))
              pfromData outcome #/= OutputScan.poutcomeNonCanonical #&& outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
            PMinAdaTxDescriptor direction totalLength lovelace ->
              outputHash
                #== step05Hash
                #&& outputState
                #== pforgetData (pdata $ pcon PPredicateAndCulpabilityAuthenticated)
                #&& plet
                  (poutputMeetsMinAdaV1 # pcoinsPerUtxoByte # pfromData totalLength # pfromData lovelace)
                  ( \meets ->
                      pif (pfromData direction #== 1) meets (pfromData direction #== 0 #&& pnot # meets)
                  )
            PMinAdaUtxoDescriptor descriptorBytes key prevRoot -> P.do
              PLedgerOutputCommitmentV1{poutputCommitment'totalLength, poutputCommitment'lovelace} <- pmatch $ pdecodeLedgerOutputCommitment # pfromData descriptorBytes
              expected <- plet $ pcon $ PStep04State key prevRoot
              outputHash
                #== step04Hash
                #&& outputState
                #== pforgetData (pdata expected)
                #&& pnot
                # (poutputMeetsMinAdaV1 # pcoinsPerUtxoByte # pfromData poutputCommitment'totalLength # pfromData poutputCommitment'lovelace)

minAdaStep04Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaStep04Validator = plam $ \step05ScriptHash computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep04Args{pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'predecessorNonMembership} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep04Args'inputIndex)
        (pfromData pstep04Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep04State{pstep04State'outRefKey, pstep04State'prevUtxosRoot} <-
            pmatch $ pexpectStateAs @PStep04State inputState
          terminalState <- plet $ pcon PPredicateAndCulpabilityAuthenticated
          pexpecting (outputScriptHash #== step05ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata terminalState)) $
              pexpecting
                ( pmatch (pfromData pstep04Args'predecessorNonMembership) $ \case
                    PRedeemerCarriedNonMembership{} ->
                      pverifyNonMembershipCarried
                        (pfromData pstep04Args'predecessorNonMembership)
                        (pfromData pstep04State'prevUtxosRoot)
                        (pfromData pstep04State'outRefKey)
                        (pfromData ptxInfo'referenceInputs)
                        (pto $ pto $ pfromData ptxInfo'redeemers)
                    PPublishedChunkNonMembership published ->
                      ppublishedChunkNonMembershipFourChunks
                        # pfromData ptxInfo'referenceInputs
                        # pfromData published
                        # pfromData pstep04State'prevUtxosRoot
                        # pfromData pstep04State'outRefKey
                )
                (pconstant True)

minAdaStep05Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
minAdaStep05Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep05Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep05Args{pstep05Args'inputIndex, pstep05Args'outputIndex, pstep05Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudProofPolicy
        fraudProofAddress
        (pexpectDatum datum)
        (pfromData pstep05Args'inputIndex)
        (pfromData pstep05Args'outputIndex)
        (pfromData pstep05Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash _threadName _prover inputState ->
          pexpecting
            (pmatch inputState $ \case PDNothing -> pconstant False; PDJust _ -> pconstant True)
            (pconstant True)

pledgerOutrefKey :: forall s. Term s (PTxOutRef :--> PByteString)
pledgerOutrefKey = phoistAcyclic $ plam $ \outRef -> P.do
  PTxOutRef{ptxOutRef'id, ptxOutRef'idx} <- pmatch outRef
  pencodeMidgardTxInput
    # pcon (PMidgardTxInput (pdata $ pto $ pfromData ptxOutRef'id) ptxOutRef'idx)

ppassForcedClaim ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PForcedSource ->
  Term s (PMaybeData PNativeTxInclusionCarriage) ->
  Term s (PMaybeData PPostUtxoMembershipV1) ->
  Term s PMinAdaFaultV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
ppassForcedClaim nextHash threadPolicy datum source inclusion post fault ownRef txInfo = P.do
  PDNothing <- pmatch inclusion
  PDNothing <- pmatch post
  PMinAdaTx claimedIndex <- pmatch fault
  PForcedSource inputIndex outputIndex header membership direction <- pmatch source
  PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
  pcontinue
    threadPolicy
    (pexpectDatum datum)
    (pfromData inputIndex)
    (pfromData outputIndex)
    ownRef
    (pfromData ptxInfo'inputs)
    (pfromData ptxInfo'outputs)
    $ \_ name _ prior outputHash outputState ->
      pexpecting (pstateIsAbsent prior) $ P.do
        subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData name) # pfromData header # pfromData membership # pfromData direction
        PVerdictSubject{psubject'direction, psubject'transactionId} <- pmatch subject
        POutputBelowMinAda index <- pmatch $ Subject.prejectionReasonOf # subject
        pexpecting (pfromData psubject'direction #== 1 #&& pfromData index #>= 0 #&& index #== claimedIndex) $ P.do
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'source} <- pmatch $ punsafeCoerce @PForcedInclusionTxV1 prootMembership'value
          PNativeTxProofSourceV1{pnativeSource'compactCbor} <- pmatch $ pfromData pforcedTx'source
          PVerifiedMidgardNativeTxCompact{pverified'txId} <- pmatch $ pverifyNativeTxCompactCborV1 # pfromData psubject'transactionId # pfromData pnativeSource'compactCbor
          expected <- plet $ pcon $ PStep02State (pdata $ pconstant "") (pdata $ pconstant False) (pdata $ pconstant "") psubject'direction (pdata pverified'txId) (pdata fault) (pcon PDNothing)
          outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

-- Both rewarding arms select the unique dispatcher spend. The yield redeemer
-- and rewarding credential are unused by the target; the spending validator
-- authenticates the arm's role NFT and zero withdrawal.
pwithMinAdaYield ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s PScriptContext ->
  (Term s PStep02State -> Term s PStep02Args -> Term s PTxInfo -> Term s PTxOut -> Term s PData -> Term s PBool) ->
  Term s PUnit
pwithMinAdaYield dispatcher ctx k = P.do
  PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  PRewardingScript _ <- pmatch pscriptContext'scriptInfo
  PTxInfo{ptxInfo'inputs, ptxInfo'redeemers, ptxInfo'outputs} <- pmatch pscriptContext'txInfo
  matching <-
    plet $
      pfilter
        # ( plam $ \input -> P.do
              PTxInInfo{ptxInInfo'resolved} <- pmatch $ pfromData input
              PTxOut{ptxOut'address} <- pmatch ptxInInfo'resolved
              PAddress{paddress'credential} <- pmatch ptxOut'address
              paddress'credential #== pcon (PScriptCredential dispatcher)
          )
        # pfromData ptxInfo'inputs
  PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch $ pfromData $ pheadSingleton # matching
  PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
  POutputDatum inline <- pmatch ptxOut'datum
  PStepDatum{pstep'data = inputState} <- pmatch $ punsafeCoerce @PStepDatum $ pto inline
  PBuiltinPair _ redeemer <- pmatch $ pheadSingleton # (pfilter # (plam $ \pair -> pfromData (pfstBuiltin # pair) #== pcon (PSpending ptxInInfo'outRef)) # (pto $ pto $ pfromData ptxInfo'redeemers))
  PContinue payload <- pmatch $ punsafeCoerce @PStepRedeemer $ pto $ pfromData redeemer
  args <- plet $ pfromData $ punsafeCoerce @(PAsData PStep02Args) payload
  PStep02Args{pstep02Args'outputIndex} <- pmatch args
  nextOutput <- plet $ pfromData $ pelemAt # pfromData pstep02Args'outputIndex # pfromData ptxInfo'outputs
  PTxOut{ptxOut'datum = nextDatum} <- pmatch nextOutput
  POutputDatum nextInline <- pmatch nextDatum
  PStepDatum{pstep'data = nextState} <- pmatch $ punsafeCoerce @PStepDatum $ pto nextInline
  PDJust nextStateData <- pmatch nextState
  pif (k (pexpectStateAs @PStep02State inputState) args pscriptContext'txInfo nextOutput (pfromData nextStateData)) (pconstant ()) perror

minAdaTxYieldValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaTxYieldValidator = plam $ \dispatcher certificatePolicy ctx ->
  pwithMinAdaYield dispatcher ctx $ \state args txInfo nextOutput outputState -> P.do
    st@PStep02State{pstep02State'grammarCheckpointHash, pstep02State'grammarComplete, pstep02State'walkCheckpointHash, pstep02State'direction, pstep02State'badTxId, pstep02State'fault} <- pmatch state
    PStep02Args{pstep02Args'outputsOpening, pstep02Args'postMembership, pstep02Args'grammarCheckpointBytes, pstep02Args'walkCheckpointBytes} <- pmatch args
    PDJust openingD <- pmatch pstep02Args'outputsOpening
    PDNothing <- pmatch pstep02Args'postMembership
    PMinAdaTx index <- pmatch $ pfromData pstep02State'fault
    PTxInfo{ptxInfo'referenceInputs} <- pmatch txInfo
    PTxOut{ptxOut'address} <- pmatch nextOutput
    PAddress{paddress'credential} <- pmatch ptxOut'address
    let opening = pfromData openingD
        anchor = pcon $ PBodyAnchor pstep02State'badTxId
        refs = pfromData ptxInfo'referenceInputs
        sameScript = paddress'credential #== pcon (PScriptCredential dispatcher)
    pif
      (pnot # pfromData pstep02State'grammarComplete)
      ( pexpecting (pfromData pstep02State'walkCheckpointHash #== pconstant "") $ P.do
          PPair view prior <-
            pmatch $
              pif
                (pfromData pstep02State'grammarCheckpointHash #== pconstant "")
                ( pexpecting (pfromData pstep02Args'grammarCheckpointBytes #== pconstant "") $
                    popenedFieldGrammarCertification # opening # anchor # poutputsFieldIndex # refs # certificatePolicy
                )
                (presumeOpenedFieldGrammarCertification # opening # anchor # poutputsFieldIndex # pfromData pstep02State'grammarCheckpointHash # pfromData pstep02Args'grammarCheckpointBytes # refs # certificatePolicy)
          next <- plet $ Walk.pcertifyFieldGrammar # view # prior # 32
          let expected = pcon st{pstep02State'grammarCheckpointHash = pdata $ Walk.pfieldGrammarCheckpointHash # next, pstep02State'grammarComplete = pdata $ Walk.pfieldGrammarIsComplete # next}
          sameScript #&& outputState #== pforgetData (pdata expected)
      )
      ( P.do
          PPair view prior <-
            pmatch $
              pif
                (pfromData pstep02State'walkCheckpointHash #== pconstant "")
                ( pexpecting (pfromData pstep02Args'walkCheckpointBytes #== pconstant "") $
                    popenedCertifiedFieldWalkFromGrammar # opening # anchor # poutputsFieldIndex # pfromData pstep02State'grammarCheckpointHash # pfromData pstep02Args'grammarCheckpointBytes # refs # certificatePolicy
                )
                (presumeOpenedFieldWalk # opening # anchor # poutputsFieldIndex # pfromData pstep02State'walkCheckpointHash # pfromData pstep02Args'walkCheckpointBytes # refs # certificatePolicy)
          remaining <- plet $ pfromData index - Walk.pwalkNextItemIndex # prior
          pexpecting (remaining #>= 0) $
            pif
              (remaining #>= 32)
              ( P.do
                  next <- plet $ Walk.pwalkSkip # view # prior # 32
                  let expected = pcon st{pstep02State'walkCheckpointHash = pdata $ Walk.pfieldWalkCheckpointHash # next}
                  sameScript #&& outputState #== pforgetData (pdata expected)
              )
              ( P.do
                  PPair output _ <- pmatch $ Walk.pwalkNext # view # (Walk.pwalkSkip # view # prior # remaining)
                  subject <- plet $ pcon $ PVerdictSubject (pdata 1) (pdata 0) (pdata 0) pstep02State'badTxId (pdata $ pconstant "") (pdata $ pcon PDNothing)
                  scan <- plet $ OutputScan.pinitialScan # pcon (OutputScan.PBoundOutput (pdata subject) index) # output
                  outputState #== pforgetData (pdata $ pcon $ PMinAdaTxScan pstep02State'direction (pdata scan))
              )
      )

minAdaUtxoYieldValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
minAdaUtxoYieldValidator = plam $ \dispatcher ctx ->
  pwithMinAdaYield dispatcher ctx $ \state args txInfo _ outputState -> P.do
    PStep02Args{pstep02Args'outputsOpening, pstep02Args'postMembership} <- pmatch args
    PDNothing <- pmatch pstep02Args'outputsOpening
    PDJust membershipD <- pmatch pstep02Args'postMembership
    PStep02State{pstep02State'fault, pstep02State'postUtxo} <- pmatch state
    PMinAdaUtxo <- pmatch $ pfromData pstep02State'fault
    PDJust postD <- pmatch pstep02State'postUtxo
    PPostUtxoStateV1{ppostState'outRef, ppostState'descriptorCbor, ppostState'postUtxosRoot, ppostState'prevUtxosRoot} <- pmatch $ pfromData postD
    PLedgerOutputCommitmentV1{poutputCommitment'outputIndex} <- pmatch $ pdecodeLedgerOutputCommitment # pfromData ppostState'descriptorCbor
    PTxOutRef{ptxOutRef'idx} <- pmatch $ pfromData ppostState'outRef
    PTxInfo{ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch txInfo
    key <- plet $ pledgerOutrefKey # pfromData ppostState'outRef
    expected <- plet $ pcon $ PMinAdaUtxoDescriptor ppostState'descriptorCbor (pdata key) ppostState'prevUtxosRoot
    pfromData poutputCommitment'outputIndex
      #== pfromData ptxOutRef'idx
      #&& outputState
      #== pforgetData (pdata expected)
      #&& pmatch
        (pfromData membershipD)
        ( \case
            PRedeemerCarriedMembership{} -> pverifyMembershipCarried (pfromData membershipD) (pfromData ppostState'postUtxosRoot) key (pfromData ppostState'descriptorCbor) (pfromData ptxInfo'referenceInputs) (pto $ pto $ pfromData ptxInfo'redeemers)
            PPublishedChunkMembership published -> ppublishedChunkMembershipFourChunks # pfromData ptxInfo'referenceInputs # pfromData published # pfromData ppostState'postUtxosRoot # key # pfromData ppostState'descriptorCbor
        )
