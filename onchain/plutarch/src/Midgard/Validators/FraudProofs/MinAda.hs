module Midgard.Validators.FraudProofs.MinAda (
  minAdaStep01Validator,
  minAdaStep02Validator,
  minAdaStep03Validator,
  minAdaStep04Validator,
  minAdaStep05Validator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
  PTxOutRef (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.FraudProof qualified as FraudProof
import Midgard.FraudProofs.Common (
  PNativeTxInclusionCarriage,
  PMembershipCarriage (..),
  PNonMembershipCarriage (..),
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
  pverifyMembershipCarried,
  pverifyNonMembershipCarried,
 )
import Midgard.FraudProofs.ChunkedInclusion (ppublishedChunkMembershipFourChunks, ppublishedChunkNonMembershipFourChunks)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  poutputsFieldIndex,
 )
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
import Midgard.LedgerOutputDescriptor (pbuildV1)
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt)
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
      PStep01Args {pstep01Args'txInclusion, pstep01Args'postUtxoMembership, pstep01Args'fault} <- pmatch args
      fault <- plet $ pfromData pstep01Args'fault
      pmatch fault $ \case
        PMinAdaTx {pminAdaTx'outputIndex} ->
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
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
      PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
      PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
      expected <- plet $ pcon $ PStep02State (pdata badTxId) (pdata fault) (pcon PDNothing)
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
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
        \PHubOracleDatum {phubOracle'stateQueue} ->
          pgetBlockDatumV1
            (pfromData ptxInfo'referenceInputs)
            phubOracle'stateQueue
            (pfromData ppostMembership'stateQueueNodeRefInputIndex)
            $ \headerD nodeKey -> P.do
              PHeaderV1 {pheader'utxosRoot, pheader'prevUtxosRoot} <- pmatch $ pfromData headerD
              descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData ppostMembership'descriptorCbor
              PLedgerOutputCommitmentV1 {poutputCommitment'outputIndex} <- pmatch descriptor
              PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <- pmatch $ pfromData ppostMembership'outRef
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
                      (pdata $ pto $ pfromData ptxOutRef'id)
                      (pdata fault)
                      (pcon $ PDJust $ pdata postState)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (outputScriptHash #== step02ScriptHash) $
                  pexpecting (nodeKey #== FraudProof.passetNameToHeaderHash # threadName) $
                    pexpecting (pfromData poutputCommitment'outputIndex #== pfromData ptxOutRef'idx) $
                      pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

minAdaStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
minAdaStep02Validator = plam $ \step03ScriptHash step05ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args {pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'outputsOpening, pstep02Args'postMembership} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep02State {pstep02State'badTxId, pstep02State'fault, pstep02State'postUtxo} <-
            pmatch $ pexpectStateAs @PStep02State inputState
          pmatch (pfromData pstep02State'fault) $ \case
            PMinAdaTx {pminAdaTx'outputIndex = badOutputIndex} ->
              pmatch pstep02Args'outputsOpening $ \case
                PDNothing -> perror
                PDJust opening ->
                  pmatch pstep02Args'postMembership $ \case
                    PDJust _ -> perror
                    PDNothing ->
                      pmatch pstep02State'postUtxo $ \case
                        PDJust _ -> perror
                        PDNothing -> P.do
                          outputsView <-
                            plet $
                              popenedFieldView
                                # pfromData opening
                                # pcon (PBodyAnchor pstep02State'badTxId)
                                # poutputsFieldIndex
                                # pfromData ptxInfo'referenceInputs
                                # certificatePolicy
                          built <- plet $ pbuildV1 # pfromData badOutputIndex # (pfieldItemAt # outputsView # pfromData badOutputIndex)
                          terminalState <- plet $ pcon PPredicateAndCulpabilityAuthenticated
                          pmatch built $ \case
                            PNothing -> perror
                            PJust descriptor ->
                              pmatch descriptor $ \PLedgerOutputCommitmentV1 {poutputCommitment'totalLength, poutputCommitment'lovelace} ->
                                pexpecting (outputScriptHash #== step05ScriptHash) $
                                  pexpecting (outputStateData #== pforgetData (pdata terminalState)) $
                                    pnot
                                      #$ poutputMeetsMinAdaV1
                                      # pcoinsPerUtxoByte
                                      # pfromData poutputCommitment'totalLength
                                      # pfromData poutputCommitment'lovelace
            PMinAdaUtxo ->
              pmatch pstep02Args'outputsOpening $ \case
                PDJust _ -> perror
                PDNothing ->
                  pmatch pstep02Args'postMembership $ \case
                    PDNothing -> perror
                    PDJust membershipD ->
                      pmatch pstep02State'postUtxo $ \case
                        PDNothing -> perror
                        PDJust postD -> P.do
                          PPostUtxoStateV1
                            { ppostState'outRef
                            , ppostState'descriptorCbor
                            , ppostState'postUtxosRoot
                            , ppostState'prevUtxosRoot
                            } <-
                            pmatch $ pfromData postD
                          key <- plet $ pledgerOutrefKey # pfromData ppostState'outRef
                          expected <-
                            plet $
                              pcon $
                                PStep03State
                                  ppostState'descriptorCbor
                                  (pdata key)
                                  ppostState'prevUtxosRoot
                          pexpecting (outputScriptHash #== step03ScriptHash) $
                            pexpecting (outputStateData #== pforgetData (pdata expected)) $
                              pexpecting
                                ( pmatch (pfromData membershipD) $ \case
                                    PRedeemerCarriedMembership {} ->
                                      pverifyMembershipCarried
                                        (pfromData membershipD)
                                        (pfromData ppostState'postUtxosRoot)
                                        key
                                        (pfromData ppostState'descriptorCbor)
                                        (pfromData ptxInfo'referenceInputs)
                                        (pto $ pto $ pfromData ptxInfo'redeemers)
                                    PPublishedChunkMembership published ->
                                      ppublishedChunkMembershipFourChunks
                                        # pfromData ptxInfo'referenceInputs
                                        # pfromData published
                                        # pfromData ppostState'postUtxosRoot
                                        # key
                                        # pfromData ppostState'descriptorCbor
                                )
                                (pconstant True)

minAdaStep03Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaStep03Validator = plam $ \step04ScriptHash computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args {pstep03Args'inputIndex, pstep03Args'outputIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex)
        (pfromData pstep03Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep03State {pstep03State'descriptorCbor, pstep03State'outRefKey, pstep03State'prevUtxosRoot} <-
            pmatch $ pexpectStateAs @PStep03State inputState
          descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData pstep03State'descriptorCbor
          PLedgerOutputCommitmentV1 {poutputCommitment'totalLength, poutputCommitment'lovelace} <- pmatch descriptor
          expected <- plet $ pcon $ PStep04State pstep03State'outRefKey pstep03State'prevUtxosRoot
          pexpecting (outputScriptHash #== step04ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) $
              pnot
                #$ poutputMeetsMinAdaV1
                # pcoinsPerUtxoByte
                # pfromData poutputCommitment'totalLength
                # pfromData poutputCommitment'lovelace

minAdaStep04Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
minAdaStep04Validator = plam $ \step05ScriptHash computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep04Args {pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'predecessorNonMembership} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep04Args'inputIndex)
        (pfromData pstep04Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep04State {pstep04State'outRefKey, pstep04State'prevUtxosRoot} <-
            pmatch $ pexpectStateAs @PStep04State inputState
          terminalState <- plet $ pcon PPredicateAndCulpabilityAuthenticated
          pexpecting (outputScriptHash #== step05ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata terminalState)) $
              pexpecting
                ( pmatch (pfromData pstep04Args'predecessorNonMembership) $ \case
                    PRedeemerCarriedNonMembership {} ->
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
      PStep05Args {pstep05Args'inputIndex, pstep05Args'outputIndex, pstep05Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
  PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <- pmatch outRef
  pencodeMidgardTxInput
    # pcon (PMidgardTxInput (pdata $ pto $ pfromData ptxOutRef'id) ptxOutRef'idx)
