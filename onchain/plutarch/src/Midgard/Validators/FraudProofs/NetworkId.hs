module Midgard.Validators.FraudProofs.NetworkId (
  networkIdStep01Validator,
  networkIdStep02Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PRedeemer,
  PScriptContext,
  PScriptHash,
  PScriptPurpose,
  PTxInInfo,
  PTxInfo (..),
  PTxOutRef (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.FraudProof qualified as FraudProof
import Midgard.FraudProofs.Common (
  PNativeTxInclusionCarriage,
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
  pverifyMembershipCarried,
  pverifyNonMembershipCarried,
 )
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView, poutputsFieldIndex)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxOutputCbor, pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.NetworkId
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pdecodeLedgerOutputCommitment)
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

networkIdStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PInteger
        :--> PScriptContext
        :--> PUnit
    )
networkIdStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args {pstep01Args'txInclusion, pstep01Args'postUtxoMembership, pstep01Args'fault} <- pmatch args
      fault <- plet $ pfromData pstep01Args'fault
      pexpecting (pisSupportedNetworkIdV1 # expectedNetworkId) $
        pmatch fault $ \case
          PTransactionNetwork ->
            pmatch pstep01Args'txInclusion $ \case
              PDJust inclusion -> pmatch pstep01Args'postUtxoMembership $ \case
                PDNothing -> ppassTransactionClaim step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId datum (pfromData inclusion) fault ownOutRef txInfo
                PDJust _ -> perror
              PDNothing -> perror
          POutputNetwork {poutputNetwork'outputIndex} ->
            pexpecting (pfromData poutputNetwork'outputIndex #>= 0) $
              pmatch pstep01Args'txInclusion $ \case
                PDJust inclusion -> pmatch pstep01Args'postUtxoMembership $ \case
                  PDNothing -> ppassTransactionClaim step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId datum (pfromData inclusion) fault ownOutRef txInfo
                  PDJust _ -> perror
                PDNothing -> perror
          POutputNetworkUtxo {} ->
            pmatch pstep01Args'txInclusion $ \case
              PDNothing -> pmatch pstep01Args'postUtxoMembership $ \case
                PDJust membership -> ppassPostUtxoClaim step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId datum (pfromData membership) fault ownOutRef txInfo
                PDNothing -> perror
              PDJust _ -> perror

ppassTransactionClaim ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s (PMaybeData PStepDatum) ->
  Term s PNativeTxInclusionCarriage ->
  Term s PNetworkIdFaultV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
ppassTransactionClaim step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId datum inclusion fault ownOutRef txInfo = P.do
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
    (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ownHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
      PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
      PNativeTxCompact {pcompact'body, pcompact'validityCode} <- pmatch pverified'txCompact
      PNativeTxBodyCompact {pbodyCompact'networkId} <- pmatch pcompact'body
      expected <- plet $ pcon $ PStep02State
        (pdata badTxId)
        (pdata pbodyCompact'networkId)
        (pdata expectedNetworkId)
        (pdata fault)
        (pcon PDNothing)
      pexpecting (pstateIsAbsent inputState) $
        pexpecting (pcompact'validityCode #== 0) $
          pexpecting (outputScriptHash #== step02ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

ppassPostUtxoClaim ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s (PMaybeData PStepDatum) ->
  Term s PPostUtxoMembershipV1 ->
  Term s PNetworkIdFaultV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
ppassPostUtxoClaim step02ScriptHash computationThreadPolicy hubOracle expectedNetworkId datum membership fault ownOutRef txInfo = P.do
  PPostUtxoMembershipV1
    { ppostMembership'inputIndex
    , ppostMembership'outputIndex
    , ppostMembership'hubRefInputIndex
    , ppostMembership'stateQueueNodeRefInputIndex
    , ppostMembership'outRef
    , ppostMembership'descriptorCbor
    , ppostMembership'membership
    , ppostMembership'predecessor
    } <- pmatch membership
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
  pcontinue computationThreadPolicy (pexpectDatum datum)
    (pfromData ppostMembership'inputIndex) (pfromData ppostMembership'outputIndex) ownOutRef
    (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ownHash threadName _prover inputState outputScriptHash outputStateData ->
      pmatch (Hub.pgetDatum # pfromData ptxInfo'referenceInputs # hubOracle # pfromData ppostMembership'hubRefInputIndex) $ \PHubOracleDatum {phubOracle'stateQueue} ->
        pgetBlockDatumV1 (pfromData ptxInfo'referenceInputs) phubOracle'stateQueue
          (pfromData ppostMembership'stateQueueNodeRefInputIndex) $ \headerD nodeKey -> P.do
            PHeaderV1 {pheader'prevUtxosRoot, pheader'utxosRoot} <- pmatch $ pfromData headerD
            descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData ppostMembership'descriptorCbor
            PLedgerOutputCommitmentV1 {poutputCommitment'outputIndex, poutputCommitment'address} <- pmatch descriptor
            PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <- pmatch $ pfromData ppostMembership'outRef
            ledgerKey <- plet $ pencodeMidgardTxInput
              # pcon (PMidgardTxInput (pdata $ pto $ pfromData ptxOutRef'id) ptxOutRef'idx)
            observed <- plet $ pmatch fault $ \case
              POutputNetworkUtxo {poutputNetworkUtxo'observedNetworkId} -> pfromData poutputNetworkUtxo'observedNetworkId
              _ -> perror
            address <- plet $ pmatch (pdecodeCanonicalAddressBytes # pfromData poutputCommitment'address) $ \case
              PJust parsed -> parsed
              PNothing -> perror
            PMidgardAddress {paddress'networkId} <- pmatch address
            postState <- plet $ pcon $ PPostUtxoStateV1
              ppostMembership'outRef
              ppostMembership'descriptorCbor
              pheader'prevUtxosRoot
              ppostMembership'predecessor
            expected <- plet $ pcon $ PStep02State
              (pdata $ pto $ pfromData ptxOutRef'id)
              (pdata pnativeNetworkIdNone)
              (pdata expectedNetworkId)
              (pdata fault)
              (pcon $ PDJust $ pdata postState)
            pexpecting (pstateIsAbsent inputState) $
              pexpecting (outputScriptHash #== step02ScriptHash) $
                pexpecting (nodeKey #== FraudProof.passetNameToHeaderHash # threadName) $
                  pexpecting (pfromData poutputCommitment'outputIndex #== pfromData ptxOutRef'idx) $
                    pexpecting
                      ( pverifyMembershipCarried
                          (pfromData ppostMembership'membership)
                          (pfromData pheader'utxosRoot)
                          ledgerKey
                          (pfromData ppostMembership'descriptorCbor)
                          (pfromData ptxInfo'referenceInputs)
                          (pto $ pto $ pfromData ptxInfo'redeemers)
                      ) $
                        pexpecting (observed #== pfromData paddress'networkId) $
                          pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

networkIdStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
networkIdStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args
        { pstep02Args'inputIndex
        , pstep02Args'outputIndex
        , pstep02Args'fraudProofMintRedeemerIndex
        , pstep02Args'outputsOpening
        , pstep02Args'predecessorCarriage
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex)
        (pfromData pstep02Args'fraudProofMintRedeemerIndex) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ownHash _threadName _prover inputState -> P.do
          PStep02State
            { pstep02State'badTxId
            , pstep02State'committedTxNetworkId
            , pstep02State'expectedNetworkId
            , pstep02State'fault
            , pstep02State'postUtxo
            } <- pmatch $ pexpectStateAs @PStep02State inputState
          pmatch (pfromData pstep02State'fault) $ \case
            PTransactionNetwork ->
              pexpecting (pisNothing pstep02Args'outputsOpening) $
                pexpecting (pisNothing pstep02Args'predecessorCarriage) $
                  pexpecting (pisNothing pstep02State'postUtxo) $
                    pisTransactionNetworkViolationV1
                      # pfromData pstep02State'committedTxNetworkId
                      # pfromData pstep02State'expectedNetworkId
            POutputNetwork {poutputNetwork'outputIndex} ->
              pmatch pstep02Args'outputsOpening $ \case
                PDNothing -> perror
                PDJust opening ->
                  pexpecting (pisNothing pstep02Args'predecessorCarriage) $
                    pexpecting (pisNothing pstep02State'postUtxo) $ P.do
                      outputsView <- plet $
                        popenedFieldView
                          # pfromData opening
                          # pcon (PBodyAnchor pstep02State'badTxId)
                          # poutputsFieldIndex
                          # pfromData ptxInfo'referenceInputs
                          # certificatePolicy
                      badOutput <- plet $ pdecodeMidgardTxOutputCbor
                        # (pfieldItemAt # outputsView # pfromData poutputNetwork'outputIndex)
                      PMidgardTxOutput {ptxOutput'address} <- pmatch badOutput
                      PMidgardAddress {paddress'networkId} <- pmatch $ pfromData ptxOutput'address
                      pisOutputNetworkViolationV1
                        # pfromData paddress'networkId
                        # pfromData pstep02State'expectedNetworkId
            POutputNetworkUtxo {poutputNetworkUtxo'observedNetworkId} ->
              pexpecting (pisNothing pstep02Args'outputsOpening) $
                pmatch pstep02State'postUtxo $ \case
                  PDNothing -> perror
                  PDJust postD -> pmatch pstep02Args'predecessorCarriage $ \case
                    PDNothing -> perror
                    PDJust predecessorD -> P.do
                      post@PPostUtxoStateV1 {ppostState'outRef, ppostState'predecessor} <-
                        pmatch $ pfromData postD
                      key <- plet $ pnetworkLedgerOutrefKey # pfromData ppostState'outRef
                      pexpecting
                        ( pverifyPredecessor
                            (pfromData ppostState'predecessor)
                            (pfromData predecessorD)
                            (pcon post)
                            key
                            (pfromData pstep02State'expectedNetworkId)
                            (pfromData ptxInfo'referenceInputs)
                            (pto $ pto $ pfromData ptxInfo'redeemers)
                        ) $
                          pisOutputNetworkViolationV1
                            # pfromData poutputNetworkUtxo'observedNetworkId
                            # pfromData pstep02State'expectedNetworkId
  where
    pisNothing m = pmatch m $ \case PDNothing -> pconstant True; PDJust _ -> pconstant False

pnetworkLedgerOutrefKey :: forall s. Term s (PTxOutRef :--> PByteString)
pnetworkLedgerOutrefKey = phoistAcyclic $ plam $ \outRef -> P.do
  PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <- pmatch outRef
  pencodeMidgardTxInput
    # pcon (PMidgardTxInput (pdata $ pto $ pfromData ptxOutRef'id) ptxOutRef'idx)

pverifyPredecessor ::
  forall s.
  Term s PPostUtxoPredecessorClaimV1 ->
  Term s PPostUtxoPredecessorCarriageV1 ->
  Term s PPostUtxoStateV1 ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pverifyPredecessor claim carriage post key expectedNetworkId referenceInputs redeemers = P.do
  PPostUtxoStateV1 {ppostState'outRef, ppostState'descriptorCbor, ppostState'prevUtxosRoot} <- pmatch post
  pmatch claim $ \case
    PIntroduced -> pmatch carriage $ \case
      PIntroducedPredecessor nonMembership ->
        pverifyNonMembershipCarried (pfromData nonMembership) (pfromData ppostState'prevUtxosRoot) key referenceInputs redeemers
      _ -> perror
    PNetworkChanged {pnetworkChanged'previousDescriptorCbor} -> pmatch carriage $ \case
      PNetworkChangedPredecessor membership -> P.do
        previous <- plet $ pdecodeLedgerOutputCommitment # pfromData pnetworkChanged'previousDescriptorCbor
        PLedgerOutputCommitmentV1 {poutputCommitment'outputIndex, poutputCommitment'address} <- pmatch previous
        PTxOutRef {ptxOutRef'idx} <- pmatch $ pfromData ppostState'outRef
        address <- plet $ pmatch (pdecodeCanonicalAddressBytes # pfromData poutputCommitment'address) $ \case
          PJust parsed -> parsed
          PNothing -> perror
        PMidgardAddress {paddress'networkId} <- pmatch address
        (pfromData pnetworkChanged'previousDescriptorCbor #/= pfromData ppostState'descriptorCbor)
          #&& pverifyMembershipCarried
            (pfromData membership)
            (pfromData ppostState'prevUtxosRoot)
            key
            (pfromData pnetworkChanged'previousDescriptorCbor)
            referenceInputs
            redeemers
          #&& (pfromData poutputCommitment'outputIndex #== pfromData ptxOutRef'idx)
          #&& (pfromData paddress'networkId #== expectedNetworkId)
      _ -> perror
