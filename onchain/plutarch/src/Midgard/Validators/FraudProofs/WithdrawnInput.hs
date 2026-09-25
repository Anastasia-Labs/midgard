module Midgard.Validators.FraudProofs.WithdrawnInput (
  withdrawnInputStep01Validator,
  withdrawnInputStep02Validator,
  withdrawnInputStep03Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView, pspendInputsFieldIndex)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..), PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.WithdrawnInput
import Midgard.LedgerState (PHeaderV1 (..), PWithdrawalBody (..), PWithdrawalInfo (..), PWithdrawalValidity (..))
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.TransitionTrace (PRootDomain (..), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

withdrawnInputStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
withdrawnInputStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch computationThreadPolicy datum redeemer ownOutRef txInfo $ \carriage -> P.do
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      ppassNativeTxToNextStepCarried computationThreadPolicy hubOracle datum carriage ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash _threadName _prover _inputState outputScriptHash outputStateData headerD badTxId badTxView -> P.do
          PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
          PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
          PHeaderV1 {pheader'withdrawalsRoot, pheader'withdrawalCount} <- pmatch $ pfromData headerD
          expected <- plet $ pcon $ PStep02State (pdata badTxId) pheader'withdrawalsRoot pheader'withdrawalCount
          pexpecting (pcompact'validityCode #== 0) $
            pexpecting (outputScriptHash #== step02ScriptHash) $
              pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

withdrawnInputStep02Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
withdrawnInputStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args {pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'spendInputsOpening, pstep02Args'badInputIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue computationThreadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex) ownOutRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $
        \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep02State {pstep02State'badTxId, pstep02State'blocksWithdrawalsRoot, pstep02State'blocksWithdrawalCount} <-
            pmatch $ pexpectStateAs @PStep02State inputState
          view <- plet $ popenedFieldView # pfromData pstep02Args'spendInputsOpening
            # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId}) # pspendInputsFieldIndex
            # pfromData ptxInfo'referenceInputs # certificatePolicy
          withdrawnInput <- plet $ pspendInputAt # view # pfromData pstep02Args'badInputIndex
          expected <- plet $ pcon $ PStep03State (pdata withdrawnInput)
            pstep02State'blocksWithdrawalsRoot pstep02State'blocksWithdrawalCount
          pexpecting (outputScriptHash #== step03ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

withdrawnInputStep03Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
withdrawnInputStep03Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args {pstep03Args'inputIndex, pstep03Args'outputIndex, pstep03Args'fraudProofMintRedeemerIndex, pstep03Args'withdrawalMembership} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      witness <- plet $ pfromData pstep03Args'withdrawalMembership
      pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex)
        (pfromData pstep03Args'fraudProofMintRedeemerIndex) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash _threadName _prover inputState -> P.do
          PStep03State {pstep03State'withdrawnInput, pstep03State'blocksWithdrawalsRoot, pstep03State'blocksWithdrawalCount} <-
            pmatch $ pexpectStateAs @PStep03State inputState
          PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <- pmatch $ pfromData pstep03State'withdrawnInput
          PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch witness
          PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalInfo) prootMembership'value
          PWithdrawalBody {pwithdrawalBody'l2Outref} <- pmatch $ pfromData pwithdrawalInfo'body
          PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PTxOutRef) pwithdrawalBody'l2Outref
          pexpecting (pmatch (pfromData pwithdrawalInfo'validity) $ \case PWithdrawalIsValid -> pconstant True; _ -> pconstant False) $
            pexpecting (pto (pfromData ptxOutRef'id) #== pfromData ptxInput'txId) $
              pexpecting (ptxOutRef'idx #== ptxInput'outputIndex) $
                pexpecting
                  (pverifyRootMembershipWithBytes witness (pdata $ pcon PWithdrawalsRootDomain)
                    (pfromData pstep03State'blocksWithdrawalsRoot) (pfromData pstep03State'blocksWithdrawalCount)
                    (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value))
                  (pconstant True)
