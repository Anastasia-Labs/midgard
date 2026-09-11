module Midgard.Validators.FraudProofs.InputSetUniqueness (
  inputSetUniquenessStep01Validator,
  inputSetUniquenessStep02Validator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening
  ( PFieldOpeningV1, PNativeTxAnchorV1 (..), PNativeTxOpeningV1 (..), panchoredFieldView
  , panchoredNativeTx, popenedFieldView, preferenceInputsFieldIndex, pspendInputsFieldIndex
  )
import Midgard.FraudProofs.InputSetUniqueness
import Midgard.ComputationThread (PStepDatum)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

inputSetUniquenessStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
inputSetUniquenessStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch computationThreadPolicy datum redeemer ownOutRef txInfo $ \carriage -> P.do
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      ppassNativeTxToNextStepCarried computationThreadPolicy hubOracle datum carriage ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
          PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
          PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
          expected <- plet $ pcon $ PStep02State (pdata badTxId)
          pexpecting (pstateIsAbsent inputState) $
            pexpecting (pcompact'validityCode #== 0) $
              pexpecting (outputScriptHash #== step02ScriptHash) $
                pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

inputSetUniquenessStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
inputSetUniquenessStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PDuplicateSpendInputs inputD outputD mintD firstD secondD openingD ->
          finalizeDuplicate datum ownOutRef txInfo computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy
            inputD outputD mintD firstD secondD openingD pspendInputsFieldIndex
        PDuplicateReferenceInputs inputD outputD mintD firstD secondD openingD ->
          finalizeDuplicate datum ownOutRef txInfo computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy
            inputD outputD mintD firstD secondD openingD preferenceInputsFieldIndex
        PSpendReferenceOverlap inputD outputD mintD spendIndexD referenceIndexD compactCborD spendCarriageD referenceCarriageD -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
            (pfromData inputD) (pfromData outputD) (pfromData mintD) ownOutRef
            (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
            \_ownScriptHash _threadName _prover inputState -> P.do
              PStep02State {pstep02State'badTxId} <- pmatch $ pexpectStateAs @PStep02State inputState
              anchored <- plet $ panchoredNativeTx
                # pcon (PBodyTxOpening $ pfromData compactCborD)
                # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
              spendView <- plet $ panchoredFieldView # anchored # pspendInputsFieldIndex
                # pfromData spendCarriageD # pfromData ptxInfo'referenceInputs # certificatePolicy
              referenceView <- plet $ panchoredFieldView # anchored # preferenceInputsFieldIndex
                # pfromData referenceCarriageD # pfromData ptxInfo'referenceInputs # certificatePolicy
              pexpecting
                (pfieldItemAt # spendView # pfromData spendIndexD #== pfieldItemAt # referenceView # pfromData referenceIndexD)
                (pconstant True)

finalizeDuplicate ::
  forall s.
  Term s (PMaybeData PStepDatum) ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PInteger) ->
  Term s (PAsData PFieldOpeningV1) ->
  Term s PInteger ->
  Term s PBool
finalizeDuplicate datum ownOutRef txInfo computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy
  inputD outputD mintD firstD secondD openingD fieldIndex = P.do
    PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
    pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
      (pfromData inputD) (pfromData outputD) (pfromData mintD) ownOutRef
      (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
      \_ownScriptHash _threadName _prover inputState -> P.do
        PStep02State {pstep02State'badTxId} <- pmatch $ pexpectStateAs @PStep02State inputState
        view <- plet $ popenedFieldView # pfromData openingD
          # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId}) # fieldIndex
          # pfromData ptxInfo'referenceInputs # certificatePolicy
        pexpecting (pfromData firstD #< pfromData secondD) $
          pexpecting (pfieldItemAt # view # pfromData firstD #== pfieldItemAt # view # pfromData secondD) (pconstant True)
