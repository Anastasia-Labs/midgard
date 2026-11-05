module Midgard.Validators.FraudProofs.InputSetUniqueness (
  inputSetUniquenessStep01Validator,
  inputSetUniquenessStep02Validator,
  inputSetUniquenessStep03Validator,
  inputSetUniquenessStep04Validator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1,
  PNativeTxAnchorV1 (..),
  PNativeTxOpeningV1 (..),
  panchoredFieldView,
  panchoredNativeTx,
  popenedFieldView,
  preferenceInputsFieldIndex,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.InputSetUniqueness
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeTxFieldAccess (pfieldItemAt, pfieldItemCount, pfixedStrideItemBatch)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

inputSetUniquenessStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
inputSetUniquenessStep01Validator = plam $ \step02ScriptHash step03ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args source <- pmatch args
      pmatch (pfromData source) $ \case
        PAcceptedSource inclusion -> P.do
          carriage <- plet $ pfromData inclusion
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          ppassNativeTxToNextStepCarried
            computationThreadPolicy
            hubOracle
            datum
            carriage
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
              PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
              PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
              expected <- plet $ pcon $ PStep02State (pdata badTxId)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (pcompact'validityCode #== 0) $
                  pexpecting (outputScriptHash #== step02ScriptHash) $
                    pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
        PForcedSource inputIndex outputIndex header membership -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ inputState outputHash outputState -> P.do
              subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # 1
              bound <- plet $ pbindForcedDuplicateReason # subject
              expected <- plet $ pcon $ PStep03State (pdata bound)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (outputHash #== step03ScriptHash) $
                  pexpecting (outputState #== pforgetData (pdata expected)) (pconstant True)

inputSetUniquenessStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
inputSetUniquenessStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PDuplicateSpendInputs inputD outputD mintD firstD secondD openingD ->
          finalizeDuplicate
            datum
            ownOutRef
            txInfo
            computationThreadPolicy
            fraudProofPolicy
            fraudProofAddress
            certificatePolicy
            inputD
            outputD
            mintD
            firstD
            secondD
            openingD
            pspendInputsFieldIndex
        PDuplicateReferenceInputs inputD outputD mintD firstD secondD openingD ->
          finalizeDuplicate
            datum
            ownOutRef
            txInfo
            computationThreadPolicy
            fraudProofPolicy
            fraudProofAddress
            certificatePolicy
            inputD
            outputD
            mintD
            firstD
            secondD
            openingD
            preferenceInputsFieldIndex
        PSpendReferenceOverlap inputD outputD mintD spendIndexD referenceIndexD compactCborD spendCarriageD referenceCarriageD -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pfinalize
            computationThreadPolicy
            fraudProofPolicy
            fraudProofAddress
            (pexpectDatum datum)
            (pfromData inputD)
            (pfromData outputD)
            (pfromData mintD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ownScriptHash _threadName _prover inputState -> P.do
              PStep02State {pstep02State'badTxId} <- pmatch $ pexpectStateAs @PStep02State inputState
              anchored <-
                plet $
                  panchoredNativeTx
                    # pcon (PBodyTxOpening $ pfromData compactCborD)
                    # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
              spendView <-
                plet $
                  panchoredFieldView
                    # anchored
                    # pspendInputsFieldIndex
                    # pfromData spendCarriageD
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              referenceView <-
                plet $
                  panchoredFieldView
                    # anchored
                    # preferenceInputsFieldIndex
                    # pfromData referenceCarriageD
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
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
finalizeDuplicate
  datum
  ownOutRef
  txInfo
  computationThreadPolicy
  fraudProofPolicy
  fraudProofAddress
  certificatePolicy
  inputD
  outputD
  mintD
  firstD
  secondD
  openingD
  fieldIndex = P.do
    PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
    pfinalize
      computationThreadPolicy
      fraudProofPolicy
      fraudProofAddress
      (pexpectDatum datum)
      (pfromData inputD)
      (pfromData outputD)
      (pfromData mintD)
      ownOutRef
      (pfromData ptxInfo'inputs)
      (pfromData ptxInfo'outputs)
      (pto $ pto $ pfromData ptxInfo'redeemers)
      $ \_ownScriptHash _threadName _prover inputState -> P.do
        PStep02State {pstep02State'badTxId} <- pmatch $ pexpectStateAs @PStep02State inputState
        view <-
          plet $
            popenedFieldView
              # pfromData openingD
              # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
              # fieldIndex
              # pfromData ptxInfo'referenceInputs
              # certificatePolicy
        pexpecting (pfromData firstD #< pfromData secondD) $
          pexpecting (pfieldItemAt # view # pfromData firstD #== pfieldItemAt # view # pfromData secondD) (pconstant True)

inputSetUniquenessStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
inputSetUniquenessStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep03Args inputIndex outputIndex compactCbor spendCarriage referenceCarriage <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState outputHash outputState -> P.do
          PStep03State boundD <- pmatch $ pexpectStateAs @PStep03State inputState
          bound <- plet $ pfromData boundD
          PBoundDuplicateInput {pbound'subject} <- pmatch bound
          Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch $ pfromData pbound'subject
          anchored <-
            plet $
              panchoredNativeTx
                # pcon (PBodyTxOpening $ pfromData compactCbor)
                # pcon (PBodyAnchor psubject'transactionId)
          spendView <-
            plet $
              panchoredFieldView
                # anchored
                # pspendInputsFieldIndex
                # pfromData spendCarriage
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          referenceView <-
            plet $
              panchoredFieldView
                # anchored
                # preferenceInputsFieldIndex
                # pfromData referenceCarriage
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          expected <-
            plet $
              pinitialScanState
                # bound
                # (pfieldItemCount # spendView)
                # (pfieldItemCount # referenceView)
                # (pto $ pfromData step04Hash)
          outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

inputSetUniquenessStep04Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
inputSetUniquenessStep04Validator = plam $ \fraudPolicy fraudAddress threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef txInfo $ \args ->
      pmatch args $ \case
        PAdvance inputIndex outputIndex opening -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
          pcontinue
            threadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \ownHash _ _ inputState outputHash outputState -> P.do
              state <- plet $ pexpectStateAs @PUniqueScanState inputState
              PUniqueScanState {..} <- pmatch state
              PBoundDuplicateInput {pbound'subject} <- pmatch $ pfromData pscan'bound
              Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch $ pfromData pbound'subject
              let ownHashBytes = pto $ pfromData ownHash
                  cursor = pfromData pscan'cursor
                  spendCount = pfromData pscan'spendCount
              pexpecting (pscanStateIsAuthentic # state) $
                pexpecting (pfromData pscan'nextHash #== ownHashBytes) $ P.do
                  readingSpend <- plet $ cursor #< spendCount
                  itemIndex <- plet $ pif readingSpend cursor (cursor - spendCount)
                  expectedCount <- plet $ pif readingSpend spendCount (pfromData pscan'referenceCount)
                  view <-
                    plet $
                      popenedFieldView
                        # pfromData opening
                        # pcon (PBodyAnchor psubject'transactionId)
                        # pif readingSpend pspendInputsFieldIndex preferenceInputsFieldIndex
                        # pfromData ptxInfo'referenceInputs
                        # certificatePolicy
                  pexpecting (pfieldItemCount # view #== expectedCount) $ P.do
                    remaining <- plet $ expectedCount - itemIndex
                    batchCount <- plet $ pif (remaining #< 128) remaining 128
                    pexpecting (batchCount #> 0) $ P.do
                      expected <-
                        plet $
                          padvanceUniqueBatch
                            # state
                            # (pfixedStrideItemBatch # view # itemIndex # batchCount)
                            # ownHashBytes
                      outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
        PFinalize inputIndex outputIndex mintIndex -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pfinalize
            threadPolicy
            fraudPolicy
            fraudAddress
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            (pfromData mintIndex)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ inputState -> puniqueScanIsComplete # pexpectStateAs @PUniqueScanState inputState
