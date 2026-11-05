module Midgard.Validators.FraudProofs.TransactionOutputNonCanonical (
  transactionOutputNonCanonicalStep01Validator,
  transactionOutputNonCanonicalStep02Validator,
  transactionOutputNonCanonicalStep03Validator,
  transactionOutputNonCanonicalStep04Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView)
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.TransactionOutputNonCanonical
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

transactionOutputNonCanonicalStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
transactionOutputNonCanonicalStep01Validator = plam $ \nextHash threadPolicy hub ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep01Args source outputIndex <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch txInfo
      pmatch (pfromData source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried
            threadPolicy
            hub
            datum
            (pfromData inclusion)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ _ outputHash outputState _ _ verified ->
              outputHash #== nextHash #&& outputState #== pforgetData (pdata $ pbindOutput # (Subject.pbindAcceptedSubject # verified) # pfromData outputIndex)
        PForcedSource inputIndex nextOutputIndex header membership direction ->
          pcontinue
            threadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData nextOutputIndex)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ _ outputHash outputState -> P.do
              subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
              outputHash #== nextHash #&& outputState #== pforgetData (pdata $ pbindOutput # subject # pfromData outputIndex)

transactionOutputNonCanonicalStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
transactionOutputNonCanonicalStep02Validator = plam $ \nextHash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep02Args inputIndex outputIndex opening <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState outputHash outputState -> P.do
          bound <- plet $ pexpectStateAs @PBoundOutput inputState
          PBoundOutput subject index <- pmatch bound
          PVerdictSubject{psubject'transactionId} <- pmatch $ pfromData subject
          view <- plet $ popenedFieldView # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # poutputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
          state <- plet $ pinitialScan # bound # (pfieldItemAt # view # pfromData index)
          outputHash #== nextHash #&& outputState #== pforgetData (pdata state)

transactionOutputNonCanonicalStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
transactionOutputNonCanonicalStep03Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep03Args inputIndex outputIndex window <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          next <- plet $ padvanceScan # pexpectStateAs @POutputScanState inputState # pfromData window
          POutputScanState{poutputScan'outcome} <- pmatch next
          outputHash
            #== pif (pfromData poutputScan'outcome #== poutcomeScanning) ownHash nextHash
            #&& outputState
            #== pforgetData (pdata next)

transactionOutputNonCanonicalStep04Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
transactionOutputNonCanonicalStep04Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef txInfo ->
    pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef txInfo $ \args -> P.do
      PStep04Args inputIndex outputIndex mintIndex <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
        $ \_ _ _ inputState ->
          pterminalContradiction # pexpectStateAs @POutputScanState inputState
