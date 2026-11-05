module Midgard.Validators.FraudProofs.FieldItemWidthIllegal (
  fieldItemWidthIllegalStep01Validator,
  fieldItemWidthIllegalStep02Validator,
  fieldItemWidthIllegalStep03Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldItemWidthIllegal
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (PBodyAnchor), popenedFieldView)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

fieldItemWidthIllegalStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
fieldItemWidthIllegalStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args{pstep01Args'source, pstep01Args'fieldIndex, pstep01Args'itemIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pmatch (pfromData pstep01Args'source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried
            computationThreadPolicy
            hubOracle
            datum
            (pfromData inclusion)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ _ nextScriptHash nextState _ _ verified ->
              let bound =
                    pbindCoordinate
                      # (Subject.pbindAcceptedSubject # verified)
                      # pfromData pstep01Args'fieldIndex
                      # pfromData pstep01Args'itemIndex
               in nextScriptHash
                    #== step02ScriptHash
                    #&& nextState
                    #== pforgetData (pdata bound)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ _ nextScriptHash nextState ->
              let subject =
                    Subject.pbindForcedSubjectToThread
                      # pto (pfromData threadName)
                      # pfromData header
                      # pfromData membership
                      # pfromData direction
                  bound =
                    pbindCoordinate
                      # subject
                      # pfromData pstep01Args'fieldIndex
                      # pfromData pstep01Args'itemIndex
               in nextScriptHash
                    #== step02ScriptHash
                    #&& nextState
                    #== pforgetData (pdata bound)

fieldItemWidthIllegalStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fieldItemWidthIllegalStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args{pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'opening} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState nextScriptHash nextState -> P.do
          PBoundCoordinate{pboundCoordinate'subject, pboundCoordinate'fieldIndex, pboundCoordinate'itemIndex} <-
            pmatch $ pexpectStateAs @PBoundCoordinate inputState
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundCoordinate'subject
          view <-
            plet $
              popenedFieldView
                # pfromData pstep02Args'opening
                # pcon (PBodyAnchor psubject'transactionId)
                # pfromData pboundCoordinate'fieldIndex
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          let authenticated =
                pauthenticateItemWidth
                  # pcon
                    ( PBoundCoordinate
                        pboundCoordinate'subject
                        pboundCoordinate'fieldIndex
                        pboundCoordinate'itemIndex
                    )
                  # (pfieldItemAt # view # pfromData pboundCoordinate'itemIndex)
          nextScriptHash
            #== step03ScriptHash
            #&& nextState
            #== pforgetData (pdata authenticated)

fieldItemWidthIllegalStep03Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fieldItemWidthIllegalStep03Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args{pstep03Args'inputIndex, pstep03Args'outputIndex, pstep03Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudProofPolicy
        fraudProofAddress
        (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex)
        (pfromData pstep03Args'outputIndex)
        (pfromData pstep03Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ _ _ inputState ->
          pterminalContradiction # pexpectStateAs @PAuthenticatedWidth inputState
