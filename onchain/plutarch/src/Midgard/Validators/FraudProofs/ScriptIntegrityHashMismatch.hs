module Midgard.Validators.FraudProofs.ScriptIntegrityHashMismatch (
  scriptIntegrityHashMismatchStep01Validator,
  scriptIntegrityHashMismatchStep02Validator,
  scriptIntegrityHashMismatchStep03Validator,
  scriptIntegrityHashMismatchStep04Validator,
  scriptIntegrityHashMismatchStep05Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxBodyCompact (..), PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ScriptIntegrityHashMismatch
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

scriptIntegrityHashMismatchStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
scriptIntegrityHashMismatchStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep01Args{pstep01Args'source} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
      pmatch (pfromData pstep01Args'source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried
            threadPolicy
            hubOracle
            datum
            (pfromData inclusion)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ _ outputHash outputState header _ verified -> P.do
              PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
              PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
              PNativeTxBodyCompact{pbodyCompact'scriptIntegrityHash} <- pmatch pcompact'body
              let expected =
                    pbindIntegrityV1
                      # (Subject.pbindAcceptedSubject # verified)
                      # pfromData pheader'validationTracesRoot
                      # pfromData pheader'validationTraceCount
                      # pbodyCompact'scriptIntegrityHash
              outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue
            threadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ _ outputHash outputState -> P.do
              subject <-
                plet $
                  Subject.pbindForcedSubjectToThread
                    # pto (pfromData threadName)
                    # pfromData header
                    # pfromData membership
                    # pfromData direction
              Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
              PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
              PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <-
                pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
              PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
              PPair verified _ <-
                pmatch $
                  pverifyNativeTxProofSourceV1
                    # pfromData pforcedTx'txId
                    # pfromData pnativeSource'compactCbor
                    # pfromData pnativeSource'witnessSetCompactCbor
                    # pfromData pnativeSource'fieldPreimageLengthsCbor
              PVerifiedMidgardNativeTxCompact{pverified'txId, pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
              PNativeTxBodyCompact{pbodyCompact'scriptIntegrityHash} <- pmatch pcompact'body
              PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
              let expected =
                    pbindIntegrityV1
                      # subject
                      # pfromData pheader'validationTracesRoot
                      # pfromData pheader'validationTraceCount
                      # pbodyCompact'scriptIntegrityHash
              pverified'txId
                #== pfromData psubject'transactionId
                #&& outputHash
                #== step02Hash
                #&& outputState
                #== pforgetData
                  (pdata expected)

scriptIntegrityHashMismatchStep02Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMismatchStep02Validator = plam $ \step03Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep02Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState outputHash outputState ->
          let expected =
                pauthenticateIntegrityV1
                  # pexpectStateAs @PBoundIntegrityV1 inputState
                  # pfromData pstep02Args'traceMembership
                  # pfromData pstep02Args'machineState
                  # pfromData pstep02Args'traceProof
                  # pfromData pstep02Args'control
                  # pfromData pstep02Args'redeemerWitnessHash
           in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

scriptIntegrityHashMismatchStep03Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMismatchStep03Validator = plam $ \step04Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep03Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex)
        (pfromData pstep03Args'outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState outputHash outputState ->
          let expected = pinitializeLanguageFoldV1 # pexpectStateAs @PAuthenticatedIntegrityV1 inputState
           in outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

scriptIntegrityHashMismatchStep04Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMismatchStep04Validator = plam $ \step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep04Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData pstep04Args'inputIndex)
        (pfromData pstep04Args'outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          next <- plet $ pfoldNextLanguageV1 # pexpectStateAs @PLanguageFoldV1 inputState
          PLanguageFoldV1{planguageFold'cursor} <- pmatch next
          pif
            (pfromData planguageFold'cursor #== 2)
            ( outputHash
                #== step05Hash
                #&& outputState
                #== pforgetData
                  (pdata $ pdecideIntegrityV1 # next)
            )
            (outputHash #== ownHash #&& outputState #== pforgetData (pdata next))

scriptIntegrityHashMismatchStep05Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
scriptIntegrityHashMismatchStep05Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep05Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
      pfinalize
        threadPolicy
        fraudPolicy
        fraudAddress
        (pexpectDatum datum)
        (pfromData pstep05Args'inputIndex)
        (pfromData pstep05Args'outputIndex)
        (pfromData pstep05Args'fraudProofMintRedeemerIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ _ _ inputState ->
          pterminalContradictionV1 # pexpectStateAs @PDecisionV1 inputState
