module Midgard.Validators.FraudProofs.ScriptIntegrityHashMissing (
  scriptIntegrityHashMissingStep01Validator,
  scriptIntegrityHashMissingStep02Validator,
  scriptIntegrityHashMissingStep03Validator,
  scriptIntegrityHashMissingScriptGrammarValidator,
  scriptIntegrityHashMissingScriptScanValidator,
  scriptIntegrityHashMissingRedeemerGrammarValidator,
  scriptIntegrityHashMissingStep04Validator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1 (..),
  PNativeTxAnchorV1 (PWitnessAnchor),
  PNativeTxOpeningV1 (PWitnessTxOpening),
  panchoredFieldView,
  panchoredNativeTx,
  popenedCertifiedFieldWalkFromGrammar,
  popenedFieldGrammarCertification,
  presumeOpenedFieldGrammarCertification,
  presumeOpenedFieldWalk,
 )
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1, pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ScriptIntegrityHashMissing
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemCount)
import Midgard.NativeTxMachineWalk (
  pcertifyFieldGrammar,
  pfieldGrammarCheckpointHash,
  pfieldGrammarIsComplete,
  pfieldWalkCheckpointHash,
  pwalkFold,
  pwalkIsComplete,
  pwalkRemaining,
 )
import Midgard.RejectionReason (PRejectionReasonV1 (PScriptIntegrityHashMissing))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

scriptIntegrityHashMissingStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
scriptIntegrityHashMissingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
      pmatch args $ \case
        PBindAccepted carriage ->
          ppassNativeTxToNextStepCarried
            threadPolicy
            hubOracle
            datum
            (pfromData carriage)
            ownRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ inputState outputHash outputState _ _ verified -> P.do
              PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
              let expected =
                    pcon $
                      PBoundAccepted
                        (pdata $ Subject.pbindAcceptedSubject # verified)
                        (pdata pcompact'witnessSetHash)
              pstateIsAbsent inputState
                #&& outputHash
                #== step02Hash
                #&& outputState
                #== pforgetData
                  (pdata expected)
        PRecordForced direction inputIndex outputIndex ->
          pexpecting (pfromData direction #== 0 #|| pfromData direction #== 1)
            $ pcontinue
              threadPolicy
              (pexpectDatum datum)
              (pfromData inputIndex)
              (pfromData outputIndex)
              ownRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
            $ \_ _ _ inputState outputHash outputState ->
              let expected = pcon $ PPendingForced direction
               in pstateIsAbsent inputState
                    #&& outputHash
                    #== step02Hash
                    #&& outputState
                    #== pforgetData
                      (pdata expected)

scriptIntegrityHashMissingStep02Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMissingStep02Validator = plam $ \step03Hash threadPolicy ctx ->
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
        $ \_ threadName _ inputState outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PBindStateV1 inputState
          bound <- plet $
            pmatch state $ \case
              PBoundAccepted subjectData witnessSetHashData ->
                pmatch (pfromData pstep02Args'forcedMembership) $ \case
                  PDNothing -> pcon $ PPair (pfromData subjectData) (pfromData witnessSetHashData)
                  PDJust _ -> perror
              PPendingForced direction ->
                pmatch (pfromData pstep02Args'forcedMembership) $ \case
                  PDNothing -> perror
                  PDJust membershipData -> P.do
                    membership <- plet $ pfromData membershipData
                    subject <-
                      plet $
                        Subject.pbindForcedSubjectToThread
                          # pto (pfromData threadName)
                          # pfromData pstep02Args'header
                          # membership
                          # pfromData direction
                    PRootMembershipProof{prootMembership'value} <- pmatch membership
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
                    PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
                    PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
                    pcon $ PPair subject pcompact'witnessSetHash
          PPair subject witnessSetHash <- pmatch bound
          Subject.PVerdictSubject{Subject.psubject'direction} <- pmatch subject
          let boundSubject =
                pif
                  (pfromData psubject'direction #== 1)
                  (plet (Subject.pbindExactRejectionReason # subject # pcon PScriptIntegrityHashMissing) $ \_ -> subject)
                  subject
              expected = pcon $ PSubjectStateV1 (pdata boundSubject) (pdata witnessSetHash)
          outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

scriptIntegrityHashMissingStep03Validator ::
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
scriptIntegrityHashMissingStep03Validator = plam $ \step04Hash scriptGrammarHash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \action -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pstep03InputIndex action)
        (pstep03OutputIndex action)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PSubjectStateV1 inputState
          PSubjectStateV1{psubjectState'subject, psubjectState'witnessSetHash} <- pmatch state
          subject <- plet $ pfromData psubjectState'subject
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
          let anchor = pcon $ PWitnessAnchor psubject'transactionId psubjectState'witnessSetHash
              refs = pfromData ptxInfo'referenceInputs
          pmatch action $ \case
            PDirect _ _ nativeTxCompactCbor witnessSet scriptWitnesses redeemers -> P.do
              anchored <-
                plet $
                  panchoredNativeTx
                    # pcon
                      ( PWitnessTxOpening
                          (pfromData nativeTxCompactCbor)
                          (pfromData witnessSet)
                      )
                    # anchor
              scriptView <-
                plet $
                  panchoredFieldView
                    # anchored
                    # 6
                    # pfromData scriptWitnesses
                    # refs
                    # certificatePolicy
              redeemerView <-
                plet $
                  panchoredFieldView
                    # anchored
                    # 8
                    # pfromData redeemers
                    # refs
                    # certificatePolicy
              verified <-
                plet $
                  pverifyNativeTxCompactCborV1
                    # pfromData psubject'transactionId
                    # pfromData nativeTxCompactCbor
              PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
              PNativeTxBodyCompact{pbodyCompact'scriptIntegrityHash} <- pmatch pcompact'body
              let expected =
                    pcon $
                      PDecisionStateV1
                        psubjectState'subject
                        (pdata pbodyCompact'scriptIntegrityHash)
                        (pdata $ pfieldViewContainsNonNativeScriptV1 # scriptView)
                        (pdata $ pfieldItemCount # redeemerView #> 0)
              pfieldItemCount
                # scriptView
                #<= pdirectFieldItemLimit
                #&& pfieldItemCount
                # redeemerView
                #<= pdirectFieldItemLimit
                #&& outputHash
                #== step04Hash
                #&& outputState
                #== pforgetData
                  (pdata expected)
            PStartStaged _ _ opening itemBudget ->
              pexpecting (pvalidBudget $ pfromData itemBudget) $ P.do
                PPair view start <-
                  pmatch $
                    popenedFieldGrammarCertification
                      # pfromData opening
                      # anchor
                      # 6
                      # refs
                      # certificatePolicy
                next <- plet $ pcertifyFieldGrammar # view # start # pfromData itemBudget
                nativeTxCompactCbor <- plet $ pwitnessOpeningBytes # pfromData opening
                verified <-
                  plet $
                    pverifyNativeTxCompactCborV1
                      # pfromData psubject'transactionId
                      # nativeTxCompactCbor
                PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
                PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
                PNativeTxBodyCompact{pbodyCompact'scriptIntegrityHash} <- pmatch pcompact'body
                let expected =
                      pcon $
                        PStagedStateV1
                          psubjectState'subject
                          psubjectState'witnessSetHash
                          (pdata pbodyCompact'scriptIntegrityHash)
                          (pdata $ pcon $ PScriptGrammar $ pdata $ pfieldGrammarCheckpointHash # next)
                outputHash #== scriptGrammarHash #&& outputState #== pforgetData (pdata expected)

scriptIntegrityHashMissingScriptGrammarValidator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMissingScriptGrammarValidator = plam $ \scriptScanHash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PScriptGrammarArgs threadPolicy datum redeemer ownRef tx $ \action -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pscriptGrammarInputIndex action)
        (pscriptGrammarOutputIndex action)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PStagedStateV1 inputState
          PStagedStateV1{pstagedState'phase} <- pmatch state
          checkpointHash <- plet $ pmatch (pfromData pstagedState'phase) $ \case
            PScriptGrammar value -> pfromData value
            _ -> perror
          let refs = pfromData ptxInfo'referenceInputs
              anchor = pstagedAnchor state
          pmatch action $ \case
            PResumeScriptGrammar _ _ opening checkpointBytes itemBudget ->
              pexpecting (pvalidBudget $ pfromData itemBudget) $ P.do
                PPair view resumed <-
                  pmatch $
                    presumeOpenedFieldGrammarCertification
                      # pfromData opening
                      # anchor
                      # 6
                      # checkpointHash
                      # pfromData checkpointBytes
                      # refs
                      # certificatePolicy
                pexpecting (pnot #$ pfieldGrammarIsComplete # resumed) $ P.do
                  next <- plet $ pcertifyFieldGrammar # view # resumed # pfromData itemBudget
                  let expected = pstateWithPhase state (pcon $ PScriptGrammar $ pdata $ pfieldGrammarCheckpointHash # next)
                  outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
            PStartScriptScan _ _ opening checkpointBytes itemBudget ->
              pexpecting (pvalidBudget $ pfromData itemBudget) $ P.do
                PPair view start <-
                  pmatch $
                    popenedCertifiedFieldWalkFromGrammar
                      # pfromData opening
                      # anchor
                      # 6
                      # checkpointHash
                      # pfromData checkpointBytes
                      # refs
                      # certificatePolicy
                PPair found next <-
                  pmatch $
                    pwalkFold @PBool
                      # view
                      # start
                      # pfromData itemBudget
                      # pconstant False
                      # pscanNonNativeScript
                let expected =
                      pstateWithPhase
                        state
                        (pcon $ PScriptScan (pdata $ pfieldWalkCheckpointHash # next) (pdata found))
                outputHash #== scriptScanHash #&& outputState #== pforgetData (pdata expected)

scriptIntegrityHashMissingScriptScanValidator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMissingScriptScanValidator = plam $ \redeemerGrammarHash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PScriptScanArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PScriptScanArgs{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      pexpecting (pvalidBudget $ pfromData pscriptScanArgs'itemBudget)
        $ pcontinue
          threadPolicy
          (pexpectDatum datum)
          (pfromData pscriptScanArgs'inputIndex)
          (pfromData pscriptScanArgs'outputIndex)
          ownRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PStagedStateV1 inputState
          PStagedStateV1{pstagedState'phase} <- pmatch state
          committed <- plet $ pmatch (pfromData pstagedState'phase) $ \case
            PScriptScan hashData foundData -> pcon $ PPair (pfromData hashData) (pfromData foundData)
            _ -> perror
          PPair checkpointHash foundBefore <- pmatch committed
          PPair view resumed <-
            pmatch $
              presumeOpenedFieldWalk
                # pfromData pscriptScanArgs'opening
                # pstagedAnchor state
                # 6
                # checkpointHash
                # pfromData pscriptScanArgs'checkpointBytes
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          PPair found next <-
            pmatch $
              pwalkFold @PBool
                # view
                # resumed
                # pfromData pscriptScanArgs'itemBudget
                # foundBefore
                # pscanNonNativeScript
          pif
            (pwalkIsComplete # next)
            ( let expected = pstateWithPhase state (pcon $ PScriptComplete $ pdata found)
               in outputHash #== redeemerGrammarHash #&& outputState #== pforgetData (pdata expected)
            )
            ( let expected = pstateWithPhase state (pcon $ PScriptScan (pdata $ pfieldWalkCheckpointHash # next) (pdata found))
               in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
            )

scriptIntegrityHashMissingRedeemerGrammarValidator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityHashMissingRedeemerGrammarValidator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PRedeemerGrammarArgs threadPolicy datum redeemer ownRef tx $ \action -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (predeemerGrammarInputIndex action)
        (predeemerGrammarOutputIndex action)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PStagedStateV1 inputState
          PStagedStateV1{..} <- pmatch state
          let refs = pfromData ptxInfo'referenceInputs
              anchor = pstagedAnchor state
              continueGrammar checkpoint found =
                let expected = pstateWithPhase state (pcon $ PRedeemerGrammar (pdata $ pfieldGrammarCheckpointHash # checkpoint) (pdata found))
                 in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
          pmatch action $ \case
            PStartRedeemerGrammar _ _ opening itemBudget ->
              pexpecting (pvalidBudget $ pfromData itemBudget) $
                pmatch (pfromData pstagedState'phase) $ \case
                  PScriptComplete foundData -> P.do
                    PPair view start <-
                      pmatch $
                        popenedFieldGrammarCertification
                          # pfromData opening
                          # anchor
                          # 8
                          # refs
                          # certificatePolicy
                    next <- plet $ pcertifyFieldGrammar # view # start # pfromData itemBudget
                    continueGrammar next (pfromData foundData)
                  _ -> perror
            PResumeRedeemerGrammar _ _ opening checkpointBytes itemBudget ->
              pexpecting (pvalidBudget $ pfromData itemBudget) $
                pmatch (pfromData pstagedState'phase) $ \case
                  PRedeemerGrammar checkpointHashData foundData -> P.do
                    PPair view resumed <-
                      pmatch $
                        presumeOpenedFieldGrammarCertification
                          # pfromData opening
                          # anchor
                          # 8
                          # pfromData checkpointHashData
                          # pfromData checkpointBytes
                          # refs
                          # certificatePolicy
                    pexpecting (pnot #$ pfieldGrammarIsComplete # resumed) $ P.do
                      next <- plet $ pcertifyFieldGrammar # view # resumed # pfromData itemBudget
                      continueGrammar next (pfromData foundData)
                  _ -> perror
            PFinishRedeemerGrammar _ _ opening checkpointBytes ->
              pmatch (pfromData pstagedState'phase) $ \case
                PRedeemerGrammar checkpointHashData foundData -> P.do
                  PPair _ start <-
                    pmatch $
                      popenedCertifiedFieldWalkFromGrammar
                        # pfromData opening
                        # anchor
                        # 8
                        # pfromData checkpointHashData
                        # pfromData checkpointBytes
                        # refs
                        # certificatePolicy
                  let expected =
                        pcon $
                          PDecisionStateV1
                            pstagedState'subject
                            pstagedState'scriptIntegrityHash
                            foundData
                            (pdata $ pwalkRemaining # start #> 0)
                  outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)
                _ -> perror

scriptIntegrityHashMissingStep04Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptIntegrityHashMissingStep04Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep04Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
      pfinalize
        threadPolicy
        fraudPolicy
        fraudAddress
        (pexpectDatum datum)
        (pfromData pstep04Args'inputIndex)
        (pfromData pstep04Args'outputIndex)
        (pfromData pstep04Args'fraudProofMintRedeemerIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ _ _ inputState -> P.do
          PDecisionStateV1{..} <- pmatch $ pexpectStateAs @PDecisionStateV1 inputState
          Subject.pterminalContradiction
            # pfromData pdecisionState'subject
            # ( pfaultHoldsV1
                  # pfromData pdecisionState'scriptIntegrityHash
                  # pfromData pdecisionState'containsNonNativeScript
                  # pfromData pdecisionState'hasRedeemers
              )

pvalidBudget :: forall s. Term s PInteger -> Term s PBool
pvalidBudget budget = budget #> 0 #&& budget #<= pstagedBatchLimit

pstep03InputIndex :: forall s. Term s PStep03Args -> Term s PInteger
pstep03InputIndex action = pmatch action $ \case
  PDirect value _ _ _ _ _ -> pfromData value
  PStartStaged value _ _ _ -> pfromData value

pstep03OutputIndex :: forall s. Term s PStep03Args -> Term s PInteger
pstep03OutputIndex action = pmatch action $ \case
  PDirect _ value _ _ _ _ -> pfromData value
  PStartStaged _ value _ _ -> pfromData value

pscriptGrammarInputIndex :: forall s. Term s PScriptGrammarArgs -> Term s PInteger
pscriptGrammarInputIndex action = pmatch action $ \case
  PResumeScriptGrammar value _ _ _ _ -> pfromData value
  PStartScriptScan value _ _ _ _ -> pfromData value

pscriptGrammarOutputIndex :: forall s. Term s PScriptGrammarArgs -> Term s PInteger
pscriptGrammarOutputIndex action = pmatch action $ \case
  PResumeScriptGrammar _ value _ _ _ -> pfromData value
  PStartScriptScan _ value _ _ _ -> pfromData value

predeemerGrammarInputIndex :: forall s. Term s PRedeemerGrammarArgs -> Term s PInteger
predeemerGrammarInputIndex action = pmatch action $ \case
  PStartRedeemerGrammar value _ _ _ -> pfromData value
  PResumeRedeemerGrammar value _ _ _ _ -> pfromData value
  PFinishRedeemerGrammar value _ _ _ -> pfromData value

predeemerGrammarOutputIndex :: forall s. Term s PRedeemerGrammarArgs -> Term s PInteger
predeemerGrammarOutputIndex action = pmatch action $ \case
  PStartRedeemerGrammar _ value _ _ -> pfromData value
  PResumeRedeemerGrammar _ value _ _ _ -> pfromData value
  PFinishRedeemerGrammar _ value _ _ -> pfromData value

pstagedAnchor :: forall s. Term s PStagedStateV1 -> Term s PNativeTxAnchorV1
pstagedAnchor state = pmatch state $ \PStagedStateV1{pstagedState'subject, pstagedState'witnessSetHash} ->
  pmatch (pfromData pstagedState'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
    pcon $ PWitnessAnchor psubject'transactionId pstagedState'witnessSetHash

pstateWithPhase :: forall s. Term s PStagedStateV1 -> Term s PStagedPhaseV1 -> Term s PStagedStateV1
pstateWithPhase state phase = pmatch state $ \s@PStagedStateV1{} -> pcon s{pstagedState'phase = pdata phase}

pscanNonNativeScript :: forall s. Term s (PBool :--> PInteger :--> PByteString :--> PBool)
pscanNonNativeScript = phoistAcyclic $ plam $ \found _ item ->
  found #|| pcontainsNonNativeScriptItemsV1 # (pcons # item # pnil)

pwitnessOpeningBytes :: forall s. Term s (PFieldOpeningV1 :--> PByteString)
pwitnessOpeningBytes = phoistAcyclic $ plam $ \opening -> pmatch opening $ \case
  PWitnessFieldOpening{pwitnessOpening'nativeTxCompactCbor} -> pfromData pwitnessOpening'nativeTxCompactCbor
  PBodyFieldOpening{} -> perror
