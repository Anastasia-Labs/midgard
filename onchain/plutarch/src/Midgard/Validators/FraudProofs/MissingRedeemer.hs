module Midgard.Validators.FraudProofs.MissingRedeemer (
  missingRedeemerStep01Validator,
  missingRedeemerStep02Validator,
  missingRedeemerStep02aValidator,
  missingRedeemerStep02bValidator,
  missingRedeemerStep03Validator,
  missingRedeemerStep04Validator,
  missingRedeemerStep05Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (PWitnessAnchor),
  popenedCertifiedFieldWalkFromGrammar,
  popenedFieldGrammarCertification,
  popenedFieldWalk,
  presumeOpenedFieldGrammarCertification,
  presumeOpenedFieldWalk,
 )
import Midgard.FraudProofs.MissingRedeemer
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldCountRequiresCertification, pfieldItemCount)
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pcertifyFieldGrammar,
  pfieldGrammarCheckpointHash,
  pfieldGrammarIsComplete,
  pfieldWalkCheckpointHash,
  pwalkIsComplete,
  pwalkNext,
  pwalkNextItemIndex,
  pwalkRemaining,
 )
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

missingRedeemerStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
missingRedeemerStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected =
                pbindPurposeV1
                  # (Subject.pbindAcceptedSubject # verified)
                  # pcompact'witnessSetHash
                  # pfromData pheader'validationTracesRoot
                  # pfromData pheader'validationTraceCount
                  # pfromData pstep01Args'purposeKind
                  # pfromData pstep01Args'purposeIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
          PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
          PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          let expected = pbindPurposeV1 # subject # pcompact'witnessSetHash # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'purposeKind # pfromData pstep01Args'purposeIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

missingRedeemerStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep02Validator = plam $ \step02aHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateDescriptorV1 # pexpectStateAs @PBoundPurposeV1 inputState # pfromData pstep02Args'traceMembership
       in outputHash #== step02aHash #&& outputState #== pforgetData (pdata expected)

missingRedeemerStep02aValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep02aValidator = plam $ \step02bHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02aArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02aArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02aArgs'inputIndex) (pfromData pstep02aArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateStageTenTraceV1 # pexpectStateAs @PAuthenticatedDescriptorV1 inputState # pfromData pstep02aArgs'machineState # pfromData pstep02aArgs'traceProof # pfromData pstep02aArgs'control
       in outputHash #== step02bHash #&& outputState #== pforgetData (pdata expected)

missingRedeemerStep02bValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep02bValidator = plam $ \step03Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02bArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02bArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02bArgs'inputIndex) (pfromData pstep02bArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected =
            pcon $
              PReady $
                pdata $
                  pauthenticateStageTenSelectionV1
                    # pexpectStateAs @PAuthenticatedStageTenV1 inputState
                    # pfromData pstep02bArgs'absolutePurposeIndex
                    # pfromData pstep02bArgs'purposeSiblings
                    # pfromData pstep02bArgs'sourceOriginKind
                    # pfromData pstep02bArgs'sourceKey
                    # pfromData pstep02bArgs'sourceLanguageTag
                    # pfromData pstep02bArgs'sourceScriptHash
                    # pfromData pstep02bArgs'sourceTotalLength
                    # pfromData pstep02bArgs'sourceItemCommitment
                    # pfromData pstep02bArgs'sourceSiblings
       in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

missingRedeemerStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03ActionV1 threadPolicy datum redeemer ownRef tx $ \action -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pactionInputIndex action) (pactionOutputIndex action) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState ->
      let refs = pfromData ptxInfo'referenceInputs
          anchor authenticated = pmatch authenticated $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'bound} -> pmatch (pfromData pauthenticatedPurpose'bound) $ \PBoundPurposeV1{pboundPurpose'subject, pboundPurpose'witnessSetHash} -> pmatch (pfromData pboundPurpose'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} -> pcon $ PWitnessAnchor psubject'transactionId pboundPurpose'witnessSetHash
          ready authenticated checkpoint count =
            let expected = pinitialScanV1 # authenticated # (pfieldWalkCheckpointHash # checkpoint) # count
             in outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)
          grammar authenticated checkpoint =
            let expected = pcon $ PGrammar (pdata authenticated) (pdata $ pfieldGrammarCheckpointHash # checkpoint)
             in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
          validBudget budget = pfromData budget #> 0 #&& pfromData budget #<= pscanBatch
       in pmatch action $ \case
            PAuthenticateDirect _ _ opening -> pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
              PReady authenticatedData -> P.do
                authenticated <- plet $ pfromData authenticatedData
                PPair view start <- pmatch $ popenedFieldWalk # pfromData opening # anchor authenticated # predeemerFieldIndex # refs # certificatePolicy
                pexpecting (pnot # (pfieldCountRequiresCertification # view)) $ ready authenticated start (pfieldItemCount # view)
              _ -> perror
            PStartGrammar _ _ opening itemBudget -> pexpecting (validBudget itemBudget) $ pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
              PReady authenticatedData -> P.do
                authenticated <- plet $ pfromData authenticatedData
                PPair view start <- pmatch $ popenedFieldGrammarCertification # pfromData opening # anchor authenticated # predeemerFieldIndex # refs # certificatePolicy
                next <- plet $ pcertifyFieldGrammar # view # start # pfromData itemBudget
                grammar authenticated next
              _ -> perror
            PResumeGrammar _ _ opening checkpointBytes itemBudget -> pexpecting (validBudget itemBudget) $ pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
              PGrammar authenticatedData checkpointHash -> P.do
                authenticated <- plet $ pfromData authenticatedData
                PPair view resumed <- pmatch $ presumeOpenedFieldGrammarCertification # pfromData opening # anchor authenticated # predeemerFieldIndex # pfromData checkpointHash # pfromData checkpointBytes # refs # certificatePolicy
                pexpecting (pnot # (pfieldGrammarIsComplete # resumed)) $ P.do
                  next <- plet $ pcertifyFieldGrammar # view # resumed # pfromData itemBudget
                  grammar authenticated next
              _ -> perror
            PFinishGrammar _ _ opening checkpointBytes -> pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
              PGrammar authenticatedData checkpointHash -> P.do
                authenticated <- plet $ pfromData authenticatedData
                PPair _ start <- pmatch $ popenedCertifiedFieldWalkFromGrammar # pfromData opening # anchor authenticated # predeemerFieldIndex # pfromData checkpointHash # pfromData checkpointBytes # refs # certificatePolicy
                ready authenticated start (pwalkRemaining # start)
              _ -> perror

missingRedeemerStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep04Validator = plam $ \step05Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pexpecting (pfromData pstep04Args'itemBudget #> 0 #&& pfromData pstep04Args'itemBudget #<= pscanBatch) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @PScanStateV1 inputState
        PScanStateV1{pscanState'authenticated, pscanState'checkpointHash} <- pmatch state
        authenticated <- plet $ pfromData pscanState'authenticated
        PPair view resumed <- pmatch $ presumeOpenedFieldWalk # pfromData pstep04Args'opening # pscanAnchor authenticated # predeemerFieldIndex # pfromData pscanState'checkpointHash # pfromData pstep04Args'checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy
        PPair scanned next <- pmatch $ padvance # view # resumed # state # pfromData pstep04Args'itemBudget
        PScanStateV1{pscanState'found} <- pmatch scanned
        pif
          (pfromData pscanState'found #|| pwalkIsComplete # next)
          (let expected = pdecisionV1 # scanned in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected))
          ( let expected = pmatch scanned $ \s@PScanStateV1{} -> pcon s{pscanState'checkpointHash = pdata $ pfieldWalkCheckpointHash # next}
             in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
          )

missingRedeemerStep05Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingRedeemerStep05Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) (pfromData pstep05Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalContradictionV1 # pexpectStateAs @PDecisionStateV1 inputState

pactionInputIndex :: forall s. Term s PStep03ActionV1 -> Term s PInteger
pactionInputIndex action = pmatch action $ \case
  PAuthenticateDirect value _ _ -> pfromData value
  PStartGrammar value _ _ _ -> pfromData value
  PResumeGrammar value _ _ _ _ -> pfromData value
  PFinishGrammar value _ _ _ -> pfromData value

pactionOutputIndex :: forall s. Term s PStep03ActionV1 -> Term s PInteger
pactionOutputIndex action = pmatch action $ \case
  PAuthenticateDirect _ value _ -> pfromData value
  PStartGrammar _ value _ _ -> pfromData value
  PResumeGrammar _ value _ _ _ -> pfromData value
  PFinishGrammar _ value _ _ -> pfromData value

pscanAnchor :: forall s. Term s PAuthenticatedPurposeV1 -> Term s PNativeTxAnchorV1
pscanAnchor authenticated = pmatch authenticated $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'bound} -> pmatch (pfromData pauthenticatedPurpose'bound) $ \PBoundPurposeV1{pboundPurpose'subject, pboundPurpose'witnessSetHash} -> pmatch (pfromData pboundPurpose'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} -> pcon $ PWitnessAnchor psubject'transactionId pboundPurpose'witnessSetHash

padvance :: forall s. Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PScanStateV1 :--> PInteger :--> PPair PScanStateV1 PFieldWalkCheckpointV1)
padvance = phoistAcyclic $ pfix $ \self -> plam $ \view checkpoint state budget ->
  pmatch state $ \PScanStateV1{pscanState'found} ->
    pif
      (budget #== 0 #|| pfromData pscanState'found #|| pwalkIsComplete # checkpoint)
      (pcon $ PPair state checkpoint)
      ( pmatch (pwalkNext # view # checkpoint) $ \(PPair item next) ->
          self # view # next # (pscanItemV1 # state # (pwalkNextItemIndex # checkpoint) # item) # (budget - 1)
      )
