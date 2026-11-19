module Midgard.Validators.FraudProofs.MintDeclaredAssetLimit (
  mintDeclaredAssetLimitStep01Validator,
  mintDeclaredAssetLimitStep02Validator,
  mintDeclaredAssetLimitStep03Validator,
  mintDeclaredAssetLimitStep04Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (PBodyAnchor),
  popenedCertifiedFieldWalkFromGrammar,
  popenedFieldGrammarCertification,
  popenedFieldWalk,
  presumeOpenedFieldGrammarCertification,
  presumeOpenedFieldWalk,
 )
import Midgard.FraudProofs.MintDeclaredAssetLimit
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldCountRequiresCertification, pfieldItemAt)
import Midgard.NativeTxMachineWalk (
  PFieldWalkCheckpointV1,
  pcertifyFieldGrammar,
  pfieldGrammarCheckpointHash,
  pfieldGrammarIsComplete,
  pfieldWalkCheckpointHash,
  pwalkIsComplete,
  pwalkNext,
  pwalkNextItemIndex,
 )
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

mintDeclaredAssetLimitStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mintDeclaredAssetLimitStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState _ _ verified ->
          let expected = pcon $ PBound $ pdata $ pbindPolicyV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'policyIndex
           in outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState ->
          let subject = Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
              expected = pcon $ PBound $ pdata $ pbindPolicyV1 # subject # pfromData pstep01Args'policyIndex
           in outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

mintDeclaredAssetLimitStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintDeclaredAssetLimitStep02Validator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02ActionV1 threadPolicy datum redeemer ownRef tx $ \action -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pactionInputIndex action) (pactionOutputIndex action) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState ->
      pmatch action $ \case
        PAuthenticateDirect _ _ opening ->
          pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
            PBound boundData -> pmatch (pfromData boundData) $ \bound@PBoundPolicyV1{pboundPolicy'subject, pboundPolicy'policyIndex} ->
              pmatch (pfromData pboundPolicy'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
                pmatch (popenedFieldWalk # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy) $ \(PPair view start) ->
                  let expected = pinitialFoldV1 # pcon bound # (pfieldItemAt # view # pfromData pboundPolicy'policyIndex) # (pfieldWalkCheckpointHash # start)
                   in pnot # (pfieldCountRequiresCertification # view) #&& outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)
            _ -> perror
        PStartGrammar _ _ opening itemBudget ->
          pexpecting (pvalidPolicyBudget $ pfromData itemBudget) $ pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
            PBound boundData -> pmatch (pfromData boundData) $ \bound@PBoundPolicyV1{pboundPolicy'subject} -> pmatch (pfromData pboundPolicy'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
              pmatch (popenedFieldGrammarCertification # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy) $ \(PPair view start) ->
                let next = pcertifyFieldGrammar # view # start # pfromData itemBudget
                    expected = pcon $ PGrammar (pdata $ pcon bound) (pdata $ pfieldGrammarCheckpointHash # next)
                 in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
            _ -> perror
        PResumeGrammar _ _ opening checkpointBytes itemBudget ->
          pexpecting (pvalidPolicyBudget $ pfromData itemBudget) $ pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
            PGrammar boundData checkpointHash -> pmatch (pfromData boundData) $ \bound@PBoundPolicyV1{pboundPolicy'subject} -> pmatch (pfromData pboundPolicy'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
              pmatch (presumeOpenedFieldGrammarCertification # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData checkpointHash # pfromData checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy) $ \(PPair view resumed) ->
                pif
                  (pnot #$ pfieldGrammarIsComplete # resumed)
                  ( let next = pcertifyFieldGrammar # view # resumed # pfromData itemBudget
                        expected = pcon $ PGrammar (pdata $ pcon bound) (pdata $ pfieldGrammarCheckpointHash # next)
                     in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
                  )
                  perror
            _ -> perror
        PFinishGrammar _ _ opening checkpointBytes ->
          pmatch (pexpectStateAs @PAuthenticationStateV1 inputState) $ \case
            PGrammar boundData checkpointHash -> pmatch (pfromData boundData) $ \bound@PBoundPolicyV1{pboundPolicy'subject, pboundPolicy'policyIndex} -> pmatch (pfromData pboundPolicy'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
              pmatch (popenedCertifiedFieldWalkFromGrammar # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData checkpointHash # pfromData checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy) $ \(PPair view start) ->
                let expected = pinitialFoldV1 # pcon bound # (pfieldItemAt # view # pfromData pboundPolicy'policyIndex) # (pfieldWalkCheckpointHash # start)
                 in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)
            _ -> perror

mintDeclaredAssetLimitStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintDeclaredAssetLimitStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pexpecting (pfromData pstep03Args'budget #> 0 #&& pfromData pstep03Args'budget #<= pstagedFoldBudget) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState ->
        let state = pexpectStateAs @PFoldStateV1 inputState
         in pmatch state $ \PFoldStateV1{pfoldState'subject, pfoldState'checkpointHash, pfoldState'outcome} -> pmatch (pfromData pfoldState'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
              pexpecting (pfromData pfoldState'outcome #== poutcomeScanning) $
                pmatch (presumeOpenedFieldWalk # pfromData pstep03Args'opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData pfoldState'checkpointHash # pfromData pstep03Args'checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy) $ \(PPair view resumed) ->
                  pmatch (padvance # view # resumed # state # pfromData pstep03Args'budget) $ \(PPair folded next) -> pmatch folded $ \f@PFoldStateV1{pfoldState'outcome = foldedOutcome} ->
                    pif
                      (pfromData foldedOutcome #== poutcomeScanning)
                      ( pif
                          (folded #/= state #&& pnot # (pwalkIsComplete # next))
                          ( let expected = pcon f{pfoldState'checkpointHash = pdata $ pfieldWalkCheckpointHash # next}
                             in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
                          )
                          perror
                      )
                      ( let expected = pdecisionV1 # folded
                         in outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)
                      )

mintDeclaredAssetLimitStep04Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintDeclaredAssetLimitStep04Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) (pfromData pstep04Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> pterminalContradictionV1 # pexpectStateAs @PDecisionStateV1 inputState

pactionInputIndex :: forall s. Term s PStep02ActionV1 -> Term s PInteger
pactionInputIndex action = pmatch action $ \case
  PAuthenticateDirect value _ _ -> pfromData value
  PStartGrammar value _ _ _ -> pfromData value
  PResumeGrammar value _ _ _ _ -> pfromData value
  PFinishGrammar value _ _ _ -> pfromData value

pactionOutputIndex :: forall s. Term s PStep02ActionV1 -> Term s PInteger
pactionOutputIndex action = pmatch action $ \case
  PAuthenticateDirect _ value _ -> pfromData value
  PStartGrammar _ value _ _ -> pfromData value
  PResumeGrammar _ value _ _ _ -> pfromData value
  PFinishGrammar _ value _ _ -> pfromData value

pvalidPolicyBudget :: forall s. Term s PInteger -> Term s PBool
pvalidPolicyBudget value = value #> 0 #&& value #<= pstagedPolicyBudget

padvance :: forall s. Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PFoldStateV1 :--> PInteger :--> PPair PFoldStateV1 PFieldWalkCheckpointV1)
padvance = phoistAcyclic $ pfix $ \self -> plam $ \view checkpoint state budget -> pmatch state $ \PFoldStateV1{pfoldState'activePolicy, pfoldState'outcome} ->
  pif
    (pfromData pfoldState'outcome #/= poutcomeScanning)
    (pcon $ PPair state checkpoint)
    ( pif
        (pfromData pfoldState'activePolicy #== pconstant "" #&& budget #< pfoldPolicyCost)
        (pcon $ PPair state checkpoint)
        ( pmatch (pwalkNext # view # checkpoint) $ \(PPair item next) ->
            let itemIndex = pwalkNextItemIndex # checkpoint
                openedAndBudget = pif (pfromData pfoldState'activePolicy #== pconstant "") (pcon $ PPair (pbeginPolicyV1 # state # itemIndex # item) (budget - pfoldPolicyCost)) (pcon $ PPair state budget)
             in pmatch openedAndBudget $ \(PPair opened remaining) -> pmatch opened $ \PFoldStateV1{pfoldState'outcome = openedOutcome} ->
                  pif
                    (pfromData openedOutcome #/= poutcomeScanning)
                    (pcon $ PPair opened checkpoint)
                    ( pmatch (pconsumeAssetsV1 # opened # itemIndex # item # remaining) $ \(PPair consumed left) -> pmatch consumed $ \PFoldStateV1{pfoldState'activePolicy = activePolicy} ->
                        pif
                          (pfromData activePolicy #== pconstant "")
                          (self # view # next # consumed # left)
                          (pcon $ PPair consumed checkpoint)
                    )
        )
    )
