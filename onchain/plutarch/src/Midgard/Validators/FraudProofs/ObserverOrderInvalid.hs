module Midgard.Validators.FraudProofs.ObserverOrderInvalid (
  observerOrderInvalidStep01Validator,
  observerOrderInvalidStep02Validator,
  observerOrderInvalidStep03Validator,
  observerOrderInvalidStep04Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (PBodyAnchor), popenedFieldWalk, presumeOpenedFieldWalk)
import Midgard.FraudProofs.ObserverOrderInvalid
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldCountRequiresCertification)
import Midgard.NativeTxMachineWalk (PFieldWalkCheckpointV1, pfieldWalkCheckpointHash, pwalkIsComplete, pwalkNext, pwalkNextItemIndex)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

observerOrderInvalidStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
observerOrderInvalidStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep01Args{pstep01Args'source, pstep01Args'observerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
      pmatch (pfromData pstep01Args'source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
            \_ _ _ _ outputHash outputState _ _ verified ->
              let expected = pcon $ PBound $ pdata $ pbindObserverV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'observerIndex
               in outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState ->
            let subject = Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
                expected = pcon $ PBound $ pdata $ pbindObserverV1 # subject # pfromData pstep01Args'observerIndex
             in outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

observerOrderInvalidStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
observerOrderInvalidStep02Validator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep02ActionV1 threadPolicy datum redeemer ownRef tx $ \action ->
      pmatch action $ \case
        PReservedAction -> perror
        PAuthenticate inputIndex outputIndex opening -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
          pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
            PBound boundD <- pmatch $ pexpectStateAs @PAuthenticationStateV1 inputState
            bound@PBoundObserverV1{pboundObserver'subject} <- pmatch $ pfromData boundD
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundObserver'subject
            PPair view start <- pmatch $ popenedFieldWalk # pfromData opening # pcon (PBodyAnchor psubject'transactionId) # pobserverFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            let expected = pinitialScanV1 # pcon bound # (pfieldWalkCheckpointHash # start)
            pnot
              # (pfieldCountRequiresCertification # view)
              #&& outputHash
              #== step03Hash
              #&& outputState
              #== pforgetData
                (pdata expected)

observerOrderInvalidStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
observerOrderInvalidStep03Validator = plam $ \step04Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep03Args{..} <- pmatch args
      let budget = pfromData pstep03Args'itemBudget
      pif
        (budget #> 0 #&& budget #<= pstagedObserverBudget)
        ( P.do
            PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
            pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
              state@PScanStateV1{pscanState'subject, pscanState'checkpointHash, pscanState'outcome} <- pmatch $ pexpectStateAs @PScanStateV1 inputState
              pif
                (pfromData pscanState'outcome #== poutcomeScanning)
                ( P.do
                    Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pscanState'subject
                    PPair view resumed <- pmatch $ presumeOpenedFieldWalk # pfromData pstep03Args'opening # pcon (PBodyAnchor psubject'transactionId) # pobserverFieldIndex # pfromData pscanState'checkpointHash # pfromData pstep03Args'checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy
                    PPair scanned next <- pmatch $ padvance # view # resumed # pcon state # budget
                    PScanStateV1{pscanState'outcome = scannedOutcome} <- pmatch scanned
                    pif
                      (pfromData scannedOutcome #== poutcomeScanning)
                      ( pnot
                          # (pwalkIsComplete # next)
                          #&& outputHash
                          #== ownHash
                          #&& outputState
                          #== pforgetData
                            (pdata $ pscanWithCheckpoint scanned $ pfieldWalkCheckpointHash # next)
                      )
                      ( outputHash
                          #== step04Hash
                          #&& outputState
                          #== pforgetData
                            (pdata $ pdecisionV1 # scanned)
                      )
                )
                perror
        )
        perror

observerOrderInvalidStep04Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
observerOrderInvalidStep04Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep04Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
      pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) (pfromData pstep04Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
        pterminalContradictionV1 # pexpectStateAs @PDecisionStateV1 inputState

padvance :: forall s. Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PScanStateV1 :--> PInteger :--> PPair PScanStateV1 PFieldWalkCheckpointV1)
padvance = phoistAcyclic $ pfix $ \self -> plam $ \view checkpoint state budget ->
  pmatch state $ \PScanStateV1{pscanState'outcome} ->
    pif
      (pnot #$ pfromData pscanState'outcome #== poutcomeScanning)
      (pcon $ PPair state checkpoint)
      ( pif
          (pwalkIsComplete # checkpoint)
          (pcon $ PPair (pexhaustScanV1 # state) checkpoint)
          ( pif
              (budget #== 0)
              (pcon $ PPair state checkpoint)
              ( let itemIndex = pwalkNextItemIndex # checkpoint
                 in pmatch (pwalkNext # view # checkpoint) $ \(PPair item next) ->
                      self # view # next # (pscanItemV1 # state # itemIndex # item) # (budget - 1)
              )
          )
      )

pscanWithCheckpoint :: forall s. Term s PScanStateV1 -> Term s PByteString -> Term s PScanStateV1
pscanWithCheckpoint state checkpointHash =
  pmatch state $ \value -> pcon value{pscanState'checkpointHash = pdata checkpointHash}
