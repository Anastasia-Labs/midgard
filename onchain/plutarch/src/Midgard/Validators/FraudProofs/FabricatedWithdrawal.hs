module Midgard.Validators.FraudProofs.FabricatedWithdrawal (
  fabricatedWithdrawalStep01Validator,
  fabricatedWithdrawalStep02Validator,
  fabricatedWithdrawalStep03Validator,
  fabricatedWithdrawalStep04Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.FabricatedWithdrawal
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

fabricatedWithdrawalStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
fabricatedWithdrawalStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args
        { pstep01Args'inputIndex
        , pstep01Args'outputIndex
        , pstep01Args'hubRefInputIndex
        , pstep01Args'stateQueueNodeRefInputIndex
        , pstep01Args'committedWithdrawal
        } <-
        pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      membership@PRootMembershipProof {prootMembership'key} <- pmatch $ pfromData pstep01Args'committedWithdrawal
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep01Args'inputIndex)
        (pfromData pstep01Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash threadName _prover inputState outputScriptHash outputStateData ->
          pverifyChallengedHeaderV1
            threadName
            hubOracle
            (pfromData pstep01Args'hubRefInputIndex)
            (pfromData pstep01Args'stateQueueNodeRefInputIndex)
            (pfromData ptxInfo'referenceInputs)
            $ \header headerHash -> P.do
              PHeaderV1 {pheader'startTime, pheader'endTime} <- pmatch $ pfromData header
              expected <-
                plet $
                  pcon $
                    PStep02State
                      (pdata headerHash)
                      pheader'startTime
                      pheader'endTime
                      (punsafeCoerce prootMembership'key)
                      (pdata $ pcommittedWithdrawalInfoHashV1 $ pcon membership)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (pverifyCommittedWithdrawalMembershipV1 header $ pcon membership) $
                  pexpecting (outputScriptHash #== step02ScriptHash) $
                    pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

fabricatedWithdrawalStep02Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
fabricatedWithdrawalStep02Validator = plam $ \step03ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args {pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'evidence} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep02State
            { pstep02State'challengedHeaderHash
            , pstep02State'headerStartTime
            , pstep02State'headerEndTime
            , pstep02State'committedWithdrawalId
            , pstep02State'committedWithdrawalInfoHash
            } <-
            pmatch $ pexpectStateAs @PStep02State inputState
          verdict <-
            plet $
              pverifyWithdrawalEvidenceV1
                hubOracle
                pstep02State'committedWithdrawalId
                (pfromData pstep02Args'evidence)
                (pfromData ptxInfo'referenceInputs)
          expected <-
            plet $
              pcon $
                PStep03State
                  pstep02State'challengedHeaderHash
                  pstep02State'headerStartTime
                  pstep02State'headerEndTime
                  pstep02State'committedWithdrawalId
                  pstep02State'committedWithdrawalInfoHash
                  (pdata verdict)
          pexpecting (outputScriptHash #== step03ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

fabricatedWithdrawalStep03Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
fabricatedWithdrawalStep03Validator = plam $ \step04ScriptHash computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args {pstep03Args'inputIndex, pstep03Args'outputIndex, pstep03Args'authenticContent} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex)
        (pfromData pstep03Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          state@PStep03State
            { pstep03State'challengedHeaderHash
            , pstep03State'headerStartTime
            , pstep03State'headerEndTime
            , pstep03State'committedWithdrawalId
            } <-
            pmatch $ pexpectStateAs @PStep03State inputState
          fault <- plet $ popenAuthenticWithdrawalContentV1 (pcon state) (pfromData pstep03Args'authenticContent)
          expected <-
            plet $
              pcon $
                PStep04State
                  pstep03State'challengedHeaderHash
                  pstep03State'headerStartTime
                  pstep03State'headerEndTime
                  pstep03State'committedWithdrawalId
                  (pdata fault)
          pexpecting (outputScriptHash #== step04ScriptHash) $
            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

fabricatedWithdrawalStep04Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
fabricatedWithdrawalStep04Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep04Args
        { pstep04Args'inputIndex
        , pstep04Args'outputIndex
        , pstep04Args'fraudProofMintRedeemerIndex
        } <-
        pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudProofPolicy
        fraudProofAddress
        (pexpectDatum datum)
        (pfromData pstep04Args'inputIndex)
        (pfromData pstep04Args'outputIndex)
        (pfromData pstep04Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash threadName _prover inputState -> P.do
          state@PStep04State {pstep04State'challengedHeaderHash} <-
            pmatch $ pexpectStateAs @PStep04State inputState
          pexpecting (pfraudCategoryIsFabricatedWithdrawalV1 # threadName)
            $ pexpecting
              (pfromData pstep04State'challengedHeaderHash #== pchallengedHeaderHashOfV1 # threadName)
            $ pfabricatedWithdrawalFaultIsEstablishedV1 (pcon state)
