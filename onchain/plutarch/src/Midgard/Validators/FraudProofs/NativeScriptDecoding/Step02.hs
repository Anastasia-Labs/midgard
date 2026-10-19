module Midgard.Validators.FraudProofs.NativeScriptDecoding.Step02 (
  nativeScriptDecodingStep02Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.FraudProofs.NativeScriptDecoding.Step02 (PStep02Args (..))
import Midgard.LedgerState (PEventKey (..), PForcedInclusionTxV1 (..))
import Midgard.RejectionReason (POperatorVerdictV1 (..), PRejectionReasonV1)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

nativeScriptDecodingStep02Validator ::
  forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingStep02Validator = plam $ \step03ScriptHash computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args
        { pstep02Args'inputIndex
        , pstep02Args'outputIndex
        , pstep02Args'header
        , pstep02Args'eventToStepMembership
        , pstep02Args'transitionStepMembership
        , pstep02Args'forcedMembership
        , pstep02Args'chosenOutpointSourceKind
        , pstep02Args'chosenOutpointCursor
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash threadName _prover inputState outputScriptHash outputStateData -> P.do
          PBindStateV1
            { pbindState'direction
            , pbindState'sourceKind
            , pbindState'verifiedTxId
            } <- pmatch (pexpectStateAs @PBindStateV1 inputState)
          direction <- plet $ pfromData pbindState'direction
          sourceKind <- plet $ pfromData pbindState'sourceKind
          verifiedTxId <- plet $ pfromData pbindState'verifiedTxId
          header <- plet $ pfromData pstep02Args'header
          eventMembership <- plet $ pfromData pstep02Args'eventToStepMembership
          stepMembership <- plet $ pfromData pstep02Args'transitionStepMembership
          chosenKind <- plet $ pfromData pstep02Args'chosenOutpointSourceKind
          chosenCursor <- plet $ pfromData pstep02Args'chosenOutpointCursor
          threadNameBytes <- plet $ pto $ pfromData threadName
          scanState <- plet $
            pif (sourceKind #== psourceKindNormal)
              ( pexpecting (direction #== pdirectionWrongfulAcceptance) $
                pmatch (pfromData pstep02Args'forcedMembership) $ \case
                  PDJust _ -> perror
                  PDNothing ->
                    pexpecting (pvalidChosenPair chosenKind chosenCursor) $
                      plet (pcon $ PL2TransactionEventKey $ pdata verifiedTxId) $ \eventKey ->
                      plet (pverifyCommittedPreStateV1 # header # eventKey # eventMembership # stepMembership) $ \priorRoot ->
                        ppreBindScanStateV1
                          # direction # sourceKind # verifiedTxId # pconstant "" # pclassPending
                          # priorRoot # chosenKind # chosenCursor
              )
              ( pexpecting (sourceKind #== psourceKindForced #&& verifiedTxId #== pconstant "") $
                pmatch (pfromData pstep02Args'forcedMembership) $ \case
                  PDNothing -> perror
                  PDJust forcedD ->
                    plet (pfromData forcedD) $ \forced -> pmatch forced $ \forcedProof ->
                    plet (punsafeCoerce @(PAsData PTxOutRef) $ prootMembership'key forcedProof) $ \orderKeyD ->
                    plet (pcon $ PForcedTransactionEventKey orderKeyD) $ \eventKey ->
                    plet (pverifyCommittedPreStateV1 # header # eventKey # eventMembership # stepMembership) $ \priorRoot ->
                    plet (pverifyForcedLeafV1 # header # forced) $ \leaf -> pmatch leaf $ \f ->
                    plet (pserialiseData # prootMembership'key forcedProof) $ \orderKeyBytes ->
                      pif (direction #== pdirectionWrongfulAcceptance)
                        ( pmatch (pfromData $ pforcedTx'verdict f) $ \case
                            PForcedTxValid -> pexpecting (pvalidChosenPair chosenKind chosenCursor) $
                              ppreBindScanStateV1
                                # direction # sourceKind # pfromData (pforcedTx'txId f) # orderKeyBytes # pclassPending
                                # priorRoot # chosenKind # chosenCursor
                            PForcedTxInvalid _ -> perror
                        )
                        ( pexpecting (direction #== pdirectionWrongfulRejection) $
                          pmatch (pfromData $ pforcedTx'verdict f) $ \case
                            PForcedTxValid -> perror
                            PForcedTxInvalid reasonData ->
                              plet (pscanAccusationOfV1 # pfromData (punsafeCoerce @(PAsData PRejectionReasonV1) reasonData)) $ \accusation ->
                              pmatch accusation $ \a ->
                                ppreBindScanStateV1
                                  # direction # sourceKind # pfromData (pforcedTx'txId f) # orderKeyBytes
                                  # pfromData (pscanAccusation'scanReasonClass a) # priorRoot
                                  # pfromData (pscanAccusation'outpointSourceKind a)
                                  # pfromData (pscanAccusation'outpointCursor a)
                        )
              )
          pexpecting
            ( (pblake2b_224 #$ pserialiseData # pforgetData pstep02Args'header)
                #== psliceBS # pidByteCount # (plengthBS # threadNameBytes - pidByteCount) # threadNameBytes
            ) $
            pexpecting (outputScriptHash #== step03ScriptHash) $
              pexpecting
                (outputStateData #== pforgetData (pdata scanState))
                (pconstant True)
  where
    pvalidChosenPair kind cursor =
      (kind #== poutpointSourceSpend #|| kind #== poutpointSourceReference)
        #&& cursor #>= 0
