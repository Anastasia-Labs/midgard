module Midgard.Validators.FraudProofs.FieldPreimageLengthMismatch (
  fieldPreimageLengthMismatchStep01Validator,
  fieldPreimageLengthMismatchStep02AcceptedValidator,
  fieldPreimageLengthMismatchStep02ForcedValidator,
  fieldPreimageLengthMismatchStep03Validator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (
  PNativeTxInclusionArgs (..),
  PNativeTxInclusionCarriage (..),
  PPublishedChunkInclusionArgs (..),
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
 )
import Midgard.FraudProofs.DaHashPreimage (PSourceEnvelopeV1 (..), pinspectSourceEnvelopeV1)
import Midgard.FraudProofs.FieldPreimageLengthMismatch
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

fieldPreimageLengthMismatchStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
fieldPreimageLengthMismatchStep01Validator = plam $ \acceptedStep02Hash forcedStep02Hash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pmatch args $ \case
        PBindAccepted inclusionD _claim ->
          let inclusion = pfromData inclusionD
              sourceCbor = psourceCborOf inclusion
           in ppassNativeTxToNextStepCarried
                computationThreadPolicy
                hubOracle
                datum
                inclusion
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'referenceInputs)
                (pfromData ptxInfo'outputs)
                (pto $ pto $ pfromData ptxInfo'redeemers)
                $ \_ _ _ inputState nextScriptHash nextState _ _ verified ->
                  pexpectNoState inputState $
                    nextScriptHash
                      #== acceptedStep02Hash
                      #&& nextState
                      #== pforgetData
                        ( pdata $
                            pcon $
                              PBoundSource
                                (pdata $ Subject.pbindAcceptedSubject # verified)
                                (pdata sourceCbor)
                        )
        PRecordForced directionD inputIndexD outputIndexD ->
          let direction = pfromData directionD
           in pif
                (direction #== 0 #|| direction #== 1)
                ( pcontinue
                    computationThreadPolicy
                    (pexpectDatum datum)
                    (pfromData inputIndexD)
                    (pfromData outputIndexD)
                    ownOutRef
                    (pfromData ptxInfo'inputs)
                    (pfromData ptxInfo'outputs)
                    $ \_ _ _ inputState nextScriptHash nextState ->
                      pexpectNoState inputState $
                        nextScriptHash
                          #== forcedStep02Hash
                          #&& nextState
                          #== pforgetData (pdata $ pcon $ PPendingForced directionD)
                )
                perror

fieldPreimageLengthMismatchStep02AcceptedValidator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fieldPreimageLengthMismatchStep02AcceptedValidator = plam $ \step03Hash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PAuthenticateAccepted inputIndexD outputIndexD claimD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ _ _ inputState nextScriptHash nextState -> P.do
              PBoundSource subjectD sourceCborD <- pmatch $ pexpectStateAs @PStep02State inputState
              Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData subjectD
              source <- plet $ pmatch (pinspectSourceEnvelopeV1 # pfromData sourceCborD) $ \case
                PNothing -> perror
                PJust value -> value
              PSourceEnvelopeV1{..} <- pmatch source
              PPair verified _ <-
                pmatch $
                  pverifyNativeTxProofSourceV1
                    # psourceEnvelope'embeddedTxId
                    # psourceEnvelope'compactCbor
                    # psourceEnvelope'witnessSetCompactCbor
                    # psourceEnvelope'fieldPreimageLengthsCbor
              evidence <-
                plet $
                  pauthenticatedLengthEvidenceV1
                    # verified
                    # psourceEnvelope'fieldPreimageLengthsCbor
                    # pfromData claimD
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              let expected = pstateFromEvidence subjectD evidence
              psourceEnvelope'embeddedTxId
                #== pfromData psubject'transactionId
                #&& nextScriptHash
                #== step03Hash
                #&& nextState
                #== pforgetData (pdata expected)
        PAuthenticateForced{} -> perror

fieldPreimageLengthMismatchStep02ForcedValidator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fieldPreimageLengthMismatchStep02ForcedValidator = plam $ \step03Hash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PAuthenticateAccepted{} -> perror
        PAuthenticateForced inputIndexD outputIndexD headerD membershipD claimD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ inputState nextScriptHash nextState -> P.do
              PPendingForced directionD <- pmatch $ pexpectStateAs @PStep02State inputState
              subject <-
                plet $
                  Subject.pbindForcedSubjectToThread
                    # pto (pfromData threadName)
                    # pfromData headerD
                    # pfromData membershipD
                    # pfromData directionD
              PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membershipD
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
              evidence <-
                plet $
                  pauthenticatedLengthEvidenceV1
                    # verified
                    # pfromData pnativeSource'fieldPreimageLengthsCbor
                    # pfromData claimD
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              boundEvidence <- plet $ pbindForcedLengthEvidenceV1 # subject # evidence
              let expected = pstateFromEvidence (pdata subject) boundEvidence
              nextScriptHash
                #== step03Hash
                #&& nextState
                #== pforgetData (pdata expected)

fieldPreimageLengthMismatchStep03Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fieldPreimageLengthMismatchStep03Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args{..} <- pmatch args
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
          pterminalContradictionV1 # pexpectStateAs @PStep03State inputState

psourceCborOf :: forall s. Term s PNativeTxInclusionCarriage -> Term s PByteString
psourceCborOf inclusion = pmatch inclusion $ \case
  PRedeemerCarriedInclusion argsD ->
    pmatch (pfromData argsD) $ \PNativeTxInclusionArgs{pinclusionArgs'nativeTxCompactCbor} ->
      pfromData pinclusionArgs'nativeTxCompactCbor
  PPublishedChunkInclusion argsD ->
    pmatch (pfromData argsD) $ \PPublishedChunkInclusionArgs{ppublishedArgs'nativeTxCompactCbor} ->
      pfromData ppublishedArgs'nativeTxCompactCbor

pexpectNoState :: forall a s. Term s (PMaybeData PData) -> Term s a -> Term s a
pexpectNoState state value = pmatch state $ \case
  PDNothing -> value
  PDJust _ -> perror

pstateFromEvidence :: forall s. Term s (PAsData Subject.PVerdictSubject) -> Term s PLengthEvidenceV1 -> Term s PStep03State
pstateFromEvidence subjectD evidence =
  pmatch evidence $ \PLengthEvidenceV1{..} ->
    pcon $
      PStep03State
        subjectD
        plengthEvidence'fieldIndex
        plengthEvidence'declaredLength
        plengthEvidence'actualLength
