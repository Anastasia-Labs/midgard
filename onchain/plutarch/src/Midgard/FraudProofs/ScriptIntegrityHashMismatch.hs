{-# LANGUAGE OverloadedStrings #-}

-- | Direct script-integrity-hash mismatch rule and staged Data ABI.
module Midgard.FraudProofs.ScriptIntegrityHashMismatch (
  PStep01Source (..),
  PStep01Args (..),
  PBoundIntegrityV1 (..),
  PStep02Args (..),
  PAuthenticatedIntegrityV1 (..),
  PStep03Args (..),
  PLanguageFoldV1 (..),
  PStep04Args (..),
  PDecisionV1 (..),
  PStep05Args (..),
  pbindIntegrityV1,
  pauthenticateIntegrityV1,
  pinitializeLanguageFoldV1,
  pfoldNextLanguageV1,
  pdecideIntegrityV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PScriptIntegrityHashMismatch), prejectionCodeOf)
import Midgard.ScriptLanguageViews (pexpectedScriptIntegrityHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (PNativeScriptsControlV1 (..), pencodeScriptIntegrityFinalizeWitnessV1)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PScriptIntegrity),
  PValidationSourceKind (PForced, PNormal),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (PAccepted, PRejected),
  phashMachineState,
  phashRejectionCode,
  phashWorkWitness,
  pverifyTraceProof,
 )

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

newtype PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PBoundIntegrityV1 (s :: S) = PBoundIntegrityV1
  { pboundIntegrity'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundIntegrity'validationTracesRoot :: Term s (PAsData PByteString)
  , pboundIntegrity'validationTraceCount :: Term s (PAsData PInteger)
  , pboundIntegrity'scriptIntegrityHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundIntegrityV1)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'machineState :: Term s (PAsData PValidationMachineStateV1)
  , pstep02Args'traceProof :: Term s (PAsData PValidationTraceProof)
  , pstep02Args'control :: Term s (PAsData PNativeScriptsControlV1)
  , pstep02Args'redeemerWitnessHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PAuthenticatedIntegrityV1 (s :: S) = PAuthenticatedIntegrityV1
  { pauthenticatedIntegrity'bound :: Term s (PAsData PBoundIntegrityV1)
  , pauthenticatedIntegrity'priorLedgerRoot :: Term s (PAsData PByteString)
  , pauthenticatedIntegrity'redeemerWitnessHash :: Term s (PAsData PByteString)
  , pauthenticatedIntegrity'selectedLanguageBitmap :: Term s (PAsData PInteger)
  , pauthenticatedIntegrity'executionCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedIntegrityV1)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PLanguageFoldV1 (s :: S) = PLanguageFoldV1
  { planguageFold'authenticated :: Term s (PAsData PAuthenticatedIntegrityV1)
  , planguageFold'cursor :: Term s (PAsData PInteger)
  , planguageFold'rebuiltLanguageBitmap :: Term s (PAsData PInteger)
  , planguageFold'selectedLanguageCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLanguageFoldV1)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PDecisionV1 (s :: S) = PDecisionV1
  { pdecision'authenticated :: Term s (PAsData PAuthenticatedIntegrityV1)
  , pdecision'expectedHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionV1)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

pbindIntegrityV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PByteString :--> PBoundIntegrityV1)
pbindIntegrityV1 = phoistAcyclic $ plam $ \subject validationTracesRoot validationTraceCount scriptIntegrityHash ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # validationTracesRoot
        #== 32
        #&& validationTraceCount
        #> 0
        #&& plengthBS
        # scriptIntegrityHash
        #== 32
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound =
              pcon $
                PBoundIntegrityV1
                  (pdata subject)
                  (pdata validationTracesRoot)
                  (pdata validationTraceCount)
                  (pdata scriptIntegrityHash)
         in pif
              (pfromData psubject'direction #== 1)
              (plet (Subject.pbindExactRejectionReason # subject # pcon PScriptIntegrityHashMismatch) $ \_ -> bound)
              bound
    )
    perror

pauthenticateIntegrityV1 ::
  forall s.
  Term
    s
    ( PBoundIntegrityV1
        :--> PRootMembershipProof
        :--> PValidationMachineStateV1
        :--> PValidationTraceProof
        :--> PNativeScriptsControlV1
        :--> PByteString
        :--> PAuthenticatedIntegrityV1
    )
pauthenticateIntegrityV1 = phoistAcyclic $ plam $ \bound membership machineState traceProof control redeemerWitnessHash -> P.do
  PBoundIntegrityV1{..} <- pmatch bound
  subject <- plet $ pfromData pboundIntegrity'subject
  Subject.PVerdictSubject{..} <- pmatch subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ pcoerceData @PEventKey prootMembership'key
  descriptor <- plet $ pcoerceData @PValidationTraceDescriptorV1 prootMembership'value
  PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} <- pmatch descriptor
  PValidationMachineStateV1{..} <- pmatch machineState
  PValidationTraceProof{ptraceProof'stateHash} <- pmatch traceProof
  PNativeScriptsControlV1{..} <- pmatch control
  let sourceKind = pfromData psubject'sourceKind
      expectedRejectionHash =
        phashRejectionCode
          # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject))
      sourceAndVerdictMatch =
        pif
          (sourceKind #== 0)
          ( pfromData pmachineState'sourceKind
              #== pcon PNormal
              #&& pfromData
                pdescriptor'verdict
              #== pcon PAccepted
          )
          ( pfromData pmachineState'sourceKind
              #== pcon PForced
              #&& pfromData
                pdescriptor'verdict
              #== pcon PRejected
              #&& pfromData
                pdescriptor'rejectionCodeHash
              #== expectedRejectionHash
          )
      expected =
        pcon $
          PAuthenticatedIntegrityV1
            (pdata bound)
            pmachineState'priorLedgerRoot
            (pdata redeemerWitnessHash)
            pnativeControl'languageBitmap
            pnativeControl'executionCount
  pif
    ( peventKeyMatchesSubject eventKey subject
        #&& pfromData
          pmachineState'eventKeyHash
        #== (pblake2b_256 #$ pserialiseData # prootMembership'key)
        #&& pverifyRootMembershipWithBytes
          membership
          (pdata $ pcon PValidationTracesRootDomain)
          (pfromData pboundIntegrity'validationTracesRoot)
          (pfromData pboundIntegrity'validationTraceCount)
          (pserialiseData # prootMembership'key)
          (pserialiseData # prootMembership'value)
        #&& pfromData
          pmachineState'transactionId
        #== pfromData
          psubject'transactionId
        #&& sourceAndVerdictMatch
        #&& pfromData
          pmachineState'phase
        #== pcon PScriptIntegrity
        #&& pfromData
          pmachineState'workRoot
        #== ( phashWorkWitness
                # pcon PScriptIntegrity
                # pfromData pmachineState'programCounter
                # ( pencodeScriptIntegrityFinalizeWitnessV1
                      # control
                      # pfromData pboundIntegrity'scriptIntegrityHash
                      # redeemerWitnessHash
                  )
            )
        #&& pfromData
          ptraceProof'stateHash
        #== (phashMachineState # machineState)
        #&& (pverifyTraceProof # descriptor # traceProof)
        #&& pfromData
          pnativeControl'executionCursor
        #== pfromData
          pnativeControl'executionCount
        #&& pfromData
          pnativeControl'executionCount
        #== pfromData
          pnativeControl'purposeCount
        #&& pfromData
          pnativeControl'languageBitmap
        #>= 0
        #&& pfromData
          pnativeControl'languageBitmap
        #<= 3
        #&& plengthBS
        # redeemerWitnessHash
        #== 32
    )
    expected
    perror

pinitializeLanguageFoldV1 :: forall s. Term s (PAuthenticatedIntegrityV1 :--> PLanguageFoldV1)
pinitializeLanguageFoldV1 = phoistAcyclic $ plam $ \authenticated ->
  pmatch authenticated $ \PAuthenticatedIntegrityV1{..} ->
    pif
      ( pfromData pauthenticatedIntegrity'selectedLanguageBitmap
          #>= 0
          #&& pfromData
            pauthenticatedIntegrity'selectedLanguageBitmap
          #<= 3
          #&& plengthBS
          # pfromData
            pauthenticatedIntegrity'redeemerWitnessHash
          #== 32
      )
      ( pcon $
          PLanguageFoldV1
            (pdata authenticated)
            (pdata 0)
            (pdata 0)
            (pdata 0)
      )
      perror

pfoldNextLanguageV1 :: forall s. Term s (PLanguageFoldV1 :--> PLanguageFoldV1)
pfoldNextLanguageV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PLanguageFoldV1{..} ->
    let cursor = pfromData planguageFold'cursor
        authenticated = pfromData planguageFold'authenticated
        selected =
          pmatch authenticated $ \PAuthenticatedIntegrityV1{pauthenticatedIntegrity'selectedLanguageBitmap} ->
            pif
              (cursor #== 0)
              (pmod # pfromData pauthenticatedIntegrity'selectedLanguageBitmap # 2 #== 1)
              (pfromData pauthenticatedIntegrity'selectedLanguageBitmap #>= 2)
     in pif
          (cursor #>= 0 #&& cursor #< 2)
          ( pcon $
              PLanguageFoldV1
                planguageFold'authenticated
                (pdata $ cursor + 1)
                ( pdata $
                    pfromData planguageFold'rebuiltLanguageBitmap
                      + pif selected (pif (cursor #== 0) 1 2) 0
                )
                (pdata $ pfromData planguageFold'selectedLanguageCount + pif selected 1 0)
          )
          perror

pdecideIntegrityV1 :: forall s. Term s (PLanguageFoldV1 :--> PDecisionV1)
pdecideIntegrityV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PLanguageFoldV1{..} ->
    let authenticated = pfromData planguageFold'authenticated
     in pmatch authenticated $ \PAuthenticatedIntegrityV1{..} ->
          pif
            ( pfromData planguageFold'cursor
                #== 2
                #&& pfromData
                  planguageFold'rebuiltLanguageBitmap
                #== pfromData
                  pauthenticatedIntegrity'selectedLanguageBitmap
                #&& pfromData
                  planguageFold'selectedLanguageCount
                #>= 0
                #&& pfromData
                  planguageFold'selectedLanguageCount
                #<= 2
            )
            ( pcon $
                PDecisionV1
                  (pdata authenticated)
                  ( pdata $
                      pexpectedScriptIntegrityHash
                        # pfromData pauthenticatedIntegrity'redeemerWitnessHash
                        # pfromData pauthenticatedIntegrity'selectedLanguageBitmap
                  )
            )
            perror

pterminalContradictionV1 :: forall s. Term s (PDecisionV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \decision ->
  pmatch decision $ \PDecisionV1{pdecision'authenticated, pdecision'expectedHash} ->
    let expectedHash = pfromData pdecision'expectedHash
     in pif
          (plengthBS # expectedHash #== 32)
          ( pmatch (pfromData pdecision'authenticated) $ \PAuthenticatedIntegrityV1{pauthenticatedIntegrity'bound} ->
              pmatch (pfromData pauthenticatedIntegrity'bound) $ \PBoundIntegrityV1{pboundIntegrity'subject, pboundIntegrity'scriptIntegrityHash} ->
                Subject.pterminalContradiction
                  # pfromData pboundIntegrity'subject
                  # (pnot #$ pfromData pboundIntegrity'scriptIntegrityHash #== expectedHash)
          )
          perror

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
  pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
    pmatch eventKey $ \case
      PL2TransactionEventKey txId ->
        pfromData psubject'sourceKind
          #== 0
          #&& pfromData
            txId
          #== pfromData
            psubject'transactionId
      PForcedTransactionEventKey txOrderId ->
        pfromData psubject'sourceKind
          #== 1
          #&& (pserialiseData # pforgetData txOrderId)
          #== pfromData
            psubject'sourceKey
      _ -> pconstant False

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
