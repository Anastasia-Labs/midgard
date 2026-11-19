{-# LANGUAGE OverloadedStrings #-}

-- | Staged exact observer-order rule over authenticated field 3.
module Midgard.FraudProofs.ObserverOrderInvalid (
  PStep01Source (..),
  PStep01Args (..),
  PBoundObserverV1 (..),
  PAuthenticationStateV1 (..),
  PStep02ActionV1 (..),
  PScanStateV1 (..),
  PStep03Args (..),
  PDecisionStateV1 (..),
  PStep04Args (..),
  pobserverFieldIndex,
  pstagedObserverBudget,
  poutcomeScanning,
  poutcomeViolation,
  poutcomeOrdered,
  pbindObserverV1,
  pinitialScanV1,
  pscanItemV1,
  pexhaustScanV1,
  pdecisionV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PObserverOrderInvalid))
import Midgard.TransitionTrace (PRootMembershipProof)

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

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  , pstep01Args'observerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PBoundObserverV1 (s :: S) = PBoundObserverV1
  { pboundObserver'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundObserver'observerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundObserverV1)

data PAuthenticationStateV1 (s :: S)
  = PBound (Term s (PAsData PBoundObserverV1))
  | PReserved (Term s (PAsData PBoundObserverV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticationStateV1)

data PStep02ActionV1 (s :: S)
  = PAuthenticate
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PReservedAction
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02ActionV1)

data PScanStateV1 (s :: S) = PScanStateV1
  { pscanState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pscanState'observerIndex :: Term s (PAsData PInteger)
  , pscanState'checkpointHash :: Term s (PAsData PByteString)
  , pscanState'seen :: Term s (PAsData PInteger)
  , pscanState'previousObserver :: Term s (PAsData PByteString)
  , pscanState'outcome :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanStateV1)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'opening :: Term s (PAsData PFieldOpeningV1)
  , pstep03Args'checkpointBytes :: Term s (PAsData PByteString)
  , pstep03Args'itemBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PDecisionStateV1 (s :: S) = PDecisionStateV1
  { pdecisionState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pdecisionState'observerIndex :: Term s (PAsData PInteger)
  , pdecisionState'violation :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionStateV1)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

pobserverFieldIndex, pstagedObserverBudget, poutcomeScanning, poutcomeViolation, poutcomeOrdered :: forall s. Term s PInteger
pobserverFieldIndex = 3
pstagedObserverBudget = 24
poutcomeScanning = 0
poutcomeViolation = 1
poutcomeOrdered = 2

pbindObserverV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PBoundObserverV1)
pbindObserverV1 = phoistAcyclic $ plam $ \subject observerIndex ->
  pif
    (Subject.psubjectIsCanonical # subject #&& observerIndex #>= 0)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundObserverV1 (pdata subject) (pdata observerIndex)
         in pif
              (pfromData psubject'direction #== 1)
              ( plet
                  ( Subject.pbindExactRejectionReason
                      # subject
                      # pcon (PObserverOrderInvalid $ pdata observerIndex)
                  )
                  $ \_ -> bound
              )
              (pif (observerIndex #> 0) bound perror)
    )
    perror

pinitialScanV1 :: forall s. Term s (PBoundObserverV1 :--> PByteString :--> PScanStateV1)
pinitialScanV1 = phoistAcyclic $ plam $ \bound checkpointHash ->
  pif
    (plengthBS # checkpointHash #== 32)
    ( pmatch bound $ \PBoundObserverV1{pboundObserver'subject, pboundObserver'observerIndex} ->
        pcon $
          PScanStateV1
            pboundObserver'subject
            pboundObserver'observerIndex
            (pdata checkpointHash)
            (pdata 0)
            (pdata $ pconstant "")
            (pdata poutcomeScanning)
    )
    perror

pscanItemV1 :: forall s. Term s (PScanStateV1 :--> PInteger :--> PByteString :--> PScanStateV1)
pscanItemV1 = phoistAcyclic $ plam $ \state itemIndex observer ->
  pmatch state $ \PScanStateV1{..} ->
    let seen = pfromData pscanState'seen
        target = pfromData pscanState'observerIndex
        previous = pfromData pscanState'previousObserver
        common =
          pfromData pscanState'outcome
            #== poutcomeScanning
            #&& itemIndex
            #== seen
            #&& itemIndex
            #<= target
            #&& plengthBS
            # observer
            #== 28
            #&& (itemIndex #== 0 #|| plengthBS # previous #== 28)
        next outcome =
          pcon $
            PScanStateV1
              pscanState'subject
              pscanState'observerIndex
              pscanState'checkpointHash
              (pdata $ seen + 1)
              (pdata observer)
              (pdata outcome)
     in pif
          common
          ( pif
              (itemIndex #== target)
              (next $ pif (previous #< observer) poutcomeOrdered poutcomeViolation)
              (pif (itemIndex #== 0 #|| previous #< observer) (next poutcomeScanning) perror)
          )
          perror

pexhaustScanV1 :: forall s. Term s (PScanStateV1 :--> PScanStateV1)
pexhaustScanV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \value@PScanStateV1{pscanState'observerIndex, pscanState'seen, pscanState'outcome} ->
    pif
      ( pfromData pscanState'outcome
          #== poutcomeScanning
          #&& pfromData
            pscanState'seen
          #<= pfromData
            pscanState'observerIndex
      )
      (pcon value{pscanState'outcome = pdata poutcomeOrdered})
      perror

pdecisionV1 :: forall s. Term s (PScanStateV1 :--> PDecisionStateV1)
pdecisionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PScanStateV1{pscanState'subject, pscanState'observerIndex, pscanState'outcome} ->
    let outcome = pfromData pscanState'outcome
     in pif
          (outcome #== poutcomeViolation #|| outcome #== poutcomeOrdered)
          ( pcon $
              PDecisionStateV1
                pscanState'subject
                pscanState'observerIndex
                (pdata $ outcome #== poutcomeViolation)
          )
          perror

pterminalContradictionV1 :: forall s. Term s (PDecisionStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PDecisionStateV1{pdecisionState'subject, pdecisionState'violation} ->
    Subject.pterminalContradiction
      # pfromData pdecisionState'subject
      # pfromData pdecisionState'violation
