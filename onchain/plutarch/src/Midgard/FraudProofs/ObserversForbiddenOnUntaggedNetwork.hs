{-# LANGUAGE OverloadedStrings #-}

-- | Direct observer/network fault state and predicates.
module Midgard.FraudProofs.ObserversForbiddenOnUntaggedNetwork (
  PStep01Source (..),
  PStep01Args (..),
  PStateV1 (..),
  PStep02Args (..),
  pbindStateV1,
  pforbiddenObserversHoldV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PObserversForbiddenOnUntaggedNetwork))
import Midgard.ScriptLanguageViews (pemptyScriptIntegrityHash)
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

newtype PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStateV1 (s :: S) = PStateV1
  { pstate'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pstate'networkId :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStateV1)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'observerOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

pbindStateV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PStateV1)
pbindStateV1 = phoistAcyclic $ plam $ \subject networkId ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& (networkId #== 0 #|| networkId #== 1 #|| networkId #== 255)
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let state = pcon $ PStateV1 (pdata subject) (pdata networkId)
         in pif
              (pfromData psubject'direction #== 1)
              ( plet
                  ( Subject.pbindExactRejectionReason
                      # subject
                      # pcon PObserversForbiddenOnUntaggedNetwork
                  )
                  $ \_ -> state
              )
              state
    )
    perror

pforbiddenObserversHoldV1 :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PBool)
pforbiddenObserversHoldV1 = phoistAcyclic $ plam $ \observerCount networkId scriptIntegrityHash ->
  pif
    (plengthBS # scriptIntegrityHash #== 32)
    ( pnot
        # (scriptIntegrityHash #== pemptyScriptIntegrityHash)
        #&& observerCount
        #> 0
        #&& networkId
        #== 255
    )
    perror

pterminalContradictionV1 :: forall s. Term s (PStateV1 :--> PInteger :--> PByteString :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state observerCount scriptIntegrityHash ->
  pmatch state $ \PStateV1{..} ->
    pif
      (observerCount #>= 0)
      ( Subject.pterminalContradiction
          # pfromData pstate'subject
          # ( pforbiddenObserversHoldV1
                # observerCount
                # pfromData pstate'networkId
                # scriptIntegrityHash
            )
      )
      perror
