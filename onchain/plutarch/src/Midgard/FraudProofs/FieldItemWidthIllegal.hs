{-# LANGUAGE OverloadedStrings #-}

-- | Direct field-item-width fault state and predicates.
module Midgard.FraudProofs.FieldItemWidthIllegal (
  PStep01Source (..),
  PStep01Args (..),
  PBoundCoordinate (..),
  PStep02Args (..),
  PAuthenticatedWidth (..),
  PStep03Args (..),
  pcoordinateIsSupported,
  pbindCoordinate,
  pauthenticateItemWidth,
  pitemWidthIsIllegal,
  pterminalContradiction,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PFieldItemWidthIllegal))
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
  , pstep01Args'fieldIndex :: Term s (PAsData PInteger)
  , pstep01Args'itemIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PBoundCoordinate (s :: S) = PBoundCoordinate
  { pboundCoordinate'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundCoordinate'fieldIndex :: Term s (PAsData PInteger)
  , pboundCoordinate'itemIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundCoordinate)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'opening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PAuthenticatedWidth (s :: S) = PAuthenticatedWidth
  { pauthenticatedWidth'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pauthenticatedWidth'fieldIndex :: Term s (PAsData PInteger)
  , pauthenticatedWidth'itemIndex :: Term s (PAsData PInteger)
  , pauthenticatedWidth'itemWidth :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedWidth)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

pcoordinateIsSupported :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pcoordinateIsSupported = phoistAcyclic $ plam $ \fieldIndex itemIndex ->
  itemIndex #>= 0 #&& (fieldIndex #== 2 #|| fieldIndex #== 5)

pbindCoordinate :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PInteger :--> PBoundCoordinate)
pbindCoordinate = phoistAcyclic $ plam $ \subject fieldIndex itemIndex ->
  pif
    (Subject.psubjectIsCanonical # subject #&& pcoordinateIsSupported # fieldIndex # itemIndex)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundCoordinate (pdata subject) (pdata fieldIndex) (pdata itemIndex)
         in pif
              (pfromData psubject'direction #== 1)
              ( plet
                  ( Subject.pbindExactRejectionReason
                      # subject
                      # pcon (PFieldItemWidthIllegal (pdata fieldIndex) (pdata itemIndex))
                  )
                  $ \_ -> bound
              )
              bound
    )
    perror

pauthenticateItemWidth :: forall s. Term s (PBoundCoordinate :--> PByteString :--> PAuthenticatedWidth)
pauthenticateItemWidth = phoistAcyclic $ plam $ \bound item ->
  pmatch bound $ \PBoundCoordinate{..} ->
    pcon $
      PAuthenticatedWidth
        pboundCoordinate'subject
        pboundCoordinate'fieldIndex
        pboundCoordinate'itemIndex
        (pdata $ plengthBS # item)

pitemWidthIsIllegal :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pitemWidthIsIllegal = phoistAcyclic $ plam $ \fieldIndex itemWidth ->
  pif
    (itemWidth #>= 0)
    (pif (fieldIndex #== 2) (itemWidth #> 16384) (pif (fieldIndex #== 5) (itemWidth #== 0) perror))
    perror

pterminalContradiction :: forall s. Term s (PAuthenticatedWidth :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \authenticated ->
  pmatch authenticated $ \PAuthenticatedWidth{..} ->
    pif
      (pcoordinateIsSupported # pfromData pauthenticatedWidth'fieldIndex # pfromData pauthenticatedWidth'itemIndex)
      ( Subject.pterminalContradiction
          # pfromData pauthenticatedWidth'subject
          # (pitemWidthIsIllegal # pfromData pauthenticatedWidth'fieldIndex # pfromData pauthenticatedWidth'itemWidth)
      )
      perror
