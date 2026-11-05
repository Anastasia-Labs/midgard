{-# LANGUAGE OverloadedStrings #-}

-- | Exact field-8 redeemer-item canonicity rule.
module Midgard.FraudProofs.RedeemerCanonicity (
  PStep01Source (..),
  PStep01Args (..),
  PBoundRedeemerV1 (..),
  PStep02Args (..),
  PTerminalStateV1 (..),
  PStep03Args (..),
  predeemerFieldIndex,
  pbindRedeemerV1,
  pitemIsCanonicalV1,
  pauthenticateItemV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.CanonicalCborScan (PCborHeadV1 (..), pheadAtV1)
import Midgard.CanonicalPlutusData (pisCanonicalPlutusDataV1)
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PRedeemerMalformed))
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
  , pstep01Args'redeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PBoundRedeemerV1 (s :: S) = PBoundRedeemerV1
  { pboundRedeemer'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundRedeemer'witnessSetHash :: Term s (PAsData PByteString)
  , pboundRedeemer'redeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundRedeemerV1)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'opening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PTerminalStateV1 (s :: S) = PTerminalStateV1
  { pterminalState'bound :: Term s (PAsData PBoundRedeemerV1)
  , pterminalState'canonical :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalStateV1)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

predeemerFieldIndex :: forall s. Term s PInteger
predeemerFieldIndex = 8

pbindRedeemerV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PBoundRedeemerV1)
pbindRedeemerV1 = phoistAcyclic $ plam $ \subject witnessSetHash redeemerIndex ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # witnessSetHash
        #== 32
        #&& redeemerIndex
        #>= 0
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound =
              pcon $
                PBoundRedeemerV1
                  (pdata subject)
                  (pdata witnessSetHash)
                  (pdata redeemerIndex)
         in pif
              (pfromData psubject'direction #== 1)
              ( plet
                  ( Subject.pbindExactRejectionReason
                      # subject
                      # pcon (PRedeemerMalformed $ pdata redeemerIndex)
                  )
                  $ \_ -> bound
              )
              bound
    )
    perror

psupportedPurpose :: forall s. Term s (PInteger :--> PBool)
psupportedPurpose = phoistAcyclic $ plam $ \tag ->
  tag #== 0 #|| tag #== 1 #|| tag #== 3 #|| tag #== 6

-- | Total exact decoder: every malformed envelope returns 'False'.
pitemIsCanonicalV1 :: forall s. Term s (PByteString :--> PBool)
pitemIsCanonicalV1 = phoistAcyclic $ plam $ \item ->
  pmatch (pheadAtV1 # item # 0 # 4) $ \case
    PNothing -> pconstant False
    PJust outer -> pmatch outer $ \(PCborHeadV1 outerOffset outerArity) ->
      pif (pnot #$ outerArity #== 4) (pconstant False) $
        pmatch (pheadAtV1 # item # outerOffset # 0) $ \case
          PNothing -> pconstant False
          PJust purpose -> pmatch purpose $ \(PCborHeadV1 purposeOffset purposeTag) ->
            pif (pnot #$ psupportedPurpose # purposeTag) (pconstant False) $
              pmatch (pheadAtV1 # item # purposeOffset # 0) $ \case
                PNothing -> pconstant False
                PJust pointer -> pmatch pointer $ \(PCborHeadV1 pointerOffset _) ->
                  pmatch (pheadAtV1 # item # pointerOffset # 2) $ \case
                    PNothing -> pconstant False
                    PJust datum -> pmatch datum $ \(PCborHeadV1 datumOffset datumLength) ->
                      plet (datumOffset + datumLength) $ \datumEnd ->
                        pif (datumEnd #> plengthBS # item) (pconstant False) $
                          pmatch (pheadAtV1 # item # datumEnd # 4) $ \case
                            PNothing -> pconstant False
                            PJust exUnits -> pmatch exUnits $ \(PCborHeadV1 exUnitsOffset exUnitsArity) ->
                              pif (pnot #$ exUnitsArity #== 2) (pconstant False) $
                                pmatch (pheadAtV1 # item # exUnitsOffset # 0) $ \case
                                  PNothing -> pconstant False
                                  PJust memory -> pmatch memory $ \(PCborHeadV1 memoryOffset _) ->
                                    pmatch (pheadAtV1 # item # memoryOffset # 0) $ \case
                                      PNothing -> pconstant False
                                      PJust steps -> pmatch steps $ \(PCborHeadV1 endOffset _) ->
                                        endOffset
                                          #== plengthBS
                                          # item
                                          #&& pisCanonicalPlutusDataV1
                                          # (psliceBS # datumOffset # datumLength # item)

pauthenticateItemV1 :: forall s. Term s (PBoundRedeemerV1 :--> PByteString :--> PTerminalStateV1)
pauthenticateItemV1 = phoistAcyclic $ plam $ \bound item ->
  pcon $
    PTerminalStateV1
      (pdata bound)
      (pdata $ pitemIsCanonicalV1 # item)

pterminalContradictionV1 :: forall s. Term s (PTerminalStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PTerminalStateV1{pterminalState'bound, pterminalState'canonical} ->
    pmatch (pfromData pterminalState'bound) $ \PBoundRedeemerV1{pboundRedeemer'subject} ->
      Subject.pterminalContradiction
        # pfromData pboundRedeemer'subject
        # (pnot # pfromData pterminalState'canonical)
