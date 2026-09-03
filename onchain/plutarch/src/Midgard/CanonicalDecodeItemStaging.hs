module Midgard.CanonicalDecodeItemStaging (
  PAuthenticatedCanonicalDecodeItemV1 (..),
  PPreparedCanonicalDecodeItemV1 (..),
  PObservedCanonicalDecodeItemV1 (..),
  PVerifiedCanonicalDecodeItemV1 (..),
  pcanonicalDecodeItemStagingVersion,
  pauthenticateCanonicalDecodeItem,
  pauthenticatedCanonicalDecodeItemIsWellFormed,
  pprepareCanonicalDecodeItem,
  ppreparedCanonicalDecodeItemIsWellFormed,
  pobserveCanonicalDecodeItem,
  pobservedCanonicalDecodeItemIsWellFormed,
  pverifyCanonicalDecodeItem,
  pverifiedCanonicalDecodeItemIsWellFormed,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PCanonicalDecodeItemObservationV1,
  PCanonicalDecodeItemProofV1,
  PCanonicalDecodeItemSourceV1,
  PValidationOneStepWitnessV1,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1,
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
 )
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), PValidationPhase (..))

pcanonicalDecodeItemStagingVersion :: forall (s :: S). Term s PInteger
pcanonicalDecodeItemStagingVersion = 1

data PAuthenticatedCanonicalDecodeItemV1 (s :: S) = PAuthenticatedCanonicalDecodeItemV1
  { pauthenticatedCanonical'version :: Term s (PAsData PInteger)
  , pauthenticatedCanonical'base :: Term s (PAsData PPreparedValidationResolutionStateV1)
  , pauthenticatedCanonical'transition :: Term s (PAsData PValidationOneStepWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedCanonicalDecodeItemV1)

data PPreparedCanonicalDecodeItemV1 (s :: S) = PPreparedCanonicalDecodeItemV1
  { ppreparedCanonical'version :: Term s (PAsData PInteger)
  , ppreparedCanonical'authenticated :: Term s (PAsData PAuthenticatedCanonicalDecodeItemV1)
  , ppreparedCanonical'source :: Term s (PAsData PCanonicalDecodeItemSourceV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPreparedCanonicalDecodeItemV1)

data PObservedCanonicalDecodeItemV1 (s :: S) = PObservedCanonicalDecodeItemV1
  { pobservedCanonical'version :: Term s (PAsData PInteger)
  , pobservedCanonical'prepared :: Term s (PAsData PPreparedCanonicalDecodeItemV1)
  , pobservedCanonical'observation :: Term s (PAsData PCanonicalDecodeItemObservationV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PObservedCanonicalDecodeItemV1)

data PVerifiedCanonicalDecodeItemV1 (s :: S) = PVerifiedCanonicalDecodeItemV1
  { pverifiedCanonical'version :: Term s (PAsData PInteger)
  , pverifiedCanonical'observed :: Term s (PAsData PObservedCanonicalDecodeItemV1)
  , pverifiedCanonical'proof :: Term s (PAsData PCanonicalDecodeItemProofV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifiedCanonicalDecodeItemV1)

pbaseIsCanonicalDecode :: forall (s :: S). Term s PPreparedValidationResolutionStateV1 -> Term s PBool
pbaseIsCanonicalDecode base = pmatch base $ \prepared ->
  pmatch (pfromData $ pprepared'resolution prepared) $ \resolution ->
  pmatch (pfromData $ presolution'preState resolution) $ \pre ->
    pfromData (pmachineState'phase pre) #== pcon PCanonicalDecode

pauthenticateCanonicalDecodeItem ::
  forall (s :: S).
  Term s (PPreparedValidationResolutionStateV1 :--> PValidationOneStepWitnessV1 :--> PAuthenticatedCanonicalDecodeItemV1)
pauthenticateCanonicalDecodeItem = phoistAcyclic $ plam $ \base transition ->
  pif (pbaseIsCanonicalDecode base)
    (pcon $ PAuthenticatedCanonicalDecodeItemV1
      (pdata pcanonicalDecodeItemStagingVersion) (pdata base) (pdata transition))
    perror

pauthenticatedCanonicalDecodeItemIsWellFormed ::
  forall (s :: S). Term s (PAuthenticatedCanonicalDecodeItemV1 :--> PBool)
pauthenticatedCanonicalDecodeItemIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pfromData (pauthenticatedCanonical'version s) #== pcanonicalDecodeItemStagingVersion
    #&& pbaseIsCanonicalDecode (pfromData $ pauthenticatedCanonical'base s)

pprepareCanonicalDecodeItem ::
  forall (s :: S).
  Term s (PAuthenticatedCanonicalDecodeItemV1 :--> PCanonicalDecodeItemSourceV1 :--> PPreparedCanonicalDecodeItemV1)
pprepareCanonicalDecodeItem = phoistAcyclic $ plam $ \authenticated source ->
  pif (pauthenticatedCanonicalDecodeItemIsWellFormed # authenticated)
    (pcon $ PPreparedCanonicalDecodeItemV1
      (pdata pcanonicalDecodeItemStagingVersion) (pdata authenticated) (pdata source))
    perror

ppreparedCanonicalDecodeItemIsWellFormed ::
  forall (s :: S). Term s (PPreparedCanonicalDecodeItemV1 :--> PBool)
ppreparedCanonicalDecodeItemIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pfromData (ppreparedCanonical'version s) #== pcanonicalDecodeItemStagingVersion
    #&& pauthenticatedCanonicalDecodeItemIsWellFormed # pfromData (ppreparedCanonical'authenticated s)

pobserveCanonicalDecodeItem ::
  forall (s :: S).
  Term s (PPreparedCanonicalDecodeItemV1 :--> PCanonicalDecodeItemObservationV1 :--> PObservedCanonicalDecodeItemV1)
pobserveCanonicalDecodeItem = phoistAcyclic $ plam $ \prepared observation ->
  pif (ppreparedCanonicalDecodeItemIsWellFormed # prepared)
    (pcon $ PObservedCanonicalDecodeItemV1
      (pdata pcanonicalDecodeItemStagingVersion) (pdata prepared) (pdata observation))
    perror

pobservedCanonicalDecodeItemIsWellFormed ::
  forall (s :: S). Term s (PObservedCanonicalDecodeItemV1 :--> PBool)
pobservedCanonicalDecodeItemIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pfromData (pobservedCanonical'version s) #== pcanonicalDecodeItemStagingVersion
    #&& ppreparedCanonicalDecodeItemIsWellFormed # pfromData (pobservedCanonical'prepared s)

pverifyCanonicalDecodeItem ::
  forall (s :: S).
  Term s (PObservedCanonicalDecodeItemV1 :--> PCanonicalDecodeItemProofV1 :--> PVerifiedCanonicalDecodeItemV1)
pverifyCanonicalDecodeItem = phoistAcyclic $ plam $ \observed proof ->
  pif (pobservedCanonicalDecodeItemIsWellFormed # observed)
    (pcon $ PVerifiedCanonicalDecodeItemV1
      (pdata pcanonicalDecodeItemStagingVersion) (pdata observed) (pdata proof))
    perror

pverifiedCanonicalDecodeItemIsWellFormed ::
  forall (s :: S). Term s (PVerifiedCanonicalDecodeItemV1 :--> PBool)
pverifiedCanonicalDecodeItemIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pfromData (pverifiedCanonical'version s) #== pcanonicalDecodeItemStagingVersion
    #&& pobservedCanonicalDecodeItemIsWellFormed # pfromData (pverifiedCanonical'observed s)
