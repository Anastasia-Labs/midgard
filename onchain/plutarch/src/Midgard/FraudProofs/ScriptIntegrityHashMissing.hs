{-# LANGUAGE OverloadedStrings #-}

-- | Exact script-integrity-hash-missing rule and seven-script Data ABI.
module Midgard.FraudProofs.ScriptIntegrityHashMissing (
  PBindStateV1 (..),
  PDecisionStateV1 (..),
  PSubjectStateV1 (..),
  PStagedPhaseV1 (..),
  PStagedStateV1 (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PScriptGrammarArgs (..),
  PScriptScanArgs (..),
  PRedeemerGrammarArgs (..),
  PStep04Args (..),
  psourcePendingForced,
  pstagedBatchLimit,
  pdirectFieldItemLimit,
  pcontainsNonNativeScriptItemsV1,
  pfieldViewContainsNonNativeScriptV1,
  pfaultHoldsV1,
  pencodeDecisionStateV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardVersionedScriptAt)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardScriptLanguage (PNativeCardanoScript),
  PMidgardVersionedScript (..),
  PNativeTxWitnessSetCompact,
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1, PFieldViewV1, pfieldItemAt, pfieldItemCount)
import Midgard.ScriptLanguageViews (pemptyScriptIntegrityHash)
import Midgard.TransitionTrace (PRootMembershipProof)

data PBindStateV1 (s :: S)
  = PBoundAccepted
      (Term s (PAsData Subject.PVerdictSubject))
      (Term s (PAsData PByteString))
  | PPendingForced (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBindStateV1)

data PDecisionStateV1 (s :: S) = PDecisionStateV1
  { pdecisionState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pdecisionState'scriptIntegrityHash :: Term s (PAsData PByteString)
  , pdecisionState'containsNonNativeScript :: Term s (PAsData PBool)
  , pdecisionState'hasRedeemers :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionStateV1)

data PSubjectStateV1 (s :: S) = PSubjectStateV1
  { psubjectState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , psubjectState'witnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSubjectStateV1)

data PStagedPhaseV1 (s :: S)
  = PScriptGrammar (Term s (PAsData PByteString))
  | PScriptScan (Term s (PAsData PByteString)) (Term s (PAsData PBool))
  | PScriptComplete (Term s (PAsData PBool))
  | PRedeemerGrammar (Term s (PAsData PByteString)) (Term s (PAsData PBool))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStagedPhaseV1)

data PStagedStateV1 (s :: S) = PStagedStateV1
  { pstagedState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pstagedState'witnessSetHash :: Term s (PAsData PByteString)
  , pstagedState'scriptIntegrityHash :: Term s (PAsData PByteString)
  , pstagedState'phase :: Term s (PAsData PStagedPhaseV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStagedStateV1)

data PStep01Args (s :: S)
  = PBindAccepted (Term s (PAsData PNativeTxInclusionCarriage))
  | PRecordForced
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'header :: Term s (PAsData PHeaderV1)
  , pstep02Args'forcedMembership :: Term s (PAsData (PMaybeData PRootMembershipProof))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S)
  = PDirect
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PNativeTxWitnessSetCompact))
      (Term s (PAsData PFieldCarriageV1))
      (Term s (PAsData PFieldCarriageV1))
  | PStartStaged
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PScriptGrammarArgs (s :: S)
  = PResumeScriptGrammar
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  | PStartScriptScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptGrammarArgs)

data PScriptScanArgs (s :: S) = PScriptScanArgs
  { pscriptScanArgs'inputIndex :: Term s (PAsData PInteger)
  , pscriptScanArgs'outputIndex :: Term s (PAsData PInteger)
  , pscriptScanArgs'opening :: Term s (PAsData PFieldOpeningV1)
  , pscriptScanArgs'checkpointBytes :: Term s (PAsData PByteString)
  , pscriptScanArgs'itemBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptScanArgs)

data PRedeemerGrammarArgs (s :: S)
  = PStartRedeemerGrammar
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PInteger))
  | PResumeRedeemerGrammar
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  | PFinishRedeemerGrammar
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerGrammarArgs)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

psourcePendingForced, pstagedBatchLimit, pdirectFieldItemLimit :: forall s. Term s PInteger
psourcePendingForced = 1
pstagedBatchLimit = 32
pdirectFieldItemLimit = 64

pcontainsNonNativeScriptItemsV1 :: forall s. Term s (PBuiltinList PByteString :--> PBool)
pcontainsNonNativeScriptItemsV1 = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    (\item rest -> pitemContainsNonNativeScript # item #|| self # rest)
    (pconstant False)
    items

pfieldViewContainsNonNativeScriptV1 :: forall s. Term s (PFieldViewV1 :--> PBool)
pfieldViewContainsNonNativeScriptV1 = phoistAcyclic $ plam $ \view ->
  plet (pfieldItemCount # view) $ \count ->
    ( pfix $ \self -> plam $ \cursor ->
        pif
          (cursor #== count)
          (pconstant False)
          (pitemContainsNonNativeScript # (pfieldItemAt # view # cursor) #|| self # (cursor + 1))
    )
      # 0

pfaultHoldsV1 :: forall s. Term s (PByteString :--> PBool :--> PBool :--> PBool)
pfaultHoldsV1 = phoistAcyclic $ plam $ \scriptIntegrityHash containsNonNativeScript hasRedeemers ->
  pif
    (plengthBS # scriptIntegrityHash #== 32)
    ( ( containsNonNativeScript
          #|| hasRedeemers
          #|| (scriptIntegrityHash #/= pemptyScriptIntegrityHash)
      )
        #&& scriptIntegrityHash
        #== pemptyScriptIntegrityHash
    )
    perror

pencodeDecisionStateV1 :: forall s. Term s (PDecisionStateV1 :--> PByteString)
pencodeDecisionStateV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PDecisionStateV1{..} ->
    pif
      (plengthBS # pfromData pdecisionState'scriptIntegrityHash #== 32)
      ( (pencodeDefiniteArrayHeader # 4)
          <> (pencodeDefiniteBytes #$ Subject.pencodeVerdictSubject # pfromData pdecisionState'subject)
          <> (pencodeDefiniteBytes # pfromData pdecisionState'scriptIntegrityHash)
          <> (pserialiseData # pforgetData pdecisionState'containsNonNativeScript)
          <> (pserialiseData # pforgetData pdecisionState'hasRedeemers)
      )
      perror

pitemContainsNonNativeScript :: forall s. Term s (PByteString :--> PBool)
pitemContainsNonNativeScript = phoistAcyclic $ plam $ \item ->
  pmatch (pdecodeMidgardVersionedScriptAt # item # 0) $ \(PPair offset script) ->
    pmatch script $ \PMidgardVersionedScript{pversionedScript'language} ->
      pif
        (offset #== plengthBS # item)
        (pfromData pversionedScript'language #/= pcon PNativeCardanoScript)
        perror
