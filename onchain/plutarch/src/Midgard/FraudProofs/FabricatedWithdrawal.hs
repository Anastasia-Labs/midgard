module Midgard.FraudProofs.FabricatedWithdrawal (
  PStep01Args (..),
  PStep02State (..),
  PWithdrawalEvidenceV1 (..),
  PWithdrawalEvidenceVerdictV1 (..),
  PStep02Args (..),
  PStep03State (..),
  PAuthenticContentOpeningV1 (..),
  PStep03Args (..),
  PFabricatedWithdrawalFaultV1 (..),
  PStep04State (..),
  PStep04Args (..),
  pfraudCategoryIsFabricatedWithdrawalV1,
  pchallengedHeaderHashOfV1,
  pverifyChallengedHeaderV1,
  pverifyCommittedWithdrawalMembershipV1,
  pcommittedWithdrawalInfoHashV1,
  pverifyWithdrawalEvidenceV1,
  popenAuthenticWithdrawalContentV1,
  pfabricatedWithdrawalFaultIsEstablishedV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PScriptHash, PTokenName, PTxInInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithNftAt)
import Midgard.FraudProof (passetNameToHeaderHash)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (PHeaderV1 (..), PWithdrawalEvent (..), PWithdrawalId)
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (PRootDomain (..), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.UserEvents (poutRefToNonce)
import Midgard.UserEvents.Withdrawal (PWithdrawalDatum (..))

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'inputIndex :: Term s (PAsData PInteger)
  , pstep01Args'outputIndex :: Term s (PAsData PInteger)
  , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'committedWithdrawal :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
  { pstep02State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep02State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep02State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep02State'committedWithdrawalId :: Term s (PAsData PWithdrawalId)
  , pstep02State'committedWithdrawalInfoHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PWithdrawalEvidenceV1 (s :: S)
  = PAbsentWithdrawalIdentity
      { pabsentWithdrawal'unspentRefInputIndex :: Term s (PAsData PInteger)
      }
  | PPresentWithdrawalEvent
      { ppresentWithdrawal'hubRefInputIndex :: Term s (PAsData PInteger)
      , ppresentWithdrawal'eventRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalEvidenceV1)

data PWithdrawalEvidenceVerdictV1 (s :: S)
  = PWithdrawalIdentityAbsent
  | PWithdrawalEventObserved
      { pwithdrawalObserved'eventDatumHash :: Term s (PAsData PByteString)
      , pwithdrawalObserved'eventInclusionTime :: Term s (PAsData PPosixTime)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalEvidenceVerdictV1)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'evidence :: Term s (PAsData PWithdrawalEvidenceV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S) = PStep03State
  { pstep03State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep03State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep03State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep03State'committedWithdrawalId :: Term s (PAsData PWithdrawalId)
  , pstep03State'committedWithdrawalInfoHash :: Term s (PAsData PByteString)
  , pstep03State'verdict :: Term s (PAsData PWithdrawalEvidenceVerdictV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PAuthenticContentOpeningV1 (s :: S)
  = PNoAuthenticContent
  | PRetainedEventDatum
      { pretainedEvent'datum :: Term s PData
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticContentOpeningV1)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'authenticContent :: Term s (PAsData PAuthenticContentOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PFabricatedWithdrawalFaultV1 (s :: S)
  = PNonexistentWithdrawalIdentity
  | PMismatchedWithdrawalContent
      { pmismatchedWithdrawal'committedInfoHash :: Term s (PAsData PByteString)
      , pmismatchedWithdrawal'authenticInfoHash :: Term s (PAsData PByteString)
      , pmismatchedWithdrawal'eventInclusionTime :: Term s (PAsData PPosixTime)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFabricatedWithdrawalFaultV1)

data PStep04State (s :: S) = PStep04State
  { pstep04State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep04State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep04State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep04State'committedWithdrawalId :: Term s (PAsData PWithdrawalId)
  , pstep04State'fault :: Term s (PAsData PFabricatedWithdrawalFaultV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

pfraudCategoryIsFabricatedWithdrawalV1 :: forall s. Term s (PAsData PTokenName :--> PBool)
pfraudCategoryIsFabricatedWithdrawalV1 = phoistAcyclic $ plam $ \assetName ->
  plet (pto $ pfromData assetName) $ \nameBytes ->
    (plengthBS # nameBytes #== pidByteCount + 28)
      #&& (psliceBS # 0 # pidByteCount # nameBytes #== phexByteStr "0000000c")

pchallengedHeaderHashOfV1 :: forall s. Term s (PAsData PTokenName :--> PByteString)
pchallengedHeaderHashOfV1 = passetNameToHeaderHash

pverifyChallengedHeaderV1 ::
  forall (s :: S) (r :: S -> Type).
  Term s (PAsData PTokenName) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  (Term s (PAsData PHeaderV1) -> Term s PByteString -> Term s r) ->
  Term s r
pverifyChallengedHeaderV1 assetName hubOracle hubRefInputIndex stateQueueNodeRefInputIndex referenceInputs k =
  pif
    (pfraudCategoryIsFabricatedWithdrawalV1 # assetName)
    ( pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex) $ \PHubOracleDatum {phubOracle'stateQueue} ->
        pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $ \header headerHash ->
          pif
            (headerHash #== pchallengedHeaderHashOfV1 # assetName)
            (k header headerHash)
            perror
    )
    perror

pverifyCommittedWithdrawalMembershipV1 ::
  forall s. Term s (PAsData PHeaderV1) -> Term s PRootMembershipProof -> Term s PBool
pverifyCommittedWithdrawalMembershipV1 headerD membership = P.do
  PHeaderV1 {pheader'withdrawalsRoot, pheader'withdrawalCount} <- pmatch $ pfromData headerD
  pmatch membership $ \proof@PRootMembershipProof {prootMembership'key, prootMembership'value} ->
    pverifyRootMembershipWithBytes
      (pcon proof)
      (pdata $ pcon PWithdrawalsRootDomain)
      (pfromData pheader'withdrawalsRoot)
      (pfromData pheader'withdrawalCount)
      (pserialiseData # prootMembership'key)
      (pserialiseData # prootMembership'value)

pcommittedWithdrawalInfoHashV1 :: forall s. Term s PRootMembershipProof -> Term s PByteString
pcommittedWithdrawalInfoHashV1 membership =
  pmatch membership $ \PRootMembershipProof {prootMembership'value} ->
    pblake2b_256 #$ pserialiseData # prootMembership'value

pverifyWithdrawalEvidenceV1 ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PWithdrawalId) ->
  Term s PWithdrawalEvidenceV1 ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PWithdrawalEvidenceVerdictV1
pverifyWithdrawalEvidenceV1 hubOracle committedWithdrawalId evidence referenceInputs =
  pmatch evidence $ \case
    PAbsentWithdrawalIdentity {pabsentWithdrawal'unspentRefInputIndex} -> P.do
      PTxInInfo {ptxInInfo'outRef} <-
        pmatch $ pfromData $ pelemAt # pfromData pabsentWithdrawal'unspentRefInputIndex # referenceInputs
      pif
        (ptxInInfo'outRef #== pfromData committedWithdrawalId)
        (pcon PWithdrawalIdentityAbsent)
        perror
    PPresentWithdrawalEvent {ppresentWithdrawal'hubRefInputIndex, ppresentWithdrawal'eventRefInputIndex} ->
      pmatch (Hub.pgetDatum # referenceInputs # hubOracle # pfromData ppresentWithdrawal'hubRefInputIndex) $ \PHubOracleDatum {phubOracle'withdrawal} -> P.do
        eventDatumData <-
          plet $
            pgetAuthenticInputDatumWithNftAt
              # referenceInputs
              # phubOracle'withdrawal
              # pdata (poutRefToNonce # committedWithdrawalId)
              # pfromData ppresentWithdrawal'eventRefInputIndex
        PWithdrawalDatum {pwithdrawalDatum'event, pwithdrawalDatum'inclusionTime} <-
          pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalDatum) eventDatumData
        PWithdrawalEvent {pwithdrawalEvent'id} <- pmatch $ pfromData pwithdrawalDatum'event
        pif
          (pwithdrawalEvent'id #== committedWithdrawalId)
          ( pcon $
              PWithdrawalEventObserved
                (pdata $ pblake2b_256 #$ pserialiseData # eventDatumData)
                pwithdrawalDatum'inclusionTime
          )
          perror

popenAuthenticWithdrawalContentV1 ::
  forall s. Term s PStep03State -> Term s PAuthenticContentOpeningV1 -> Term s PFabricatedWithdrawalFaultV1
popenAuthenticWithdrawalContentV1 state opening = P.do
  PStep03State
    { pstep03State'committedWithdrawalId
    , pstep03State'committedWithdrawalInfoHash
    , pstep03State'verdict
    } <-
    pmatch state
  pmatch (pfromData pstep03State'verdict) $ \case
    PWithdrawalIdentityAbsent -> pmatch opening $ \case
      PNoAuthenticContent -> pcon PNonexistentWithdrawalIdentity
      PRetainedEventDatum {} -> perror
    PWithdrawalEventObserved {pwithdrawalObserved'eventDatumHash, pwithdrawalObserved'eventInclusionTime} ->
      pmatch opening $ \case
        PRetainedEventDatum {pretainedEvent'datum} -> P.do
          PWithdrawalDatum {pwithdrawalDatum'event} <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalDatum) pretainedEvent'datum
          PWithdrawalEvent {pwithdrawalEvent'id, pwithdrawalEvent'info} <- pmatch $ pfromData pwithdrawalDatum'event
          authenticInfoHash <- plet $ pblake2b_256 #$ pserialiseData # pforgetData pwithdrawalEvent'info
          pif
            ( (pblake2b_256 #$ pserialiseData # pretainedEvent'datum)
                #== pfromData pwithdrawalObserved'eventDatumHash
                #&& pwithdrawalEvent'id
                #== pstep03State'committedWithdrawalId
                #&& authenticInfoHash
                #/= pfromData pstep03State'committedWithdrawalInfoHash
            )
            ( pcon $
                PMismatchedWithdrawalContent
                  pstep03State'committedWithdrawalInfoHash
                  (pdata authenticInfoHash)
                  pwithdrawalObserved'eventInclusionTime
            )
            perror
        PNoAuthenticContent -> perror

pfabricatedWithdrawalFaultIsEstablishedV1 :: forall s. Term s PStep04State -> Term s PBool
pfabricatedWithdrawalFaultIsEstablishedV1 state = P.do
  PStep04State {pstep04State'headerStartTime, pstep04State'headerEndTime, pstep04State'fault} <- pmatch state
  pmatch (pfromData pstep04State'fault) $ \case
    PNonexistentWithdrawalIdentity -> pconstant True
    PMismatchedWithdrawalContent
      { pmismatchedWithdrawal'committedInfoHash
      , pmismatchedWithdrawal'authenticInfoHash
      , pmismatchedWithdrawal'eventInclusionTime
      } ->
        (pmismatchedWithdrawal'committedInfoHash #/= pmismatchedWithdrawal'authenticInfoHash)
          #&& (pfromData pstep04State'headerStartTime #< pfromData pmismatchedWithdrawal'eventInclusionTime)
          #&& (pfromData pmismatchedWithdrawal'eventInclusionTime #<= pfromData pstep04State'headerEndTime)
