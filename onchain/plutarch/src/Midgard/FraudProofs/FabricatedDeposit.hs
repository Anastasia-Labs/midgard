module Midgard.FraudProofs.FabricatedDeposit (
  PStep01Args (..),
  PStep02State (..),
  PDepositEvidenceV1 (..),
  PDepositEvidenceVerdictV1 (..),
  PStep02Args (..),
  PStep03State (..),
  PAuthenticContentOpeningV1 (..),
  PStep03Args (..),
  PFabricatedDepositFaultV1 (..),
  PStep04State (..),
  PStep04Args (..),
  pfraudCategoryIsFabricatedDepositV1,
  pchallengedHeaderHashOfV1,
  pverifyChallengedHeaderV1,
  pverifyCommittedDepositMembershipV1,
  pcommittedDepositInfoHashV1,
  pverifyDepositEvidenceV1,
  popenAuthenticDepositContentV1,
  pfabricatedDepositFaultIsEstablishedV1,
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
import Midgard.LedgerState (PDepositEvent (..), PDepositId, PHeaderV1 (..))
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (PRootDomain (..), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.UserEvents (poutRefToNonce)
import Midgard.UserEvents.Deposit (PDepositDatum (..))

-- | Step 01 arguments, in Aiken declaration and wire order.
data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'inputIndex :: Term s (PAsData PInteger)
  , pstep01Args'outputIndex :: Term s (PAsData PInteger)
  , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'committedDeposit :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | Authenticated header facts and committed deposit carried into step 02.
data PStep02State (s :: S) = PStep02State
  { pstep02State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep02State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep02State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep02State'committedDepositId :: Term s (PAsData PDepositId)
  , pstep02State'committedDepositInfoHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PDepositEvidenceV1 (s :: S)
  = PAbsentDepositIdentity
      { pabsentDeposit'unspentRefInputIndex :: Term s (PAsData PInteger)
      }
  | PPresentDepositEvent
      { ppresentDeposit'hubRefInputIndex :: Term s (PAsData PInteger)
      , ppresentDeposit'eventRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDepositEvidenceV1)

data PDepositEvidenceVerdictV1 (s :: S)
  = PDepositIdentityAbsent
  | PDepositEventObserved
      { pdepositObserved'eventDatumHash :: Term s (PAsData PByteString)
      , pdepositObserved'eventInclusionTime :: Term s (PAsData PPosixTime)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDepositEvidenceVerdictV1)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'evidence :: Term s (PAsData PDepositEvidenceV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S) = PStep03State
  { pstep03State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep03State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep03State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep03State'committedDepositId :: Term s (PAsData PDepositId)
  , pstep03State'committedDepositInfoHash :: Term s (PAsData PByteString)
  , pstep03State'verdict :: Term s (PAsData PDepositEvidenceVerdictV1)
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

data PFabricatedDepositFaultV1 (s :: S)
  = PNonexistentDepositIdentity
  | PMismatchedDepositContent
      { pmismatchedDeposit'committedInfoHash :: Term s (PAsData PByteString)
      , pmismatchedDeposit'authenticInfoHash :: Term s (PAsData PByteString)
      , pmismatchedDeposit'eventInclusionTime :: Term s (PAsData PPosixTime)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFabricatedDepositFaultV1)

data PStep04State (s :: S) = PStep04State
  { pstep04State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep04State'headerStartTime :: Term s (PAsData PPosixTime)
  , pstep04State'headerEndTime :: Term s (PAsData PPosixTime)
  , pstep04State'committedDepositId :: Term s (PAsData PDepositId)
  , pstep04State'fault :: Term s (PAsData PFabricatedDepositFaultV1)
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

pfraudCategoryIsFabricatedDepositV1 :: forall s. Term s (PAsData PTokenName :--> PBool)
pfraudCategoryIsFabricatedDepositV1 = phoistAcyclic $ plam $ \assetName ->
  plet (pto $ pfromData assetName) $ \nameBytes ->
    (plengthBS # nameBytes #== pidByteCount + 28)
      #&& (psliceBS # 0 # pidByteCount # nameBytes #== phexByteStr "0000000b")

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
    (pfraudCategoryIsFabricatedDepositV1 # assetName)
    ( pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex) $ \PHubOracleDatum {phubOracle'stateQueue} ->
        pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $ \header headerHash ->
          pif
            (headerHash #== pchallengedHeaderHashOfV1 # assetName)
            (k header headerHash)
            perror
    )
    perror

pverifyCommittedDepositMembershipV1 ::
  forall s. Term s (PAsData PHeaderV1) -> Term s PRootMembershipProof -> Term s PBool
pverifyCommittedDepositMembershipV1 headerD membership = P.do
  PHeaderV1 {pheader'depositsRoot, pheader'depositCount} <- pmatch $ pfromData headerD
  pmatch membership $ \proof@PRootMembershipProof {prootMembership'key, prootMembership'value} ->
    pverifyRootMembershipWithBytes
      (pcon proof)
      (pdata $ pcon PDepositsRootDomain)
      (pfromData pheader'depositsRoot)
      (pfromData pheader'depositCount)
      (pserialiseData # prootMembership'key)
      (pserialiseData # prootMembership'value)

pcommittedDepositInfoHashV1 :: forall s. Term s PRootMembershipProof -> Term s PByteString
pcommittedDepositInfoHashV1 membership =
  pmatch membership $ \PRootMembershipProof {prootMembership'value} ->
    pblake2b_256 #$ pserialiseData # prootMembership'value

pverifyDepositEvidenceV1 ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PDepositId) ->
  Term s PDepositEvidenceV1 ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PDepositEvidenceVerdictV1
pverifyDepositEvidenceV1 hubOracle committedDepositId evidence referenceInputs =
  pmatch evidence $ \case
    PAbsentDepositIdentity {pabsentDeposit'unspentRefInputIndex} -> P.do
      PTxInInfo {ptxInInfo'outRef} <-
        pmatch $ pfromData $ pelemAt # pfromData pabsentDeposit'unspentRefInputIndex # referenceInputs
      pif
        (ptxInInfo'outRef #== pfromData committedDepositId)
        (pcon PDepositIdentityAbsent)
        perror
    PPresentDepositEvent {ppresentDeposit'hubRefInputIndex, ppresentDeposit'eventRefInputIndex} ->
      pmatch (Hub.pgetDatum # referenceInputs # hubOracle # pfromData ppresentDeposit'hubRefInputIndex) $ \PHubOracleDatum {phubOracle'deposit} -> P.do
        eventDatumData <-
          plet $
            pgetAuthenticInputDatumWithNftAt
              # referenceInputs
              # phubOracle'deposit
              # pdata (poutRefToNonce # committedDepositId)
              # pfromData ppresentDeposit'eventRefInputIndex
        PDepositDatum {pdepositDatum'event, pdepositDatum'inclusionTime} <-
          pmatch $ pfromData $ punsafeCoerce @(PAsData PDepositDatum) eventDatumData
        PDepositEvent {pdepositEvent'id} <- pmatch $ pfromData pdepositDatum'event
        pif
          (pdepositEvent'id #== committedDepositId)
          ( pcon $
              PDepositEventObserved
                (pdata $ pblake2b_256 #$ pserialiseData # eventDatumData)
                pdepositDatum'inclusionTime
          )
          perror

popenAuthenticDepositContentV1 ::
  forall s. Term s PStep03State -> Term s PAuthenticContentOpeningV1 -> Term s PFabricatedDepositFaultV1
popenAuthenticDepositContentV1 state opening = P.do
  PStep03State
    { pstep03State'committedDepositId
    , pstep03State'committedDepositInfoHash
    , pstep03State'verdict
    } <-
    pmatch state
  pmatch (pfromData pstep03State'verdict) $ \case
    PDepositIdentityAbsent -> pmatch opening $ \case
      PNoAuthenticContent -> pcon PNonexistentDepositIdentity
      _ -> perror
    PDepositEventObserved {pdepositObserved'eventDatumHash, pdepositObserved'eventInclusionTime} ->
      pmatch opening $ \case
        PRetainedEventDatum {pretainedEvent'datum} -> P.do
          PDepositDatum {pdepositDatum'event} <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PDepositDatum) pretainedEvent'datum
          PDepositEvent {pdepositEvent'id, pdepositEvent'info} <- pmatch $ pfromData pdepositDatum'event
          authenticInfoHash <- plet $ pblake2b_256 #$ pserialiseData # pforgetData pdepositEvent'info
          pif
            ( (pblake2b_256 #$ pserialiseData # pretainedEvent'datum)
                #== pfromData pdepositObserved'eventDatumHash
                #&& pdepositEvent'id
                #== pstep03State'committedDepositId
                #&& authenticInfoHash
                #/= pfromData pstep03State'committedDepositInfoHash
            )
            ( pcon $
                PMismatchedDepositContent
                  pstep03State'committedDepositInfoHash
                  (pdata authenticInfoHash)
                  pdepositObserved'eventInclusionTime
            )
            perror
        _ -> perror

pfabricatedDepositFaultIsEstablishedV1 :: forall s. Term s PStep04State -> Term s PBool
pfabricatedDepositFaultIsEstablishedV1 state = P.do
  PStep04State {pstep04State'headerStartTime, pstep04State'headerEndTime, pstep04State'fault} <- pmatch state
  pmatch (pfromData pstep04State'fault) $ \case
    PNonexistentDepositIdentity -> pconstant True
    PMismatchedDepositContent
      { pmismatchedDeposit'committedInfoHash
      , pmismatchedDeposit'authenticInfoHash
      , pmismatchedDeposit'eventInclusionTime
      } ->
        (pmismatchedDeposit'committedInfoHash #/= pmismatchedDeposit'authenticInfoHash)
          #&& (pfromData pstep04State'headerStartTime #< pfromData pmismatchedDeposit'eventInclusionTime)
          #&& (pfromData pmismatchedDeposit'eventInclusionTime #<= pfromData pstep04State'headerEndTime)
