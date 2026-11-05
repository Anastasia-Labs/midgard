module Midgard.FraudProofs.DoubleWithdraw (
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  pchallengedHeaderHashOfV1,
  pverifyChallengedHeaderV1,
  pverifyCommittedWithdrawalMembershipV1,
  pdoubleWithdrawFaultIsEstablishedV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PScriptHash, PTokenName, PTxInInfo, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProof (passetNameToHeaderHash)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.HubOracle qualified as Hub
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.LedgerState (
  PHeaderV1 (..),
  PWithdrawalBody (..),
  PWithdrawalId,
  PWithdrawalInfo (..),
  PWithdrawalValidity (..),
 )
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (
  PRootDomain (..),
  PRootMembershipProof (..),
  pverifyRootMembershipWithBytes,
 )

-- | Aiken @double_withdraw/step_01.Args@, in declaration/wire order.
data PStep01Args s = PStep01Args
  { pstep01Args'inputIndex :: Term s (PAsData PInteger)
  , pstep01Args'outputIndex :: Term s (PAsData PInteger)
  , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'committedWithdrawal :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | Fixed-size handoff from the first committed payable leaf.
data PStep02State s = PStep02State
  { pstep02State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep02State'firstWithdrawalId :: Term s (PAsData PWithdrawalId)
  , pstep02State'firstL2Outref :: Term s (PAsData PTxOutRef)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @double_withdraw/step_02.Args@, in declaration/wire order.
data PStep02Args s = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'hubRefInputIndex :: Term s (PAsData PInteger)
  , pstep02Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pstep02Args'committedWithdrawal :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | The 28-byte challenged-header suffix of a computation-thread token name.
pchallengedHeaderHashOfV1 :: forall s. Term s (PAsData PTokenName :--> PByteString)
pchallengedHeaderHashOfV1 = passetNameToHeaderHash

-- | Authenticate the header through hub policy, state-queue node, and token name.
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
  plet (pto (pfromData assetName)) $ \assetNameBytes ->
    pif
      (plengthBS # assetNameBytes #== pidByteCount + 28)
      ( pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex) $
          \PHubOracleDatum {phubOracle'stateQueue} ->
            pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $
              \header headerHash ->
                pif
                  (headerHash #== pchallengedHeaderHashOfV1 # assetName)
                  (k header headerHash)
                  perror
      )
      perror

-- | Verify one committed withdrawal against the header's counted root.
pverifyCommittedWithdrawalMembershipV1 ::
  forall s. Term s (PAsData PHeaderV1) -> Term s PRootMembershipProof -> Term s PBool
pverifyCommittedWithdrawalMembershipV1 headerD membership = P.do
  PHeaderV1 {pheader'withdrawalsRoot, pheader'withdrawalCount} <- pmatch $ pfromData headerD
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  pverifyRootMembershipWithBytes
    membership
    (pdata $ pcon PWithdrawalsRootDomain)
    (pfromData pheader'withdrawalsRoot)
    (pfromData pheader'withdrawalCount)
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)

-- | Two distinct payable withdrawal events drain the same L2 output.
pdoubleWithdrawFaultIsEstablishedV1 ::
  forall s. Term s PStep02State -> Term s PRootMembershipProof -> Term s PBool
pdoubleWithdrawFaultIsEstablishedV1 state second = P.do
  PStep02State {pstep02State'firstWithdrawalId, pstep02State'firstL2Outref} <- pmatch state
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch second
  PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
    pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalInfo) prootMembership'value
  PWithdrawalBody {pwithdrawalBody'l2Outref} <- pmatch $ pfromData pwithdrawalInfo'body
  (pnot # (prootMembership'key #== pforgetData pstep02State'firstWithdrawalId))
    #&& (pwithdrawalBody'l2Outref #== pforgetData pstep02State'firstL2Outref)
    #&& pmatch (pfromData pwithdrawalInfo'validity) (\case PWithdrawalIsValid -> pconstant True; _ -> pconstant False)
