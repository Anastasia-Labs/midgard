module Midgard.FraudProofs.CrossBlockDuplicateEvent (
  PDuplicateEventKindV1 (..),
  PCommittedDuplicateEventProofV1 (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  pfraudCategoryIsCrossBlockDuplicateEventV1,
  pchallengedHeaderHashOfV1,
  pverifyChallengedHeaderV1,
  pverifyCommittedEventMembershipV1,
  pcommittedEventKindV1,
  pcommittedEventKeyV1,
  pverifyConfirmedDuplicateV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptHash, PTokenName, PTxInInfo, PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetAuthenticInputDatumAndAssetNameWithPolicyAt)
import Midgard.FraudProof (passetNameToHeaderHash)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.MpfProof (phasValueHash)
import Midgard.Settlement (PSettlementDatum (..))
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (PRootCountProof (..), PRootDomain (..), PRootMembershipProof (..), pverifyRootCountProof, pverifyRootMembershipWithBytes)

-- | Aiken @cross_block_duplicate_event/step_01.DuplicateEventKindV1@.
data PDuplicateEventKindV1 (s :: S)
  = PDuplicateDepositV1
  | PDuplicateWithdrawalV1
  | PDuplicateForcedTransactionV1
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDuplicateEventKindV1)

-- | The challenged membership proof, tagged by its counted source-root domain.
data PCommittedDuplicateEventProofV1 (s :: S)
  = PCommittedDuplicateDepositV1
      { pcommittedDuplicateDeposit'membership :: Term s (PAsData PRootMembershipProof)
      }
  | PCommittedDuplicateWithdrawalV1
      { pcommittedDuplicateWithdrawal'membership :: Term s (PAsData PRootMembershipProof)
      }
  | PCommittedDuplicateForcedTransactionV1
      { pcommittedDuplicateForced'membership :: Term s (PAsData PRootMembershipProof)
      }
  | PCommittedDuplicateEventDigestV1
      { pcommittedDuplicateDigest'eventKind :: Term s (PAsData PDuplicateEventKindV1)
      , pcommittedDuplicateDigest'membership :: Term s (PAsData PRootMembershipProof)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommittedDuplicateEventProofV1)

-- | Aiken step-01 arguments, in declaration/wire order.
data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'inputIndex :: Term s (PAsData PInteger)
  , pstep01Args'outputIndex :: Term s (PAsData PInteger)
  , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , pstep01Args'committedEvent :: Term s (PAsData PCommittedDuplicateEventProofV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | Authenticated facts carried from step 01 to the confirmed-history step.
data PStep02State (s :: S) = PStep02State
  { pstep02State'challengedHeaderHash :: Term s (PAsData PByteString)
  , pstep02State'settlementPolicyId :: Term s (PAsData PCurrencySymbol)
  , pstep02State'eventKind :: Term s (PAsData PDuplicateEventKindV1)
  , pstep02State'eventKey :: Term s (PAsData PTxOutRef)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken step-02 arguments, in declaration/wire order.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'settlementRefInputIndex :: Term s (PAsData PInteger)
  , pstep02Args'settledEvent :: Term s (PAsData PCommittedDuplicateEventProofV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

{- | Internal selection result. Scott encoding keeps the three tag branches
small; the expensive MPF verifier is applied once after domain selection.
-}
data PEventMembershipBinding (s :: S) = PEventMembershipBinding
  { peventBinding'membership :: Term s (PAsData PRootMembershipProof)
  , peventBinding'domain :: Term s (PAsData PRootDomain)
  , peventBinding'root :: Term s PByteString
  , peventBinding'count :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PEventMembershipBinding)

-- | Reserved catalogue id @00000016@ followed by one 28-byte header hash.
pfraudCategoryIsCrossBlockDuplicateEventV1 ::
  forall s. Term s (PAsData PTokenName :--> PBool)
pfraudCategoryIsCrossBlockDuplicateEventV1 = phoistAcyclic $ plam $ \assetName ->
  plet (pto $ pfromData assetName) $ \nameBytes ->
    (plengthBS # nameBytes #== pidByteCount + 28)
      #&& (psliceBS # 0 # pidByteCount # nameBytes #== phexByteStr "00000016")

pchallengedHeaderHashOfV1 :: forall s. Term s (PAsData PTokenName :--> PByteString)
pchallengedHeaderHashOfV1 = passetNameToHeaderHash

-- | Authenticate the challenged header through the hub and state queue.
pverifyChallengedHeaderV1 ::
  forall (s :: S) (r :: S -> Type).
  Term s (PAsData PTokenName) ->
  Term s (PAsData PScriptHash) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  (Term s (PAsData PHeaderV1) -> Term s PByteString -> Term s PHubOracleDatum -> Term s r) ->
  Term s r
pverifyChallengedHeaderV1 assetName hubOracle hubRefInputIndex stateQueueNodeRefInputIndex referenceInputs k =
  pif
    (pfraudCategoryIsCrossBlockDuplicateEventV1 # assetName)
    ( pmatch (Hub.pgetDatum # referenceInputs # hubOracle # hubRefInputIndex) $ \hubDatum@PHubOracleDatum {phubOracle'stateQueue} ->
        pgetBlockDatumV1 referenceInputs phubOracle'stateQueue stateQueueNodeRefInputIndex $ \header headerHash ->
          pif
            (headerHash #== pchallengedHeaderHashOfV1 # assetName)
            (k header headerHash (pcon hubDatum))
            perror
    )
    perror

pverifyMembership ::
  forall s.
  Term s (PAsData PRootMembershipProof) ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyMembership membershipD domain root count = P.do
  membership@PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch $ pfromData membershipD
  pverifyRootMembershipWithBytes
    (pcon membership)
    domain
    root
    count
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)

-- The digest is already the MPF value hash; serialising or hashing it again
-- would authenticate a different leaf.
pverifyDigestMembership ::
  forall s.
  Term s (PAsData PRootMembershipProof) ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyDigestMembership membershipD domain root count = P.do
  PRootMembershipProof
    { prootMembership'domain
    , prootMembership'root
    , prootMembership'phasRoot
    , prootMembership'count
    , prootMembership'key
    , prootMembership'value
    , prootMembership'proof
    } <-
    pmatch $ pfromData membershipD
  (count #> 0)
    #&& pverifyRootCountProof
      (pcon $ PRootCountProof prootMembership'domain prootMembership'root prootMembership'phasRoot prootMembership'count)
      domain
      root
      count
    #&& ( phasValueHash
            # pfromData prootMembership'phasRoot
            # (pserialiseData # prootMembership'key)
            # (pfromData $ punsafeCoerce @(PAsData PByteString) prootMembership'value)
            # pfromData prootMembership'proof
        )

pverifySelectedMembership ::
  forall s.
  Term s PCommittedDuplicateEventProofV1 ->
  Term s (PAsData PRootMembershipProof) ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifySelectedMembership event membership domain root count = pmatch event $ \case
  PCommittedDuplicateEventDigestV1 _ _ -> pverifyDigestMembership membership domain root count
  _ -> pverifyMembership membership domain root count

-- | Open the challenged leaf under the counted root selected by its event tag.
pverifyCommittedEventMembershipV1 ::
  forall s. Term s (PAsData PHeaderV1) -> Term s PCommittedDuplicateEventProofV1 -> Term s PBool
pverifyCommittedEventMembershipV1 headerD committedEvent = P.do
  PHeaderV1
    { pheader'depositsRoot
    , pheader'depositCount
    , pheader'withdrawalsRoot
    , pheader'withdrawalCount
    , pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    } <-
    pmatch $ pfromData headerD
  binding <- plet $ pmatch committedEvent $ \case
    PCommittedDuplicateDepositV1 membership ->
      pcon $
        PEventMembershipBinding
          membership
          (pdata $ pcon PDepositsRootDomain)
          (pfromData pheader'depositsRoot)
          (pfromData pheader'depositCount)
    PCommittedDuplicateWithdrawalV1 membership ->
      pcon $
        PEventMembershipBinding
          membership
          (pdata $ pcon PWithdrawalsRootDomain)
          (pfromData pheader'withdrawalsRoot)
          (pfromData pheader'withdrawalCount)
    PCommittedDuplicateForcedTransactionV1 membership ->
      pcon $
        PEventMembershipBinding
          membership
          (pdata $ pcon PForcedTransactionsV1RootDomain)
          (pfromData pheader'forcedTransactionsRoot)
          (pfromData pheader'forcedTransactionCount)
    PCommittedDuplicateEventDigestV1 kind membership -> pmatch (pfromData kind) $ \case
      PDuplicateDepositV1 ->
        pcon $
          PEventMembershipBinding
            membership
            (pdata $ pcon PDepositsRootDomain)
            (pfromData pheader'depositsRoot)
            (pfromData pheader'depositCount)
      PDuplicateWithdrawalV1 ->
        pcon $
          PEventMembershipBinding
            membership
            (pdata $ pcon PWithdrawalsRootDomain)
            (pfromData pheader'withdrawalsRoot)
            (pfromData pheader'withdrawalCount)
      PDuplicateForcedTransactionV1 ->
        pcon $
          PEventMembershipBinding
            membership
            (pdata $ pcon PForcedTransactionsV1RootDomain)
            (pfromData pheader'forcedTransactionsRoot)
            (pfromData pheader'forcedTransactionCount)
  PEventMembershipBinding membership domain root count <- pmatch binding
  pverifySelectedMembership committedEvent membership domain root count

pcommittedEventKindV1 :: forall s. Term s PCommittedDuplicateEventProofV1 -> Term s (PAsData PDuplicateEventKindV1)
pcommittedEventKindV1 committedEvent = pmatch committedEvent $ \case
  PCommittedDuplicateDepositV1 _ -> pdata $ pcon PDuplicateDepositV1
  PCommittedDuplicateWithdrawalV1 _ -> pdata $ pcon PDuplicateWithdrawalV1
  PCommittedDuplicateForcedTransactionV1 _ -> pdata $ pcon PDuplicateForcedTransactionV1
  PCommittedDuplicateEventDigestV1 kind _ -> kind

pcommittedEventKeyV1 :: forall s. Term s PCommittedDuplicateEventProofV1 -> Term s (PAsData PTxOutRef)
pcommittedEventKeyV1 committedEvent = pmatch committedEvent $ \case
  PCommittedDuplicateDepositV1 membership -> pkey membership
  PCommittedDuplicateWithdrawalV1 membership -> pkey membership
  PCommittedDuplicateForcedTransactionV1 membership -> pkey membership
  PCommittedDuplicateEventDigestV1 _ membership -> pkey membership
  where
    pkey membership = pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'key} ->
      punsafeCoerce @(PAsData PTxOutRef) prootMembership'key

-- | Prove that confirmed settlement history commits the same key and domain.
pverifyConfirmedDuplicateV1 ::
  forall s.
  Term s PStep02State ->
  Term s PInteger ->
  Term s PCommittedDuplicateEventProofV1 ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PByteString
pverifyConfirmedDuplicateV1 state settlementRefInputIndex settledEvent referenceInputs = P.do
  PStep02State
    { pstep02State'challengedHeaderHash
    , pstep02State'settlementPolicyId
    , pstep02State'eventKind
    , pstep02State'eventKey
    } <-
    pmatch state
  pgetAuthenticInputDatumAndAssetNameWithPolicyAt
    referenceInputs
    pstep02State'settlementPolicyId
    settlementRefInputIndex
    $ \settledHeaderName settlementDatumData ->
      plet (pto $ pfromData settledHeaderName) $ \settledHeaderHash ->
        pif
          ( (plengthBS # settledHeaderHash #== 28)
              #&& (settledHeaderHash #/= pfromData pstep02State'challengedHeaderHash)
          )
          ( P.do
              PSettlementDatum
                { psettlement'depositsRoot
                , psettlement'withdrawalsRoot
                , psettlement'forcedTransactionsRoot
                } <-
                pmatch $ pfromData $ punsafeCoerce @(PAsData PSettlementDatum) settlementDatumData
              binding <- plet $ pmatch (pfromData pstep02State'eventKind) $ \case
                PDuplicateDepositV1 -> pmatch settledEvent $ \case
                  PCommittedDuplicateDepositV1 membership ->
                    pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                      pcon $
                        PEventMembershipBinding
                          membership
                          (pdata $ pcon PDepositsRootDomain)
                          (pfromData psettlement'depositsRoot)
                          (pfromData prootMembership'count)
                  PCommittedDuplicateEventDigestV1 kind membership ->
                    pif
                      (kind #== pstep02State'eventKind)
                      ( pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                          pcon $
                            PEventMembershipBinding
                              membership
                              (pdata $ pcon PDepositsRootDomain)
                              (pfromData psettlement'depositsRoot)
                              (pfromData prootMembership'count)
                      )
                      perror
                  _ -> perror
                PDuplicateWithdrawalV1 -> pmatch settledEvent $ \case
                  PCommittedDuplicateWithdrawalV1 membership ->
                    pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                      pcon $
                        PEventMembershipBinding
                          membership
                          (pdata $ pcon PWithdrawalsRootDomain)
                          (pfromData psettlement'withdrawalsRoot)
                          (pfromData prootMembership'count)
                  PCommittedDuplicateEventDigestV1 kind membership ->
                    pif
                      (kind #== pstep02State'eventKind)
                      ( pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                          pcon $
                            PEventMembershipBinding
                              membership
                              (pdata $ pcon PWithdrawalsRootDomain)
                              (pfromData psettlement'withdrawalsRoot)
                              (pfromData prootMembership'count)
                      )
                      perror
                  _ -> perror
                PDuplicateForcedTransactionV1 -> pmatch settledEvent $ \case
                  PCommittedDuplicateForcedTransactionV1 membership ->
                    pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                      pcon $
                        PEventMembershipBinding
                          membership
                          (pdata $ pcon PForcedTransactionsV1RootDomain)
                          (pfromData psettlement'forcedTransactionsRoot)
                          (pfromData prootMembership'count)
                  PCommittedDuplicateEventDigestV1 kind membership ->
                    pif
                      (kind #== pstep02State'eventKind)
                      ( pmatch (pfromData membership) $ \PRootMembershipProof {prootMembership'count} ->
                          pcon $
                            PEventMembershipBinding
                              membership
                              (pdata $ pcon PForcedTransactionsV1RootDomain)
                              (pfromData psettlement'forcedTransactionsRoot)
                              (pfromData prootMembership'count)
                      )
                      perror
                  _ -> perror
              PEventMembershipBinding membership domain root count <- pmatch binding
              PRootMembershipProof {prootMembership'key} <- pmatch $ pfromData membership
              pif
                ( prootMembership'key
                    #== pforgetData pstep02State'eventKey
                    #&& pverifySelectedMembership settledEvent membership domain root count
                )
                settledHeaderHash
                perror
          )
          perror
