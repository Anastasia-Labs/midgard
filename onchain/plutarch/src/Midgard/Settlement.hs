{- |
Module      : Midgard.Settlement
Description : Partial Plutarch port of @lib/midgard/settlement.ak@.

Only the spend redeemer is ported so far: the operator directory reads it out of
the transaction when slashing an operator whose resolution claim was disproved.
-}
module Midgard.Settlement (
  PSpendRedeemer (..),
  PResolutionClaim (..),
  PSettlementDatum (..),
  PMintRedeemer (..),
  pdecodeMintRedeemer,
  PEventType (..),
  PEventMembershipProof (..),
  pvalidEventInclusion,
  pgetDatum,
  pvalidCountedMembership,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (pasByteStr, pasConstr, pasInt, pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PTokenName (..),
  POutputDatum (..),
  PPubKeyHash,
  PRedeemer,
  PScriptPurpose,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PMerkleRoot, PPosixTime)
import Midgard.LedgerState (
  PMidgardTxValidity,
  PWithdrawalInfo (..),
  PWithdrawalValidity,
  punsafeEventToKeyValuePair,
 )
import Midgard.UserEvents.TxOrder (pforcedInclusionKeyValue)
import Midgard.Common.Utils (
  pgetAuthenticInputWithNftAt,
  pgetAuthenticInputWithPolicyAt,
  pplutarchPhasRaw,
 )
import Midgard.TransitionTrace (
  PRootCountProof (..),
  PRootDomain (..),
  PRootMembershipProof (..),
  pverifyRootCountProof,
 )

{- | Aiken @settlement.SpendRedeemer@.

Constructor order fixes the tag: @AttachResolutionClaim@ 0,
@DisproveResolutionClaim@ 1, @Resolve@ 2. The directory's slashing path matches
on tag 1.

As with "Midgard.StateQueue", fields this consumer does not read stay 'PData' —
giving them real types would pull in @EventType@, @EventMembershipProof@ and the
transition-trace proof types. The same caveat applies: Aiken's @expect@ would
validate the whole redeemer, and the settlement validator does so in the same
transaction.
-}
data PSpendRedeemer (s :: S)
  = PAttachResolutionClaim
      { pstlAttach'settlementInputIndex :: Term s (PAsData PInteger)
      , pstlAttach'settlementOutputIndex :: Term s (PAsData PInteger)
      , pstlAttach'hubRefInputIndex :: Term s (PAsData PInteger)
      , pstlAttach'activeOperatorsNodeInputIndex :: Term s (PAsData PInteger)
      , pstlAttach'activeOperatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pstlAttach'operator :: Term s (PAsData PPubKeyHash)
      , pstlAttach'schedulerRefInputIndex :: Term s (PAsData PInteger)
      }
  | PDisproveResolutionClaim
      { pstlDisprove'settlementInputIndex :: Term s (PAsData PInteger)
      , pstlDisprove'settlementOutputIndex :: Term s (PAsData PInteger)
      , pstlDisprove'hubRefInputIndex :: Term s (PAsData PInteger)
      , pstlDisprove'operatorsRedeemerIndex :: Term s (PAsData PInteger)
      , pstlDisprove'operator :: Term s (PAsData PPubKeyHash)
      , pstlDisprove'operatorIsActive :: Term s (PAsData PBool)
      , pstlDisprove'unresolvedEventRefInputIndex :: Term s (PAsData PInteger)
      , pstlDisprove'unresolvedEventAssetName :: Term s (PAsData PTokenName)
      , pstlDisprove'eventType :: Term s (PAsData PEventType)
      , pstlDisprove'membershipProof :: Term s (PAsData PEventMembershipProof)
      , pstlDisprove'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
      }
  | PResolve {pstlResolve'settlementId :: Term s (PAsData PTokenName)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

-- | Aiken @settlement.ResolutionClaim@.
data PResolutionClaim (s :: S) = PResolutionClaim
  { presolutionClaim'resolutionTime :: Term s (PAsData PPosixTime)
  , presolutionClaim'operator :: Term s (PAsData PPubKeyHash)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolutionClaim)

{- | Aiken @settlement.Datum@.

The four roots a settlement commits to, plus the operator's resolution claim
once attached. Each root is a /counted/ root — see "Midgard.TransitionTrace".
-}
data PSettlementDatum (s :: S) = PSettlementDatum
  { psettlement'depositsRoot :: Term s (PAsData PMerkleRoot)
  , psettlement'withdrawalsRoot :: Term s (PAsData PMerkleRoot)
  , psettlement'forcedTransactionsRoot :: Term s (PAsData PMerkleRoot)
  , psettlement'transactionsRoot :: Term s (PAsData PMerkleRoot)
  , psettlement'resolutionClaim :: Term s (PMaybeData PResolutionClaim)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSettlementDatum)

{- | Aiken @settlement.get_datum@.

Reads a settlement's datum from a reference input authenticated by its NFT
policy. The asset name is not pinned — a settlement's name identifies which
settlement it is, and the caller does not know it in advance.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PSettlementDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs settlementPolicyId settlementRefInputIndex -> P.do
    PTxInInfo {ptxInInfo'resolved} <-
      pmatch $
        pfromData
          ( pgetAuthenticInputWithPolicyAt
              # referenceInputs
              # settlementPolicyId
              # settlementRefInputIndex
          )
    PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
    pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} ->
        pfromData (punsafeCoerce @(PAsData PSettlementDatum) (pto poutputDatum'outputDatum))
      _ -> perror

{- | Aiken @settlement.valid_counted_membership@.

Proves that @(key, value)@ is in the tree a settlement committed to under
@expected_root@. Three things have to line up, and each closes a different gap:

  * the witness's key and value must serialise to the ones being claimed, so the
    proof cannot be for some other entry;
  * the counted-root proof must reconstruct @expected_root@ from the witness's
    raw MPF root and count, so a tree of a different size cannot be substituted;
  * the @phas@ staking validator must be running in this transaction over
    exactly that raw root, key, value and proof, which is what actually verifies
    Merkle membership.

@count > 0@ is required outright: an empty tree contains nothing, so a
membership proof against one is meaningless.
-}
pvalidCountedMembership ::
  forall (s :: S).
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PRootMembershipProof ->
  Term s PData ->
  Term s PData ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pvalidCountedMembership expectedDomain expectedRoot witness key value redeemers = P.do
  PRootMembershipProof
    { prootMembership'domain
    , prootMembership'root
    , prootMembership'phasRoot
    , prootMembership'count
    , prootMembership'key
    , prootMembership'value
    , prootMembership'proof
    } <-
    pmatch witness
  keyBytes <- plet $ pserialiseData # key
  valueBytes <- plet $ pserialiseData # value
  count <- plet $ pfromData prootMembership'count
  phasRoot <- plet $ pfromData prootMembership'phasRoot
  -- Aiken's `and { .. }` short-circuits, and the last conjunct *errors* on
  -- mismatch rather than returning False, so the order is load-bearing: a
  -- malformed claim must be rejected by an earlier check before the phas
  -- delegation is reached. `#&&` is lazy in its second argument; `pand'List`
  -- would not be.
  count
    #> 0
    #&& prootMembership'key
    #== pforgetData (pdata keyBytes)
    #&& prootMembership'value
    #== pforgetData (pdata valueBytes)
    #&& pverifyRootCountProof
        ( pcon
            ( PRootCountProof
                { prootCount'domain = prootMembership'domain
                , prootCount'root = prootMembership'root
                , prootCount'phasRoot = pdata phasRoot
                , prootCount'count = prootMembership'count
                }
            )
        )
      expectedDomain
      expectedRoot
      count
    #&& pplutarchPhasRaw
      phasRoot
      keyBytes
      valueBytes
      (pforgetData prootMembership'proof)
      redeemers

{- | Aiken @settlement.MintRedeemer@.

Tags: @Spawn@ 0, @Remove@ 1. Built only by 'pdecodeMintRedeemer', which is what
authenticates the wire shape.
-}
data PMintRedeemer (s :: S)
  = PSpawn
      { pspawn'settlementId :: Term s (PAsData PTokenName)
      , pspawn'outputIndex :: Term s (PAsData PInteger)
      , pspawn'stateQueueMergeRedeemerIndex :: Term s (PAsData PInteger)
      , pspawn'hubRefInputIndex :: Term s (PAsData PInteger)
      }
  | PRemove
      { premove'settlementId :: Term s (PAsData PTokenName)
      , premove'inputIndex :: Term s (PAsData PInteger)
      , premove'spendRedeemerIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @settlement.decode_mint_redeemer@.

The Aiken original hand-decodes this rather than writing @expect r: MintRedeemer@,
and the port keeps that shape because the checks are the point: the constructor
tag must be 0 or 1, the field count must be exactly 4 or 3 respectively, and
each field must be of the right @Data@ kind. An unknown tag fails.

This is a cross-script boundary — other validators hand this redeemer over — so
a redeemer that merely /looks/ close enough must be rejected rather than read
positionally. @pasByteStr@ and @pasInt@ supply the per-field type checking by
erroring on the wrong kind.
-}
pdecodeMintRedeemer :: forall (s :: S). Term s (PData :--> PMintRedeemer)
pdecodeMintRedeemer = phoistAcyclic $
  plam $ \redeemerData -> P.do
    decoded <- plet $ pasConstr # redeemerData
    tag <- plet $ pfstBuiltin # decoded
    fields <- plet $ psndBuiltin # decoded
    asName <- plet $ plam (\d -> pdata (pcon (PTokenName (pasByteStr # d))))
    asInt <- plet $ plam (\d -> pdata (pasInt # d))
    pif
      (tag #== 0)
      ( pif
          (plength # fields #== 4)
          ( pcon
              ( PSpawn
                  { pspawn'settlementId = asName # (phead # fields)
                  , pspawn'outputIndex = asInt # (phead #$ ptail # fields)
                  , pspawn'stateQueueMergeRedeemerIndex =
                      asInt # (phead #$ ptail #$ ptail # fields)
                  , pspawn'hubRefInputIndex =
                      asInt # (phead #$ ptail #$ ptail #$ ptail # fields)
                  }
              )
          )
          (ptraceInfoError "settlement mint redeemer: Spawn arity")
      )
      ( pif
          (tag #== 1)
          ( pif
              (plength # fields #== 3)
              ( pcon
                  ( PRemove
                      { premove'settlementId = asName # (phead # fields)
                      , premove'inputIndex = asInt # (phead #$ ptail # fields)
                      , premove'spendRedeemerIndex =
                          asInt # (phead #$ ptail #$ ptail # fields)
                      }
                  )
              )
              (ptraceInfoError "settlement mint redeemer: Remove arity")
          )
          (ptraceInfoError "settlement mint redeemer: unknown tag")
      )

{- | Aiken @settlement.EventType@.

Which of the three user events an inclusion proof is about, carrying the
verdict where one applies. Tags: @Deposit@ 0, @Withdrawal@ 1, @TxOrder@ 2.

A deposit carries no verdict because there is nothing to judge — the funds
either arrived or they did not.
-}
data PEventType (s :: S)
  = PDeposit
  | PWithdrawal {pevtWithdrawal'validityOverride :: Term s (PAsData PWithdrawalValidity)}
  | PTxOrder {pevtTxOrder'validityOverride :: Term s (PAsData PMidgardTxValidity)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventType)

{- | Aiken @settlement.EventMembershipProof@.

One wrapper per event kind. All three carry the same proof type; the wrapper
exists so the caller cannot hand a deposit proof to the withdrawal arm.
-}
data PEventMembershipProof (s :: S)
  = PDepositMembership {pdepositMembership'witness :: Term s (PAsData PRootMembershipProof)}
  | PWithdrawalMembership {pwithdrawalMembership'witness :: Term s (PAsData PRootMembershipProof)}
  | PTxOrderMembership {ptxOrderMembership'witness :: Term s (PAsData PRootMembershipProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventMembershipProof)

{- | Aiken @settlement.valid_event_inclusion@.

Proves an unresolved user event was, in fact, absorbed into the settlement's
block. The event is produced as an authenticated reference input, its key/value
pair is derived, and that pair must be a member of the matching root.

Each arm derives its value differently, and the difference is the point:

  * a deposit's info is read straight out of its datum — there is no verdict;
  * a withdrawal's info is read and then has the claimed verdict /substituted/
    before the membership check;
  * a transaction order has no stored value at all, so the whole
    'PForcedInclusionTxV1' is reassembled around the claimed verdict.

In every case the claimant names the verdict and the proof must corroborate it
against the operator's committed root, which is what stops a disputant inventing
one.
-}
pvalidEventInclusion ::
  forall (s :: S).
  Term s PEventType ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PEventMembershipProof ->
  Term s (PAsData PTokenName) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pvalidEventInclusion
  eventType
  depositScriptHash
  withdrawalScriptHash
  txOrderScriptHash
  depositsRoot
  withdrawalsRoot
  forcedTransactionsRoot
  membershipProof
  unresolvedEventAssetName
  unresolvedEventRefInputIndex
  referenceInputs
  redeemers = P.do
    eventDatumOf <-
      plet $
        plam
          ( \policy -> P.do
              PTxInInfo {ptxInInfo'resolved} <-
                pmatch $
                  pfromData
                    ( pgetAuthenticInputWithNftAt
                        # referenceInputs
                        # policy
                        # unresolvedEventAssetName
                        # unresolvedEventRefInputIndex
                    )
              PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
              pmatch ptxOut'datum $ \case
                POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
                _ -> perror
          )
    pmatch eventType $ \case
      PDeposit -> P.do
        let (depositId, depositInfo) =
              punsafeEventToKeyValuePair (eventDatumOf # depositScriptHash)
        pmatch membershipProof $ \case
          PDepositMembership {pdepositMembership'witness} ->
            pvalidCountedMembership
              (pdata (pcon PDepositsRootDomain))
              depositsRoot
              (pfromData pdepositMembership'witness)
              depositId
              depositInfo
              redeemers
          _ -> perror
      PWithdrawal {pevtWithdrawal'validityOverride} -> P.do
        let (withdrawalId, withdrawalInfoData) =
              punsafeEventToKeyValuePair (eventDatumOf # withdrawalScriptHash)
        PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'signature} <-
          pmatch (pfromData (punsafeCoerce @(PAsData PWithdrawalInfo) withdrawalInfoData))
        overridden <-
          plet $
            pforgetData
              ( pdata
                  ( pcon
                      ( PWithdrawalInfo
                          { pwithdrawalInfo'body = pwithdrawalInfo'body
                          , pwithdrawalInfo'signature = pwithdrawalInfo'signature
                          , pwithdrawalInfo'validity = pevtWithdrawal'validityOverride
                          }
                      )
                  )
              )
        pmatch membershipProof $ \case
          PWithdrawalMembership {pwithdrawalMembership'witness} ->
            pvalidCountedMembership
              (pdata (pcon PWithdrawalsRootDomain))
              withdrawalsRoot
              (pfromData pwithdrawalMembership'witness)
              withdrawalId
              overridden
              redeemers
          _ -> perror
      PTxOrder {pevtTxOrder'validityOverride} -> P.do
        -- The order's datum is an OptimisticDatum, so its event is field 0.
        eventData <-
          plet $ phead #$ psndBuiltin # (pasConstr # (eventDatumOf # txOrderScriptHash))
        let (txOrderId, forcedInclusionTx) =
              pforcedInclusionKeyValue eventData pevtTxOrder'validityOverride
        pmatch membershipProof $ \case
          PTxOrderMembership {ptxOrderMembership'witness} ->
            pvalidCountedMembership
              (pdata (pcon PForcedTransactionsV1RootDomain))
              forcedTransactionsRoot
              (pfromData ptxOrderMembership'witness)
              txOrderId
              forcedInclusionTx
              redeemers
          _ -> perror
