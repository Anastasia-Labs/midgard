{- |
Module      : Midgard.UserEvents.TxOrder
Description : Plutarch port of
              @lib/midgard/user-events/tx-order-v1.ak@.

A transaction order is a user's request that an L2 transaction be included. It
is the third user event, and the only one whose payload is a transaction rather
than a value movement.

Order material is authenticated through the positional field-carriage door
while the order NFT is minted.
-}
module Midgard.UserEvents.TxOrder (
  PTxOrderDatum (..),
  PMintRedeemer (..),
  pgetDatum,
  PSpendRedeemer (..),
  pforcedInclusionKeyValue,
  pmaterialCarriageMatchesEvent,
  pverifyOrderMaterial,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PAddress, POutputDatum, PScriptHash)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PTxInInfo,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottStruct (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithPolicyAt)
import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxFieldPreimageLengthsV1,
  pnativeTxProofCommitmentV1,
  pverifyNativeTxProofSourceV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.LedgerState (
  PForcedInclusionTxV1 (..),
  PNativeTxProofSourceV1 (..),
  PTxOrderEventV1,
  PTxOrderPayloadV1 (..),
  punsafeEventToKeyValuePair,
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1,
  pauthenticatedWholeFieldView,
  pemptyFieldCommitment,
  pfieldCount,
  pfieldTotalLength,
 )
import Midgard.RejectionReason (POperatorVerdictV1)
import Midgard.UserEvents qualified as UserEvents
import Midgard.TransitionTrace (PRootMembershipProof)

{- | Aiken @tx_order.Datum = user_events.OptimisticDatum<TxOrderEventV1>@.

Same five fields as the withdrawal datum, and the same constraint on their
order: "Midgard.UserEvents" reads the first three positionally.
-}
data PTxOrderDatum (s :: S) = PTxOrderDatum
  { ptxOrderDatum'event :: Term s (PAsData PTxOrderEventV1)
  , ptxOrderDatum'inclusionTime :: Term s (PAsData PPosixTime)
  , ptxOrderDatum'witness :: Term s (PAsData PScriptHash)
  , ptxOrderDatum'refundAddress :: Term s (PAsData PAddress)
  , ptxOrderDatum'refundDatum :: Term s POutputDatum
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTxOrderDatum)

{- | Aiken @tx_order_v1.MintRedeemer@.

The shared user-event redeemer is wrapped with one carriage entry for each
non-empty transaction field, in ascending positional field order.
-}
data PMintRedeemer (s :: S) = PMintRedeemer
  { ptxOrderMint'event :: Term s (PAsData UserEvents.PMintRedeemer)
  , ptxOrderMint'materialCarriage ::
      Term s (PAsData (PBuiltinList (PAsData PFieldCarriageV1)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

-- | Aiken @tx_order.SpendRedeemer@ — a record, so @Constr 0@.
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { ptxOrderSpend'inputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'outputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'hubRefInputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'settlementRefInputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'burnRedeemerIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'membershipProof :: Term s (PAsData PRootMembershipProof)
  , ptxOrderSpend'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'validityOverride :: Term s (PAsData POperatorVerdictV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @tx_order.forced_inclusion_key_value@.

Turns a transaction order's event into the key/value pair a block's
@forced_transactions_root@ stores: the order's id, and a 'PForcedInclusionTxV1'
built from the payload's transaction id and proof source plus the /caller's/
claimed validity verdict.

The verdict comes from the redeemer, not from the order — an order carries no
verdict, because it is written before any operator has judged it. That is why
the value has to be reassembled here rather than read out: proving an order's
inclusion means proving it was included *with a particular verdict*, and the
claimant has to name which.

Returned as a Haskell pair, matching how the single call site consumes it.
-}
pforcedInclusionKeyValue ::
  forall (s :: S).
  Term s PData ->
  Term s (PAsData POperatorVerdictV1) ->
  (Term s PData, Term s PData)
pforcedInclusionKeyValue txOrderEventData validityOverride =
  let (txOrderIdData, payloadData) = punsafeEventToKeyValuePair txOrderEventData
      payload = pfromData (punsafeCoerce @(PAsData PTxOrderPayloadV1) payloadData)
      forcedTx = pmatch payload $ \(PTxOrderPayloadV1 {ptxOrderPayload'txId, ptxOrderPayload'source}) ->
        pforgetData
          ( pdata
              ( pcon
                  ( PForcedInclusionTxV1
                      { pforcedTx'txId = ptxOrderPayload'txId
                      , pforcedTx'source = ptxOrderPayload'source
                      , pforcedTx'verdict = validityOverride
                      }
                  )
              )
          )
   in (txOrderIdData, forcedTx)

-- | Aiken @tx_order_v1.field_length@.
pfieldLength ::
  forall (s :: S).
  Term s (PNativeTxFieldPreimageLengthsV1 :--> PInteger :--> PInteger)
pfieldLength = phoistAcyclic $
  plam $ \lengths fieldIndex -> P.do
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs
      , plengths'referenceInputs
      , plengths'outputs
      , plengths'requiredObservers
      , plengths'requiredSigners
      , plengths'mint
      , plengths'addressWitnesses
      , plengths'scriptWitnesses
      , plengths'redeemers
      } <-
      pmatch lengths
    pif (fieldIndex #== 0) plengths'spendInputs $
      pif (fieldIndex #== 1) plengths'referenceInputs $
        pif (fieldIndex #== 2) plengths'outputs $
          pif (fieldIndex #== 3) plengths'requiredObservers $
            pif (fieldIndex #== 4) plengths'requiredSigners $
              pif (fieldIndex #== 5) plengths'mint $
                pif (fieldIndex #== 6) plengths'scriptWitnesses $
                  pif (fieldIndex #== 7) plengths'addressWitnesses $
                    pif (fieldIndex #== 8) plengths'redeemers 0

-- | A burn has no material to authenticate and therefore has one spelling.
pmaterialCarriageMatchesEvent ::
  forall (s :: S).
  Term
    s
    ( UserEvents.PMintRedeemer
        :--> PBuiltinList (PAsData PFieldCarriageV1)
        :--> PBool
    )
pmaterialCarriageMatchesEvent = phoistAcyclic $
  plam $ \event materialCarriage ->
    pmatch event $ \case
      UserEvents.PBurnEventNFT {} -> pnull # materialCarriage
      UserEvents.PAuthenticateEvent {} -> pconstant True

-- | Compact structures and positional metadata authenticated before carriage.
data POrderMaterialDirectoryV1 (s :: S) = POrderMaterialDirectoryV1
  { porderDirectory'verified :: Term s PVerifiedMidgardNativeTxCompact
  , porderDirectory'witnessSet :: Term s PNativeTxWitnessSetCompact
  , porderDirectory'commitments :: Term s (PBuiltinList PByteString)
  , porderDirectory'lengths :: Term s PNativeTxFieldPreimageLengthsV1
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct POrderMaterialDirectoryV1)

pmaterialDirectoryV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PNativeTxProofSourceV1
        :--> POrderMaterialDirectoryV1
    )
pmaterialDirectoryV1 = phoistAcyclic $
  plam $ \transactionId transactionCommitment source -> P.do
    PNativeTxProofSourceV1
      { pnativeSource'compactCbor
      , pnativeSource'witnessSetCompactCbor
      , pnativeSource'fieldPreimageLengthsCbor
      } <-
      pmatch source
    compactCbor <- plet (pfromData pnativeSource'compactCbor)
    witnessSetCompactCbor <- plet (pfromData pnativeSource'witnessSetCompactCbor)
    lengthsCbor <- plet (pfromData pnativeSource'fieldPreimageLengthsCbor)
    PPair verified witnessSet <-
      pmatch
        ( pverifyNativeTxProofSourceV1
            # transactionId
            # compactCbor
            # witnessSetCompactCbor
            # lengthsCbor
        )
    pif
      ( pnativeTxProofCommitmentV1
          # compactCbor
          # witnessSetCompactCbor
          # lengthsCbor
          #== transactionCommitment
      )
      ( P.do
        PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
        PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
        PNativeTxBodyCompact
          { pbodyCompact'spendInputsHash
          , pbodyCompact'referenceInputsHash
          , pbodyCompact'outputsHash
          , pbodyCompact'requiredObserversHash
          , pbodyCompact'requiredSignersHash
          , pbodyCompact'mintHash
          } <-
          pmatch pcompact'body
        PNativeTxWitnessSetCompact
          { pwitnessSetCompact'addrTxWitsHash
          , pwitnessSetCompact'scriptTxWitsHash
          , pwitnessSetCompact'redeemerTxWitsHash
          } <-
          pmatch witnessSet
        pcon
          POrderMaterialDirectoryV1
            { porderDirectory'verified = verified
            , porderDirectory'witnessSet = witnessSet
            , porderDirectory'commitments =
                pcons # pbodyCompact'spendInputsHash
                  #$ pcons # pbodyCompact'referenceInputsHash
                  #$ pcons # pbodyCompact'outputsHash
                  #$ pcons # pbodyCompact'requiredObserversHash
                  #$ pcons # pbodyCompact'requiredSignersHash
                  #$ pcons # pbodyCompact'mintHash
                  #$ pcons # pfromData pwitnessSetCompact'scriptTxWitsHash
                  #$ pcons # pfromData pwitnessSetCompact'addrTxWitsHash
                  #$ pcons # pfromData pwitnessSetCompact'redeemerTxWitsHash
                  # pnil
            , porderDirectory'lengths = pdecodeNativeTxFieldPreimageLengthsV1 # lengthsCbor
            }
      )
      perror

pauthenticateMaterialFields ::
  forall (s :: S).
  Term
    s
    ( POrderMaterialDirectoryV1
        :--> PInteger
        :--> PBuiltinList (PAsData PFieldCarriageV1)
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PBool
    )
pauthenticateMaterialFields = phoistAcyclic $
  pfix $ \self ->
    plam $ \directory fieldIndex materialCarriage referenceInputs certificatePolicyId ->
      pmatch directory $ \POrderMaterialDirectoryV1
        { porderDirectory'verified
        , porderDirectory'witnessSet
        , porderDirectory'commitments
        , porderDirectory'lengths
        } ->
          pif
            (fieldIndex #>= pfieldCount)
            (pnull # materialCarriage)
            ( plet (pelemAt # fieldIndex # porderDirectory'commitments) $ \commitment ->
                plet (pfieldLength # porderDirectory'lengths # fieldIndex) $ \declaredLength ->
                  pif
                    (commitment #== pemptyFieldCommitment)
                    ( pif
                        (declaredLength #== 1)
                        ( self
                            # directory
                            # (fieldIndex + 1)
                            # materialCarriage
                            # referenceInputs
                            # certificatePolicyId
                        )
                        perror
                    )
                    ( pmatch materialCarriage $ \case
                        PCons fieldCarriage rest ->
                          plet
                            ( pauthenticatedWholeFieldView
                                # porderDirectory'verified
                                # porderDirectory'witnessSet
                                # fieldIndex
                                # pfromData fieldCarriage
                                # referenceInputs
                                # certificatePolicyId
                            )
                            $ \view ->
                              pif
                                (pfieldTotalLength # view #== declaredLength)
                                ( self
                                    # directory
                                    # (fieldIndex + 1)
                                    # rest
                                    # referenceInputs
                                    # certificatePolicyId
                                )
                                perror
                        PNil -> perror
                    )
            )

-- | Authenticate every non-empty field through §8's materialising door.
pverifyOrderMaterial ::
  forall (s :: S).
  Term
    s
    ( PTxOrderPayloadV1
        :--> PBuiltinList (PAsData PFieldCarriageV1)
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PBool
    )
pverifyOrderMaterial = phoistAcyclic $
  plam $ \payload materialCarriage referenceInputs certificatePolicyId ->
    pmatch payload $ \PTxOrderPayloadV1
      { ptxOrderPayload'txId
      , ptxOrderPayload'transactionCommitment
      , ptxOrderPayload'source
      } ->
        pauthenticateMaterialFields
          # ( pmaterialDirectoryV1
                # pfromData ptxOrderPayload'txId
                # pfromData ptxOrderPayload'transactionCommitment
                # pfromData ptxOrderPayload'source
            )
          # 0
          # materialCarriage
          # referenceInputs
          # certificatePolicyId

{- | Aiken @tx_order.get_datum@.

Reads a tx order's datum from a reference input authenticated by its policy id.
The asset /name/ is unconstrained here, because each tx order carries its own
one-off event NFT; what identifies it is the policy.

Aiken's @expect tx_order_datum: Datum = ...@ structurally validates the datum;
the coercion below does not, so a malformed datum fails at the first field read
rather than up front. Both reject.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PTxOrderDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs txorderPolicyId txorderInputIndex ->
    pfromData
      ( punsafeCoerce @(PAsData PTxOrderDatum)
          ( pgetAuthenticInputDatumWithPolicyAt
              # referenceInputs
              # txorderPolicyId
              # txorderInputIndex
          )
      )
