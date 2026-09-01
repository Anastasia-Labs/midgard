{- |
Module      : Midgard.UserEvents.TxOrder
Description : Plutarch port of
              @lib/midgard/user-events/tx-order-v1.ak@.

A transaction order is a user's request that an L2 transaction be included. It
is the third user event, and the only one whose payload is a transaction rather
than a value movement.

The module also owns the ordered field-receipt chain used while an order is
minted. That counted publication scheme is retired, but its verifier remains a
live Aiken call site and is kept here literally until that surface is removed.
-}
module Midgard.UserEvents.TxOrder (
  PTxOrderDatum (..),
  pgetDatum,
  PSpendRedeemer (..),
  pforcedInclusionKeyValue,
  pfieldReceiptAssetName,
  pverifyOrderReceipts,
  pverifyReceiptChainLink,
  pfieldFragmentBurnAuthorized,
  pfieldReceiptBurnAuthorized,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress (..), PCredential (..), POutputDatum (..), PScriptHash)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedCollection (pverifyBoundedCollectionItem)
import Midgard.BoundedItem (pchunkCount)
import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithPolicyAt, pquantityOfMint)
import Midgard.Common.Value (pquantityOfValue)
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
  PChunkProofV1 (..),
  PForcedInclusionTxV1 (..),
  PItemProofV1 (..),
  PMidgardTxValidity,
  PNativeTxProofSourceV1 (..),
  PTxFieldPreimageV1 (..),
  PTxFieldReceiptV1 (..),
  PTxOrderEventV1,
  PTxOrderPayloadV1 (..),
  punsafeEventToKeyValuePair,
 )
import Midgard.NativeTxFieldAccess (pemptyFieldCommitment)
import Midgard.UserEvents (poutRefToNonce)
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

-- | Aiken @tx_order.SpendRedeemer@ — a record, so @Constr 0@.
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { ptxOrderSpend'inputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'outputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'hubRefInputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'settlementRefInputIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'burnRedeemerIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'membershipProof :: Term s (PAsData PRootMembershipProof)
  , ptxOrderSpend'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
  , ptxOrderSpend'validityOverride :: Term s (PAsData PMidgardTxValidity)
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
  Term s (PAsData PMidgardTxValidity) ->
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
                      , pforcedTx'operatorValidity = validityOverride
                      }
                  )
              )
          )
   in (txOrderIdData, forcedTx)

{- | Aiken @tx_order_v1.field_receipt_asset_name@.

The name a field chunk's receipt NFT is minted under. It is the blake2b-256 of a
domain tag followed by every coordinate that identifies the chunk: which order
(policy, output reference), which transaction (commitment), and where in it
(field, item, chunk).

Two consequences follow from it being a hash of all of that. A receipt cannot be
moved between orders or transactions, and — because the same coordinates always
produce the same name — a chunk cannot be receipted twice: the second mint would
collide with the first, which the ledger forbids while the first is alive.

The bounds are @expect@s, so they error rather than returning @False@. The
interesting one is @field_index < 9@: a Midgard transaction has exactly nine
fields, and the one-byte encoding below would otherwise let a tenth alias.
-}
pfieldReceiptAssetName ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTxOutRef
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PTokenName
    )
pfieldReceiptAssetName = phoistAcyclic $
  plam $ \txOrderPolicyId txOrderId transactionCommitment fieldIndex itemIndex chunkIndex ->
    pmatch (pfromData txOrderId) $ \PTxOutRef {ptxOutRef'id, ptxOutRef'idx} ->
      plet (pto (pfromData ptxOutRef'id)) $ \transactionId ->
        plet (pfromData ptxOutRef'idx) $ \outputIndex ->
          plet (pto (pfromData txOrderPolicyId)) $ \policyBytes ->
            pif
              ( pand'List
                  [ plengthBS # policyBytes #== 28
                  , plengthBS # transactionId #== 32
                  , 0 #<= outputIndex
                  , plengthBS # transactionCommitment #== 32
                  , 0 #<= fieldIndex
                  , fieldIndex #< 9
                  , 0 #<= itemIndex
                  , 0 #<= chunkIndex
                  ]
              )
              ( pcon
                  ( PTokenName
                      ( pblake2b_256
                          #$ pfieldReceiptV1Domain
                          <> policyBytes
                          <> transactionId
                          <> pbigEndian 8 outputIndex
                          <> transactionCommitment
                          <> pbigEndian 1 fieldIndex
                          <> pbigEndian 8 itemIndex
                          <> pbigEndian 8 chunkIndex
                      )
                  )
              )
              perror

-- | Aiken @tx_order_v1.field_receipt_v1_domain@ — @"MidgardTxFieldReceiptV1"@.
pfieldReceiptV1Domain :: forall (s :: S). Term s PByteString
pfieldReceiptV1Domain = pconstant "MidgardTxFieldReceiptV1"

-- | Aiken @bytearray.from_int_big_endian@, with the width fixed at the call site.
pbigEndian :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width n = pintegerToByteString # pmostSignificantFirst # width # n

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

-- | Aiken @tx_order_v1.next_non_empty_field@.
pnextNonEmptyField ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList PByteString
        :--> PNativeTxFieldPreimageLengthsV1
        :--> PInteger
        :--> PInteger
    )
pnextNonEmptyField = phoistAcyclic $
  pfix $ \self -> plam $ \commitments lengths fieldIndex ->
    pif
      (fieldIndex #>= 9)
      9
      ( plet (pelemAt # fieldIndex # commitments) $ \commitment ->
          pif
            (commitment #== pemptyFieldCommitment)
            ( pif
                (pfieldLength # lengths # fieldIndex #== 1)
                (self # commitments # lengths # (fieldIndex + 1))
                perror
            )
            fieldIndex
      )

pcanonicalHeaderSize :: forall (s :: S). Term s (PInteger :--> PInteger)
pcanonicalHeaderSize = phoistAcyclic $
  plam $ \itemCount ->
    pif (itemCount #< 0) perror $
      pif (itemCount #< 24) 1 $
        pif (itemCount #<= 255) 2 $
          pif (itemCount #<= 65_535) 3 5

pitemEncodedSize :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pitemEncodedSize = phoistAcyclic $
  plam $ \fieldIndex itemLength ->
    pif (itemLength #< 0) perror $
      let bytesSize = pcanonicalHeaderSize # itemLength + itemLength
       in pif (fieldIndex #== 0) bytesSize $
            pif (fieldIndex #== 1) bytesSize $
              pif (fieldIndex #== 2) bytesSize $
                pif (fieldIndex #== 3) bytesSize $
                  pif (fieldIndex #== 4) bytesSize $
                    pif
                      (fieldIndex #== 5)
                      (pif (itemLength #> 1) (itemLength - 1) perror)
                      $ pif (fieldIndex #== 6) itemLength
                      $ pif (fieldIndex #== 7) bytesSize
                      $ pif (fieldIndex #== 8) itemLength perror

{- | Aiken @tx_order_v1.material_directory@.

Authenticates the compact source once and returns its nine positional field
commitments together with the separately committed encoded lengths.
-}
pmaterialDirectory ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PNativeTxProofSourceV1
        :--> PPair (PBuiltinList PByteString) PNativeTxFieldPreimageLengthsV1
    )
pmaterialDirectory = phoistAcyclic $
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
          pcon $
            PPair
              ( pcons # pbodyCompact'spendInputsHash
                  #$ pcons # pbodyCompact'referenceInputsHash
                  #$ pcons # pbodyCompact'outputsHash
                  #$ pcons # pbodyCompact'requiredObserversHash
                  #$ pcons # pbodyCompact'requiredSignersHash
                  #$ pcons # pbodyCompact'mintHash
                  #$ pcons # pfromData pwitnessSetCompact'scriptTxWitsHash
                  #$ pcons # pfromData pwitnessSetCompact'addrTxWitsHash
                  #$ pcons # pfromData pwitnessSetCompact'redeemerTxWitsHash
                  # pnil
              )
              (pdecodeNativeTxFieldPreimageLengthsV1 # lengthsCbor)
      )
      perror

preceiptIdentityIsValid ::
  forall (s :: S).
  Term
    s
    ( PTxFieldReceiptV1
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PTxOutRef
        :--> PByteString
        :--> PBool
    )
preceiptIdentityIsValid = phoistAcyclic $
  plam $ \receipt fieldReceiptPolicyId txOrderPolicyId txOrderId transactionCommitment ->
    pmatch receipt $ \PTxFieldReceiptV1
      { ptxFieldReceipt'fieldReceiptPolicyId
      , ptxFieldReceipt'txOrderPolicyId
      , ptxFieldReceipt'txOrderId
      , ptxFieldReceipt'transactionCommitment
      } ->
        pand'List
          [ ptxFieldReceipt'fieldReceiptPolicyId #== fieldReceiptPolicyId
          , ptxFieldReceipt'txOrderPolicyId #== txOrderPolicyId
          , ptxFieldReceipt'txOrderId #== txOrderId
          , pfromData ptxFieldReceipt'transactionCommitment #== transactionCommitment
          ]

preceiptDescriptorIsValid ::
  forall (s :: S).
  Term s (PTxFieldReceiptV1 :--> PBuiltinList PByteString :--> PBool)
preceiptDescriptorIsValid = phoistAcyclic $
  plam $ \receipt commitments ->
    pmatch receipt $ \PTxFieldReceiptV1
      {ptxFieldReceipt'collectionProof, ptxFieldReceipt'chunkIndex} ->
        plet (pfromData ptxFieldReceipt'collectionProof) $ \proof ->
          pmatch proof $ \PItemProofV1
            { pitemProof'fieldIndex
            , pitemProof'itemCount
            , pitemProof'itemIndex
            , pitemProof'itemLength
            } ->
              let fieldIndex = pfromData pitemProof'fieldIndex
                  itemIndex = pfromData pitemProof'itemIndex
                  chunkIndex = pfromData ptxFieldReceipt'chunkIndex
               in (fieldIndex #>= 0)
                    #&& (fieldIndex #< 9)
                    #&& (itemIndex #>= 0)
                    #&& (itemIndex #< pfromData pitemProof'itemCount)
                    #&& (chunkIndex #>= 0)
                    #&& (chunkIndex #< pchunkCount # pfromData pitemProof'itemLength)
                    #&& ( pverifyBoundedCollectionItem
                            # (pelemAt # fieldIndex # commitments)
                            # proof
                        )

pcollectionSuccessorIsValid ::
  forall (s :: S).
  Term s (PItemProofV1 :--> PItemProofV1 :--> PBool)
pcollectionSuccessorIsValid = phoistAcyclic $
  plam $ \previous current ->
    pmatch previous $ \PItemProofV1
      { pitemProof'version = previousVersion
      , pitemProof'fieldIndex = previousFieldIndex
      , pitemProof'itemCount = previousItemCount
      , pitemProof'itemIndex = previousItemIndex
      , pitemProof'frontier = previousFrontier
      } ->
        pmatch current $ \PItemProofV1
          { pitemProof'version = currentVersion
          , pitemProof'fieldIndex = currentFieldIndex
          , pitemProof'itemCount = currentItemCount
          , pitemProof'itemIndex = currentItemIndex
          , pitemProof'frontier = currentFrontier
          } ->
            pand'List
              [ currentVersion #== previousVersion
              , currentFieldIndex #== previousFieldIndex
              , currentItemCount #== previousItemCount
              , pfromData currentItemIndex #== pfromData previousItemIndex + 1
              , currentFrontier #== previousFrontier
              ]

preceiptSizeIsValid ::
  forall (s :: S).
  Term
    s
    ( PTxFieldReceiptV1
        :--> PNativeTxFieldPreimageLengthsV1
        :--> PInteger
        :--> PBool
    )
preceiptSizeIsValid = phoistAcyclic $
  plam $ \receipt lengths sizeBeforeItem ->
    pmatch receipt $ \PTxFieldReceiptV1
      { ptxFieldReceipt'collectionProof
      , ptxFieldReceipt'chunkIndex
      , ptxFieldReceipt'fieldEncodedSize
      } ->
        pmatch (pfromData ptxFieldReceipt'collectionProof) $ \PItemProofV1
          { pitemProof'fieldIndex
          , pitemProof'itemCount
          , pitemProof'itemIndex
          , pitemProof'itemLength
          } ->
            let fieldIndex = pfromData pitemProof'fieldIndex
                itemLength = pfromData pitemProof'itemLength
                finalChunk =
                  pfromData ptxFieldReceipt'chunkIndex + 1 #== pchunkCount # itemLength
                size =
                  pif finalChunk (sizeBeforeItem + pitemEncodedSize # fieldIndex # itemLength) sizeBeforeItem
             in pand'List
                  [ pfromData ptxFieldReceipt'fieldEncodedSize #== size
                  , pif
                      (finalChunk #&& pfromData pitemProof'itemIndex + 1 #== pfromData pitemProof'itemCount)
                      (size #== pfieldLength # lengths # fieldIndex)
                      (pconstant True)
                  ]

{- | Aiken @tx_order_v1.verify_receipt_chain_link@.

Checks the newly published receipt is the first chunk, the next chunk or item,
or the first chunk of the next non-empty field, with the exact running encoded
size at every transition.
-}
pverifyReceiptChainLink ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PNativeTxProofSourceV1
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PTxOutRef
        :--> PMaybe PTxFieldReceiptV1
        :--> PTxFieldReceiptV1
        :--> PBool
    )
pverifyReceiptChainLink = phoistAcyclic $
  plam $ \transactionId transactionCommitment source fieldReceiptPolicyId txOrderPolicyId txOrderId predecessor receipt -> P.do
    PPair commitments lengths <-
      pmatch $ pmaterialDirectory # transactionId # transactionCommitment # source
    proof <-
      plet $
        pmatch receipt $ \PTxFieldReceiptV1 {ptxFieldReceipt'collectionProof} ->
          pfromData ptxFieldReceipt'collectionProof
    pif
      ( preceiptIdentityIsValid
          # receipt
          # fieldReceiptPolicyId
          # txOrderPolicyId
          # txOrderId
          # transactionCommitment
          #&& preceiptDescriptorIsValid
          # receipt
          # commitments
      )
      ( pmatch predecessor $ \case
          PNothing ->
            pmatch receipt $ \PTxFieldReceiptV1
              { ptxFieldReceipt'predecessorReceiptReference
              , ptxFieldReceipt'chunkIndex
              } ->
                pmatch proof $ \PItemProofV1
                  {pitemProof'fieldIndex, pitemProof'itemIndex, pitemProof'itemCount} ->
                    plet (pnextNonEmptyField # commitments # lengths # 0) $ \firstField ->
                      pand'List
                        [ pfromData
                            ( punsafeCoerce
                                @(PAsData (PMaybeData PTxOutRef))
                                ptxFieldReceipt'predecessorReceiptReference
                            )
                            #== pcon PDNothing
                        , firstField #< 9
                        , pfromData pitemProof'fieldIndex #== firstField
                        , pfromData pitemProof'itemIndex #== 0
                        , pfromData ptxFieldReceipt'chunkIndex #== 0
                        , preceiptSizeIsValid
                            # receipt
                            # lengths
                            # (pcanonicalHeaderSize # pfromData pitemProof'itemCount)
                        ]
          PJust previous ->
            pif
              ( preceiptIdentityIsValid
                  # previous
                  # fieldReceiptPolicyId
                  # txOrderPolicyId
                  # txOrderId
                  # transactionCommitment
                  #&& preceiptDescriptorIsValid
                  # previous
                  # commitments
              )
              ( pmatch previous $ \PTxFieldReceiptV1
                  { ptxFieldReceipt'collectionProof = previousCollectionProof
                  , ptxFieldReceipt'chunkIndex = previousChunkIndex
                  , ptxFieldReceipt'fieldEncodedSize = previousFieldEncodedSize
                  } ->
                    plet (pfromData previousCollectionProof) $ \previousProof ->
                      pmatch previousProof $ \PItemProofV1
                        { pitemProof'fieldIndex = previousFieldIndex
                        , pitemProof'itemCount = previousItemCount
                        , pitemProof'itemIndex = previousItemIndex
                        , pitemProof'itemLength = previousItemLength
                        } ->
                          pmatch receipt $ \PTxFieldReceiptV1 {ptxFieldReceipt'chunkIndex} ->
                            pmatch proof $ \PItemProofV1
                              { pitemProof'fieldIndex
                              , pitemProof'itemIndex
                              , pitemProof'itemCount
                              } ->
                                plet (pchunkCount # pfromData previousItemLength) $ \previousChunkCount ->
                                  pif
                                    (pfromData previousChunkIndex + 1 #< previousChunkCount)
                                    ( pand'List
                                        [ proof #== previousProof
                                        , pfromData ptxFieldReceipt'chunkIndex
                                            #== pfromData previousChunkIndex + 1
                                        , preceiptSizeIsValid
                                            # receipt
                                            # lengths
                                            # pfromData previousFieldEncodedSize
                                        ]
                                    )
                                    ( pif
                                        (pfromData previousItemIndex + 1 #< pfromData previousItemCount)
                                        ( pand'List
                                            [ pcollectionSuccessorIsValid # previousProof # proof
                                            , pfromData ptxFieldReceipt'chunkIndex #== 0
                                            , preceiptSizeIsValid
                                                # receipt
                                                # lengths
                                                # pfromData previousFieldEncodedSize
                                            ]
                                        )
                                        ( plet
                                            ( pnextNonEmptyField
                                                # commitments
                                                # lengths
                                                # (pfromData previousFieldIndex + 1)
                                            )
                                            $ \nextField ->
                                              pand'List
                                                [ nextField #< 9
                                                , pfromData pitemProof'fieldIndex #== nextField
                                                , pfromData pitemProof'itemIndex #== 0
                                                , pfromData ptxFieldReceipt'chunkIndex #== 0
                                                , preceiptSizeIsValid
                                                    # receipt
                                                    # lengths
                                                    # (pcanonicalHeaderSize # pfromData pitemProof'itemCount)
                                                ]
                                        )
                                    )
              )
              perror
      )
      perror

pterminalReceiptIsValid ::
  forall (s :: S).
  Term
    s
    ( PTxFieldReceiptV1
        :--> PBuiltinList PByteString
        :--> PNativeTxFieldPreimageLengthsV1
        :--> PBool
    )
pterminalReceiptIsValid = phoistAcyclic $
  plam $ \receipt commitments lengths ->
    pmatch receipt $ \PTxFieldReceiptV1
      { ptxFieldReceipt'collectionProof
      , ptxFieldReceipt'chunkIndex
      , ptxFieldReceipt'fieldEncodedSize
      } ->
        pmatch (pfromData ptxFieldReceipt'collectionProof) $ \PItemProofV1
          { pitemProof'fieldIndex
          , pitemProof'itemCount
          , pitemProof'itemIndex
          , pitemProof'itemLength
          } ->
            let fieldIndex = pfromData pitemProof'fieldIndex
             in pand'List
                  [ preceiptDescriptorIsValid # receipt # commitments
                  , pfromData ptxFieldReceipt'chunkIndex + 1
                      #== pchunkCount # pfromData pitemProof'itemLength
                  , pfromData pitemProof'itemIndex + 1 #== pfromData pitemProof'itemCount
                  , pfromData ptxFieldReceipt'fieldEncodedSize
                      #== pfieldLength # lengths # fieldIndex
                  , pnextNonEmptyField # commitments # lengths # (fieldIndex + 1) #== 9
                  ]

{- | Aiken @tx_order_v1.verify_order_receipts@.

An empty material directory needs no receipt. Otherwise the payload must name a
reference input holding the exact terminal receipt NFT at the configured script.
-}
pverifyOrderReceipts ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PTxOutRef
        :--> PTxOrderPayloadV1
        :--> PBool
    )
pverifyOrderReceipts = phoistAcyclic $
  plam $ \referenceInputs receiptScriptHash fieldReceiptPolicyId txOrderPolicyId txOrderId payload -> P.do
    PTxOrderPayloadV1
      { ptxOrderPayload'txId
      , ptxOrderPayload'transactionCommitment
      , ptxOrderPayload'source
      , ptxOrderPayload'terminalReceiptReference
      } <-
      pmatch payload
    transactionCommitment <- plet (pfromData ptxOrderPayload'transactionCommitment)
    PPair commitments lengths <-
      pmatch
        ( pmaterialDirectory
            # pfromData ptxOrderPayload'txId
            # transactionCommitment
            # pfromData ptxOrderPayload'source
        )
    pmatch
      (pfromData $ punsafeCoerce @(PAsData (PMaybeData PTxOutRef)) ptxOrderPayload'terminalReceiptReference)
      $ \case
        PDNothing -> pnextNonEmptyField # commitments # lengths # 0 #== 9
        PDJust receiptReferenceData -> P.do
          receiptInput <-
            plet $
              pmatch
                ( pfind
                    # plam
                      ( \input ->
                          pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef} ->
                            ptxInInfo'outRef #== pfromData receiptReferenceData
                      )
                    # referenceInputs
                )
                $ \case
                  PJust input -> pfromData input
                  PNothing -> perror
          PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch receiptInput
          PTxOut
            { ptxOut'address
            , ptxOut'value
            , ptxOut'datum
            , ptxOut'referenceScript
            } <-
            pmatch ptxInInfo'resolved
          PAddress {paddress'credential} <- pmatch ptxOut'address
          receipt <-
            plet $
              pfromData $
                punsafeCoerce @(PAsData PTxFieldReceiptV1) $
                  pmatch ptxOut'datum $ \case
                    POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
                    _ -> perror
          PTxFieldReceiptV1
            { ptxFieldReceipt'collectionProof
            , ptxFieldReceipt'chunkIndex
            } <-
            pmatch receipt
          PItemProofV1 {pitemProof'fieldIndex, pitemProof'itemIndex} <-
            pmatch (pfromData ptxFieldReceipt'collectionProof)
          let receiptAssetName =
                pfieldReceiptAssetName
                  # txOrderPolicyId
                  # txOrderId
                  # transactionCommitment
                  # pfromData pitemProof'fieldIndex
                  # pfromData pitemProof'itemIndex
                  # pfromData ptxFieldReceipt'chunkIndex
          pand'List
            [ ptxInInfo'outRef #== pfromData receiptReferenceData
            , pmatch paddress'credential $ \case
                PScriptCredential actualHash -> actualHash #== receiptScriptHash
                _ -> perror
            , pmatch ptxOut'referenceScript $ \case
                PDNothing -> pconstant True
                PDJust _ -> perror
            , preceiptIdentityIsValid
                # receipt
                # fieldReceiptPolicyId
                # txOrderPolicyId
                # txOrderId
                # transactionCommitment
            , pquantityOfValue
                # pto (pfromData ptxOut'value)
                # fieldReceiptPolicyId
                # pdata receiptAssetName
                #== 1
            , pterminalReceiptIsValid # receipt # commitments # lengths
            ]

{- | Aiken @tx_order_v1.field_fragment_burn_authorized@.

A published field preimage may be spent only in a transaction that is dismantling
the order it belongs to: both the order's own event NFT and this chunk's receipt
NFT must be burnt in the same transaction. The preimage UTxO is therefore not
independently releasable — it goes when the order goes, and only then.
-}
pfieldFragmentBurnAuthorized ::
  forall (s :: S).
  Term s (PAsData PTxFieldPreimageV1 :--> PMintValue :--> PBool)
pfieldFragmentBurnAuthorized = phoistAcyclic $
  plam $ \field mint ->
    pmatch (pfromData field) $
      \PTxFieldPreimageV1
        { ptxFieldPreimage'fieldReceiptPolicyId
        , ptxFieldPreimage'txOrderPolicyId
        , ptxFieldPreimage'txOrderId
        , ptxFieldPreimage'transactionCommitment
        , ptxFieldPreimage'proof
        } ->
          pmatch (pfromData ptxFieldPreimage'proof) $
            \PChunkProofV1 {pchunkProof'fieldIndex, pchunkProof'itemIndex, pchunkProof'chunkIndex} ->
              pbothBurnt
                ptxFieldPreimage'txOrderPolicyId
                ptxFieldPreimage'fieldReceiptPolicyId
                ptxFieldPreimage'txOrderId
                (pfromData ptxFieldPreimage'transactionCommitment)
                (pfromData pchunkProof'fieldIndex)
                (pfromData pchunkProof'itemIndex)
                (pfromData pchunkProof'chunkIndex)
                mint

{- | Aiken @tx_order_v1.field_receipt_burn_authorized@.

The same condition for the receipt UTxO. Note where the three coordinates come
from: the field and item indices off the /collection/ proof, but the chunk index
off the receipt's own field — a receipt carries no chunk proof, only the index of
the chunk it acknowledges.
-}
pfieldReceiptBurnAuthorized ::
  forall (s :: S).
  Term s (PAsData PTxFieldReceiptV1 :--> PMintValue :--> PBool)
pfieldReceiptBurnAuthorized = phoistAcyclic $
  plam $ \receipt mint ->
    pmatch (pfromData receipt) $
      \PTxFieldReceiptV1
        { ptxFieldReceipt'fieldReceiptPolicyId
        , ptxFieldReceipt'txOrderPolicyId
        , ptxFieldReceipt'txOrderId
        , ptxFieldReceipt'transactionCommitment
        , ptxFieldReceipt'collectionProof
        , ptxFieldReceipt'chunkIndex
        } ->
          pmatch (pfromData ptxFieldReceipt'collectionProof) $
            \PItemProofV1 {pitemProof'fieldIndex, pitemProof'itemIndex} ->
              pbothBurnt
                ptxFieldReceipt'txOrderPolicyId
                ptxFieldReceipt'fieldReceiptPolicyId
                ptxFieldReceipt'txOrderId
                (pfromData ptxFieldReceipt'transactionCommitment)
                (pfromData pitemProof'fieldIndex)
                (pfromData pitemProof'itemIndex)
                (pfromData ptxFieldReceipt'chunkIndex)
                mint

-- | The condition both @*_burn_authorized@ predicates reduce to.
pbothBurnt ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTxOutRef) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PMintValue ->
  Term s PBool
pbothBurnt
  txOrderPolicyId
  fieldReceiptPolicyId
  txOrderId
  transactionCommitment
  fieldIndex
  itemIndex
  chunkIndex
  mint =
    pand'List
      [ pquantityOfMint # mint # txOrderPolicyId # pdata (poutRefToNonce # txOrderId) #== -1
      , pquantityOfMint # mint # fieldReceiptPolicyId # pdata receiptAssetName #== -1
      ]
    where
      receiptAssetName =
        pfieldReceiptAssetName
          # txOrderPolicyId
          # txOrderId
          # transactionCommitment
          # fieldIndex
          # itemIndex
          # chunkIndex

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
