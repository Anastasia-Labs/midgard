{- |
Module      : Midgard.UserEvents.TxFieldReceipt
Description : Plutarch port of
              @lib/midgard/user-events/tx-field-receipt-v1.ak@.

The minting policy for field receipts — the tokens that acknowledge a published
chunk of a forced transaction.

Both publication and burning are ported. Publication retains the retired
counted-opening call chain literally; honest flat §4 field commitments still
cannot satisfy it, but all guards and rejection boundaries match Aiken.
-}
module Midgard.UserEvents.TxFieldReceipt (
  PMintRedeemer (..),
  pvalidatePublication,
  pvalidateBurnReceipts,
  pverifyReceiptBurns,
  ptokensUnderPolicy,
  ptokensUnderPolicyOfValue,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Data.Kind (Type)

import Plutarch.Core.List (pheadSingleton)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PMintValue,
  POutputDatum (..),
  PScriptHash,
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value (PLedgerValue)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.FraudProofs.NativeTx.Transaction (pverifyMidgardTransactionFieldChunkV1)
import Midgard.LedgerState (
  PItemProofV1 (..),
  PNativeTxProofSourceV1 (..),
  PTxFieldPreimageV1 (..),
  PTxFieldReceiptV1 (..),
 )
import Midgard.UserEvents.TxOrder (
  pfieldReceiptAssetName,
  pfieldReceiptBurnAuthorized,
  pverifyReceiptChainLink,
 )

{- | Aiken @tx_field_receipt_v1.MintRedeemer@.

The policy's redeemer, both constructors. Tags are wire format: @PublishField@ is
@Constr 0@ and @BurnReceipts@ @Constr 1@.

The @PublishField@ arm is declared in full even though nothing here can satisfy
it — see 'Midgard.Validators.TxFieldReceipt.txFieldReceiptMintValidator'. A
redeemer type with one arm missing would be a different type, and an SDK that
built against it would produce bytes this policy could not decode at all, which
is a worse failure than the honest refusal the branch actually is.
-}
data PMintRedeemer (s :: S)
  = PPublishField
      { ppublishField'fieldReferenceInputIndex :: Term s (PAsData PInteger)
      , ppublishField'predecessorReceiptReferenceInputIndex :: Term s (PAsData PInteger)
      , ppublishField'receiptOutputIndex :: Term s (PAsData PInteger)
      , ppublishField'transactionId :: Term s (PAsData PByteString)
      , ppublishField'source :: Term s (PAsData PNativeTxProofSourceV1)
      }
  | PBurnReceipts {pburnReceipts'receiptInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @tx_field_receipt_v1.verify_publication@.

Every indexed UTxO is authenticated before the two proof predicates run. The
new receipt must reproduce the field datum's identity and proof coordinates,
hold the one NFT minted under this policy, and link to either no predecessor or
the exact authenticated predecessor receipt.
-}
pvalidatePublication ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PBuiltinList (PAsData PTxOut)
        :--> PMintValue
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PNativeTxProofSourceV1
        :--> PBool
    )
pvalidatePublication = phoistAcyclic $
  plam $ \fieldPreimageScriptHash receiptScriptHash ownPolicy referenceInputs outputs mint fieldReferenceInputIndex predecessorIndex receiptOutputIndex transactionId source -> P.do
    PNativeTxProofSourceV1
      { pnativeSource'compactCbor
      , pnativeSource'witnessSetCompactCbor
      , pnativeSource'fieldPreimageLengthsCbor
      } <-
      pmatch source
    PTxInInfo {ptxInInfo'outRef = fieldReference, ptxInInfo'resolved = fieldOutput} <-
      pmatch $ pfromData (pelemAt # fieldReferenceInputIndex # referenceInputs)
    PTxOut
      { ptxOut'address = fieldAddress
      , ptxOut'datum = fieldDatum
      , ptxOut'referenceScript = fieldReferenceScript
      } <-
      pmatch fieldOutput
    PAddress {paddress'credential = fieldCredential} <- pmatch fieldAddress
    field <- plet $ pdecodeInlineDatum @PTxFieldPreimageV1 fieldDatum
    PTxFieldPreimageV1
      { ptxFieldPreimage'fieldReceiptPolicyId
      , ptxFieldPreimage'txOrderPolicyId
      , ptxFieldPreimage'txOrderId
      , ptxFieldPreimage'transactionCommitment
      , ptxFieldPreimage'collectionProof
      , ptxFieldPreimage'proof
      } <-
      pmatch field
    PChunkProofV1
      { pchunkProof'fieldIndex
      , pchunkProof'itemIndex
      , pchunkProof'chunkIndex
      } <-
      pmatch (pfromData ptxFieldPreimage'proof)

    receiptOutput <- plet $ pfromData (pelemAt # receiptOutputIndex # outputs)
    PTxOut
      { ptxOut'address = receiptAddress
      , ptxOut'value = receiptValue
      , ptxOut'datum = receiptDatum
      , ptxOut'referenceScript = receiptReferenceScript
      } <-
      pmatch receiptOutput
    PAddress {paddress'credential = receiptCredential} <- pmatch receiptAddress
    receipt <- plet $ pdecodeInlineDatum @PTxFieldReceiptV1 receiptDatum
    PTxFieldReceiptV1
      { ptxFieldReceipt'fieldReceiptPolicyId
      , ptxFieldReceipt'txOrderPolicyId
      , ptxFieldReceipt'txOrderId
      , ptxFieldReceipt'transactionCommitment
      , ptxFieldReceipt'collectionProof
      , ptxFieldReceipt'chunkIndex
      , ptxFieldReceipt'fieldReference
      , ptxFieldReceipt'predecessorReceiptReference
      } <-
      pmatch receipt

    _shapeIsValid <-
      plet $
        pif
          ( pand'List
          [ pmatch fieldCredential $ \case
              PScriptCredential actualHash -> actualHash #== fieldPreimageScriptHash
              _ -> perror
          , pmatch fieldReferenceScript $ \case
              PDNothing -> pconstant True
              PDJust _ -> perror
          , ptxFieldPreimage'fieldReceiptPolicyId #== ownPolicy
          , pmatch receiptCredential $ \case
              PScriptCredential actualHash -> actualHash #== receiptScriptHash
              _ -> perror
          , pmatch receiptReferenceScript $ \case
              PDNothing -> pconstant True
              PDJust _ -> perror
          , ptxFieldReceipt'fieldReceiptPolicyId #== ownPolicy
          , ptxFieldReceipt'txOrderPolicyId #== ptxFieldPreimage'txOrderPolicyId
          , ptxFieldReceipt'txOrderId #== ptxFieldPreimage'txOrderId
          , ptxFieldReceipt'transactionCommitment #== ptxFieldPreimage'transactionCommitment
          , ptxFieldReceipt'collectionProof #== ptxFieldPreimage'collectionProof
          , ptxFieldReceipt'chunkIndex #== pchunkProof'chunkIndex
          , ptxFieldReceipt'fieldReference #== pdata fieldReference
              ]
          )
          (pconstant @PBool True)
          perror

    predecessor <-
      plet $
        pif
          (predecessorIndex #< 0)
          ( pif
              ( predecessorIndex #== -1
                  #&& pfromData
                    ( punsafeCoerce
                        @(PAsData (PMaybeData PTxOutRef))
                        ptxFieldReceipt'predecessorReceiptReference
                    )
                    #== pcon PDNothing
              )
              (pcon PNothing)
              perror
          )
          ( P.do
              PTxInInfo
                { ptxInInfo'outRef = predecessorReference
                , ptxInInfo'resolved = predecessorOutput
                } <-
                pmatch $ pfromData (pelemAt # predecessorIndex # referenceInputs)
              PTxOut
                { ptxOut'address = predecessorAddress
                , ptxOut'value = predecessorValue
                , ptxOut'datum = predecessorDatum
                , ptxOut'referenceScript = predecessorReferenceScript
                } <-
                pmatch predecessorOutput
              PAddress {paddress'credential = predecessorCredential} <-
                pmatch predecessorAddress
              previous <- plet $ pdecodeInlineDatum @PTxFieldReceiptV1 predecessorDatum
              PTxFieldReceiptV1
                { ptxFieldReceipt'txOrderPolicyId = previousTxOrderPolicyId
                , ptxFieldReceipt'txOrderId = previousTxOrderId
                , ptxFieldReceipt'transactionCommitment = previousTransactionCommitment
                , ptxFieldReceipt'collectionProof = previousCollectionProof
                , ptxFieldReceipt'chunkIndex = previousChunkIndex
                } <-
                pmatch previous
              PItemProofV1
                { pitemProof'fieldIndex = previousFieldIndex
                , pitemProof'itemIndex = previousItemIndex
                } <-
                pmatch (pfromData previousCollectionProof)
              let previousAssetName =
                    pfieldReceiptAssetName
                      # previousTxOrderPolicyId
                      # previousTxOrderId
                      # pfromData previousTransactionCommitment
                      # pfromData previousFieldIndex
                      # pfromData previousItemIndex
                      # pfromData previousChunkIndex
                  previousEntry =
                    pheadSingleton
                      # (ptokensUnderPolicyOfValue # pfromData predecessorValue # ownPolicy)
              pif
                ( pand'List
                    [ pmatch predecessorCredential $ \case
                        PScriptCredential actualHash -> actualHash #== receiptScriptHash
                        _ -> perror
                    , pmatch predecessorReferenceScript $ \case
                        PDNothing -> pconstant True
                        PDJust _ -> perror
                    , pfromData
                        ( punsafeCoerce
                            @(PAsData (PMaybeData PTxOutRef))
                            ptxFieldReceipt'predecessorReceiptReference
                        )
                        #== pcon (PDJust (pdata predecessorReference))
                    , pfstBuiltin # previousEntry #== pdata previousAssetName
                    , pfromData (psndBuiltin # previousEntry) #== 1
                    ]
                )
                (pcon $ PJust previous)
                perror
          )

    let receiptAssetName =
          pfieldReceiptAssetName
            # ptxFieldPreimage'txOrderPolicyId
            # ptxFieldPreimage'txOrderId
            # pfromData ptxFieldPreimage'transactionCommitment
            # pfromData pchunkProof'fieldIndex
            # pfromData pchunkProof'itemIndex
            # pfromData pchunkProof'chunkIndex
        mintedEntry = pheadSingleton # (ptokensUnderPolicy # mint # ownPolicy)
        outputEntry =
          pheadSingleton # (ptokensUnderPolicyOfValue # pfromData receiptValue # ownPolicy)

    pif
      ( pand'List
          [ pfstBuiltin # mintedEntry #== pdata receiptAssetName
          , pfromData (psndBuiltin # mintedEntry) #== 1
          , pfstBuiltin # outputEntry #== pdata receiptAssetName
          , pfromData (psndBuiltin # outputEntry) #== 1
          ]
      )
      ( pverifyMidgardTransactionFieldChunkV1
          # transactionId
          # pfromData ptxFieldPreimage'transactionCommitment
          # pfromData pnativeSource'compactCbor
          # pfromData pnativeSource'witnessSetCompactCbor
          # pfromData pnativeSource'fieldPreimageLengthsCbor
          # pfromData ptxFieldPreimage'collectionProof
          # pfromData ptxFieldPreimage'proof
          #&& pverifyReceiptChainLink
          # transactionId
          # pfromData ptxFieldPreimage'transactionCommitment
          # source
          # ownPolicy
          # ptxFieldPreimage'txOrderPolicyId
          # ptxFieldPreimage'txOrderId
          # predecessor
          # receipt
      )
      perror

pdecodeInlineDatum ::
  forall (a :: S -> Type) (s :: S).
  PIsData a =>
  Term s POutputDatum ->
  Term s a
pdecodeInlineDatum datum =
  pfromData $
    punsafeCoerce @(PAsData a) $
      pmatch datum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror

{- | Aiken @tx_field_receipt_v1.validate@ — the @BurnReceipts@ branch.

The redeemer supplies one input index per receipt being burnt, and the two lists
— the policy's entries in @tx.mint@, and those indices — are consumed in
lockstep by 'pverifyReceiptBurns'. Two guards sit in front of that walk.

At least one token must be burnt, so the branch cannot be used as a no-op that
mints nothing and proves nothing. And the indices must be pairwise distinct: the
walk pairs each burnt name with the input at the next index, so a repeated index
would let one receipt UTxO account for two burnt tokens.
-}
pvalidateBurnReceipts ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PMintValue
        :--> PBuiltinList (PAsData PInteger)
        :--> PBool
    )
pvalidateBurnReceipts = phoistAcyclic $
  plam $ \receiptScriptHash ownPolicy inputs mint receiptInputIndices -> P.do
    burnedTokens <- plet $ ptokensUnderPolicy # mint # ownPolicy
    pif
      ( pand'List
          [ pnot #$ pnull # burnedTokens
          , pallDistinct # receiptInputIndices
          ]
      )
      ( pverifyReceiptBurns
          # receiptScriptHash
          # ownPolicy
          # inputs
          # mint
          # burnedTokens
          # receiptInputIndices
      )
      perror

{- | Aiken @tx_field_receipt_v1.verify_receipt_burns@.

Walks the burnt tokens and the supplied input indices together. For each burnt
token there must be an input that is a receipt UTxO holding exactly that token,
whose datum reproduces the token's name from its own coordinates, and whose burn
is authorised by the order being burnt in the same transaction.

The lockstep is the point. Ending with tokens left over would mean a name was
burnt with no receipt behind it; ending with indices left over is rejected too,
which is what keeps the redeemer from carrying unrelated inputs along.

Both lists are walked to exhaustion together, so the check is total in both
directions — and every failure inside is an @expect@, so this errors rather than
returning @False@.
-}
pverifyReceiptBurns ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PMintValue
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        :--> PBuiltinList (PAsData PInteger)
        :--> PBool
    )
pverifyReceiptBurns = phoistAcyclic $
  plam $ \receiptScriptHash ownPolicy inputs mint burnedTokens receiptInputIndices ->
    let go = pfix $ \self -> plam $ \tokens indices ->
          pelimList
            ( \tokenEntry restTokens ->
                P.do
                  assetName <- plet $ pfstBuiltin # tokenEntry
                  -- expect quantity == -1
                  pif
                    (pfromData (psndBuiltin # tokenEntry) #== -1)
                    ( pelimList
                        ( \indexData restIndices ->
                            pif
                              ( pburnedReceiptIsAccountedFor
                                  receiptScriptHash
                                  ownPolicy
                                  inputs
                                  mint
                                  assetName
                                  (pfromData indexData)
                              )
                              (self # restTokens # restIndices)
                              perror
                        )
                        -- expect [input_index, ..rest_indices] = receipt_input_indices
                        perror
                        indices
                    )
                    perror
            )
            -- expect [] = receipt_input_indices
            (pif (pnull # indices) (pconstant True) perror)
            tokens
     in go # burnedTokens # receiptInputIndices

{- | One step of the walk: the input at @index@ is the receipt whose token
@assetName@ is being burnt.

Five conditions, and it is worth being clear on why the name is checked twice
over. The datum's coordinates must /hash/ to the burnt name, which is what stops
a receipt being destroyed under some other chunk's name; and the input's own
value must /hold/ that name, which is what stops the redeemer pointing at a
receipt UTxO that is not the one the burnt token came from.
-}
pburnedReceiptIsAccountedFor ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s (PAsData PTokenName) ->
  Term s PInteger ->
  Term s PBool
pburnedReceiptIsAccountedFor receiptScriptHash ownPolicy inputs mint assetName index = P.do
  PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pelemAt # index # inputs)
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
      punsafeCoerce @(PAsData PTxFieldReceiptV1) $
        pmatch ptxOut'datum $ \case
          POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
          _ -> perror
  PTxFieldReceiptV1
    { ptxFieldReceipt'fieldReceiptPolicyId
    , ptxFieldReceipt'txOrderPolicyId
    , ptxFieldReceipt'txOrderId
    , ptxFieldReceipt'transactionCommitment
    , ptxFieldReceipt'collectionProof
    , ptxFieldReceipt'chunkIndex
    } <-
    pmatch (pfromData receipt)
  PItemProofV1 {pitemProof'fieldIndex, pitemProof'itemIndex} <-
    pmatch (pfromData ptxFieldReceipt'collectionProof)
  pand'List
    [ pmatch paddress'credential $ \case
        PScriptCredential h -> h #== receiptScriptHash
        PPubKeyCredential _ -> perror
    , pmatch ptxOut'referenceScript $ \case
        PDNothing -> pconstant True
        PDJust _ -> perror
    , ptxFieldReceipt'fieldReceiptPolicyId #== ownPolicy
    , pdata
        ( pfieldReceiptAssetName
            # ptxFieldReceipt'txOrderPolicyId
            # ptxFieldReceipt'txOrderId
            # pfromData ptxFieldReceipt'transactionCommitment
            # pfromData pitemProof'fieldIndex
            # pfromData pitemProof'itemIndex
            # pfromData ptxFieldReceipt'chunkIndex
        )
        #== assetName
    , -- expect [Pair(input_asset_name, 1)] = value |> tokens(own_policy)
      plet (pheadSingleton # (ptokensUnderPolicyOfValue # pfromData ptxOut'value # ownPolicy)) $
        \entry ->
          pand'List
            [ pfstBuiltin # entry #== assetName
            , pfromData (psndBuiltin # entry) #== 1
            ]
    , pfieldReceiptBurnAuthorized # receipt # mint
    ]

{- | Aiken @assets.tokens@ over the mint field, flattened with @dict.to_pairs@.

An absent policy yields the empty list, matching @tokens@ on a value that does
not mention it.
-}
ptokensUnderPolicy ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptokensUnderPolicy = phoistAcyclic $
  plam $ \mint policy ->
    pmatch (AssocMap.plookup # pfromData policy # pto (pto mint)) $ \case
      PNothing -> pcon PNil
      PJust tokenMap -> pto (pto tokenMap)

-- | 'ptokensUnderPolicy' for an ordinary output value rather than the mint field.
ptokensUnderPolicyOfValue ::
  forall (s :: S).
  Term
    s
    ( PLedgerValue
        :--> PAsData PCurrencySymbol
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptokensUnderPolicyOfValue = phoistAcyclic $
  plam $ \value policy ->
    pmatch (AssocMap.plookup # pfromData policy # pto (pto value)) $ \case
      PNothing -> pcon PNil
      PJust tokenMap -> pto (pto tokenMap)

{- | Aiken's @list.length(list.unique(xs)) == list.length(xs)@.

Quadratic, like the original — these lists are one entry per receipt burnt in a
single transaction.
-}
pallDistinct ::
  forall (s :: S). Term s (PBuiltinList (PAsData PInteger) :--> PBool)
pallDistinct = phoistAcyclic $
  pfix $ \self -> plam $ \xs ->
    pelimList
      ( \x rest ->
          pand'List
            [ pnot #$ pelem # x # rest
            , self # rest
            ]
      )
      (pconstant True)
      xs
