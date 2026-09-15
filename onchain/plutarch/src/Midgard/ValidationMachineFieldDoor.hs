{- |
Module      : Midgard.ValidationMachineFieldDoor
Description : Plutarch port of @lib/midgard/validation-machine-field-door-v1.ak@.

The validation machine consumes the flat §4 field commitments through the
same authenticated field door as every other consumer. A machine step supplies
only a carriage and the field/item indices; item bytes, lengths, counts and any
bounded-item commitment are derived from the authenticated whole preimage.

Every carriage tier is materialised as a whole view here. Machine phases rely
on the authenticated item count and often carry partially consumed items into
later steps, so the lazy tier-3 view is not sufficient at this boundary.
-}
module Midgard.ValidationMachineFieldDoor (
  PMachineFieldDoorV1 (..),
  PMachineFieldItemV1 (..),
  popenMachineFieldItem,
  popenMachineFieldItemAt,
  pmachineFieldCount,
  pmachineFieldItemCount,
  pmachineFieldItemBytes,
  pmachineFieldItemChunk,
  pmachineFieldItemCommitment,
  pmachineFieldItemChunkCount,
  pmachineFieldItemLength,
  pmachineFieldNextItemOffset,
  pmachineFieldItemBytesMatch,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottStruct (..))

import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxWitnessSetCompact,
  PVerifiedMidgardNativeTxCompact,
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1,
  PFieldViewV1,
  pauthenticatedWholeFieldView,
  pfieldItemCount,
  pfieldItemExtent,
  pfieldItemHeaderAt,
  pfieldReadRange,
 )

-- | Context needed by the three field-carriage tiers.
data PMachineFieldDoorV1 (s :: S) = PMachineFieldDoorV1
  { pmachineDoor'referenceInputs :: Term s (PBuiltinList (PAsData PTxInInfo))
  , pmachineDoor'certificatePolicyId :: Term s (PAsData PCurrencySymbol)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct PMachineFieldDoorV1)

-- | One item located inside an authenticated whole field view.
data PMachineFieldItemV1 (s :: S) = PMachineFieldItemV1
  { pmachineItem'view :: Term s PFieldViewV1
  , pmachineItem'fieldIndex :: Term s PInteger
  , pmachineItem'itemIndex :: Term s PInteger
  , pmachineItem'itemOffset :: Term s PInteger
  , pmachineItem'itemLength :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct PMachineFieldItemV1)

-- | Authenticate a field and locate an item by its index.
popenMachineFieldItem ::
  forall (s :: S).
  Term
    s
    ( PMachineFieldDoorV1
        :--> PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PMachineFieldItemV1
    )
popenMachineFieldItem = phoistAcyclic $
  plam $ \door verified witnessSet fieldIndex itemIndex carriage -> P.do
    PMachineFieldDoorV1 {pmachineDoor'referenceInputs, pmachineDoor'certificatePolicyId} <-
      pmatch door
    view <- plet $
      pauthenticatedWholeFieldView
        # verified
        # witnessSet
        # fieldIndex
        # carriage
        # pmachineDoor'referenceInputs
        # pmachineDoor'certificatePolicyId
    PPair itemOffset itemLength <- pmatch (pfieldItemExtent # view # itemIndex)
    pcon
      PMachineFieldItemV1
        { pmachineItem'view = view
        , pmachineItem'fieldIndex = fieldIndex
        , pmachineItem'itemIndex = itemIndex
        , pmachineItem'itemOffset = itemOffset
        , pmachineItem'itemLength = itemLength
        }

-- | Authenticate a field and locate an item from a carried wrapper offset.
popenMachineFieldItemAt ::
  forall (s :: S).
  Term
    s
    ( PMachineFieldDoorV1
        :--> PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PMachineFieldItemV1
    )
popenMachineFieldItemAt = phoistAcyclic $
  plam $ \door verified witnessSet fieldIndex itemIndex itemWrapperOffset carriage -> P.do
    PMachineFieldDoorV1 {pmachineDoor'referenceInputs, pmachineDoor'certificatePolicyId} <-
      pmatch door
    view <- plet $
      pauthenticatedWholeFieldView
        # verified
        # witnessSet
        # fieldIndex
        # carriage
        # pmachineDoor'referenceInputs
        # pmachineDoor'certificatePolicyId
    PPair itemOffset itemLength <- pmatch (pfieldItemHeaderAt # view # itemWrapperOffset)
    pcon
      PMachineFieldItemV1
        { pmachineItem'view = view
        , pmachineItem'fieldIndex = fieldIndex
        , pmachineItem'itemIndex = itemIndex
        , pmachineItem'itemOffset = itemOffset
        , pmachineItem'itemLength = itemLength
        }

-- | Authenticate a field and return its item count without locating an item.
pmachineFieldCount ::
  forall (s :: S).
  Term
    s
    ( PMachineFieldDoorV1
        :--> PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PInteger
    )
pmachineFieldCount = phoistAcyclic $
  plam $ \door verified witnessSet fieldIndex carriage ->
    pmatch door $ \PMachineFieldDoorV1 {pmachineDoor'referenceInputs, pmachineDoor'certificatePolicyId} ->
      pfieldItemCount
        # ( pauthenticatedWholeFieldView
              # verified
              # witnessSet
              # fieldIndex
              # carriage
              # pmachineDoor'referenceInputs
              # pmachineDoor'certificatePolicyId
          )

-- | The authenticated field count carried by an opened item.
pmachineFieldItemCount :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PInteger)
pmachineFieldItemCount = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'view} ->
      pfieldItemCount # pmachineItem'view

-- | The item's full payload bytes.
pmachineFieldItemBytes :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PByteString)
pmachineFieldItemBytes = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'view, pmachineItem'itemOffset, pmachineItem'itemLength} ->
      pfieldReadRange # pmachineItem'view # pmachineItem'itemOffset # pmachineItem'itemLength

-- | One bounded-item chunk sliced from the authenticated item.
pmachineFieldItemChunk ::
  forall (s :: S). Term s (PMachineFieldItemV1 :--> PInteger :--> PByteString)
pmachineFieldItemChunk = phoistAcyclic $
  plam $ \item chunkIndex ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'view, pmachineItem'itemOffset, pmachineItem'itemLength} ->
      plet (BoundedItem.pexpectedChunkLength # pmachineItem'itemLength # chunkIndex) $ \length' ->
        pfieldReadRange
          # pmachineItem'view
          # (pmachineItem'itemOffset + chunkIndex * BoundedItem.pchunkBytes)
          # length'

-- | A bounded-item commitment derived from authenticated bytes.
pmachineFieldItemCommitment :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PByteString)
pmachineFieldItemCommitment = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'fieldIndex, pmachineItem'itemIndex} ->
      BoundedItem.pfromBytes
        # pmachineItem'fieldIndex
        # pmachineItem'itemIndex
        # (pmachineFieldItemBytes # item)

-- | Number of bounded-item chunks in the authenticated item.
pmachineFieldItemChunkCount :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PInteger)
pmachineFieldItemChunkCount = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'itemLength} ->
      BoundedItem.pchunkCount # pmachineItem'itemLength

-- | The authenticated payload length.
pmachineFieldItemLength :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PInteger)
pmachineFieldItemLength = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'itemLength} -> pmachineItem'itemLength

-- | Offset of the next item's §5.1 wrapper.
pmachineFieldNextItemOffset :: forall (s :: S). Term s (PMachineFieldItemV1 :--> PInteger)
pmachineFieldNextItemOffset = phoistAcyclic $
  plam $ \item ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'itemOffset, pmachineItem'itemLength} ->
      pmachineItem'itemOffset + pmachineItem'itemLength

-- | Whether candidate bytes are exactly the authenticated item payload.
pmachineFieldItemBytesMatch ::
  forall (s :: S). Term s (PMachineFieldItemV1 :--> PByteString :--> PBool)
pmachineFieldItemBytesMatch = phoistAcyclic $
  plam $ \item bytes ->
    pmatch item $ \PMachineFieldItemV1 {pmachineItem'itemLength} ->
      plengthBS # bytes #== pmachineItem'itemLength
        #&& pmachineFieldItemBytes # item #== bytes
