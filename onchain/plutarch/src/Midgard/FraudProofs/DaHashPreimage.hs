{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.FraudProofs.DaHashPreimage
Description : Plutarch port of @lib/midgard/fraud-proofs/da-hash-preimage/{rule,step-01,step-02}.ak@.

The normative rule, the thread state and the redeemer payloads of the
da-hash-preimage family (GOAL_SPEC.md Q44).

=== The rule

Every leaf of a committed block's @transactions_root@ is a @(key, value)@ pair in
which the key is the canonical native-V1 transaction id /of that value/: a
producer commits @(native_tx_id_v1(body_cbor(value)), value)@ and nothing else.

The violation is a committed leaf whose key is __not__ the hash of its own
value's body preimage. Such a leaf breaks hash/preimage correspondence, and no
other family can ever open it: every native family runs
@verify_native_tx_compact_cbor_v1@, which requires
@native_tx_id_for_version(version, body_cbor) == key@. The block therefore hides
a transaction nothing can dispute — which is why the catalogue records this row's
severity as /provability/ rather than as a ledger fault.

=== Why the check needs no decoder

This is the one family whose step-01 must __not__ run the native codec
precondition, because that precondition is exactly the property in dispute. What
replaces it is arithmetic. The canonical compact encoding is fixed-framed at both
ends:

@
0x84 ‖ serialise(version) ‖ body_cbor ‖ 0x58 0x20 ‖ wsh32 ‖ validity
\\___________  ___________/            \\____________  ____________/
            2 bytes                              35 bytes
@

so the body preimage of an honest leaf is exactly
@slice(value, 2, len(value) - 37)@. The verifier re-hashes that slice and compares
it with the committed key.

The soundness argument runs in both directions and neither needs the leaf to be
decodable. For an /honest/ leaf the slice __is__ the encoder's @body_cbor@, so the
derived id always equals the key and the challenge can never finalize. For a
/faulty/ leaf the same total computation convicts, whether the value is a
well-formed transaction committed under a foreign key or arbitrary bytes.
-}
module Midgard.FraudProofs.DaHashPreimage (
  -- * The rule
  pcompactV1HeadByteCount,
  pcompactV1TailByteCount,
  pcompactV1FrameByteCount,
  pcommittedLeafBodyCborV1,
  pderiveCommittedLeafTxIdV1,
  pcommittedLeafIsUnderframedV1,
  pisDaHashPreimageViolationV1,

  -- * The thread
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (psliceLen)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxIdForVersion)
import Midgard.FraudProofs.NativeTx.Types (pnativeTxVersionV1)

-- | Aiken @rule.compact_v1_head_byte_count@ — @0x84@ plus the canonical version byte.
pcompactV1HeadByteCount :: forall (s :: S). Term s PInteger
pcompactV1HeadByteCount = 2

{- | Aiken @rule.compact_v1_tail_byte_count@.

The definite 32-byte witness-set hash (@0x58 0x20@ plus 32) and the single-byte
canonical validity code, which @expect_validity_code@ bounds to @0..=5@ and so
never widens past one byte.
-}
pcompactV1TailByteCount :: forall (s :: S). Term s PInteger
pcompactV1TailByteCount = 35

{- | Aiken @rule.compact_v1_frame_byte_count@ — the shortest a canonical leaf can
be, which is the framing alone around an empty body.
-}
pcompactV1FrameByteCount :: forall (s :: S). Term s PInteger
pcompactV1FrameByteCount = pcompactV1HeadByteCount + pcompactV1TailByteCount

{- | Aiken @rule.committed_leaf_body_cbor_v1@.

__Total.__ A leaf shorter than the canonical frame clamps to the empty slice
rather than erroring — and that clamp is not a §7.3 violation, because it is not
standing in for a value the caller will act on: such a leaf is convicted outright
by 'pcommittedLeafIsUnderframedV1' before the derived id is ever compared.
-}
pcommittedLeafBodyCborV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pcommittedLeafBodyCborV1 = phoistAcyclic $
  plam $ \committedLeafValue ->
    plet (plengthBS # committedLeafValue - pcompactV1FrameByteCount) $ \bodyByteCount ->
      pif
        (bodyByteCount #<= 0)
        (pconstant "")
        (psliceLen # committedLeafValue # pcompactV1HeadByteCount # bodyByteCount)

{- | Aiken @rule.derive_committed_leaf_tx_id_v1@.

The canonical native-V1 transaction id the committed leaf value actually commits
to. Never fails.
-}
pderiveCommittedLeafTxIdV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pderiveCommittedLeafTxIdV1 = phoistAcyclic $
  plam $ \committedLeafValue ->
    pnativeTxIdForVersion
      # pnativeTxVersionV1
      # (pcommittedLeafBodyCborV1 # committedLeafValue)

{- | Aiken @rule.committed_leaf_is_underframed_v1@.

A leaf too short to carry the canonical frame cannot be a transaction at all, so
its key cannot be a hash-preimage commitment of it.
-}
pcommittedLeafIsUnderframedV1 :: forall (s :: S). Term s (PInteger :--> PBool)
pcommittedLeafIsUnderframedV1 = phoistAcyclic $
  plam $ \committedLeafByteCount -> committedLeafByteCount #< pcompactV1FrameByteCount

{- | Aiken @rule.is_da_hash_preimage_violation_v1@.

The adjudicated predicate, stated over the three values step-01 commits into the
thread. Underframing is checked first and short-circuits: for a leaf too short to
frame, the derived id is a hash of the empty slice and comparing it would be
comparing against nothing.
-}
pisDaHashPreimageViolationV1 ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PBool)
pisDaHashPreimageViolationV1 = phoistAcyclic $
  plam $ \committedTxId derivedTxId committedLeafByteCount ->
    pif
      (pcommittedLeafIsUnderframedV1 # committedLeafByteCount)
      (pconstant True)
      (pnot # (derivedTxId #== committedTxId))

{- | Aiken @da_hash_preimage/step_02.State@ — the evidence triple.

Every component is derived in step-01 from authenticated bytes, so step-02
adjudicates committed values only. The byte count travels alongside the two ids
because the underframed case has no meaningful derived id to compare.
-}
data PStep02State (s :: S) = PStep02State
  { -- | MPF key the block committed the leaf under.
    pstep02State'committedTxId :: Term s (PAsData PByteString)
  , -- | Canonical native-V1 id the committed leaf value itself commits to.
    pstep02State'derivedTxId :: Term s (PAsData PByteString)
  , -- | Byte length of the committed leaf value.
    pstep02State'committedLeafByteCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @da_hash_preimage/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
