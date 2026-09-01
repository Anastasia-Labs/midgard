{- |
Module      : Midgard.NativeTxMachineWalk
Description : Plutarch port of @lib/midgard/native-tx-machine-walk-v1.ak@.

The dispute machine's walk core — the spec's §10. "Midgard.NativeTxFieldAccess"
is the one door: it authenticates a field's preimage against the positionally
extracted flat hash and hands back a 'PFieldViewV1' to slice. This module is
what the machine does /with/ that view when one transaction is not enough — it
turns a view into a __resumable walk__.

Three properties it owns.

__Authenticate-once is structural, not a convention__ (§7.1). A walk opens a
view exactly once per transaction and then carries a /position/. Every further
item costs a wrapper decode and a slice, never another hash over the preimage,
so a dispute touching a field pays that field's full-preimage hash check once
however many items it reads.

__Positions, not bytes__ (§7.6). 'PFieldWalkCheckpointV1' is five scalars and
one 32-byte transaction id. Its wire form is exactly
'pfieldWalkCheckpointBytes' long /whatever the field holds/, so it cannot be
smuggling preimage content: two walks that reach the same position over
different preimage bytes serialise identically.

__Arithmetic where the format bought it__ (§5.3, §10.4). Fields 0/1, 3/4 and 7
have fixed strides, so relocating inside them is arithmetic and their
checkpoints are self-authenticating in O(1). Only the variable-width fields
2/5/6/8 actually walk.

=== What a checkpoint does and does not authenticate (§10.2)

For a fixed-stride field the pair @(next_item_index, next_offset)@ satisfies
@next_offset == header_len + stride · next_item_index@, so a resume re-derives
it and a forged position simply fails. For a variable-width field the position
is a function of the preimage that costs a full re-walk to recompute, so it is
deliberately /not/ recomputed: its integrity is inductive, from the step chain
that produced it — the opening position is derived rather than supplied, and
every advance is a walk actually performed over authenticated bytes.

What is enforced unconditionally is that a checkpoint cannot be pointed at bytes
it did not come from: identity (@tx_id@, @field_index@) and shape
(@total_length@, @item_count@) must match the freshly authenticated view, and
the offset must land on a decodable §5.1 item head inside it.

=== Opacity is the mechanism, and it survives the port

'PFieldWalkCheckpointV1' is exported __without its constructor__, and
'pdecodeFieldWalkCheckpoint' and the checkpoint-taking resume are not exported
at all. That is the Haskell spelling of Aiken's @opaque type@ plus its two
private functions, and it is load-bearing rather than tidy: outside this module
a position can only have been derived from an authenticated view
('popenFieldWalk'), advanced over authenticated bytes ('pwalkNext',
'pwalkFold', 'pwalkSkip'), or put through 'presumeFieldWalkFromCommitment' —
the digest check and then the §10.2 binding check. There is no fourth way in,
so a literal checkpoint at a prover-chosen offset — the one thing §10.2 item 7
cannot catch — is not a thing a caller can write.

What this module cannot decide is where a caller's @committed@ digest came
from. A validator that sourced it from a redeemer rather than from thread state
would be back to trusting the prover's arithmetic on a variable-width position.
That half of §10.6 is normative on dispute entry points and is not something a
library can hold for them.

=== One grammar, one verdict

Nothing here reads a §5.1 head on its own: every step goes through the door's
'pfieldItemHeaderAt', so one logical field keeps exactly one verdict (§6.1).

=== A note on strictness

Several guards below are chained with the lazy @#&&@ / @#||@ rather than with
@pand'List@, and that is not a style choice. Aiken's @and {}@ and @or {}@
short-circuit, and here the last conjunct of the §10.2 binding check /reads
bytes/ — it can abort. Evaluating it after an earlier conjunct has already
failed would turn a clean @False@ into an abort, which is a different rejection
mode than the original's.
-}
module Midgard.NativeTxMachineWalk (
  -- * The checkpoint
  -- $opacity
  PFieldWalkCheckpointV1,
  pfieldWalkCheckpointBytes,

  -- * Opening and resuming
  popenFieldWalk,
  presumeFieldWalkFromCommitment,

  -- * Reading a position
  pwalkIsComplete,
  pwalkRemaining,
  pwalkFieldIndex,
  pwalkNextItemIndex,
  pwalkTxId,

  -- * Advancing
  pwalkNext,
  pwalkFold,
  pwalkSkip,

  -- * The fixed-stride shortcut
  pspendInputAt,
  pspendInputCount,

  -- * Wire form
  pencodeFieldWalkCheckpoint,
  pfieldWalkCheckpointHash,
) where

{- $opacity
'PFieldWalkCheckpointV1' is exported without its constructor on purpose; see the
module header. Do not add @(..)@ here.
-}

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (
  pbyteStringToInteger,
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.FraudProofs.NativeTx.Codec (pbyteAt, psliceLen)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTxInput,
  PNativeTxWitnessSetCompact,
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1,
  PFieldViewV1,
  pauthenticatedFieldView,
  pfieldCount,
  pfieldHeaderLen,
  pfieldItemAt,
  pfieldItemCount,
  pfieldItemHeaderAt,
  pfieldReadRange,
  pfieldTotalLength,
  pfieldViewStride,
  pfixedItemWrapperBytes,
  pmaxFieldItemCount,
  pmaxTransactionAggregateFieldBytes,
  pspendInputItemBytes,
  pspendInputStride,
  pwalkDerivedStride,
 )

--------------------------------------------------------------------------------
-- §10.2 the checkpoint
--------------------------------------------------------------------------------

{- | Aiken @native_tx_machine_walk_v1.FieldWalkCheckpointV1@.

Where a walk over one field of one transaction has got to — and nothing else
(§7.6). @nextItemIndex@ items are done; @nextOffset@ is the byte offset of the
__wrapper__ of the item that is not. @totalLength@ and @itemCount@ pin the shape
of the view the positions were taken against, so a checkpoint from a
differently-shaped carriage of the same field cannot be resumed.

There is no accumulator here on purpose. What a rule /learns/ from a walk is the
rule's own business and already has a home — the machine's work root commits it
— while the walk core's contract is only "where I was". Folding an accumulator
in would make every consuming rule share one state shape and would put
rule-specific bytes one refactor away from a structure §7.6 requires to stay
positional.

Scott-encoded: it never crosses a data boundary. Its /thread-carriable/ form is
'pencodeFieldWalkCheckpoint''s 53 bytes, not a @Constr@.
-}
data PFieldWalkCheckpointV1 (s :: S) = PFieldWalkCheckpointV1
  { pcheckpoint'txId :: Term s PByteString
  -- ^ The L2 transaction whose field this walks. Bound to the tx-id-verified
  -- compact structures at open and at resume, so a checkpoint cannot be
  -- replayed against another transaction.
  , pcheckpoint'fieldIndex :: Term s PInteger
  -- ^ §2.5's positional field index. Plain hashing (§4) removed field-index
  -- domain separation, so fields 0/1 and 3/4 alias on identical content: the
  -- index is what tells them apart and it therefore travels with the position.
  , pcheckpoint'totalLength :: Term s PInteger
  -- ^ The authenticated preimage length the walk was opened against.
  , pcheckpoint'itemCount :: Term s PInteger
  -- ^ The authenticated item count (§5.2). A field whose count is /not/
  -- authenticated — a variable-width field under tier 3 — cannot be walked at
  -- all: 'pfieldItemCount' aborts rather than hand back the header's
  -- self-asserted number, and this module does not work around it.
  , pcheckpoint'nextItemIndex :: Term s PInteger
  -- ^ Items @[0, nextItemIndex)@ are done.
  , pcheckpoint'nextOffset :: Term s PInteger
  -- ^ Byte offset of item @nextItemIndex@'s §5.1 wrapper; equals
  -- @totalLength@ exactly when the walk is complete.
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PFieldWalkCheckpointV1)

{- | Aiken @native_tx_machine_walk_v1.checkpoint_domain@ — ASCII
@MidgardFieldWalkCheckpointV1@.

New surface: none of §4's prohibited counted-scheme domains is reused.
-}
pcheckpointDomain :: forall (s :: S). Term s PByteString
pcheckpointDomain = phexByteStr "4d6964676172644669656c6457616c6b436865636b706f696e745631"

{- | Aiken @native_tx_machine_walk_v1.field_walk_checkpoint_bytes@ — @53@.

§10.3. The wire form's length is a __constant__, independent of the field, the
carriage tier, the preimage and the position. That is the property that makes
"positions, not bytes" checkable rather than merely asserted.
-}
pfieldWalkCheckpointBytes :: forall (s :: S). Term s PInteger
pfieldWalkCheckpointBytes = 53

--------------------------------------------------------------------------------
-- §10.1 / §10.2 opening and resuming
--------------------------------------------------------------------------------

{- | Aiken @native_tx_machine_walk_v1.open_field_walk@.

Open a walk over field @fieldIndex@ of the transaction committed by @verified@.

The view is authenticated through the one door, so the expected hash is
extracted positionally from the committed compact structures and never supplied
(§4). The opening position is /derived/ from that authenticated view — it is not
a redeemer argument — which is the base case the inductive argument in this
module's header rests on.
-}
popenFieldWalk ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
popenFieldWalk = phoistAcyclic $
  plam $ \verified witnessSet fieldIndex carriage referenceInputs certificatePolicyId -> P.do
    view <-
      plet $
        pauthenticatedFieldView
          # verified
          # witnessSet
          # fieldIndex
          # carriage
          # referenceInputs
          # certificatePolicyId
    PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
    pcon (PPair view (pwalkAtStart # pverified'txId # fieldIndex # view))

-- | Aiken @native_tx_machine_walk_v1.walk_at_start@.
pwalkAtStart ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PFieldViewV1 :--> PFieldWalkCheckpointV1)
pwalkAtStart = phoistAcyclic $
  plam $ \txId fieldIndex view ->
    pcon
      ( PFieldWalkCheckpointV1
          { pcheckpoint'txId = txId
          , pcheckpoint'fieldIndex = fieldIndex
          , pcheckpoint'totalLength = pfieldTotalLength # view
          , pcheckpoint'itemCount = pfieldItemCount # view
          , pcheckpoint'nextItemIndex = 0
          , pcheckpoint'nextOffset = pfieldHeaderLen # view
          }
      )

{- | Aiken @native_tx_machine_walk_v1.resume_field_walk@.

Resume a carried walk in a follow-on transaction. The field is named by the
__checkpoint__, not by a fresh redeemer argument, so a resume cannot be pointed
at a different slot than the one that was opened; and the transaction id must be
this transaction's, so a checkpoint cannot be replayed across disputes.

This transaction pays its own single authenticate-once hash — it holds different
bytes in a different script context and has no way not to — but it re-pays none
of the /items/ the earlier transaction walked.

__Deliberately private.__ §10.6 is normative: dispute entry points must resume
through the commitment, never from a redeemer-supplied checkpoint, and this is
the form that takes a checkpoint the caller already trusts. A variable-width
position is explicitly not recomputable, so exporting this would let a caller
hand a prover-chosen starting position straight through the one check that
cannot catch it.
-}
presumeFieldWalk ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PFieldWalkCheckpointV1
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PFieldViewV1
    )
presumeFieldWalk = phoistAcyclic $
  plam $ \verified witnessSet checkpoint carriage referenceInputs certificatePolicyId -> P.do
    PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
    PFieldWalkCheckpointV1 {pcheckpoint'txId, pcheckpoint'fieldIndex} <- pmatch checkpoint
    pexpecting (pcheckpoint'txId #== pverified'txId) $ P.do
      view <-
        plet $
          pauthenticatedFieldView
            # verified
            # witnessSet
            # pcheckpoint'fieldIndex
            # carriage
            # referenceInputs
            # certificatePolicyId
      pexpecting (pcheckpointIsBoundToView # view # checkpoint) view

{- | Aiken @native_tx_machine_walk_v1.resume_field_walk_from_commitment@.

Resume from __thread state__ rather than from a checkpoint the caller already
trusts — the form every real step uses (§10.6).

A computation thread carries @committed@, the 32-byte
'pfieldWalkCheckpointHash' of the position it stopped at, and the step
re-supplies the 53 positional bytes that hash to it. That is what makes
"positions, not bytes" a working arrangement instead of a restriction: the
thread's state stays one digest whatever the field holds, and the bytes it
commits are re-derivable by anyone from public data.

It is also what closes the one gap 'presumeFieldWalk' cannot close on its own. A
variable-width position is not recomputable in O(1), so a caller that took a raw
checkpoint from a redeemer would be trusting the prover's arithmetic. Here the
position is pinned by a digest the /previous/ step committed, so the only
positions that can be resumed are ones a walk over authenticated bytes actually
reached.
-}
presumeFieldWalkFromCommitment ::
  forall (s :: S).
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact
        :--> PByteString
        :--> PByteString
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
presumeFieldWalkFromCommitment = phoistAcyclic $
  plam $
    \verified witnessSet committed checkpointBytes carriage referenceInputs certificatePolicyId -> P.do
      checkpoint <- plet $ pdecodeFieldWalkCheckpoint # checkpointBytes
      pexpecting ((pfieldWalkCheckpointHash # checkpoint) #== committed) $
        pcon
          ( PPair
              ( presumeFieldWalk
                  # verified
                  # witnessSet
                  # checkpoint
                  # carriage
                  # referenceInputs
                  # certificatePolicyId
              )
              checkpoint
          )

{- | Aiken @native_tx_machine_walk_v1.checkpoint_is_bound_to_view@.

The §10.2 binding check, split out because it is the whole of what a resume can
honestly verify and it should be readable as one thing.

Three of the bounds below are §10.8 backstops rather than live guards, and it is
worth saying which and why. A checkpoint only reaches here through
'pdecodeFieldWalkCheckpoint', which re-encodes what it read and demands the
input back — so 'pencodeFieldWalkCheckpoint''s own domain assertions have
already run on this value. Two of them (@nextItemIndex >= 0@,
@nextItemIndex <= itemCount@) are word-for-word the same condition, and a third
(@nextOffset <= totalLength@) is the same condition on the checkpoint's own
@totalLength@, which the first clause here pins to the view's. They stay because
this function is meant to read as the complete statement of what a resume
verifies, not as the residue left over after another function's bounds.

The chaining is lazy (@#&&@, not @pand'List@) because the final branch reads
bytes and can abort: evaluating it after an earlier conjunct has already failed
would turn a @False@ into an abort.
-}
pcheckpointIsBoundToView ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PBool)
pcheckpointIsBoundToView = phoistAcyclic $
  plam $ \view checkpoint -> P.do
    PFieldWalkCheckpointV1
      { pcheckpoint'totalLength
      , pcheckpoint'itemCount
      , pcheckpoint'nextItemIndex
      , pcheckpoint'nextOffset
      } <-
      pmatch checkpoint
    stride <- plet $ pfieldViewStride # view
    headerLen <- plet $ pfieldHeaderLen # view
    pcheckpoint'totalLength
      #== (pfieldTotalLength # view)
      #&& pcheckpoint'itemCount
      #== (pfieldItemCount # view)
      #&& pcheckpoint'nextItemIndex
      #>= 0
      #&& pcheckpoint'nextItemIndex
      #<= pcheckpoint'itemCount
      -- Live, and the only one of the four bounds that is: a position aimed
      -- into the §5.1 array header passes the encoder and can find a decodable
      -- head there.
      #&& pcheckpoint'nextOffset
      #>= headerLen
      #&& pcheckpoint'nextOffset
      #<= pcheckpoint'totalLength
      #&& pif
        (pcheckpoint'nextItemIndex #== pcheckpoint'itemCount)
        -- §5.1 leaves no trailing bytes, so a finished walk has exactly one
        -- admissible offset whichever kind of field it was. This branch reads
        -- no bytes, so it is also the whole of what refuses a walk that
        -- declares itself finished where it happened to stop.
        (pcheckpoint'nextOffset #== pcheckpoint'totalLength)
        ( pif
            (stride #> pwalkDerivedStride)
            -- §10.2 — self-authenticating in O(1): the position is a function
            -- of the index alone, so a forged offset cannot survive.
            (pcheckpoint'nextOffset #== headerLen + stride * pcheckpoint'nextItemIndex)
            -- Not recomputable without the full re-walk this whole design
            -- exists to avoid. What is checkable in O(1) is that the offset
            -- lands on a decodable §5.1 head whose item ends inside the
            -- authenticated bytes.
            ( pmatch (pfieldItemHeaderAt # view # pcheckpoint'nextOffset) $
                \(PPair payloadOffset len) ->
                  payloadOffset + len #<= pcheckpoint'totalLength
            )
        )

--------------------------------------------------------------------------------
-- §10.4 advancing
--------------------------------------------------------------------------------

-- | Aiken @native_tx_machine_walk_v1.walk_is_complete@.
pwalkIsComplete :: forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PBool)
pwalkIsComplete = phoistAcyclic $
  plam $ \checkpoint ->
    pmatch checkpoint $
      \PFieldWalkCheckpointV1 {pcheckpoint'nextItemIndex, pcheckpoint'itemCount} ->
        pcheckpoint'nextItemIndex #== pcheckpoint'itemCount

-- | Aiken @native_tx_machine_walk_v1.walk_remaining@ — items still to visit.
pwalkRemaining :: forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PInteger)
pwalkRemaining = phoistAcyclic $
  plam $ \checkpoint ->
    pmatch checkpoint $
      \PFieldWalkCheckpointV1 {pcheckpoint'itemCount, pcheckpoint'nextItemIndex} ->
        pcheckpoint'itemCount - pcheckpoint'nextItemIndex

{- | Aiken @native_tx_machine_walk_v1.walk_field_index@ — which of §2.5's nine
slots this walk is over.

A reader, not a route in: §10.6's clause withholds the /constructor/ and the
decoder, because those turn caller-chosen data into a position. Reading a scalar
out of a position a caller already holds grants nothing. §12's fault statements
ask, so that a statement about field 0 cannot be adjudicated against a walk over
field 1 — §4's plain hashing removed field-index domain separation, so nothing
else would notice.
-}
pwalkFieldIndex :: forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PInteger)
pwalkFieldIndex = phoistAcyclic $
  plam $ \checkpoint ->
    pmatch checkpoint $ \PFieldWalkCheckpointV1 {pcheckpoint'fieldIndex} ->
      pcheckpoint'fieldIndex

-- | Aiken @native_tx_machine_walk_v1.walk_next_item_index@.
pwalkNextItemIndex :: forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PInteger)
pwalkNextItemIndex = phoistAcyclic $
  plam $ \checkpoint ->
    pmatch checkpoint $ \PFieldWalkCheckpointV1 {pcheckpoint'nextItemIndex} ->
      pcheckpoint'nextItemIndex

{- | Aiken @native_tx_machine_walk_v1.walk_tx_id@ — the L2 transaction this walk
is over.

'pwalkFieldIndex''s reasoning, one axis over: the field index says /which slot/,
this says /whose/. §12's adjudications ask, so that an accusation is bound to
the transaction it accuses — without it a fault statement is a claim about
/some/ field of /some/ transaction, and the same bytes would prove against any
transaction whose field happened to hold a matching item.
-}
pwalkTxId :: forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PByteString)
pwalkTxId = phoistAcyclic $
  plam $ \checkpoint ->
    pmatch checkpoint $ \PFieldWalkCheckpointV1 {pcheckpoint'txId} -> pcheckpoint'txId

{- | Aiken @native_tx_machine_walk_v1.walk_next@.

The item the checkpoint points at, plus the checkpoint one past it. This is the
walk's single step and the only place a position advances. It reads from
@nextOffset@ — never from item 0 — which is what makes a resumed walk cost its
own items and nothing more.

Three of the assertions here are §10.8 backstops rather than live guards. The
completeness check is one: every complete checkpoint a caller can hold sits at
@totalLength@, so the head read below would refuse anything it does. The
@payloadOffset@ half of the wrapper check is another: the door's head reader
admits minimal widths only, so the payload offset is a function of the declared
length, and every §5.3 stride's @stride - 2@ falls in the middle band. The
@nextOffset <= totalLength@ bound is the third, since the read itself goes
through 'pfieldReadRange', which refuses any extent outside the authenticated
bytes. Each stays as the line that notices if the implication behind it ever
stops holding.
-}
pwalkNext ::
  forall (s :: S).
  Term
    s
    ( PFieldViewV1
        :--> PFieldWalkCheckpointV1
        :--> PPair PByteString PFieldWalkCheckpointV1
    )
pwalkNext = phoistAcyclic $
  plam $ \view checkpoint -> P.do
    PFieldWalkCheckpointV1
      { pcheckpoint'txId
      , pcheckpoint'fieldIndex
      , pcheckpoint'totalLength
      , pcheckpoint'itemCount
      , pcheckpoint'nextItemIndex
      , pcheckpoint'nextOffset
      } <-
      pmatch checkpoint
    pexpecting (pnot #$ pwalkIsComplete # checkpoint) $ P.do
      PPair payloadOffset len <- pmatch (pfieldItemHeaderAt # view # pcheckpoint'nextOffset)
      stride <- plet $ pfieldViewStride # view
      nextOffset <- plet $ payloadOffset + len
      nextItemIndex <- plet $ pcheckpoint'nextItemIndex + 1
      pexpecting
        ( -- §7.2: arithmetic locates an item, it does not excuse reading its
          -- wrapper. For a fixed-stride field the stride pins the wrapper's one
          -- admissible spelling, and the walk holds it to that.
          ( stride
              #== pwalkDerivedStride
              #|| ( payloadOffset
                      #== pcheckpoint'nextOffset + pfixedItemWrapperBytes
                      #&& len
                      #== stride - pfixedItemWrapperBytes
                  )
          )
            -- §7.3 abort, never clamp — and §5.1's no-trailing-bytes rule at
            -- the end.
            #&& nextOffset
            #<= pcheckpoint'totalLength
            #&& ( nextItemIndex
                    #< pcheckpoint'itemCount
                    #|| nextOffset
                    #== pcheckpoint'totalLength
                )
        )
        ( pcon
            ( PPair
                (pfieldReadRange # view # payloadOffset # len)
                ( pcon
                    ( PFieldWalkCheckpointV1
                        { pcheckpoint'txId = pcheckpoint'txId
                        , pcheckpoint'fieldIndex = pcheckpoint'fieldIndex
                        , pcheckpoint'totalLength = pcheckpoint'totalLength
                        , pcheckpoint'itemCount = pcheckpoint'itemCount
                        , pcheckpoint'nextItemIndex = nextItemIndex
                        , pcheckpoint'nextOffset = nextOffset
                        }
                    )
                )
            )
        )

{- | Aiken @native_tx_machine_walk_v1.walk_fold@.

Visit at most @budget@ items, folding each into @state@.

The budget is what makes a walk interruptible: a step takes as many items as its
transaction can afford, commits the returned checkpoint, and the next step
resumes from it. 'pwalkIsComplete' on the returned checkpoint is how a caller
learns whether it finished.

@step@ receives the item's index alongside its bytes, because index-sensitive
rules (order, dedup, "the item at position k") are most of what walks a field,
and recovering the index from a fold state is exactly the sort of bookkeeping
this core exists to remove.
-}
pwalkFold ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PFieldViewV1
        :--> PFieldWalkCheckpointV1
        :--> PInteger
        :--> a
        :--> (a :--> PInteger :--> PByteString :--> a)
        :--> PPair a PFieldWalkCheckpointV1
    )
pwalkFold = phoistAcyclic $
  pfix $ \self -> plam $ \view checkpoint budget state step ->
    pexpecting (budget #>= 0) $
      pif
        (budget #== 0 #|| pwalkIsComplete # checkpoint)
        (pcon (PPair state checkpoint))
        ( P.do
            index <- plet $ pwalkNextItemIndex # checkpoint
            PPair item advanced <- pmatch (pwalkNext # view # checkpoint)
            self # view # advanced # (budget - 1) # (step # state # index # item) # step
        )

{- | Aiken @native_tx_machine_walk_v1.walk_skip@.

Relocate @count@ items forward without visiting them.

For a fixed-stride field this is one multiplication (§10.4) — the whole point of
§5.3's fixed three-byte output index. For a variable-width field there is
nothing to compute from, so it walks; the cost difference is the format's and
not something this module can paper over.
-}
pwalkSkip ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PInteger :--> PFieldWalkCheckpointV1)
pwalkSkip = phoistAcyclic $
  plam $ \view checkpoint count -> P.do
    PFieldWalkCheckpointV1
      { pcheckpoint'txId
      , pcheckpoint'fieldIndex
      , pcheckpoint'totalLength
      , pcheckpoint'itemCount
      , pcheckpoint'nextItemIndex
      } <-
      pmatch checkpoint
    target <- plet $ pcheckpoint'nextItemIndex + count
    pexpecting (count #>= 0 #&& target #<= pcheckpoint'itemCount) $ P.do
      stride <- plet $ pfieldViewStride # view
      pif
        (stride #> pwalkDerivedStride)
        ( pcon
            ( PFieldWalkCheckpointV1
                { pcheckpoint'txId = pcheckpoint'txId
                , pcheckpoint'fieldIndex = pcheckpoint'fieldIndex
                , pcheckpoint'totalLength = pcheckpoint'totalLength
                , pcheckpoint'itemCount = pcheckpoint'itemCount
                , pcheckpoint'nextItemIndex = target
                , pcheckpoint'nextOffset = (pfieldHeaderLen # view) + stride * target
                }
            )
        )
        (pwalkSkipByWalking # view # checkpoint # count)

-- | Aiken @native_tx_machine_walk_v1.walk_skip_by_walking@.
pwalkSkipByWalking ::
  forall (s :: S).
  Term s (PFieldViewV1 :--> PFieldWalkCheckpointV1 :--> PInteger :--> PFieldWalkCheckpointV1)
pwalkSkipByWalking = phoistAcyclic $
  pfix $ \self -> plam $ \view checkpoint count ->
    pif
      (count #<= 0)
      checkpoint
      ( pmatch (pwalkNext # view # checkpoint) $
          \(PPair _item advanced) -> self # view # advanced # (count - 1)
      )

--------------------------------------------------------------------------------
-- §10.5 the fixed-stride shortcut: spend and reference inputs
--------------------------------------------------------------------------------

{- | Aiken @native_tx_machine_walk_v1.spend_input_at@.

§5.3 / §10.5. A spend- or reference-input item is exactly
'pspendInputItemBytes' at 'pspendInputStride', so item @index@ is located by one
multiplication and read by one slice: __no walk is entered and the cost does not
grow with @index@__. That is the return on the format's sole deliberately
non-minimal encoding, the fixed three-byte output index.

The wrapper is still decoded and held to the stride (§7.2), so the O(1) path
admits exactly one byte form for one logical item.

The stride guard is the one doing the refusing. The width check is a §10.8
constants-consistency backstop: once the stride guard has passed, the door's
@field_item_extent@ already pins the length, and this is the one line that goes
red if the three §5.3 constants ever stop agreeing.
-}
pspendInputAt ::
  forall (s :: S). Term s (PFieldViewV1 :--> PInteger :--> PMidgardTxInput)
pspendInputAt = phoistAcyclic $
  plam $ \view index ->
    pexpecting ((pfieldViewStride # view) #== pspendInputStride) $
      plet (pfieldItemAt # view # index) $ \item ->
        pexpecting
          ((plengthBS # item) #== pspendInputItemBytes)
          (pdecodeMidgardTxInputCbor # item)

{- | Aiken @native_tx_machine_walk_v1.spend_input_count@.

The authenticated item count of a field-0/1 view (§5.2). Guarded on the stride
so a variable-width view cannot be read as inputs by accident.
-}
pspendInputCount :: forall (s :: S). Term s (PFieldViewV1 :--> PInteger)
pspendInputCount = phoistAcyclic $
  plam $ \view ->
    pexpecting
      ((pfieldViewStride # view) #== pspendInputStride)
      (pfieldItemCount # view)

--------------------------------------------------------------------------------
-- §10.3 checkpoint wire form
--------------------------------------------------------------------------------

{- | Aiken @native_tx_machine_walk_v1.encode_field_walk_checkpoint@.

The checkpoint as thread-carriable bytes.

Fixed-width scalars, not canonical-minimal ones: §5.3 already establishes that
this format pins a fixed width where a constant size is worth more than
minimality, and here it is worth a great deal. A constant length is what lets a
test /prove/ the structure carries no preimage content instead of re-reading the
encoder and taking its word.
-}
pencodeFieldWalkCheckpoint ::
  forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PByteString)
pencodeFieldWalkCheckpoint = phoistAcyclic $
  plam $ \checkpoint -> P.do
    PFieldWalkCheckpointV1
      { pcheckpoint'txId
      , pcheckpoint'fieldIndex
      , pcheckpoint'totalLength
      , pcheckpoint'itemCount
      , pcheckpoint'nextItemIndex
      , pcheckpoint'nextOffset
      } <-
      pmatch checkpoint
    pexpecting
      ( (plengthBS # pcheckpoint'txId)
          #== 32
          #&& pcheckpoint'fieldIndex
          #>= 0
          #&& pcheckpoint'fieldIndex
          #< pfieldCount
          #&& pcheckpoint'totalLength
          #> 0
          #&& pcheckpoint'totalLength
          #<= pmaxTransactionAggregateFieldBytes
          #&& pcheckpoint'itemCount
          #>= 0
          #&& pcheckpoint'itemCount
          #<= pmaxFieldItemCount
          #&& pcheckpoint'nextItemIndex
          #>= 0
          #&& pcheckpoint'nextItemIndex
          #<= pcheckpoint'itemCount
          #&& pcheckpoint'nextOffset
          #>= 0
          #&& pcheckpoint'nextOffset
          #<= pcheckpoint'totalLength
      )
      ( pconstant "\x86\x58\x20"
          <> pcheckpoint'txId
          <> pconstant "\x41"
          <> pbigEndian 1 pcheckpoint'fieldIndex
          <> pconstant "\x43"
          <> pbigEndian 3 pcheckpoint'totalLength
          <> pconstant "\x43"
          <> pbigEndian 3 pcheckpoint'itemCount
          <> pconstant "\x43"
          <> pbigEndian 3 pcheckpoint'nextItemIndex
          <> pconstant "\x43"
          <> pbigEndian 3 pcheckpoint'nextOffset
      )

{- | Aiken @native_tx_machine_walk_v1.decode_field_walk_checkpoint@.

Fail-closed inverse of 'pencodeFieldWalkCheckpoint'. The bounds are not
re-listed: re-encoding the decoded value and demanding the input back is both
the canonicity check (§6.1 — one admissible spelling) and the range check,
without a second reader of the same grammar to drift from the first.

__Deliberately private, for the same reason the type is opaque.__ A public
decoder is a public constructor: it turns 53 caller-chosen bytes into a
position, which is exactly the capability the opacity exists to withhold. The
only way through it is 'presumeFieldWalkFromCommitment', which decodes and then
holds the result to a digest and to the §10.2 binding check.
-}
pdecodeFieldWalkCheckpoint ::
  forall (s :: S). Term s (PByteString :--> PFieldWalkCheckpointV1)
pdecodeFieldWalkCheckpoint = phoistAcyclic $
  plam $ \bytes ->
    pexpecting ((plengthBS # bytes) #== pfieldWalkCheckpointBytes) $ P.do
      checkpoint <-
        plet $
          pcon
            ( PFieldWalkCheckpointV1
                { pcheckpoint'txId = psliceLen # bytes # 3 # 32
                , pcheckpoint'fieldIndex = pbyteAt # bytes # 36
                , pcheckpoint'totalLength = pbeInt bytes 38
                , pcheckpoint'itemCount = pbeInt bytes 42
                , pcheckpoint'nextItemIndex = pbeInt bytes 46
                , pcheckpoint'nextOffset = pbeInt bytes 50
                }
            )
      pexpecting ((pencodeFieldWalkCheckpoint # checkpoint) #== bytes) checkpoint

{- | Aiken @native_tx_machine_walk_v1.field_walk_checkpoint_hash@.

The 32-byte commitment a computation thread carries in place of the checkpoint
itself, so that thread state stays fixed-size whatever it is tracking.
-}
pfieldWalkCheckpointHash ::
  forall (s :: S). Term s (PFieldWalkCheckpointV1 :--> PByteString)
pfieldWalkCheckpointHash = phoistAcyclic $
  plam $ \checkpoint ->
    pblake2b_256 #$ pcheckpointDomain <> (pencodeFieldWalkCheckpoint # checkpoint)

--------------------------------------------------------------------------------
-- Local helpers
--------------------------------------------------------------------------------

-- | Aiken's @expect cond@ — evaluate to @value@ when @cond@ holds, abort otherwise.
pexpecting :: forall (a :: S -> Type) (s :: S). Term s PBool -> Term s a -> Term s a
pexpecting cond value = pif cond value perror

-- | Aiken @bytearray.to_int_big_endian@ over a three-byte slice at @offset@.
pbeInt :: forall (s :: S). Term s PByteString -> Term s PInteger -> Term s PInteger
pbeInt bytes offset =
  pbyteStringToInteger # pmostSignificantFirst #$ psliceLen # bytes # offset # 3

-- | Aiken @bytearray.from_int_big_endian@, with the width fixed at the call site.
pbigEndian ::
  forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width n = pintegerToByteString # pmostSignificantFirst # width # n
