{- |
Module      : Midgard.FraudProofs.FieldOpening
Description : Plutarch port of @lib/midgard/fraud-proofs/field-opening-v1.ak@.

How a family step __downstream of its own step-01__ reaches one of the nine
committed fields — the spec's §4, §8.8 and §10.

'Midgard.NativeTxFieldAccess.pauthenticatedFieldView' is the one door, and it
takes the tx-id-verified /compact structures/, never a free-standing field hash.
A family's step-01 has those structures in hand —
@pass_native_tx_to_next_step@ hands them over — but a step-02 and beyond does
not: it arrives holding whatever the computation thread's datum carried forward.
This module is the bridge, and it is a bridge with exactly one plank: __the
thread carries the transaction id, and the step re-derives the compact
structures from redeemer bytes bound to it.__

=== Why the transaction id and not the field hash

Under §4's plain hashing a field commitment carries no field index and no domain
tag, so field 0 and field 1 preimages with the same items hash alike, and a hash
on its own says nothing about which slot it came from. Carrying the /hash/
forward is what forced every downstream step to re-hash a whole reproduced item
list to use it — the shape measured as the Q1X-F6 defect, where reproducing the
spend-input collection put two families past the ledger's memory cap at the
admissible 296-input cardinality. Carrying the /transaction id/ forward instead
costs the same 32 bytes of thread state and buys the door: positional extraction
happens inside it, and the read that follows is a slice.

=== What binds what

  * The __anchor__ comes from thread state, which the family's own step-01 wrote
    off the compact structure the block's @transactions_root@ committed. It is
    never a redeemer argument here.
  * The compact CBOR comes from the redeemer, and
    @verify_native_tx_compact_cbor_v1@ re-derives the id from it: wrong bytes
    simply fail. Nothing is trusted about its provenance.
  * The __field index is a literal at the call site__, never a redeemer
    argument, so a step that means to read field 0 cannot be steered onto field
    1 — which matters precisely because §4 removed the domain separation that
    used to make that impossible.
  * The __carriage tier__ is the prover's choice among §8's three, and the door
    verifies whichever is named. For tier 3 the certificate carries the field
    commitment welded by the minting policy, and the door requires it to equal
    the commitment reached from these anchored structures. This applies to all
    fields, including witness-set fields 6–8.

=== The two-arm split, twice

The @Body@/@Witness@ split of 'PFieldOpeningV1' is §2.5's field table made
structural. Fields 0–5 are committed by the transaction body and the door reads
them without consulting any witness set; fields 6–8 live in the witness set, and
the door will only read them once the supplied witness set re-derives to a
@witness_set_hash@ __the thread anchored__. Having two constructors rather than
one optional field means a step cannot supply a witness set that is silently
ignored, and cannot omit one that is silently required.

'PNativeTxAnchorV1' is split the same way, for a reason that is easy to get
wrong: __the transaction id does not commit the witness set.__ §3's two-level
split hashes the /body/ CBOR alone, so of the §2.5 compact structure's three
parts — body, @witness_set_hash@, @validity_code@ — the id covers exactly one.
Re-deriving the id from redeemer bytes therefore pins fields 0–5 and pins
nothing whatever about fields 6–8: a prover may hand over the genuine body
followed by any trailing 34 bytes it likes, and those bytes re-derive to the
same transaction id. So the @witness_set_hash@ has to arrive from somewhere the
prover does not choose, and the only such place is the same thread state the id
arrives from.

=== Paying the anchor once

A step that opens __more than one field__ of the same transaction says so once.
'panchoredNativeTx' verifies the anchor and hands back a 'PAnchoredNativeTxV1';
'panchoredFieldView' and 'panchoredFieldWalk' then open each field against it,
one carriage per field, with the §2.5 pairing re-run every time. The
single-field 'popenedFieldView' and 'popenedFieldWalk' are exactly
that sequence with one field in it.

Why the split is worth a type: re-deriving the transaction id costs roughly 211k
memory units, and a two-field step through the single-field entry points paid it
twice — while the obvious cheap fix, reaching the door directly with one
pre-verified structure, buys the saving by stepping outside both guards.

=== Opacity, and one forced evaluation

'PAnchoredNativeTxV1' is exported __without its constructor__, which is the
Haskell spelling of Aiken's @opaque type@: 'panchoredNativeTx' is the only way
to obtain one, so "these structures were checked against the thread's anchor" is
a property of the value rather than a claim its holder makes about itself.

Aiken evaluates the codec check strictly; Plutarch would not. The guard in
'panchoredNativeTx' therefore opens with a comparison that is a tautology given
@verify_native_tx_compact_cbor_v1@ — it either returns a record whose id is the
argument or aborts — purely to force the call. Without it a step that obtained a
handle and then took a branch that never read it would validate where Aiken
aborts.
-}
module Midgard.FraudProofs.FieldOpening (
  -- * §2.5's positional field table
  pspendInputsFieldIndex,
  preferenceInputsFieldIndex,
  poutputsFieldIndex,
  prequiredObserversFieldIndex,
  prequiredSignersFieldIndex,
  pmintFieldIndex,
  pscriptWitnessesFieldIndex,
  paddressWitnessesFieldIndex,
  predeemersFieldIndex,
  pfirstWitnessSetFieldIndex,

  -- * Openings and anchors
  PFieldOpeningV1 (..),
  PNativeTxOpeningV1 (..),
  PNativeTxAnchorV1 (..),

  -- * The anchored handle
  -- $opacity
  PAnchoredNativeTxV1,
  panchoredNativeTx,
  panchoredNativeTxVersion,
  punanchoredValidityCodeOf,

  -- * Opening fields against a handle
  panchoredFieldView,
  panchoredFieldWalk,

  -- * Single-field openings
  popenedFieldView,
  popenedFieldWalk,
  presumeOpenedFieldWalk,
  popenedFieldGrammarCertification,
  presumeOpenedFieldGrammarCertification,
  popenedCertifiedFieldWalkFromGrammar,

  -- * Folding a whole field
  pfoldOpenedField,
) where

-- \$opacity
-- 'PAnchoredNativeTxV1' is exported without its constructor on purpose; see the
-- module header. Do not add @(..)@ here.

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottStruct (..))

import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1,
  PFieldViewV1,
  pauthenticatedFieldView,
  pfieldItemCount,
 )
import Midgard.NativeTxMachineWalk (
  PFieldGrammarCheckpointV1,
  PFieldWalkCheckpointV1,
  popenCertifiedFieldWalkFromGrammarCommitment,
  popenFieldGrammarCertification,
  popenFieldWalk,
  presumeFieldGrammarCertificationFromCommitment,
  presumeFieldWalkFromCommitment,
  pwalkFold,
  pwalkIsComplete,
 )

--------------------------------------------------------------------------------
-- §2.5's positional field table
--------------------------------------------------------------------------------

{- $fieldtable
Call sites pass one of these literals rather than a redeemer argument, so which
slot a step reads is a property of the compiled validator. Spelling them out is
not decoration: §4 removed field-index domain separation, so fields 0 and 1 —
and 3 and 4 — commit identically for identical items, and the index is the
/only/ thing that tells a reference-input opening from a spend-input one.
-}

-- | Aiken @field_opening_v1.spend_inputs_field_index@ — @0@.
pspendInputsFieldIndex :: forall (s :: S). Term s PInteger
pspendInputsFieldIndex = 0

-- | Aiken @field_opening_v1.reference_inputs_field_index@ — @1@.
preferenceInputsFieldIndex :: forall (s :: S). Term s PInteger
preferenceInputsFieldIndex = 1

-- | Aiken @field_opening_v1.outputs_field_index@ — @2@.
poutputsFieldIndex :: forall (s :: S). Term s PInteger
poutputsFieldIndex = 2

-- | Aiken @field_opening_v1.required_observers_field_index@ — @3@.
prequiredObserversFieldIndex :: forall (s :: S). Term s PInteger
prequiredObserversFieldIndex = 3

-- | Aiken @field_opening_v1.required_signers_field_index@ — @4@.
prequiredSignersFieldIndex :: forall (s :: S). Term s PInteger
prequiredSignersFieldIndex = 4

-- | Aiken @field_opening_v1.mint_field_index@ — @5@.
pmintFieldIndex :: forall (s :: S). Term s PInteger
pmintFieldIndex = 5

-- | Aiken @field_opening_v1.script_witnesses_field_index@ — @6@.
pscriptWitnessesFieldIndex :: forall (s :: S). Term s PInteger
pscriptWitnessesFieldIndex = 6

-- | Aiken @field_opening_v1.address_witnesses_field_index@ — @7@.
paddressWitnessesFieldIndex :: forall (s :: S). Term s PInteger
paddressWitnessesFieldIndex = 7

-- | Aiken @field_opening_v1.redeemers_field_index@ — @8@.
predeemersFieldIndex :: forall (s :: S). Term s PInteger
predeemersFieldIndex = 8

{- | Aiken @field_opening_v1.first_witness_set_field_index@.

§2.5's split point: fields 0–5 are the body's, 6–8 the witness set's. Named off
the table rather than restated as its own literal, so the boundary and the
indices cannot drift apart.
-}
pfirstWitnessSetFieldIndex :: forall (s :: S). Term s PInteger
pfirstWitnessSetFieldIndex = pscriptWitnessesFieldIndex

--------------------------------------------------------------------------------
-- Openings and anchors
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.FieldOpeningV1@.

Everything a downstream step needs to re-open one field of the transaction its
thread is disputing, and nothing it does not.

Constructor order is wire format — this is a redeemer field on the step
validators — so @BodyFieldOpening@ is @Constr 0@ and @WitnessFieldOpening@
@Constr 1@.
-}
data PFieldOpeningV1 (s :: S)
  = {- | Fields 0–5. No witness set is consulted, so none is carried: a step
    reading the body cannot be handed one to ignore.
    -}
    PBodyFieldOpening
      { pbodyOpening'nativeTxCompactCbor :: Term s (PAsData PByteString)
      , pbodyOpening'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  | {- | Fields 6–8. The witness set is unauthenticated on arrival, and so — for
    these fields — is the compact CBOR's trailing @witness_set_hash@: the
    transaction id does not commit it. Both are checked against the thread's
    @WitnessAnchor@ before anything is read.
    -}
    PWitnessFieldOpening
      { pwitnessOpening'nativeTxCompactCbor :: Term s (PAsData PByteString)
      , pwitnessOpening'witnessSet :: Term s (PAsData PNativeTxWitnessSetCompact)
      , pwitnessOpening'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldOpeningV1)

{- | Aiken @field_opening_v1.NativeTxOpeningV1@.

The transaction-identifying half of an opening: everything that binds the
disputed transaction to the thread's anchor, with __no carriage attached__.

'PFieldOpeningV1' pairs one of these with exactly one carriage, because a step
that opens one field has exactly one preimage to carry. A step that opens /two/
has two preimages and still only one transaction, and that asymmetry is what
this type exists for: the anchor is paid once and each field then names its own
carriage.

__Internal representation, not a frozen wire surface.__ 'PFieldOpeningV1' is a
redeemer field and its constructor order cannot move; this type appears in no
datum, redeemer or validator parameter, has no SDK twin, and is only ever built
inside the caller that immediately hands it to 'panchoredNativeTx'. It is
Scott-encoded here for exactly that reason, and its constructors are laid out in
'PFieldOpeningV1''s order for readability rather than because anything depends
on it.
-}
data PNativeTxOpeningV1 (s :: S)
  = -- | Fields 0–5. No witness set is consulted, so none is carried.
    PBodyTxOpening {pbodyTxOpening'nativeTxCompactCbor :: Term s PByteString}
  | {- | Fields 6–8. Both members are unauthenticated on arrival and are checked
    against the thread's @WitnessAnchor@ before anything is read.
    -}
    PWitnessTxOpening
      { pwitnessTxOpening'nativeTxCompactCbor :: Term s PByteString
      , pwitnessTxOpening'witnessSet :: Term s PNativeTxWitnessSetCompact
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct PNativeTxOpeningV1)

{- | Aiken @field_opening_v1.NativeTxAnchorV1@.

What a family's own step-01 must put in thread state for a downstream step to
re-open a field of the transaction the thread is disputing.

Both constructors are call-site literals, never redeemer arguments: which one a
step uses is a property of the compiled validator, exactly as the field index
is.

The two arms exist because §3's transaction-id preimage is the body alone. For
fields 0–5 the id is a complete anchor and @BodyAnchor@ carries it. For fields
6–8 it is not an anchor at all — the @witness_set_hash@ sits /outside/ the
preimage — so @WitnessAnchor@ carries that hash alongside it. The value must be
the one read off the compact structure the block's counted
@transactions_root@ committed; anything a later redeemer supplies is the
prover's own and anchors nothing.

Constructor order is wire format: @BodyAnchor@ is @Constr 0@, @WitnessAnchor@
@Constr 1@.
-}
data PNativeTxAnchorV1 (s :: S)
  = -- | Fields 0–5. 32 bytes of thread state.
    PBodyAnchor {pbodyAnchor'txId :: Term s (PAsData PByteString)}
  | {- | Fields 6–8. 64 bytes of thread state — the same width the retired
    per-collection-hash idiom cost, spent on a value that covers all three
    witness-set fields instead of one.
    -}
    PWitnessAnchor
      { pwitnessAnchor'txId :: Term s (PAsData PByteString)
      , pwitnessAnchor'witnessSetHash :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxAnchorV1)

{- | Aiken @field_opening_v1.unread_witness_set@.

The witness set handed to the door for a body field.

The door's signature takes one for every field, but for @field_index < 6@ it
short-circuits before looking (§2.5's table is positional and the body's
commitments are in the body). Constructing the unread value __here__ rather than
taking one from the redeemer is the point: there is no argument for a prover to
fill, so nothing can ride in under a field the door will not read.
-}
punreadWitnessSet :: forall (s :: S). Term s PNativeTxWitnessSetCompact
punreadWitnessSet =
  pcon
    ( PNativeTxWitnessSetCompact
        { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant "")
        , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant "")
        , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant "")
        }
    )

--------------------------------------------------------------------------------
-- The anchored handle
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.AnchoredNativeTxV1@.

A disputed transaction whose __anchor has already been paid for__: the compact
structures re-derived from redeemer bytes and checked against thread state,
once, however many fields the step goes on to open.

Opaque on purpose — see the module header. 'panchoredNativeTx' is the only way
to obtain one, so a step cannot assemble the cheap version by hand and pass it
off as the checked one.

@opensWitnessSetFields@ records which §2.5 half the opening was for. It is not a
convenience: the split has to stay enforced __per field opened__, because a
handle checked out under a body opening must still be refused at field 6 —
otherwise the multi-field form would be a way around the very pairing the
single-field form enforces.
-}
data PAnchoredNativeTxV1 (s :: S) = PAnchoredNativeTxV1
  { panchored'verified :: Term s PVerifiedMidgardNativeTxCompact
  , panchored'witnessSet :: Term s PNativeTxWitnessSetCompact
  , panchored'opensWitnessSetFields :: Term s PBool
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottStruct PAnchoredNativeTxV1)

{- | Aiken @field_opening_v1.anchored_native_tx@.

Pay the anchor once (§7.1, one level out from the door's own
authenticate-once).

Both levels §2.5 has are checked here, exactly as the single-field entry points
always checked them: the transaction id is re-derived from the supplied bytes,
and for a witness-set opening the re-derived compact structure's
@witness_set_hash@ must be the __anchored__ one. What is /not/ checked here is
anything per-field — the field index and the carriage tier are
'panchoredFieldView''s and 'panchoredFieldWalk''s, because a handle exists
precisely to be opened at more than one of each.

The witness-set check is the one the transaction id cannot make for itself.
Without it a prover supplies the genuine body — which re-derives to the anchored
id and so satisfies the first check — followed by a @witness_set_hash@ of its
own choosing, and then "authenticates" a witness set of its own choosing against
it. The empty witness set is the useful forgery: it makes every absence rule
(§2.5 fields 6 and 7) true of every transaction, which is a slashing proof
against an honest operator.
-}
panchoredNativeTx ::
  forall (s :: S).
  Term s (PNativeTxOpeningV1 :--> PNativeTxAnchorV1 :--> PAnchoredNativeTxV1)
panchoredNativeTx = phoistAcyclic $
  plam $ \opening anchor ->
    pmatch opening $ \case
      PBodyTxOpening {pbodyTxOpening'nativeTxCompactCbor} ->
        -- The thread anchored a body dispute. A `WitnessAnchor` here would mean
        -- the family's own step-01 recorded a witness-set anchor and this step
        -- is opening it without the witness set that anchor names, so the two
        -- §2.5 halves have to agree before any byte is read.
        pmatch anchor $ \case
          PBodyAnchor {pbodyAnchor'txId} -> P.do
            txId <- plet $ pfromData pbodyAnchor'txId
            verified <-
              plet $ pverifyNativeTxCompactCborV1 # txId # pbodyTxOpening'nativeTxCompactCbor
            PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
            -- Tautological given the verifier, and there to force it; see the
            -- module header.
            pexpecting (pverified'txId #== txId) $
              pcon
                ( PAnchoredNativeTxV1
                    { panchored'verified = verified
                    , panchored'witnessSet = punreadWitnessSet
                    , panchored'opensWitnessSetFields = pconstant False
                    }
                )
          PWitnessAnchor {} -> perror
      PWitnessTxOpening {pwitnessTxOpening'nativeTxCompactCbor, pwitnessTxOpening'witnessSet} ->
        -- The thread anchored a witness-set dispute. A `BodyAnchor` carries no
        -- `witness_set_hash`, so there would be nothing to authenticate the
        -- supplied witness set against.
        pmatch anchor $ \case
          PWitnessAnchor {pwitnessAnchor'txId, pwitnessAnchor'witnessSetHash} -> P.do
            txId <- plet $ pfromData pwitnessAnchor'txId
            verified <-
              plet $ pverifyNativeTxCompactCborV1 # txId # pwitnessTxOpening'nativeTxCompactCbor
            PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
            PNativeTxCompact {pcompact'witnessSetHash} <- pmatch pverified'txCompact
            pexpecting (pcompact'witnessSetHash #== pfromData pwitnessAnchor'witnessSetHash) $
              pcon
                ( PAnchoredNativeTxV1
                    { panchored'verified = verified
                    , panchored'witnessSet = pwitnessTxOpening'witnessSet
                    , panchored'opensWitnessSetFields = pconstant True
                    }
                )
          PBodyAnchor {} -> perror

{- | Aiken @field_opening_v1.anchored_native_tx_version@.

The native-transaction version the anchored compact structure was committed
under.

__Anchored.__ @verify_native_tx_compact_cbor_v1@ requires the version byte to be
present in the compact bytes /and/ feeds it to the id derivation alongside the
body, so it is inside §3's derivation rather than beside it.
-}
panchoredNativeTxVersion ::
  forall (s :: S). Term s (PAnchoredNativeTxV1 :--> PInteger)
panchoredNativeTxVersion = phoistAcyclic $
  plam $ \anchored ->
    pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified} ->
      pmatch panchored'verified $ \PVerifiedMidgardNativeTxCompact {pverified'version} ->
        pverified'version

{- | Aiken @field_opening_v1.unanchored_validity_code_of@.

The §2.5 @validity_code@ sitting in the compact bytes this handle was built
from. __Named for what it is not__: every other function here says @anchored@
because the anchor covers what it returns, and this one does not, so it does not
claim it.

__NOT anchored, and that is not a defect this function can fix.__ §3's id
preimage is the body CBOR alone, so of the compact structure's four parts —
version, body, @witness_set_hash@, @validity_code@ — re-deriving the id pins the
first two and says nothing about the last two. A prover may hand over the
genuine body followed by any trailing @validity_code@ it likes and the bytes
still re-derive to the committed transaction id. A rule that asserts something
/about/ this value is asserting it about the prover's bytes; a rule that needs
the committed value must reach it the way the block committed it, through the
structure its own step-01 was handed.

It is exposed anyway, and narrowly, because the transition-trace family's
@validity_code == 0@ read predates the handle and is unchanged by it. That call
site is sound for a reason this handle knows nothing about: its compact bytes
come out of a @transactions_root@ leaf whose exact bytes it re-checks, so the
value is committed by the /root/, not by the anchor. Any new caller owes the
same argument in its own words, which is what the name is for.

__There is deliberately no accessor for @witness_set_hash@.__ For a witness
opening it is the anchored value; for a body opening it is the prover's, and the
handle does not say which arm produced it. Handing it out would put the one
ingredient a reproduced forgery needs — an unanchored @witness_set_hash@ to
authenticate an arbitrary witness set against — one door call away from a value
the type system labels anchored.
-}
punanchoredValidityCodeOf ::
  forall (s :: S). Term s (PAnchoredNativeTxV1 :--> PInteger)
punanchoredValidityCodeOf = phoistAcyclic $
  plam $ \anchored ->
    pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified} ->
      pmatch panchored'verified $ \PVerifiedMidgardNativeTxCompact {pverified'txCompact} ->
        pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'validityCode} ->
          pcompact'validityCode

--------------------------------------------------------------------------------
-- Opening fields against a handle
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.anchored_field_view@.

Authenticate field @fieldIndex@ of an already-anchored transaction and hand back
a view to slice.

Every guard the single-field 'popenedFieldView' applies is applied here, and
applied __per field__: the §2.5 body/witness pairing against the half the handle
was opened under, and the tier check against the tier this particular field
names. Opening a second field re-runs both; only the anchor is shared.
-}
panchoredFieldView ::
  forall (s :: S).
  Term
    s
    ( PAnchoredNativeTxV1
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PFieldViewV1
    )
panchoredFieldView = phoistAcyclic $
  plam $ \anchored fieldIndex carriage referenceInputs certificatePolicyId ->
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        pauthenticatedFieldView
          # panchored'verified
          # panchored'witnessSet
          # fieldIndex
          # carriage
          # referenceInputs
          # certificatePolicyId

{- | Aiken @field_opening_v1.anchored_field_walk@.

The same per-field authentication, opened as a __walk__ instead of a view (§10).

Returns the view together with the walk's derived starting position, which is
what 'pfoldOpenedField' and any resumable family rule take. The position comes
from the walk core, so it is derived from the authenticated view and is never a
redeemer argument (§10.2).
-}
panchoredFieldWalk ::
  forall (s :: S).
  Term
    s
    ( PAnchoredNativeTxV1
        :--> PInteger
        :--> PFieldCarriageV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
panchoredFieldWalk = phoistAcyclic $
  plam $ \anchored fieldIndex carriage referenceInputs certificatePolicyId ->
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        popenFieldWalk
          # panchored'verified
          # panchored'witnessSet
          # fieldIndex
          # carriage
          # referenceInputs
          # certificatePolicyId

--------------------------------------------------------------------------------
-- Single-field openings
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.opened_field_view@.

Authenticate field @fieldIndex@ of the transaction the thread is disputing and
hand back a view to slice. The anchor is thread state; the opening is the
redeemer's.

The single-field spelling of 'panchoredNativeTx' followed by
'panchoredFieldView', and nothing else: one field, so the anchor is paid once
either way. A step opening two or more fields should hold the handle itself
rather than call this twice.
-}
popenedFieldView ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PFieldViewV1
    )
popenedFieldView = phoistAcyclic $
  plam $ \opening anchor fieldIndex referenceInputs certificatePolicyId ->
    panchoredFieldView
      # (panchoredNativeTx # (ptxOpeningOf # opening) # anchor)
      # fieldIndex
      # (pcarriageOf # opening)
      # referenceInputs
      # certificatePolicyId

{- | Aiken @field_opening_v1.opened_field_walk@.

The same authentication, opened as a __walk__ instead of a view (§10).
-}
popenedFieldWalk ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
popenedFieldWalk = phoistAcyclic $
  plam $ \opening anchor fieldIndex referenceInputs certificatePolicyId ->
    panchoredFieldWalk
      # (panchoredNativeTx # (ptxOpeningOf # opening) # anchor)
      # fieldIndex
      # (pcarriageOf # opening)
      # referenceInputs
      # certificatePolicyId

{- | Aiken @field_opening_v1.resume_opened_field_walk@.

Re-authenticate the transaction and field opening in the current L1 context,
then admit the supplied checkpoint bytes only through the digest committed by
the preceding computation-thread state.
-}
presumeOpenedFieldWalk ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
presumeOpenedFieldWalk = phoistAcyclic $
  plam $ \opening anchor fieldIndex committed checkpointBytes referenceInputs certificatePolicyId -> P.do
    anchored <- plet $ panchoredNativeTx # (ptxOpeningOf # opening) # anchor
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        presumeFieldWalkFromCommitment
          # panchored'verified
          # panchored'witnessSet
          # committed
          # checkpointBytes
          # (pcarriageOf # opening)
          # referenceInputs
          # certificatePolicyId

popenedFieldGrammarCertification ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldGrammarCheckpointV1
    )
popenedFieldGrammarCertification = phoistAcyclic $
  plam $ \opening anchor fieldIndex referenceInputs certificatePolicyId -> P.do
    anchored <- plet $ panchoredNativeTx # (ptxOpeningOf # opening) # anchor
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        popenFieldGrammarCertification
          # panchored'verified
          # panchored'witnessSet
          # fieldIndex
          # (pcarriageOf # opening)
          # referenceInputs
          # certificatePolicyId

presumeOpenedFieldGrammarCertification ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldGrammarCheckpointV1
    )
presumeOpenedFieldGrammarCertification = phoistAcyclic $
  plam $ \opening anchor fieldIndex committed checkpointBytes referenceInputs certificatePolicyId -> P.do
    anchored <- plet $ panchoredNativeTx # (ptxOpeningOf # opening) # anchor
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        presumeFieldGrammarCertificationFromCommitment
          # panchored'verified
          # panchored'witnessSet
          # committed
          # checkpointBytes
          # (pcarriageOf # opening)
          # referenceInputs
          # certificatePolicyId

popenedCertifiedFieldWalkFromGrammar ::
  forall (s :: S).
  Term
    s
    ( PFieldOpeningV1
        :--> PNativeTxAnchorV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
popenedCertifiedFieldWalkFromGrammar = phoistAcyclic $
  plam $ \opening anchor fieldIndex committed checkpointBytes referenceInputs certificatePolicyId -> P.do
    anchored <- plet $ panchoredNativeTx # (ptxOpeningOf # opening) # anchor
    pexpecting (pfieldPairsWith # anchored # fieldIndex) $
      pmatch anchored $ \PAnchoredNativeTxV1 {panchored'verified, panchored'witnessSet} ->
        popenCertifiedFieldWalkFromGrammarCommitment
          # panchored'verified
          # panchored'witnessSet
          # committed
          # checkpointBytes
          # (pcarriageOf # opening)
          # referenceInputs
          # certificatePolicyId

--------------------------------------------------------------------------------
-- The two per-field guards
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.field_pairs_with@.

Asserts that this field may be opened against this handle.

One thing pairs up, and it is per-field rather than per-anchor, which is why it
cannot live in 'panchoredNativeTx': the §2.5 half. An opening that carries no witness set may not be
read at a witness-set index — the door would otherwise be handed this module's
internal 'punreadWitnessSet' as if it were the transaction's real witness set —
and an opening that carries one may not be read at a body index, where it would
be silently ignored. Neither direction is reachable from an honest family, which
is exactly why it is asserted rather than assumed.

Before the certificate field-hash weld a second guard rejected tier-3 carriage
at witness-set fields. The mint now welds the certificate's @field_hash@ to the
verified chunks and the field-access door compares it to the anchored positional
commitment, so every tier reaches every field and that blanket guard is gone.
-}
pfieldPairsWith ::
  forall (s :: S).
  Term s (PAnchoredNativeTxV1 :--> PInteger :--> PBool)
pfieldPairsWith = phoistAcyclic $
  plam $ \anchored fieldIndex ->
    pmatch anchored $ \PAnchoredNativeTxV1 {panchored'opensWitnessSetFields} ->
      pexpecting
        (panchored'opensWitnessSetFields #== (fieldIndex #>= pfirstWitnessSetFieldIndex))
        (pconstant True)

-- | Aiken @field_opening_v1.tx_opening_of@ — the carriage-free half of a single-field opening.
ptxOpeningOf :: forall (s :: S). Term s (PFieldOpeningV1 :--> PNativeTxOpeningV1)
ptxOpeningOf = phoistAcyclic $
  plam $ \opening ->
    pmatch opening $ \case
      PBodyFieldOpening {pbodyOpening'nativeTxCompactCbor} ->
        pcon
          ( PBodyTxOpening
              { pbodyTxOpening'nativeTxCompactCbor = pfromData pbodyOpening'nativeTxCompactCbor
              }
          )
      PWitnessFieldOpening {pwitnessOpening'nativeTxCompactCbor, pwitnessOpening'witnessSet} ->
        pcon
          ( PWitnessTxOpening
              { pwitnessTxOpening'nativeTxCompactCbor =
                  pfromData pwitnessOpening'nativeTxCompactCbor
              , pwitnessTxOpening'witnessSet = pfromData pwitnessOpening'witnessSet
              }
          )

-- | Aiken @field_opening_v1.carriage_of@.
pcarriageOf :: forall (s :: S). Term s (PFieldOpeningV1 :--> PFieldCarriageV1)
pcarriageOf = phoistAcyclic $
  plam $ \opening ->
    pmatch opening $ \case
      PBodyFieldOpening {pbodyOpening'carriage} -> pfromData pbodyOpening'carriage
      PWitnessFieldOpening {pwitnessOpening'carriage} -> pfromData pwitnessOpening'carriage

--------------------------------------------------------------------------------
-- Folding a whole field
--------------------------------------------------------------------------------

{- | Aiken @field_opening_v1.fold_opened_field@.

Fold every item of an opened field, in order, exactly once.

The absence rules — "the required signature is not in the witness set", "the
required native script is not in the script witnesses" — are the ones that
genuinely have to see the whole field, and this is how they see it. It goes
through the walk core rather than indexing, because for the variable-width
fields (2, 5, 6, 8) reading item @i@ re-walks from item 0 every time: indexing an
N-item field costs O(N²) head decodes where a walk costs O(N). The fixed-stride
fields would be fine either way; using one form for both is what keeps the
difference from being an accident of which family wrote the loop.

The budget is the field's own authenticated item count and completion is
asserted, so this is the /non/-resumable case by construction: a rule that says
"no item satisfies P" is only true of a walk that reached the end. A family that
cannot afford the whole field in one transaction wants
"Midgard.NativeTxMachineWalk" directly and a checkpoint in its thread state, not
this.

It takes the walk-opening pair __whole__ rather than a view and a checkpoint
separately. Two arguments would let a caller cross them — the view of one field,
the checkpoint of another — and completion is a property of the /checkpoint/
alone, so a checkpoint taken over a shorter field would report a finished walk
over a longer one and an absence claim would be asserted over items nothing ever
read. No call site crosses them today; taking the pair means none can.

__Two limits, both loud.__ For a variable-width field this is O(N) by
construction, so a family that folds one is buying a cost that grows with the
disputed transaction — measured for the only such caller in this wave at about
2.7× the memory basis at the 224-witness envelope, fitting to roughly 81. And
the item count is only /authenticated/ for a variable-width field under carriage
tiers 1–2: under tier 3 'pfieldItemCount' aborts rather than hand back the §5.1
header's self-assertion, so such a field cannot be folded under certified
carriage at all. Both aborts are unconditional — nothing here clamps.
-}
pfoldOpenedField ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PPair PFieldViewV1 PFieldWalkCheckpointV1
        :--> a
        :--> (a :--> PInteger :--> PByteString :--> a)
        :--> a
    )
pfoldOpenedField = phoistAcyclic $
  plam $ \opened state step -> P.do
    PPair view checkpoint <- pmatch opened
    PPair folded finalCheckpoint <-
      pmatch (pwalkFold # view # checkpoint # (pfieldItemCount # view) # state # step)
    pexpecting (pwalkIsComplete # finalCheckpoint) folded

--------------------------------------------------------------------------------
-- Local helpers
--------------------------------------------------------------------------------

-- | Aiken's @expect cond@ — evaluate to @value@ when @cond@ holds, abort otherwise.
pexpecting :: forall (a :: S -> Type) (s :: S). Term s PBool -> Term s a -> Term s a
pexpecting cond value = pif cond value perror
