# Midgard

Midgard is an optimistic rollup on Cardano. This glossary pins the ubiquitous
language of its L1 dispute machinery and compact-transaction commitments.

## Language

### Dispute access model (flat field-hash commitments)

**Field preimage**:
The canonical raw bytes of one compact-tx field; its flat blake2b-256 hash is
the field commitment.

**Chunk carriage**:
Publication of a field preimage as on-chain chunk UTxOs that dispute steps
reference. Preimage bytes are only ever referenced, never copied into a step.

**Validation thread**:
The chain of L1 step transactions prosecuting a single fault claim, linked by
a state token.
_Avoid_: proof session, dispute chain

**Step**:
One L1 transaction in a validation thread.

**Step envelope**:
The hard ceiling (16,384 bytes) that an entire step transaction — datums,
redeemers, and all — must fit within.

**Authenticate-once**:
The access model in which a thread's first touch of a field verifies the flat
hash over the full preimage and marks the carriage authentic; every later
step trusts thread lineage and has only its target chunk in view.
Authentication is lazy per-field: only fields a thread actually reads are
ever authenticated.
_Avoid_: per-step re-authentication, eager thread-open authentication

**Offset-and-slice access**:
Reading an item by slicing authenticated carriage bytes at a known offset.

**Fixed-stride field**:
A field whose items share one exact width, making every item offset
arithmetic: spend inputs, reference inputs, observers, signers.

**Boundary discovery**:
The header walk that locates item boundaries in a variable-width field. Paid
per access by the step that needs it; never persisted (no offset table).

**Resumable walk checkpoint**:
The small thread-state record — field, byte offset, item index — that carries
a paused walk across steps. Its validity is lineage-vouched: each step
verifies the segment it walked before writing the next checkpoint.

**Positions-not-bytes**:
The thread-state invariant: the continuing datum may carry offsets, indices,
fixed-width scalars, and 32-byte digests — never verbatim preimage content.

**Per-asset conservation fault**:
The fault statement "this transaction fails value conservation for asset A",
with the prover naming A. Its accumulator is a single integer; its truth
implies general non-conservation.
_Avoid_: whole-Value conservation proof

**Witness-minimal fault statement**:
The principle that every fault family is stated as the smallest existential
claim implying the block-level fault, with the prover naming the witness —
the asset, the item index, the byte position.

**Value bookmark**:
The fixed scalar record — byte offset, policies remaining, assets remaining,
running sum — that carries a paused intra-Value walk across steps. Values
have fixed grammar depth, so no stack is ever needed.

**Canonical-Data Acceptor (CDA)**:
The checkpointable byte-level acceptor that proves a committed datum or
redeemer payload is not canonical serialized Plutus Data. Its parse stack
rides as a bounded window plus a hash-chained spill; spilled frames are
resupplied transiently by the prover and verified against the chain digest.
_Avoid_: full on-chain decode, datum Merkleization
