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
reference. This is the certified tier; smaller preimages can use inline
redeemer or single raw-UTxO carriage. See [MidgardTx §8](docs/spec/midgard-tx.md#8-field-preimage-carriage-three-tiers).

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
The lazy per-field access rule: authenticate a field before slicing it, and
leave untouched fields unopened. Authentication is scoped to the chosen
carriage: inline and raw-UTxO consumers hash the full preimage in each consuming
transaction; certified carriage authenticates selected chunks against the
mint-verified manifest. A thread checkpoint does not by itself replace these
checks. See [MidgardTx §7](docs/spec/midgard-tx.md#7-access-invariants-normative-for-every-consumer).

**Offset-and-slice access**:
Reading an item by slicing authenticated carriage bytes at a known offset.

**Fixed-stride field**:
A field whose items share one exact width, making every item offset
arithmetic: spend inputs, reference inputs, observers, signers, and address
witnesses.

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
The byte-level canonicity and interior-access routines for serialized Plutus
Data. The current recursive scanner distinguishes canonical bytes from forms
the Aiken stdlib can materialize, and supports typed access to canonical
bignums and tag-102 constructors. It does not expose a resumable parse-stack
checkpoint; the native-script pushdown has its own hash-chained frame protocol.
See [MidgardTx §11.2](docs/spec/midgard-tx.md#112-the-canonical-data-acceptor-case-b).
_Avoid_: full on-chain decode, datum Merkleization
