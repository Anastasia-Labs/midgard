# `transactionOutputNonCanonical` V1 size and transition plan

- Frozen category ID: `00000029` (central registration is deferred to the
  primary integrator).
- Logical topology: four steps after generic computation-thread `Init`.
- Authenticated subject: exactly one field-2 output item at an operator-committed
  `(transaction_id, output_index)` coordinate.

## Physical validators and canonical state

1. `transaction-output-non-canonical/step-01` binds an accepted transaction
   inclusion or forced leaf and the output coordinate. The forced branch
   accepts only `OutputNonCanonical { output_index }`. It imports the shared
   verdict-subject binder but no output decoder or scan engine. Successor state
   is `BoundOutputV1` at the exact step-02 hash.
2. `transaction-output-non-canonical/step-02` authenticates field 2 through the
   native field-opening door, selects the exact item, and binds its length and
   content identity into `OutputScanStateV1` with the initial
   `LedgerOutputScanControlV1`. It imports field access and the scan initializer
   but no terminal proof-token logic. The selected item is never accepted from
   caller-authored bytes.
3. `transaction-output-non-canonical/step-03` is the only resumable validator.
   Step 02 binds the authenticated item's length, Blake2b-256, and every
   4,095-byte chunk digest in the thread. Each transition checks the exact
   prior checkpoint and authenticates its current chunk plus at most one
   lookahead chunk against those digests before advancing
   `ledger-output-scan-v1`. It returns to the input-derived own script hash
   while scanning or to step 04 at terminal outcome. The 8,190-byte maximum
   window permits a CBOR token crossing a chunk boundary without accepting a
   whole item from the caller.
4. `transaction-output-non-canonical/step-04` imports only the exact terminal
   scan predicate and shared terminal polarity/finalizer. It convicts wrongful
   acceptance iff canonical reconstruction failed, and wrongful rejection iff
   reconstruction reached the unique exact terminal.

Every physical spend validator retains the authenticated common cancel path.
Only step 03 self-loops, and every successful scan transition strictly advances
the cursor or scan stage.

## Maximum shape and carriage

The family owns field 2 only. A selected output item is capped at 16,384 bytes;
larger items belong to `fieldItemWidthIllegal`. The enclosing committed field
may reach 32,768 bytes and therefore uses the shared three-chunk `Certified`
field carriage. The scan consumes authenticated windows no larger than 8,190
bytes: one 4,095-byte chunk and one boundary lookahead chunk. The maximum
fixture is a 16,384-byte output item with the longest
admissible address/value/datum/reference-script structure and the most
expensive successful scan frontier. The malformed twin changes one canonical
header/order byte without changing the authenticated coordinate. The adjacent
16,385-byte item is refused before thread construction by the detector and,
when a prover authenticates it anyway, by the step-02 scan initializer on
chain; it belongs to the width family.

The ledger measures both maximum shapes: the 16,384-byte malformed item inside
a 32,768-byte Certified field (rejected at the first window, so its scan is a
single 8,190-byte transition) and the 16,384-byte canonical item (a
42-byte address/value head and a 16,339-byte inline datum, the rule's own
maximum selector shape) scanned to its exact terminal, whose first transition
carries the widest window and is the most expensive scan step in both
directions.

## Unrelated-adapter exclusion

- Step 01 imports source/reason binding only.
- Step 02 imports field-2 item authentication and scan initialization only.
- Step 03 imports only authenticated window access and
  `ledger-output-scan-v1`.
- Step 04 imports only exact terminality and terminal polarity.

No applied script imports resolved-input membership, mint parsing, signer or
observer logic, redeemer/native-script decoding, CEK execution, minimum-Ada,
or value-preservation adapters. Reference-script semantics inside an output
are parsed only as part of canonical output reconstruction; this family does
not decide structural script validity.

## Production and fit gate

The package-owned detector derives accepted/forced evidence from authenticated
local Kupmios L1 plus public retained DA. It scans every output coordinate,
selects only the exact `OutputNonCanonical` contradiction, publishes/certifies
field carriage itself, derives scan checkpoints itself, and accepts no
evidence, stage, checkpoint, or submit callback. A central-journal bridge
persists evidence digest, each transaction intent and identity, publication and
certificate actuation, scan cursor, confirmation, and restart reconciliation.

The real Lucid suite starts at generic `Init`, covers both directions, both
honest refusals, reason/coordinate/source/item/window/checkpoint substitution,
cancel from each nonterminal physical state, interruption/resume after a real
step-03 checkpoint, final proof mint, descendant/target removal, maximum and
adjacent-over-bound evidence. Every applied script is published in a complete
signed transaction from the fresh testnet blueprint. The deterministic ledger
records signed bytes, memory, CPU, and margins against 16,384 bytes,
16,500,000 memory, and 10,000,000,000 CPU; publications must also remain at or
below the 15,872-byte reserve target. Local UPLC evaluation remains enabled and
no raised limit or `oversized` route is admissible.

The frozen applied parameter order is:

1. step 04: fraud-proof policy id, fraud-proof token address,
   computation-thread policy id;
2. step 03: step-04 validator hash, computation-thread policy id (the
   authenticated own-input hash closes the scan self-loop without a circular
   deployment);
3. step 02: step-03 validator hash, computation-thread policy id,
   field-preimage-certificate policy id;
4. step 01: step-02 validator hash, computation-thread policy id, hub-oracle
   script hash.

The machine-readable ledger is
`transaction-output-non-canonical-v1-fit-ledger.json`, ledger digest
`8e285ee6cbc82741594a0b7c6f890ac965cac322e32582d672b68b10ed64060e`, bound to
the testnet blueprint with SHA-256
`6236b3f8d2bbf663d184018e3281934370745ecdf078edf4cbaa34e06a4f2a6b` built by
`aiken v1.1.23+5adf783`.
