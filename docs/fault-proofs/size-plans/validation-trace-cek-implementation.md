# CEK semantic publication implementation

The context, core and execution-selection named plans remain the source of
truth. Fresh baseline sizes are 94,268, 68,689 and 45,486 raw bytes respectively.
An isolated selection probe that decodes only auxiliary constructor 11 reduces
the last to 32,752 bytes. Narrow decoding is necessary but insufficient.

Execution selection keeps the planned dispatcher and authenticated rewarding
validators. Its material partition needs a bidirectional frontier: program
kinds 0–4 reach Data nodes (kind 5), while Data kinds 5–7 reach blob nodes
(kinds 3–4). Program verification starts at the selected term plus the claimed
Data blob roots and derives the exact Data roots. Data verification starts at
those exact Data roots and derives the exact blob roots. Both frontier lists
are sorted and unique, every visited entry authenticates its typed preimage,
and the sum of visited counts must cover all entries. Blob traversal cannot
introduce Data roots, so a claimed extra blob frontier cannot create an
otherwise unreachable Data component. The two predicates jointly authenticate
all graph reachability, byte totals, node totals and absence of orphan entries.
Direct and published routes supply the same typed entries. Raw sidecar evidence
remains available to the off-chain admission layer.

Context and core use forward-only computation-thread chains as planned. The
plan's settle-to-binder hash parameter would create a cycle with the forward
continuation chain; immutable thread origin, exact forward output hashes and
exact stage/progress datums establish provenance without that back edge.
Every terminal settles only an authenticated computation-thread continuation.
Cancellation remains available at every physical step.

Context stage 0/9 Data traversal consumes the shared ScriptSources item-proof
chain, owned by the parent integration workstream. Its pending carrier binds
opaque CEK state, current item control, exact witness hash and claimed next
control. The shared terminal returns the authenticated result to a fixed CEK
return script. There is no second traversal implementation in this workstream.

All physical bodies must publish under the actual transaction ceiling, and all
maximum lifecycle transactions must meet the shared Van Rossem reserve. Record
measurements with the shared writer and current-blueprint verifier; no raised
limits or oversized reference-publication exemptions establish fit.

### Canonical material carriage correction

The first real Plutus selection exposed an existing cross-language encoding
mismatch. SDK sidecar entries contain `[root, bytes(DA value)]`, with the DA
value itself encoded as `[1, kind, preimage]`. The former Aiken parser and its
handwritten test vectors used `[root, [1, kind, preimage]]`. The parser now
opens the committed byte string and re-encodes that exact byte envelope when
checking canonical equality. The old unwrapped form is refused. No SDK wire
format changed. A real Plutus identity selection now executes all four yields,
produces its award, mints the proof, and removes the forged block.

### Bounded selection material traversal

The four-yield single transaction exhausts the 14 million emulator memory
limit on a valid 165-node graph. Selection therefore also authenticates a
forward continuation carrying a hash-linked pending stack, visited PHAS root,
actual node/byte counts and the exact admitted envelope totals. Each transaction
opens one typed task and a visited membership or insertion proof. Program and
Data rewarding validators authenticate the preimage and exact ordered child
stack update. DataPair has at most three children. Repeated roots still verify
the requested type and length before skipping their children and counts.
The award is reachable only with an empty stack and exact node and byte totals.
No admitted graph bound is reduced.

The dispatcher and program/Data task yields are 7,370, 7,431 and 7,907 raw
bytes. A real 165-node graph and a Data/blob graph with repeated references both
complete selection, proof mint and fraudulent block removal. Restart is tested
by losing the process after an accepted task transaction, round-tripping the
checkpoint and retained raw sidecar through JSON, fetching the live UTxO and
replaying deterministic material traversal to its exact authenticated state.
Cancellation consumes a live intermediate checkpoint without an award.

The maximum task ledger measures standalone authenticated continuation
contexts, separately from full-chain provenance tests: a 4,095-byte blob task
plus 64-step visited proof is 14,049 signed bytes, 6,877,123 memory and
2,058,441,056 CPU; the three-child Data task with maximum-width counters and
64-step proof is 9,977 bytes, 7,682,179 memory and 2,361,657,530 CPU. This is
atomic evidence coverage, not a claim to have submitted 1,597,819 transactions.

Regenerate both blueprint-bound ledgers from the repository root using Node 22
and pnpm 9 after a normal pinned testnet build:

```sh
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/cek-material-traversal-maximum.test.ts tests/cek-selection-yield-lifecycle.test.ts
```
