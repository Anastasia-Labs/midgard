# `witnessScriptDecoding` V1 size and transition plan

- Proposed category ID: `00000022`. This slice is implemented before the
  Wave-1 ID-23 catalogue insertion because catalogue IDs are positional; the
  primary integrator retains all central identity wiring.
- Logical topology: four family-scoped computation-thread steps after generic
  `Init`.
- Authenticated subject: one accepted or forced native transaction and exactly
  one field-6 script-witness ordinal.

## Physical validators and state

1. `fraud-proofs/witness-script-decoding/step-01`: imports only the Wave-0
   verdict-subject substrate and native transaction subject binders. It fixes
   accepted/forced provenance, direction, and `script_index`; a forced
   wrongful-rejection thread must carry exactly one of
   `WitnessScriptHeaderMalformed`, `WitnessNativeScriptMalformed`,
   `WitnessNativeScriptNodeLimit`, or `WitnessNativeScriptDepthLimit` with the
   same index. It forwards `BoundWitnessScriptV1` only to step 02.
2. `fraud-proofs/witness-script-decoding/step-02`: imports the authenticated
   field-opening door for field 6, the bounded-item commitment builder, and the
   frozen native-script header decoder. It opens the exact witness item,
   commits its chunks, and either records a header refusal/non-native
   contradiction or initializes the frozen structural control. It forwards
   `WitnessScriptScanStateV1` only to step 03.
3. `fraud-proofs/witness-script-decoding/step-03`: imports only the bounded
   chunk verifier and the existing canonical
   `native-script-decoding/engine.budgeted_scan_v1` wrapper over
   `native_script_scan_v1`. Each continuation authenticates the current and,
   where required, adjacent chunk, checks the domain-separated checkpoint over
   source identity, cursor/control, total length, item commitment, and this
   script hash, then either self-loops to the same applied hash or closes to
   step 04. The scan control itself is supplied canonically and must hash to
   the carried checkpoint. Cancellation remains available on every visit.
4. `fraud-proofs/witness-script-decoding/step-04`: imports only the Wave-0
   terminal-polarity helper. It recomputes exact reason polarity from the
   authenticated terminal result, burns the computation-thread token, and
   mints the permanent fraud-proof token. It also retains the common cancel
   path.

The canonical state carries the `VerdictSubjectV1`, script index, item length
and bounded-item commitment, canonical control bytes/hash, next expected step
hash, and terminal result class. No item or chunk bytes persist in the datum.

## Semantic engine and maximum evidence

The decisive rules are a narrow family module plus the existing frozen
`midgard/native_script_scan_v1` primitives. Header failure is kept distinct
from a tag-0 payload structural failure; non-native languages are a successful
decoder result and therefore can only contradict a forced decoding rejection.
The structural result classes preserve malformed, node-limit, and depth-limit
exactly.

Maximum dynamic evidence is one 32,768-byte field-6 preimage, one selected
item spanning at most nine 4,095-byte bounded-item chunks, one field-opening
carriage (direct, publication, or certified/chunked), two adjacent item chunks
per scan transaction, the canonical scan control, and the frame witnesses
consumed by that transaction's explicit budget. The supported node/depth
frontier is the protocol's 16,384 bound; exact-bound and adjacent-over-bound
vectors are required.

## Reachability and unrelated-adapter proof

- step 01 cannot reach field interpretation or any structural scanner;
- step 02 reaches only field 6, bounded-item commitment construction, and the
  versioned-script header rule;
- step 03 reaches only bounded item chunks and the frozen native structural
  scan engine;
- step 04 reaches only exact result/reason polarity and generic finalization.

No applied validator imports resolved-output descriptors, ledger-output tries,
signatures, observers, redeemers, CEK, mint/value folds, or native-script
evaluation. The resolved-reference family's source adapters do not enter these
scripts; only its pure scan fold is reused.

## Production evidence and recovery

`deriveWitnessScriptDecodingEvidenceFromCanonicalBlockV1` and
`detectWitnessScriptDecodingCompleteReplayV1` scan every accepted field-6
script coordinate and only the exact coordinate carried by each of the four
typed forced-rejection reasons. The production execute surface reconstructs
the accepted PHAS inclusion or forced leaf membership from authenticated L1
and public retained DA; it accepts no caller-authored evidence. Raw, Certified,
and certificate transactions are locally evaluated and intent-journaled before
submission. The same durable central journal binds the family evidence
identity, exact transaction hash, scan checkpoint transition, restart
reconciliation, final proof mint, and leased target/descendant removal.

## Planned fit gate

Build with the pinned compiler under `aiken build --env testnet`, publish every
applied script in a complete signed reference-script transaction, and execute
the full Lucid Evolution lifecycle with `MIDGARD_REAL_BLUEPRINT_PATH` pointing
to the fresh isolated blueprint and the shared Van Rossem parameters. Record
signed bytes, memory, CPU, and remaining margins for all four publications,
every proof/resume/cancel/finalize/removal transaction, direct and certified
carriage, maximum node/depth paths, and the adjacent-over-bound refusal.
Acceptance requires publication size `<= 15,872`, hard size `<= 16,384`, memory
`<= 16,500,000`, and CPU `<= 10,000,000,000`, with no oversized route, raised
parameter, or disabled local evaluation.

## Local implementation evidence

- Compiler: `aiken v1.1.23+5adf783`.
- Isolated `testnet` build: green.
- Focused semantic selectors: 14 collected, 14 passed. The maximum measured
  selector used 1,632,504 memory and 633,407,680 CPU.
- Raw applied-script bodies from the fresh blueprint (diagnostic only): step
  01 = 14,540 bytes, step 02 = 10,159 bytes, step 03 = 11,345 bytes, step 04 =
  2,539 bytes. Every raw body is below the 15,872-byte publication target; this
  is not substituted for a signed publication measurement.
- TypeScript evidence, parity, exact classifier identities, mutation,
  checkpoint, durable restart, journal, and workflow tests: 21/21. The fit
  ledger reproduction test is 1/1 and the real accepted and forced Lucid
  lifecycle tests are 2/2, for a 24/24 family gate. The maximum retained field
  is 32,768 bytes and produces nine authenticated bounded-item chunks.

The signed machine-readable ledger is
`witness-script-decoding-v1-fit-ledger.json`. All four validators publish in
ordinary complete signed transactions: 14,922, 10,542, 11,693, and 2,932
bytes, leaving 1,462, 5,842, 4,691, and 13,452 bytes of Van Rossem headroom.
The real wrongful-acceptance journey executes Init, accepted bind, Certified
field-6 publication/certification, one resumable scan, refusal close, final
burn/mint, and target removal. Cancellation is executed from all four physical
states, including the closed step-04 checkpoint. The real forced journey binds
an exact typed rejection to a decodable script, executes all four steps, mints
the permanent proof, and removes the target. The largest lifecycle transaction
is the 9,732-byte scan close; the most expensive execution is the resumable
scan at 2,592,603 memory and 1,022,630,443 CPU. The reproducible ledger digest
is `6a9480090a685beed8f63f5d10bec605a5be948f2c6c5db774731b0010a2f9f5`.
Every measured
margin is positive under 16,384 bytes, 16,500,000 memory, and 10,000,000,000
CPU, without an oversized publication route or raised parameter.
