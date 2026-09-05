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

## Integrated verification

The family is integrated on the shared branch and measured against the complete
807-validator testnet blueprint compiled with `aiken v1.1.23+5adf783`.
The blueprint SHA-256 is
`db03f84b0157ac51bd26b69dc2d548bb7186847697db607c5c8699479bfb9094`.
The family has four physical spending validators; the blueprint also emits
an `else` entry for each. Their raw bodies are 14,540, 10,293, 11,321, and
2,539 bytes. Signed publication measurements, rather than these raw sizes,
are the publication gate.

The registered-chain lifecycle covers accepted malformed headers, malformed
native payloads, and empty tag-0 payloads; all four forced-rejection reason
arms; cancellation at all four steps; authenticated source, carriage, item,
scan-control and checkpoint mutations; permanent proof minting; and target
removal. The maximum-width script uses 133 resumes. The maximum-depth script
uses 1,368 resumes and one close, all as locally evaluated transactions.
Removal reference scripts are published before the journey, so the long scan
does not leave deployment publications beyond Lucid's default expiry. The
ledger writer requires all seven successful removal measurements before it
can write evidence.

The aggregate field bound is 32,768 bytes. Maximum field carriage uses three
certified publication chunks; the selected script item uses up to nine
4,095-byte authenticated scan chunks. Accepted node-limit and depth-limit
violations cannot fit this aggregate bound, so the lifecycle coverage gate
explicitly records those two unreachable directions. Exact 16,384 and adjacent
16,385 node/depth boundaries remain semantic engine tests. The reachable
maximum-width and maximum-depth forced contradictions use 1,059 nodes and
depth 10,909 respectively.

Reproduce the family gates from `onchain/aiken` with `aiken build --env testnet`
and `scripts/run-focused-check.mjs`, selecting all 21 test names from
`midgard/fraud_proofs/witness_script_decoding/rule.test`. Run the fault-proof
package's `witness-script-decoding` Vitest files with
`MIDGARD_REAL_BLUEPRINT_PATH` set to the absolute integrated blueprint path.
Set `MIDGARD_WRITE_FIT_LEDGER=1` only for the full lifecycle file to regenerate
`witness-script-decoding-v1-fit-ledger.json`; leave `MIDGARD_WSD_DEEP_DEPTH`
unset so the maximum-depth evidence is measured. Then update the fit-ledger
test's blueprint pin and run it against the regenerated ledger.

Verification passed: 21/21 focused Aiken semantic tests, 21/21 fault-proof
workflow and journal tests, 10/10 real lifecycle tests, 1/1 fit-ledger test,
3/3 SDK ABI tests, 2/2 SDK catalogue-registration tests, and 12/12
contract-inspection tests. The fault-proof package typecheck, touched
TypeScript lint, and changed Aiken file formatting checks also passed.

The integrated ledger contains 71 measurements. Signed family publications
are 14,922, 10,676, 11,669, and 2,932 bytes. The largest measured transaction
is a 15,872-byte certified carriage publication. The maximum measured scan
resume uses 7,515,101 memory and 2,932,991,712 CPU units. Every measured hard
size, memory, CPU, and family publication-reserve margin is positive under
the shared Van Rossem parameters. The ledger SHA-256 is
`559963ce72b63f1bbb5c7da6276abaa7675ce0ce92063f1e105c9034cfd4ca1b`.
