# `executionSourceScriptDecoding` V1 size and transition plan

- Frozen category ID: `00000031`.
- Typed reasons: `ExecutionNativeScriptMalformed`,
  `ExecutionNativeScriptNodeLimit`, and `ExecutionNativeScriptDepthLimit`.
- Logical topology: five family-scoped computation-thread steps after generic
  `Init`.
- Authenticated subject: one accepted or forced native transaction, exactly one
  execution-frontier ordinal, and the exact source descriptor selected by that
  execution.

## Physical validators and state

1. `fraud-proofs/execution-source-script-decoding/step-01` binds accepted or
   forced provenance, direction, transaction identity, and `execution_index`.
   A forced wrongful-rejection thread must carry exactly one of this family's
   three typed reasons with the same execution ordinal. It forwards only a
   `BoundExecutionV1` to step 02.
2. `fraud-proofs/execution-source-script-decoding/step-02` authenticates the
   selected execution descriptor and its source descriptor against the
   canonical purpose, source, and execution frontiers reconstructed from the
   retained validation-machine witness. It fixes source origin/key, language,
   script hash, total length, and bounded-item commitment and forwards only an
   `AuthenticatedExecutionSourceV1` to step 03.
3. `fraud-proofs/execution-source-script-decoding/step-03` opens the exact
   inline field-6 or resolved reference-output script item named by that source
   descriptor. It verifies the bounded-item commitment and versioned-script
   header, records an immediate malformed/non-native result where applicable,
   or initializes the frozen structural control. It forwards only an
   `ExecutionSourceScanStateV1` to step 04.
4. `fraud-proofs/execution-source-script-decoding/step-04` imports the bounded
   chunk verifier and frozen
   `native-script-decoding/engine.budgeted_scan_v1`. Each transaction verifies
   its current checkpoint plus the current and optional adjacent chunk. It
   either self-loops with an exact successor control/checkpoint or closes to
   step 05. Cancellation remains available on every visit.
5. `fraud-proofs/execution-source-script-decoding/step-05` recomputes exact
   direction/reason polarity from the authenticated terminal result, burns the
   computation-thread token, and mints the permanent fraud-proof token.

The canonical carried state includes the `VerdictSubjectV1`, execution index,
source origin/key/index, language and script hash, total length and item
commitment, canonical scan control, next expected script hash, checkpoint, and
terminal class. No raw script item, transaction, resolved output, Merkle proof,
or chunk bytes persist in the datum.

## Semantic engine and maximum evidence

The decisive structural semantics are delegated unchanged to the frozen
`midgard/native_script_scan_v1` primitives through
`midgard/fraud_proofs/native_script_decoding/engine.budgeted_scan_v1`. The
family adapters only bind the execution coordinate and source descriptor and
authenticate the exact source item. Header and payload failures both map to
`ExecutionNativeScriptMalformed`; native node/depth results remain distinct;
a non-native or canonically decodable item is no fault.

Maximum dynamic evidence is a 32,768-byte script item spanning nine 4,095-byte
bounded-item chunks, the execution/purpose/source membership paths at the
protocol frontier, one inline field opening or one resolved-output membership,
two adjacent chunks per scan transaction, canonical scan control, and bounded
frame witnesses. The supported node/depth frontier is exactly 16,384. Tests
must cover the exact boundary and the adjacent over-bound refusal, malformed
headers/payloads, a decodable wrongful-rejection contradiction, interruption
and resume, and execution/source/item substitution.

## Reachability and unrelated-adapter proof

- step 01 cannot interpret a script or accept an execution/source proof;
- step 02 authenticates only the selected purpose/source/execution tuple;
- step 03 opens only the exact item named by the authenticated source;
- step 04 reaches only bounded chunks and the frozen structural scanner;
- step 05 reaches only terminal class/reason polarity and generic finalization.

No applied validator imports observer ordering, signatures, redeemer
canonicity, CEK evaluation, mint/value folds, or output canonical reconstruction.
Witness-script and output-reference-script subject adapters do not enter this
family's applied scripts.

## Planned fit and parity gate

### Authorized shared replay extension

The ID31 accepted-malformed arm additionally owns the minimal shared
validation-machine extension that reconstructs a total trace directly from an
authenticated canonical transaction envelope when malformed field-6 bytes
prevent the full native decoder from returning. The extension must preserve
the existing canonical replay path unchanged and emit the exact pre-rejection
`NativeScripts` machine state, control/work witness, trace proof, and descriptor
from envelope-derived values only. Regression coverage binds the raw bytes,
transaction identity, source/purpose/execution roots, deterministic trace root,
and refuses substituted bytes or caller-prepared state/proof authority.

Production retained replay uses the minimal public validation witness bundle
committed transitively by the existing validation-trace descriptor. There is
exactly one bundle per applicable event/execution coordinate: the exact
machine state, trace membership proof, NativeScripts phase/program counter,
work-witness CBOR, and typed native-execution auxiliary witness. Admission
recomputes the work root from phase/counter/witness bytes, verifies the state
hash and trace proof against the L1-root-authenticated descriptor, and then
reconstructs every purpose/source/execution membership against the control's
committed frontiers. Duplicate, orphan, coordinate-, sibling-, leaf-, raw-item-,
descriptor-, or checkpoint-substituted bundles fail closed. No predecessor
ledger preimage, private database, singleton trace, or caller callback is used.
The family exports an asynchronous complete canonical replayer which scans all
accepted transactions and all forced-invalid leaves, emits the three exact
typed violation IDs, and orders detections by position then detection ID. The
manifest-bound runner takes infrastructure and reference identities only;
retained evidence, source selection, L1 stage observation, intent journaling,
submission, restart reconciliation, final mint, and removal stay package-owned.

Build an isolated `testnet` blueprint with the pinned Aiken compiler, publish
all five applied scripts in complete signed Lucid Evolution reference-script
transactions, and run accepted plus forced lifecycles with local UPLC
evaluation under the ordinary Van Rossem parameters. The machine-readable
ledger records every publication, bind, source authentication, item opening,
scan/resume, cancellation, final burn/mint, and target/descendant removal row.

Acceptance requires signed publication size `<= 15,872`, hard transaction size
`<= 16,384`, memory `<= 16,500,000`, and CPU `<= 10,000,000,000`, with positive
margins and no oversized route, raised protocol limits, or disabled local
evaluation. Focused Aiken and TypeScript parity vectors must agree on source
leaf, execution leaf, checkpoint, result class, exact-boundary, and adjacent
refusal behavior.

## Measured implementation evidence

The reproducible `execution-source-script-decoding-v1-fit-ledger.json`
(shared `midgard-van-rossem-fit-ledger-v1` schema, bound to the SHA-256 of the
isolated `aiken v1.1.23+5adf783` testnet blueprint) is written by the
registered-chain lifecycle suite from complete signed Lucid Evolution
measurements on the ordinary Van Rossem parameters with local UPLC evaluation.
It records the five reference publications (15,032, 15,730, 6,868, 12,194 and
2,990 signed bytes; the narrowest publication reserve is 142 bytes at step 02)
and every lifecycle row at the maximum supported shapes:

- the accepted direction at the exact 32,768-byte field-6 cap: a zero-payload
  item spanning all nine bounded chunks, refused at its first token by one
  step-04 verdict transaction that authenticates the two-chunk window
  (9,735 signed bytes, 1,561,868 memory, 619,384,540 CPU);
- the forced NodeLimit direction over the widest `any [all [], sig × n,
all [] × k]` script that fits the cap, folded through 16-step resumable
  segments across all nine chunk windows; the widest window transaction is
  9,960 signed bytes, 7,064,276 memory and 2,771,469,944 CPU, the tightest
  lifecycle margins in the ledger (6,424 bytes, 9,435,724 memory,
  7,228,530,056 CPU), and the exact-end close is 1,130 bytes;
- the forced DepthLimit direction over forty nested containers, whose
  frame-stage segments consume frame witnesses through the stack
  (6,955,825 memory at the first segment);
- the empty-payload bind close, the honest forced and honest accepted
  refusals, cancellation from every one of the five physical steps, the
  permanent mint and the leased removal.

Two on-chain defects were closed while measuring: the bind now goes through
the frozen engine's `bind_machine_v1`, so a tag-0 empty payload closes
malformed instead of aborting, and step 04 authenticates the chunk window at
every stage, so a planned segment that opens on a frame step no longer stalls
(any container with roughly eight or more children was unreachable before).

The lifecycle declares its coverage honestly: the wrongful-ACCEPTANCE
directions of NodeLimit and DepthLimit and the adjacent-over-bound refusal are
not realisable on chain, because the frozen scan bounds both frontiers at
16,384 and a node costs at least three bytes, so no item inside the 32,768-byte
field cap can reach either limit; the exact and adjacent node/depth edges are
pinned by the rule selectors instead. The byte cap on the field-6 preimage is
not a bound of this chain: no applied step carries a byte limit of its own,
and the deterministic machine still emits a native execution descriptor for a
32,769-byte preimage, so there is no on-chain refusal of an over-cap item to
exhibit in this family.

The family durable state records the exact source and target stage, out-ref,
structural control, checkpoint, and locally evaluated transaction hash before
submission. Restart accepts only the exact authenticated target cursor, and
the nonterminal stages share the same intent-first cancellation path.
