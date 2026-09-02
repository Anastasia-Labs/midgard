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

The reproducible `execution-source-script-decoding-v1-fit-ledger.json` records
the isolated testnet build and complete signed Lucid reference publications.
The five signed sizes are 15,032, 15,730, 6,777, 12,217, and 2,990 bytes;
the narrowest publication reserve is therefore 142 bytes at step 02. The
largest focused scan/resume vector consumes 2,105,421 memory and 860,868,927
CPU, leaving 14,394,579 memory and 9,139,131,073 CPU under the ordinary Van
Rossem limits. The maximum item remains 32,768 bytes / nine bounded chunks.
The family durable state records the exact source and target stage, out-ref,
structural control, checkpoint, and locally evaluated transaction hash before
submission. Restart accepts only the exact authenticated target cursor, and
the four nonterminal stages share the same intent-first cancellation path.
The measured forced real chain (generic Init through permanent mint and
canonical removal) records signed sizes of 1,221, 1,758, 2,666, 981, 999, 916,
and 2,060 bytes respectively. The accepted-malformed raw field-6 chain records
1,221, 2,023, 2,616, 936, 954, 916, and 2,060 bytes. Both retain positive byte
and ExUnit margins; the four real cancellation fixtures burn the thread from
physical step 01, step 02, step 03, and scan outputs. Every full-chain step-02
also refuses a substituted script hash before the authenticated transaction is
submitted.
