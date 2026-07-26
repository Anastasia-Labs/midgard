# Canonical V1 authenticated reference-script checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `87017127`

This is the boundary for the bounded emulator-only reference-script milestone.
The formal canonical-V1 consolidation goal remains **blocked** and was neither
replaced nor completed. P4 resolver/coverage work did not resume. No Docker
topology, live submission or deployment, stress run, semantic-resolver
expansion, or release-evidence digest change occurred.

## Authenticated publication and fail-closed consumption

The fixture now publishes the exact applied
`contracts.validationTraceDispute.firstStep` spending validator selected by
the SDK contract builder and by the existing node-runtime target
`V1 validation-trace dispute`. It uses the shared
`completeReferenceScriptPublicationTxProgram`, a timelocked native auth policy,
and the canonical `V1ValidationTraceDispute` role token. The publication is
constructed by a fresh Lucid client whose `maxTxSize` is exactly 16,384, fully
signed, submitted to the emulator, and resolved back from its concrete out-ref.
No UTxO is fabricated directly in the fixture.

The production open constructor no longer attaches the first-step validator.
It instead:

1. requires the canonical auth-policy record and token-name mapping;
2. derives the native minting-policy id from its CBOR and requires it to equal
   the declared manifest policy id;
3. requires `contracts.validationTraceDispute.refScriptUTxO`;
4. fetches exactly that manifest-bound out-ref;
5. requires a reference script whose hash equals both the deployment entry and
   the freshly applied first-step hash;
6. requires exactly one nonzero asset under the auth policy, namely one
   canonical validation-dispute role token; and
7. supplies the UTxO through `readFrom` with the hub-oracle and state-queue
   reference inputs.

Missing metadata, a missing/unavailable out-ref, absent or mismatched script,
wrong/duplicate auth-policy assets, or a non-canonical policy fails before
construction. There is no attached-script or unauthenticated production
fallback.

## Target parameters

The emulator uses a public preprod epoch-303 parameter snapshot observed on
2026-07-26:

- `maxTxSize = 16,384`;
- `maxTxExecutionUnits = 16,500,000 memory / 10,000,000,000 steps`;
- snapshot SHA-256
  `d4178364b6c45216bf51d067e0a8360ce8060e8a2bef8213cf4121376380c137`.

The snapshot remains diagnostic observation evidence. Emulator submission
does not satisfy P5's trusted-parameter or actual target-network transaction
gate, and the release-evidence digest remains unset.

## Fully signed transaction measurements

The recorder parses the complete signed CBOR submitted to the emulator and
sums the execution units in all redeemers. Counts include concrete transaction
inputs, reference inputs, outputs, vkey/native/Plutus witnesses, redeemers, and
witness datums. Output datums are inline and therefore do not appear in the
witness-datum set.

| Transaction | Signed bytes | Byte margin | Memory | Memory margin | Steps | Step margin | Inputs / refs / outputs | Vkeys / native / redeemers / datums / P1 / P2 / P3 scripts |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |
| Authenticated reference-script publication | 13,256 | +3,128 | 0 | +16,500,000 | 0 | +10,000,000,000 | 1 / 0 / 3 | 1 / 1 / 0 / 0 / 0 / 0 / 0 |
| Validation-dispute open publication | 5,860 | +10,524 | 1,645,470 | +14,854,530 | 679,310,442 | +9,320,689,558 | 2 / 3 / 2 | 1 / 0 / 1 / 0 / 0 / 0 / 0 |
| Semantic resolution | 11,411 | +4,973 | 1,182,410 | +15,317,590 | 472,088,839 | +9,527,911,161 | 2 / 0 / 2 | 1 / 0 / 1 / 0 / 0 / 0 / 1 |
| Settlement/award | 7,238 | +9,146 | 245,511 | +16,254,489 | 86,745,086 | +9,913,254,914 | 2 / 0 / 2 | 1 / 0 / 3 / 0 / 0 / 0 / 3 |

The open has three authenticated reference inputs and zero attached Plutus
scripts. It is 12,726 bytes smaller than the prior attached-validator shape
and fits with substantial byte and execution margins.

## Lifecycle evidence

Both commands ran from `demo/midgard-fault-proofs/`, used direct local
binaries, one Vitest fork, one in-memory emulator, and a 4 GiB V8 heap ceiling.

Invalid-forced validation dispute through award:

```sh
NODE_OPTIONS=--max-old-space-size=4096 \
MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS=/tmp/midgard-preprod-epoch-params-2026-07-26.json \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/submit-init-emulator.test.ts \
  -t "opens, bisects, resolves, and awards a validation dispute end to end" \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 passed, 10 skipped; lifecycle 100.391 s, total
102.06 s). It completed authenticated publication, open, source
authentication, every generated midpoint reveal, boundary preparation,
selected one-step resolution, semantic resolution, and award.

Established transition-trace control:

```sh
NODE_OPTIONS=--max-old-space-size=4096 \
./node_modules/.bin/vitest run \
  tests/submit-init-emulator.test.ts \
  -t "submits and removes a tail transition-trace fraud proof end to end" \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 passed, 10 skipped; lifecycle 2.339 s, total 4.16 s).

## Phase status and boundary

| Phase | Previous | Current | Reason |
| --- | --- | --- | --- |
| P1 split control plane / proof fit | FAIL | **FAIL overall; critical open subpath emulator-fit PASS** | The exact authenticated script publication and complete signed open now fit at 16,384 with +3,128 and +10,524 margins. The P1 gate still requires every parameterized hub/control publication, and P5 explicitly requires actual target-network construction rather than emulator-only evidence. |
| P4 forced/misclassification | FAIL overall; one lifecycle subpath PASS | **Unchanged** | The same invalid-forced accepted-operator/rejected-challenger lifecycle still reaches award. No opposite direction, retained-data, matrix-row, resolver, or preprod work was added. |

Unsupported or incomplete activation continues to fail closed.

## Narrow verification

The final worktree copy passed:

- `tsc --noEmit` in `demo/midgard-fault-proofs`;
- targeted ESLint with zero warnings for all four touched TypeScript
  source/test files;
- both serialized lifecycle commands above; and
- `git diff --check`.

## Smallest justified next milestone

Stop here before remaining P4 rows. The smallest next capability milestone is
to inventory and construct the remaining P1 parameterized hub/control
publication shapes under the same exact 16,384-byte envelope, then distinguish
which still need a reference-script route. Actual preprod construction and
submission, trusted effective/pending parameter binding, remaining P4
fund-safety rows and opposite forced-verdict direction, P5 release evidence,
P6 deployment, and stress remain separately gated.
