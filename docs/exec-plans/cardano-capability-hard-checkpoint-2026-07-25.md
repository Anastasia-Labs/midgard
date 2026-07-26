# Cardano capability hard checkpoint — 2026-07-25

Branch: `codex/tx-validation-capability-checkpoint`
Base HEAD: `8e008639cc424637c0c587ac2ae9acaf1918b922`

This checkpoint pauses resolver expansion and reports binary evidence. A failed
gate remains fail closed.

## Goal reconciliation

The formal goal tracker still records the canonical-V1 consolidation objective
from `canonical-v1-consolidation.md` as **blocked**. It is not complete: its
fresh V1 deployment, full named build/test/E2E ladder, and exhaustive
compatibility-removal acceptance evidence have not passed.

`cardano-capability-proof-completion.md` is the mandatory successor plan. It
does not complete the consolidation goal retroactively, and consolidation does
not satisfy capability/proof completion. The successor release goal also
remains incomplete.

## P0–P4 result

| Phase | Result | Evidence |
| --- | --- | --- |
| P0 freeze/baseline | **PASS** | The release-evidence digest remains unset; current documentation says diagnostics do not activate the profile; no deployment claim relies on the monolithic verifier. |
| P1 split control plane | **FAIL** | No complete real applied/parameterized publication transaction has been constructed and measured below 16,384 bytes with margin. |
| P2 incremental commitments | **FAIL** | Item/chunk machinery exists, but maximum Cardano-capable content has not passed the complete retained-DA-to-L1 reveal and terminal-fold gate for every dynamic family. |
| P3 full one-step semantics | **FAIL** | Supported rules still have missing L1 verifiers, and multiple current applied resolver scripts exceed the standalone 16,384-byte necessary bound. |
| P4 forced/misclassification | **FAIL** | The refreshed coverage matrix still has missing/partial fund-safety rows. The complete lifecycle fixture currently fails before open at header commit. |

## Rule-to-proof coverage

The current-branch audit is recorded in
`../fault-proofs/coverage-matrix.md`. Canonical V1 now represents mint/burn,
scripts, redeemers, reference inputs/scripts, script credentials, protected
outputs, observers, and forced transactions, but representation and unit
verification are not equivalent to a complete challenge, correction, and
release path.

## Lifecycle evidence

The new focused fixture constructs a fraudulent operator claim for an invalid
forced transaction and automatically prepares every authenticated midpoint and
the final one-step argument. The intended path is:

`init → open → source verification → authenticated bisection reveals → enter
resolution → boundary preparation → selected resolver preparation → semantic
one-step resolution → award`.

Command:

```sh
NODE_OPTIONS=--max-old-space-size=4096 pnpm --filter @al-ft/midgard-fault-proofs exec vitest run tests/submit-init-emulator.test.ts -t "opens, bisects, resolves, and awards a validation dispute end to end" --pool=forks --poolOptions.forks.singleFork=true
```

Result: **FAIL** at `setup.header-commit.complete`; the applied Aiken spending
validator exits prematurely. The established transition-trace E2E case fails
at the same step, proving this is a current generic commit-path regression
rather than success of the new path. Consequently there is no complete
open-to-award demonstration at this checkpoint.

## Actual transaction-fit evidence

The target-network provider, deployment manifest, and signing credentials are
not available in this environment. No actual applied/parameterized
publication, resolution, or settlement transaction was therefore submitted or
measured. Existing standalone-script and emulator/CML framing measurements are
diagnostic only.

| Transaction family | Full bytes | Witnesses/redeemers/datums/outputs | Ex-units | Margin to 16,384 | Gate |
| --- | ---: | --- | ---: | ---: | --- |
| Largest real publication | unavailable | not measured | unavailable | unavailable | **FAIL** |
| Largest real resolution | unavailable | not measured | unavailable | unavailable | **FAIL** |
| Largest real settlement/award | unavailable | not measured | unavailable | unavailable | **FAIL** |

## Remaining work partition

Required for the originally named #459/D-S5/D-S9/versioning/size-bound/feature-
gate objective:

1. Repair and prove the header-commit prerequisite, then make the complete
   forced/misclassification lifecycle pass.
2. Finish every missing fund-safety verifier in the matrix, preserving
   unilateral proofs where one transaction can establish the fault.
3. Split only the remaining oversized applied resolvers and complete
   item/chunk/fold semantics for values, scripts, reference scripts, and other
   dynamic fields.
4. Construct and measure the largest actual target-network publication,
   resolution, and settlement transactions, including all framing and ex-units.
5. Generate capability parity/release evidence and set the digest only when
   every required mapping and boundary path passes.

Broader full-release readiness, after that objective, includes trusted final
parameters, exhaustive economics/timing/DA and adversarial corpora, runbook and
manifest operational proof, fresh deployment/restart/recovery, and the bounded
P6 E2E/stress ladder under one exclusive Midgard topology.

The shortest bounded route is therefore commit-path repair → one complete
lifecycle → matrix-closing P3/P4 proofs → actual transaction-fit measurements
→ release digest. P6 deployment/stress is not evidence for an unfinished P1–P5
proof surface and must not start first.
