# TQ-17 — Make emulator coverage boundaries explicit

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit section**: §12
- **Rules**: R2, R3, R7, R9 in [principles](principles.md)
- **Coordination**: TQ-06 improves refusal attribution; TQ-03 and TQ-15 cover
  resource evidence and its live producers.

## Problem and boundary

An always-successful substitute can isolate a builder or workflow test, but it
cannot prove that the substituted validator authorizes the transaction. A
raised emulator limit can permit semantic exploration without proving real L1
publication fit. The suite name and acceptance report must state which
boundaries actually ran.

## Current source corrections

- [Contract options](../../demo/midgard-fault-proofs/tests/support/emulator/contracts.ts)
  currently default `alwaysFraudProofCatalogue` and `alwaysStateQueue` to
  `false`. Individual suites opt into substitutes. The old claim of a blanket
  always-successful default is inaccurate; inspect each harness construction
  and real-family option separately.
- [Reference publication](../../demo/midgard-fault-proofs/tests/support/emulator/reference-scripts.ts)
  no longer uses the cited `> 14_000` predicate. The current waiver compares
  raw script size to `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`; the helper
  measures the complete signed publication and enforces its margin unless
  `oversized` is enabled. A waiver or named allowlist remains diagnostic and
  cannot establish release fit, regardless of how it is selected.
- [Catalogue registration](../../demo/midgard-fault-proofs/tests/support/emulator/catalogue-registration.test.ts)
  compares the registered entry with the harness's first-step script. This
  checks internal wiring. A claim about every real canonical family additionally
  requires the intended applied scripts and authenticated membership route.
  Pairwise hash distinctness is neither sufficient nor necessarily required.
- [Shared harness assertions](../../demo/midgard-fault-proofs/tests/support/emulator/harness.ts)
  can enforce useful setup invariants across callers. `expect` in a named
  shared assertion helper is not inherently a defect. Distinguish pure fixture
  construction from a hidden lifecycle test whose failure name is misleading.
- The invalid-signature lifecycle's existing oracle distinguishes a `Spend`
  failure from a `Mint` failure. It does not identify an exact validator hash;
  do not describe it as doing so.

## Focused work

1. For lifecycle/removal suites used as acceptance, record the actual script
   identities, substitutions, catalogue-membership checks, and transaction
   limits. Correct any name that claims a boundary which was bypassed.
2. Where real on-chain authorization is required, exercise the real applied
   validator with a successful fixture control and a discriminating refusal
   case. Preserve independent builder tests when they serve another purpose.
3. Keep oversized diagnostic results separate from release evidence. A
   release claim needs measured complete signed transactions under the stated
   ledger limits and the actual deployment/publication route.
4. In registration tests, authenticate category IDs and applied first-step
   identity and verify membership where that is the claim. Do not replace
   this with a uniqueness-only assertion.
5. Preserve useful shared assertions. Move an embedded scenario into a named
   test only when it improves attribution without losing the setup invariant.
   Improve error rendering where needed: plain `Error` commonly stringifies
   as `{}`, but objects with enumerable properties or custom serialization
   need not do so.

## Acceptance

Each acceptance claim identifies the boundaries exercised and its limitations.
No waived publication or substituted validator is counted as proof of the real
boundary. Focused checks preserve happy-path and intended-refusal coverage.
Do not require zero harness substitutes, zero shared `expect` calls, or a
hash-distinctness roster as a proxy for completeness.
