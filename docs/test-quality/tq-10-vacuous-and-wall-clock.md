# TQ-10 — Strengthen weak refusal and timing oracles

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit sections**: §5 and §10
- **Rules**: R1, R3, R4, R9, R11 in [principles](principles.md)
- **Coordination**: TQ-04–06 cover refusal helpers; TQ-03 covers measurements.

## Problem and boundary

A non-empty error message is weaker than a particular refusal code, but it is
not a tautology: `new Error().message` is empty. Such a check can establish
that diagnostics exist. It cannot establish which validator refused or that a
failure occurred for the reason named by an adversarial scenario.

An unguarded loop can pass when its input is empty. An expected non-empty
scenario set should be checked independently, unless empty input is itself a
valid case or another assertion already establishes the required membership.

Real-time checks are not automatically defects. Unit logic benefits from a
controlled clock; transport deadlines, real process scheduling, database
expiry, and cancellation integration may also need real-clock verification.
A configured timeout is an allowance, not observed runtime.

## Review candidates

- Refusal-message checks in the native-script-decoding,
  committed-field-shape, and double-withdraw emulator suites: distinguish a
  diagnostic-presence claim from a specific guard claim. Add the appropriate
  oracle before removing a weaker one.
- Artifact and roster loops: establish the required row/scenario set, not
  merely a non-zero count derived from whatever rows happened to be returned.
- [DA request deadlines](../../demo/midgard-core/tests/da-request-deadline.test.ts):
  the first case checks a timeout error, abort count, and a real elapsed bound.
  Preserve stalled-stream and late-open cancellation behavior. Use controlled
  time for deterministic logic where appropriate, while keeping an explicit
  integration check if real scheduling is part of the contract.
- Shared-process RSS and short database-expiry waits: investigate isolation,
  repeated measurements, and scheduling margins before prescribing removal.
  An entry-count cap and RSS measure test different properties.
- Byte, count, or diagnostic-length thresholds: identify the declared limit
  or regression they protect. A conservative application reserve can be valid
  even when it differs from a ledger maximum.

## Work and acceptance

1. Classify each candidate by its claimed observable and required execution
   layer. Do not infer a defect from a matcher spelling or timer API alone.
2. For specific negative scenarios, preserve a successful fixture control and
   a refusal signal that distinguishes the intended failure from setup errors.
3. For required collections, demonstrate that removing a required member is
   detected; permit empty collections where the contract permits them.
4. Stabilize timing tests through controlled clocks, process isolation,
   bounded waits, or justified margins as appropriate. Keep performance
   regression measurements separate from functional correctness claims.
5. Run the affected tests and report measured results and any infrastructure
   limitation. Do not require zero wall-clock tests or zero non-empty-message
   assertions as a proxy for quality.
