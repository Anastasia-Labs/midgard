# Test-quality remediation tasks

Status: Active
Last reviewed: 2026-09-07

Nineteen proposed tasks organize the [test-quality review](../test-quality-audit.md).
They are a review backlog, not authorization to delete every search match or a
claim that runtime remediation is complete. The semantic review corrected
false positives and stale examples in the original audit; use the current
cards and [principles](principles.md).

## How to use a task

Read the named source and its producer/consumer before changing a check. Search
commands locate candidates and may miss multiline forms or include valid
assertions. Record executed tests and current measurements for the actual
change; do not use historical site counts or declared timeouts as evidence.

Keep required verifier inputs and release gates. A current test can be useful
because it guards a producer's present guarantee. A saved measurement does not
replace current execution, and passing remaining tests does not prove that a
removed assertion added no coverage.

Update task status in place. When delivered, retain durable decisions and
required evidence, then remove completed task prose according to the
[documentation policy](../DOCUMENTATION_POLICY.md).

## Sequencing

Establish provenance and required execution lanes first (TQ-01–03), then improve
refusal oracles (TQ-04–07). Review individual assertions and contracts before
consolidating their harnesses (TQ-08–18). Add a narrow guardrail with each
confirmed repair where useful (TQ-19). A reviewed baseline may prevent new
violations while existing candidates are still being classified.

## Tasks

| Task                                             | Intended work                                                       |
| ------------------------------------------------ | ------------------------------------------------------------------- |
| [TQ-01](tq-01-blueprint-provenance.md)           | Classify evidence and validate current-build provenance             |
| [TQ-02](tq-02-fail-closed-skips.md)              | Enforce required lanes and document optional prerequisites          |
| [TQ-03](tq-03-timeout-and-budget-basis.md)       | Make timeout and execution-budget contracts explicit                |
| [TQ-04](tq-04-typed-refusal-codes.md)            | Use structured refusal causes where available                       |
| [TQ-05](tq-05-name-the-failure-mode.md)          | Make the assertion establish the claimed failure mode               |
| [TQ-06](tq-06-discriminating-onchain-refusal.md) | Distinguish on-chain refusal from unrelated failures                |
| [TQ-07](tq-07-aiken-fail-vacuity.md)             | Review fixture controls and actual Aiken test selection             |
| [TQ-08](tq-08-tautological-assertions.md)        | Remove confirmed self-comparisons while preserving integrity checks |
| [TQ-09](tq-09-type-system-restatements.md)       | Review and strengthen runtime shape checks                          |
| [TQ-10](tq-10-vacuous-and-wall-clock.md)         | Strengthen weak predicates and appropriate timing tests             |
| [TQ-11](tq-11-source-text-assertions.md)         | Match source checks and behavioral tests to their contracts         |
| [TQ-12](tq-12-derive-not-photograph.md)          | Classify pins without weakening identity or budget gates            |
| [TQ-13](tq-13-snapshot-artifacts.md)             | Preserve regression coverage while reducing snapshot noise          |
| [TQ-14](tq-14-mocking-and-seams.md)              | Review interaction contracts and semantic test-only branches        |
| [TQ-15](tq-15-fit-ledger-consolidation.md)       | Consolidate fit evidence without losing live measurements           |
| [TQ-16](tq-16-template-drift.md)                 | Share scenarios where it preserves coverage and readability         |
| [TQ-17](tq-17-harness-flags.md)                  | State exactly which validators harness substitutions exercise       |
| [TQ-18](tq-18-inverted-and-misnamed.md)          | Preserve fail-closed checks and correct overpromising names         |
| [TQ-19](tq-19-guardrails.md)                     | Add proportionate checks for confirmed defect classes               |

## Current evidence inventory

There are 43 retained fit-ledger JSON inputs: 25 with current-build/live
comparison consumers and 18 saved self-consistency fixtures. There are 22
publication-fit test files and 40 fit-ledger test files. These counts describe
different surfaces; they do not prove duplicate execution or readiness.
TQ-01 records the provenance comparison procedure and its limitations.

Already changed source examples include the reverse-checkpoint byte vector,
the protocol-based oversized threshold, the empty missing-installation set,
and the removal of obsolete README-heading and KNOWN RED history checks/prose.
The relevant cards identify these rather than scheduling the old forms again.
No removable-line quota or blanket assertion-shape ban is a completion target.
