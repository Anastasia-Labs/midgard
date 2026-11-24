# Test-quality remediation tasks

Status: Active
Last reviewed: 2026-11-23

These proposed tasks are a review backlog, not authorization to delete every
search match or a claim that runtime remediation is complete. Use the cards
with the [principles](principles.md) and the
[writing-tests skill](../../.agents/skills/writing-tests/SKILL.md).

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
[documentation policy](../DOCUMENTATION_POLICY.md). A guardrail for a confirmed
defect class lands as a lint rule under
[lint-rules.md](../agents/lint-rules.md), with its fixtures and baseline.

## Sequencing

Establish required execution lanes first (TQ-02–03), then improve refusal
oracles (TQ-04–07). Review individual assertions and contracts before
consolidating their harnesses (TQ-08–18).

## Tasks

| Task                                             | Intended work                                                       |
| ------------------------------------------------ | ------------------------------------------------------------------- |
| [TQ-02](tq-02-fail-closed-skips.md)              | Enforce required lanes and document optional prerequisites          |
| [TQ-03](tq-03-timeout-and-budget-basis.md)       | Make timeout and execution-budget contracts explicit                |
| [TQ-04](tq-04-typed-refusal-codes.md)            | Use structured refusal causes where available                       |
| [TQ-05](tq-05-name-the-failure-mode.md)          | Make the assertion establish the claimed failure mode               |
| [TQ-06](tq-06-discriminating-onchain-refusal.md) | Distinguish on-chain refusal from unrelated failures                |
| [TQ-07](tq-07-aiken-fail-vacuity.md)             | Review fixture controls and actual Aiken test selection             |
| [TQ-08](tq-08-tautological-assertions.md)        | Remove confirmed self-comparisons while preserving integrity checks |
| [TQ-09](tq-09-type-system-restatements.md)       | Review and strengthen runtime shape checks                          |
| [TQ-10](tq-10-vacuous-and-wall-clock.md)         | Strengthen weak predicates and appropriate timing tests             |
| [TQ-12](tq-12-derive-not-photograph.md)          | Classify pins without weakening identity or budget gates            |
| [TQ-14](tq-14-mocking-and-seams.md)              | Review interaction contracts and semantic test-only branches        |
| [TQ-16](tq-16-template-drift.md)                 | Share scenarios where it preserves coverage and readability         |
| [TQ-17](tq-17-harness-flags.md)                  | State exactly which validators harness substitutions exercise       |
| [TQ-18](tq-18-inverted-and-misnamed.md)          | Preserve fail-closed checks and correct overpromising names         |

No removable-line quota or blanket assertion-shape ban is a completion target.
