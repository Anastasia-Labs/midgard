# TQ-02 — Make required execution fail closed and optional lanes visible

Status: Proposed; obsolete helper commentary removed
Last reviewed: 2026-09-07 (runner and prerequisite review)

Audit: §8, §11. Rule: R7. Related: all tasks relying on suite execution.

## Problem and current evidence

Required coverage can disappear when a suite skips after a prerequisite check
or when its runner is never invoked. An explicit optional integration lane is
not itself a false green: the report must distinguish skipped from executed.

For example, `missing-redeemer-publication-fit.test.ts` reads the blueprint at
module scope before `describe.runIf(hasFamily)`. A missing file therefore
fails collection; a present blueprint missing the family can skip the suite.
Do not describe both situations as a silent fresh-checkout skip.

Tests under `scripts/` and `devnet/` may run through explicit `node --test`
package commands. Their location outside Vitest's `tests/` glob does not prove
that CI omits them. Inspect the complete workflow-to-package-command chain.
The current node package registers Architecture G corpus, configuration, soak
and closure tests; node-tools registers Phase 4 asset and summary tests.

The historical KNOWN RED changelog in
`tests/support/emulator/blueprints.ts` has already been removed. That
documentation portion is closed; runtime loader behavior was not changed.

## Proposed work

1. Inventory actual required and optional lanes, prerequisite tokens, package
   commands, workflow invocations and skip reports. Treat old site counts as
   discovery notes rather than a verified current inventory.
2. For required family coverage, fail on a missing family or prerequisite.
   Optional platform, database or costly measurement lanes need a runnable
   command and visible reason when not selected.
3. Wire genuinely uninvoked required suites. Retain useful opt-in Postgres
   integration tests even if CI infrastructure is not yet available, and state
   the resulting coverage gap. Moving a file outside a test directory does not
   resolve missing execution.
4. Distinguish generators from tests by behavior and consumers. Preserve live
   publication/evaluation coverage when separating artifact generation.
5. Build before checks that intentionally consume `dist`, or verify that the
   distribution matches source. Source-only checks may use workspace source
   resolution; stale output must not satisfy a cross-language acceptance gate.
6. Keep loader parameter checks where appropriate, but do not treat an arity
   check as a substitute for correctly applied emulator deployments succeeding
   and refusing in both relevant polarities, as required by `AGENTS.md`.

## Acceptance

- Every required lane proves it collected and ran its named tests.
- Every remaining optional lane documents its command, prerequisites and
  limitations; unavailable Postgres does not trigger deletion of useful tests.
- CI discovery follows actual invocations, including standalone Node suites.
- Required build absence and family absence have explicit non-passing outcomes.
- Parameter changes update builders and fixtures and rerun the applicable
  emulator scenarios; no dedicated arity gate substitutes for them.
