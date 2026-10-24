# TQ-14 — Review mocks and test seams against their claimed behavior

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit section**: §6
- **Rules**: R2, R3, R10 in [principles](principles.md)

## Problem and boundary

A mocked collaborator cannot establish that collaborator's real implementation
works. It can establish how the system under test uses the collaborator,
including required call counts, ordering, arguments, and forbidden effects.
Those interactions may be the observable contract: exactly-once submission,
revalidation after a state change, or no mutation lease after refusal.

Do not impose a general preference for order over count, absence over presence,
or state over interactions. Identify the contract before replacing an oracle.
Mocking an owned interface is legitimate; separate adapter/integration tests
must cover real external behavior where required.

## Focused source review

- [Provider-evidence preflight tests](../../demo/midgard-node/tests/block-commitment-provider-evidence-preflight.test.ts)
  vary which probe fails and check that no lease is taken. Some also assert
  repeated fetch counts. Determine whether each repeat protects freshness or
  is incidental before suggesting a cache or deleting the count. Identical
  returned idle state does not make the no-lease assertions redundant.
- [The resolved-output journal adapter](../../demo/midgard-fault-proofs/src/resolved-output-non-canonical/central-journal.ts)
  accepts a `testOnlyJournalCategoryAlias` only under `NODE_ENV=test` and uses
  it in workflow identity. Similar aliases occur in related family adapters.
  Identify which tests claim the real category's journal/recovery behavior.
  Migrate those tests to the real category where possible and review whether
  the alias can be isolated to test support. This is a semantic decision-path
  difference, unlike a read-only diagnostic accessor.
- Historical preimage, cancellation, and event-handler tests with mocked
  collaborators: check which production function still runs. Retain useful
  orchestration coverage; add real admission/persistence coverage when the
  name claims it. Do not delete a mixed suite solely because it uses mocks.
- Private-method overrides and `ForTest` diagnostics: prefer an owned injected
  boundary or an existing public observable when it preserves the assertion.
  Constructors, controlled clocks, funding fixtures, and read-only diagnostics
  are not automatically semantic bypasses.

## Work and acceptance

1. For every changed test, record the system under test, substituted boundary,
   intended observable, and where the real collaborator is verified.
2. Keep counts and orderings that encode a contract. Remove only incidental
   coupling after showing the replacement retains the relevant signal.
3. Review test-dependent branches by reachability and effect, not spelling.
   Production decisions claimed by a suite must not silently take a weaker
   test route. Changes to identity, funding, or authenticated observations
   receive the same review as other protocol-adjacent behavior changes.
4. Run focused tests through the intended production decision path. A useful
   fake or assertion helper may remain in test support; no blanket ban on
   `NODE_ENV`, `vi.mock`, `expect`, or `ForTest` establishes equivalence.

Source scan counts are leads, not a defect inventory or deletion quota.
