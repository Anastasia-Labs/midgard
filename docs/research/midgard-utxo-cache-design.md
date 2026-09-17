# UTxO Cache Design Decision

Status: Proposed — no implementation or adoption approved.

Reviewed: 2026-09-07.

## Problem

Repeated wallet and script UTxO queries can add latency to transaction building.
Concurrent builders can also select the same input, while delayed provider views
can reintroduce inputs already spent by a local submission. A cache could reduce
queries and coordinate selection, but it must remain an optimization: it cannot
become wallet, chain, or protocol authority.

Before adoption, measure query latency, stale-input failures, contention, and
rebuild cost in the current commit, merge, and scheduler flows. Compare those
measurements with the existing state-queue mutation leases, pending-finalization journals, and
submission recovery machinery.
This proposal does not authorize a new package or changes to those flows.

## Authority and scope

An operator-wallet cache requires either a single service that owns all spending
for that wallet or durable coordination shared by every spending process.
An in-process mutex alone cannot provide that guarantee. Script UTxOs can be
spent externally, so a local checkout only prevents conflicting local selection;
it cannot guarantee that an input will remain available on chain.

Keep operator-main, merge, reference-script, DA submitter, and script roles
separate. Persist role, network, and deployment identity with their records;
reject mismatched deployment state on startup. Protocol-specific selection,
such as identifying the canonical state-queue tail, remains in the caller.
The generic component may index outrefs, credentials, reservations, and exact
transaction effects; it must not decide whether a commit or merge is valid.

## Required invariants

- Input checkout is exclusive, durable, and fenced against expired or superseded
  workers. Selection and reservation are atomic. A snapshot is advisory and
  cannot authorize spending.
- Every wallet candidate exposed to Lucid completion is checked out and passed
  through `presetWalletInputs`. Inspect the exact signed transaction to ensure
  its controlled spend inputs are a subset of held wallet/script checkouts.
  Release checked-out candidates that the final transaction does not spend.
- Before calling a submit provider, atomically persist the signed transaction
  identity, exact spent inputs, produced outputs, validity bounds, reservation
  ownership, and all affected roles. Production persistence is database-backed;
  a crash cannot record wallet effects without the corresponding script effects.
- Inputs of prepared or ambiguously submitted transactions remain hidden across
  restarts. A timeout, missing mempool entry, or expired worker lease is not proof
  of non-acceptance and cannot release them.
- Produced outputs derive from the exact signed body and output indexes, never
  from builder intent. Speculative outputs are retained for reconciliation but
  are never selectable. Accepted or observed local outputs may be selected only
  under explicit local-chaining policy, disabled by default, with provenance
  sufficient to invalidate dependencies after rollback.
- Refresh merges against the latest hidden inputs, reservations, missing markers,
  and submission attempts. Serialize the operation or use generation/CAS checks
  so a provider query started earlier cannot overwrite a newer reservation or
  reintroduce a locally spent input.
- Provider/mempool observations are evidence with explicit provenance and
  freshness. Mempool failure or absence cannot restore hidden inputs or prove
  script-state freshness. External spends invalidate script selections.
- Reconcile unresolved attempts using transaction status, canonical-chain
  evidence, validity expiry, or an explicit recovery record that proves the
  transaction cannot land. Handle rollback of observed transactions and dependent
  local outputs explicitly; never equate submission acceptance with finality.

## Submission and recovery boundary

A small facade should own checkout, inspection, durable preparation, and outcome
recording around caller-owned build/sign/submit callbacks. Ordinary callers
should not manually coordinate reservation ids and transaction effects or use
raw wallet snapshots as default inputs. Diagnostics may expose snapshots without
making them spend authority.

The sequence is:

1. Reconcile durable state and refresh before serving; refuse readiness when old
   unresolved attempts or reservations have no explainable recovery state.
2. Atomically check out wallet candidates and caller-selected script inputs.
3. Build and sign, verify ownership, and persist all effects before submission.
4. Record accepted, ambiguous, or definitively rejected outcomes. Retain hidden
   inputs for ambiguous outcomes; release or mark stale inputs only on evidence.
5. Reconcile confirmation, rollback, and external script changes, retaining enough
   evidence for restart and audit.

Build/sign failures before submission release checkouts. Stale-input failures
invalidate the affected wallet or script input, refresh the relevant context,
and return a typed error for a bounded caller-owned rebuild. Provider error
classification needs context: `BadInputsUTxO`, unknown output references, or
value-conservation errors are not proof that a commit or merge succeeded, and
an ambiguous outcome must first be reconciled against the exact signed hash.
The component must preserve signed context instead of accepting only a returned
tx hash, and must not own generic retry loops.

## Alternatives

| Option                                                    | What must justify it                                                                                                    |
| --------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| Improve current provider queries and durable coordination | Prefer when it removes the measured bottleneck without introducing another state machine.                               |
| Add a facade over existing durable coordination           | Prefer when ownership is already correct but callers repeat unsafe selection/submission bookkeeping.                    |
| Extract a reusable cache package                          | Require multiple concrete consumers, shared semantics, and a simpler total design than extending existing coordination. |

Do not introduce compatibility adapters or schema migrations for undeployed
versions. If adopted before launch, replace development schemas in place and
follow the repository's reset/redeploy rules.

## Adoption evidence

An implementation proposal must identify its source-of-truth store, integration
boundary, callers, and measured benefit before selecting an API or module layout.
Acceptance requires tests that exercise:

- Two builders/processes contending for one input; stale lease owners cannot
  mutate or submit through a newer checkout.
- Refresh racing with checkout, preparation, external spends, and rollback.
- Crashes before submit, after provider acceptance but before local recording,
  and during multi-role persistence; restart never reuses unresolved inputs.
- Exact signed-body inspection, unspent candidate release, foreign/unreserved
  input rejection, and speculative output exclusion.
- Accepted local chaining only when explicitly enabled; dependent-output
  invalidation, expiry reconciliation, and script-input staleness.
- Caller-owned bounded rebuilds, typed ambiguous outcomes, and role/deployment
  isolation.

Operational evidence must include per-role reservation and unresolved-attempt
ages, refresh lag, stale-input and rebuild rates, recovery diagnostics, readiness
behavior, and a runbook for provider outages and unexplained attempts. Benchmark
query reduction and end-to-end latency against the existing implementation under
contention; adoption requires correctness evidence as well as a measurable gain.
