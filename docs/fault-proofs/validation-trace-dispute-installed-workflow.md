# Installed validation trace dispute workflow

`validationTraceDispute` is the sole interactive fault-proof family. The
watcher installs it alongside the other catalogue families: `WATCHER_MISSING_WORKFLOW_CATEGORIES` is empty
and every `PlutusExecutionFailed` rejection routes to this family.

The family uses shared dispute machinery with a manifest-bound workflow,
on-chain cursor, move planner, actuator, and the same `WorkflowAdapterRunner`
interface as the other categories.

## Roster and step count

The manifest roster is derived from three family constants and never re-typed
at a consumer:

- `VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES` — the six control
  validators (`opener`, `source`, `game`, `boundary`, `timeout`, `award`).
- `VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES` — the three witness
  scripts the submitters attach by reference: `computationThreadMint`,
  `fraudProofMint`, and the `phas.membership.withdraw` verifier the shared
  `submitInit` executes when the runner opens the thread. Reference-script
  carriage is fail-closed, so all three must be bound.
- `VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES` — the nine shared
  cursor-removal contracts.

Eighteen roles in that order are what startup readiness admits and what
`taggedReferenceOutRefs` flattens, so a roster change fails closed rather than
silently re-pairing out-refs to roles. Startup also requires the manifest's
`cekProgramMaterialSpend` entry. Prepare, semantic, stage, and yield references
beyond the eighteen tagged roles are resolved from the deployment identity at
action time; eighteen is not the complete physical reference-script inventory.

The deployed interactive chain is `VALIDATION_TRACE_DISPUTE_STEP_COUNT = 175`.
The SDK family builder composes the `steps` array — six control validators,
the proof item, every semantic resolver, the shared ScriptSources item chain,
the CEK context and item stages, the canonical decode item stages, and the
fourteen prepare resolvers — and fails the build if its length is not exactly
that constant. The count therefore cannot drift from the real composition.

The watcher proof-thread indexer and SDK builder tests consume the same
step-count constant. Derive the physical roster from those authorities rather
than restoring old copied counts.

## What "installed" means

Installation is completeness of _moves_, not unilateral instant completion. An
interactive family cannot finish on its own schedule — the counterparty holds
half the turns. The requirement is that the watcher always has an action on its
turn, including claiming the timeout when the operator stalls, and that it
correctly waits when the turn is not its own.

`assertStartupReady` asserts that move-completeness. The cursor is derived
exclusively from on-chain state — turn indicator (`awaiting_operator` /
`awaiting_challenger`), bisection depth, resolution and semantic sub-chain
positions including the ledger-output-proof span-attach, fact-attach, step and
terminal-finalize positions, enter-timeout, `timeout_claimable`, award, and
removed — so a cold restart re-derives its position without consulting any
in-memory plan.

## Verification

The [installed lifecycle scenarios](../../demo/midgard-fault-proofs/tests/validation-trace-dispute-installed-lifecycle.test.ts)
exercise an honest-responding counterparty through full bisection, resolution,
award and removal; a stalled operator through timeout and removal; and a cold
restart that re-derives its waiting position/deadline from L1. They refuse
unadmitted caller-authored challenges and forged operator midpoint proofs.
The emulator must advance a block before cold restart so outputs are queryable;
mempool acknowledgement alone is not confirmation.

Run those scenarios together with the workflow-plan, workflow-runtime,
typed-reason-disposition, watcher application and proof-thread-indexer tests.
Use the real blueprint for SDK contract-builder scenarios and verify that each
selected scenario collects rather than returning early because the blueprint
variable is absent. Parameter-order and composed-length assertions must execute.

[Resolver publication](../../demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts)
measures every current semantic resolver and compares the complete saved ledger
on ordinary runs. This establishes publication; it does not establish every
maximum semantic execution. [Remaining acceptance](execution-plan.md) requires
registered provenance, refusal, recovery, execution margins, real-node and
preprod correction as well.
