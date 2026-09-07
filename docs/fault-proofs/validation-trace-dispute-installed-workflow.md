# Installed validation trace dispute workflow

`validationTraceDispute` is the sole interactive fault-proof family and the
last category the watcher lacked. With its runner installed the watcher
workflow registry reads 54/54: `WATCHER_MISSING_WORKFLOW_CATEGORIES` is empty
and every `PlutusExecutionFailed` rejection routes to this family.

The family reuses the shared seventeen-step dispute machinery unchanged. What
is new is family-local: a workflow identity and binding, a cursor derived from
on-chain state, a move planner, an actuator, and a runner surface whose
external `WorkflowAdapterRunner` contract is identical to the other
fifty-three.

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
silently re-pairing out-refs to roles.

The deployed interactive chain is `VALIDATION_TRACE_DISPUTE_STEP_COUNT = 175`.
The SDK family builder composes the `steps` array — six control validators,
the proof item, every semantic resolver, the shared ScriptSources item chain,
the CEK context and item stages, the canonical decode item stages, and the
fourteen prepare resolvers — and fails the build if its length is not exactly
that constant. The count therefore cannot drift from the real composition.

Both earlier pins are reconciled against it rather than restated:

- the watcher proof-thread indexer's `stepCount: 121` and its hand-written
  six-entry `deployedStepContractNames` list now read
  `VALIDATION_TRACE_DISPUTE_STEP_COUNT` and
  `Object.values(VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES)`;
- the SDK builder test's two `toHaveLength(139)` pins now assert the constant.
  The distinct-script-hash tally in the same test moved 151 → 205: it was
  unreachable while the stale 139 pin failed first, and the previously
  untallied semantic and stage validators supply the difference.

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

## Verification receipt

Blueprint used by the journey tests (`aiken v1.1.23+5adf783`, normal testnet
build at the branch base, no lane commit touches `onchain/`):
`618dda7547bf0ad8e23b91062d47ae181ba37ac2617d5acebc7ccda3e04a4f5d`.

Journey coverage — `demo/midgard-fault-proofs/tests/validation-trace-dispute-installed-lifecycle.test.ts`,
3/3, every watcher move driven through
`createManifestBoundValidationTraceDisputeWorkflow` /
`runOrResumeManifestBoundValidationTraceDisputeWorkflow` on a real emulator
ledger, never through a direct submit helper:

- Honest-responding operator: detect, initiate, the full bisection game against
  a real counterparty, one-step resolution, award, removal.
- Operator stall: the watcher plays to a counterparty turn, the operator never
  answers, the response deadline lapses, and the re-derived cursor arms the
  timeout claim. The journal shows `enter_timeout`, `timeout` and `remove`
  stages and no `award` stage.
- Interruption and resume: the runner is killed at an awaiting-counterparty
  position and restarted cold — a new constructor and a freshly reopened
  journal, no surviving workflow object, plan or cursor. It re-derives the same
  waiting position and deadline from chain state, waits while it is not its
  turn, acts once the operator answers, and completes.

Refusal polarity, both at the exact check and both inside the honest journey:

- a caller-authored challenge byte-identical to the admitted one is refused by
  the admission registry before any binding work
  (`production validation-trace challenge is not admitted`);
- a forged operator midpoint — the honest committed proof with one tampered
  state-hash byte — is refused at the midpoint-verification conjunct
  (`Invalid operator midpoint proof`), and the honest reveal at that same
  position is then accepted.

Suites green: watcher `fault-proof-application` 3/3 (registry 54/54, the
`PlutusExecutionFailed` classification sweep, and the reference-roster
preflight); `typed-reason-disposition` 6/6 with the runner-registry residue pin
empty; `validation-trace-dispute-workflow-plan` 6/6; SDK `fault-proof` 30/30;
watcher `proof-thread-indexer` 18/18; `submit-init-emulator-min-ada` and
`submit-init-emulator-validation-dispute-script-sources-observer-max` 7/7.
`tsc --noEmit` clean in `midgard-fault-proofs`, `midgard-watcher` and
`midgard-sdk`.

## Reconciliation notes

The oversized `state_queue.mint` reference-script publication
(Anastasia-Labs/midgard#649) is pre-existing and system-wide: the ten-parameter
script applied to this deployment is 16,498 bytes and cannot be published under
the real 16,384-byte L1 envelope at all. Production code never raises a limit
for it. The journey tests publish the removal validators through the same
test-driver deployment mechanism the other families' tests use, with the issue
cited at the use site.

The runner is already idempotent across an unconfirmed submission: it replays
the journal's outstanding `submission_intent` and returns `pending` rather than
re-planning. The emulator nevertheless reports a mempool transaction as
confirmed while its outputs are not yet queryable, so the journey harness lands
each move in a block before the next cold restart; otherwise a restart would
re-derive a cursor from a chain that does not yet show the move.

Two reds in adjacent suites are pre-existing — both fail identically at the
branch base `ca9cb71fa` with this lane's work stashed, and neither touches this
family: the CEK context planner's mixed-width 1,304-asset source maximum
(`CEK output context differs from the evaluated context`, 1 of 26), and the
watcher settlement indexer (18 of 26). The reference-input over-basis item is
owned by another lane.
