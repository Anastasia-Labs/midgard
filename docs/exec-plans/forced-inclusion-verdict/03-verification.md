# Task 3: Verify the final change and prepare deployment acceptance

- Status: Proposed
- Last reviewed: 2026-09-10
- Implementation boundary: independent review, required local verification, defect repair, and a concrete deployment acceptance handoff.
- Dependencies: Tasks 1 and 2 complete on the same integration branch, with the ADR, acceptance matrix, final diff, and artifact/test evidence available.
- Non-goals: live submissions, persistent-state resets, mainnet deployment, merging/publishing, or claiming completion of the wider canonical V1 program.

## What this task accomplishes

Check that the completed change delivers the promised safety property and that
its proofs still work within Cardano limits. Review the code against the agreed
design, challenge its assumptions, and verify the actual rebuilt artifacts.
The output distinguishes what is locally proven from what still needs a live
deployment run. It is a release-readiness assessment, not deployment itself.

Read [Task 1](01-protocol-design.md), including the shared execution contract and
its resulting ADR/matrix, and [Task 2](02-implementation.md). Perform a fresh
review of the complete implementation against those requirements; do not accept
the implementation summary or its fixtures as the specification. Another task's
passing evidence may be reused only when its source, environment, parameters,
and artifact identities match and its scenarios establish the claimed behavior.

## Review and repair

1. **Confirm the final input.** Identify the baseline, integration revision,
   relevant uncommitted input, and exact diff. Verify Task 2's generated schemas
   actually describe the final source. When using a disposable Aiken copy,
   follow the build skill's instructions for excluding stale caches and
   blueprints; preserve the shared checkout.
2. **Audit each trust boundary.** Trace L1 authentication → submitted commitment
   → DA/block leaf → machine and field openings → proof terminal → settlement
   or correction. Account for every affected consumer from Task 1. In particular,
   distinguish the immutable submitted transaction, the operator's authenticated
   claim, and the independently computed outcome. Check that no consumer hashes
   or trusts an effective view as an independent source.
3. **Challenge the implementation.** Review and execute Task 2's full behavioral
   matrix, including altered witnesses with the same `tx_id`, altered lengths,
   foreign order/header/context, both false-verdict directions, wrong reason
   coordinates, valid-block non-challengeability, and a rejecting dispute with
   a nonempty claimed delta. Construct adversarial inputs through actual boundary
   encodings, not only through typed constructors that exclude malformed input.
4. **Verify capability and public reconstruction.** Remeasure the affected L1
   order mint/settlement and registered proof workflows, including applicable
   complete-item carriage tiers and maximum/adjacent vectors. Keep existing
   capability limits, margins, DA retention, and challenge-time requirements.
   Inspect watcher reconstruction and relevant restart/rollback and incompatible
   deployment-identity cases. Any unmeasured required path remains a failed gate.
5. **Fix discovered defects and revalidate.** Local repairs are in scope. Add
   meaningful regression scenarios, rerun affected checks, and refresh artifacts.
   A change to the protocol decision must be reconciled with the ADR and matrix
   before it is accepted. After each behavior-bearing repair, re-review the
   affected boundaries; a previous review or measurement is not automatically current.

## Verification contract

Task 1 freezes exact focused tests, fit producers, environment, and working
directories. Use current package manifests and CLI help to reconcile stale
commands, without dropping the obligation. The applicable minimum is defined by
`docs/exec-plans/GOAL_SPEC.md` §13. Use its serialized workspace builds and
typechecks; run large Aiken vectors one compiler process at a time.

From the repository root, in the declared Node/pnpm environment:

```bash
nix develop ./demo --command bash -c 'node --version && pnpm --version'
pnpm --dir demo -r --if-present --workspace-concurrency=1 run build
pnpm --dir demo -r --if-present --workspace-concurrency=1 run typecheck
pnpm --dir demo run lint
pnpm --dir demo run format-check
pnpm --dir demo run test:tx-prep:local
pnpm --dir demo run fixtures:transaction-root-v1:check
pnpm --dir docs-site run check:links
git diff --check
```

The first command checks the demo toolchain; it does not put later commands
inside that environment. Enter the declared environment for those commands.
Use docs-site's own declared package manager for its checks. Run `make spec` from
the repository root because this change updates the technical specification.

From `onchain/aiken`, using the fork/revision pinned by
`.github/workflows/aiken-ci.yml` and the build skill:

```bash
aiken --version
aiken fmt --check
aiken check --skip-tests
aiken check
aiken build --env testnet
```

Run the normal testnet build before dependent real-contract emulator suites,
regardless of the order of these grouped command examples. Assert compiler
identity, not only a compatible version number. Run exact large vectors with
`scripts/run-focused-check.mjs` and assert the expected nonzero collected count.
Set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute regenerated blueprint path for
the suites that consume it. Diagnostic traced artifacts cannot supply final fit
evidence for the untraced release artifact.

Run full package suites for `midgard-core`, `midgard-validation`,
`midgard-fault-proofs`, `midgard-sdk`, `da-committee-node`, `midgard-node`, and
`midgard-watcher` with `pnpm --dir demo/<package> test`, plus additional touched
packages identified in Task 1. Run the retained-DA and breadth checks where their
requirements apply:

```bash
pnpm --dir demo/midgard-fault-proofs run test:cardano-capability-p2-retained-da
pnpm --dir demo/midgard-validation run test:cardano-capability-p2:data-breadth
```

Use [the fit-evidence guide](../../fault-proofs/size-plans/README.md) to identify
the lifecycle producers that execute and measure the current registered paths.
A historical JSON snapshot, a changed digest, or publication-only measurements
cannot establish current end-to-end fit. Use the existing protocol thresholds
and bind results to source, compiler, build flags, blueprint, applied parameters,
and validator hashes. Report command, working directory, environment, exit code,
nonzero test count where applicable, duration, and evidence path. Never accept a
zero-test filter or an ignored failure as verification.

## Deliver the assessment and deployment handoff

Write `docs/exec-plans/forced-inclusion-verdict-verification.md` as the concise
evidence index required by this change's acceptance matrix. Identify its current
consumer as the review/release decision for this change. Include:

- The reviewed revision/diff and baseline provenance, ADR, measured artifact
  identities, reproduction commands, results, resolved findings, and residual
  failures. Dirty-tree evidence must remain identifiable and provisional until
  it corresponds to the release commit; behavior-bearing changes invalidate
  dependent evidence.
- Separate judgments for implementation conformance, local behavioral coverage,
  proof capability/fit, and deployment acceptance. A missing required check is
  unsatisfied, even if the underlying limitation predates this change.
- A concrete deployment plan naming the new identity/artifact inputs, affected
  persisted formats, incompatible old identities, required state handling, and
  recovery constraints. Never propose attaching new code to incompatible old
  state or silently wiping a deployment.
- The exact remaining live acceptance obligations and their owner/runbook. Read
  `.agents/skills/midgard-e2e-acceptance/SKILL.md` and the relevant live reference
  before preparing this portion; use current CLI/help and finalizer requirements.
  Identify the target and required resources from existing project context;
  unresolved target/funding information remains explicit. Include both forced
  classification directions, public-data reconstruction, correction economics,
  final reconciliation, and all broader gates the selected live run requires.
  Local results cannot replace these gates.

Do not create a new generic verification harness or progress-manifest checker.
Retain only evidence needed by a named current check or release requirement.
Do not update public readiness to accepted while required live evidence is absent.

## Completion gate

- The independent review covers the entire affected boundary inventory and has
  no unresolved correctness or proof-soundness defect.
- Every required local check and fresh affected fit measurement passes against
  the final implementation/artifacts, with reproducible evidence. A required
  failure or unavailable prerequisite leaves this task incomplete; record the
  blocker and continue independent work rather than manufacturing closure.
- The evidence index states the scope and limitations precisely and provides a
  concrete deployment acceptance handoff. It explicitly states that live
  acceptance is outstanding unless matching authoritative evidence already exists.
- Lasting decisions/specifications and coverage/readiness statements agree with
  the implementation and evidence. No assertion of production or launch readiness
  relies solely on completing these three tasks.

## Progress and handoff

Execution not started; waiting for Tasks 1 and 2. The final user-facing result
should explain what changed, what was proven, and the concrete remaining live
acceptance action. Retire completed task diaries under the documentation policy
after their necessary decisions and evidence have been retained.
