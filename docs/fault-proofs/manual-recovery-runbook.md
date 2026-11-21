# Fault-Proof Manual Recovery Runbook

Use this branch after interruption, ambiguous submission, rollback, retained-DA
failure, or watcher readiness loss. Recovery attaches to the same deployment;
it does not reset local or on-chain state. The watcher alerts that lead here,
and the checks to run before freezing anything, are in
[`docs/operations/alerts.md`](../operations/alerts.md).

## 1. Freeze actuation

Stop new watcher admissions while preserving the decision journal, raw DA
responses, and provider observations. Keep the existing deployment manifest
and wallet identity.

Completion: no new workflow can submit while recovery is in progress.

## 2. Read every transaction of ours from L1

The watcher keeps no local record of what it sent: its in-flight
transactions live in process memory only. Read the prover wallet's and the
objective's transactions from the watcher's L1 follower and classify each as
confirmed, rolled back, definitely absent, or still in flight. A transaction
that spends only wallet coins is in flight until its short validity bound has
passed.

Completion: every transaction of ours has an authenticated terminal
observation, or its validity bound has passed, or recovery remains paused.

## 3. Re-read protocol state

Read the challenged header, computation-thread token, permanent proof token,
operator node, scheduler and correction lock from L1. Re-fetch the retained payload and proof artifacts with the original
deployment fingerprint.

Completion: the next action follows from L1 and the retained evidence. A
manifest mismatch, missing evidence or provider disagreement keeps recovery
paused.

## 4. Resume through the owner

Restart the production watcher with the same journal directory. A fault still
in the finalized queue is admitted again, and its category runner reads the
workflow's progress from the chain: a completed proof that has not removed the
header resumes at removal, and a confirmed removal resumes at terminal
verification. A next transaction that spends only wallet coins waits out one
wallet-only validity window first (`fault_proof_restart_wait`).

The manual `resume-workflow` CLI is diagnostic unless invoked through the
application owner that supplies the admitted permits and runtime registry.
Submission stays automatic after the restart: the resumed watcher signs and
submits the next action itself. Use `workflow-readiness` (see the
[challenger runbook](challenger-runbook.md#3-detect-and-prosecute)) to
inspect a workflow without submitting.

Completion: the resumed workflow advances on L1 with exactly one thread of
ours on the header.

## 5. Escalation cases

- Missing retained data after an authenticated attestation: retain all local
  evidence and keep readiness failed while the availability challenge runs.
- L1 rollback: rewind to the last common chain point, invalidate observations
  above it, and let the watcher replay before resubmission.
- Provider disagreement: preserve both raw responses and wait for the required
  finality agreement.
- Concurrent challenger win: verify the target is already removed and the bond
  paid; the watcher reads it as an idempotent terminal result.
- An objective held as `fault_proof_resubmission_required` or
  `fault_proof_start_deadline_passed`: its header has matured, or (for a
  resubmission hold) the signed objective is not the watcher's current proof
  target, and the watcher holds reconciliation-only authority, so a restart
  does not resubmit it. Preserve the journals and escalate while the challenge window
  is open; proving is permissionless, so another challenger can still prove
  the fault.
- Reference-script or deployment mismatch: stop. Repair by deploying a coherent
  identity; never substitute a locally rebuilt hash into an existing manifest.

Completion: recovery ends only with independently verified removal/correction
or an explicit fail-closed readiness state with all evidence preserved.
