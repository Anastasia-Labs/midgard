# Fault-Proof Manual Recovery Runbook

Use this branch after interruption, ambiguous submission, rollback, retained-DA
failure, or watcher readiness loss. Recovery attaches to the same deployment;
it does not reset local or on-chain state.

## 1. Freeze actuation

Stop new watcher admissions while preserving the decision journal, workflow
journal, funding reservations, raw DA responses, and provider observations.
Keep the existing deployment manifest and wallet identity.

Completion: no new workflow can submit while reconciliation is in progress.

## 2. Reconcile every recorded transaction

For each journaled transaction, query the configured local Kupo/Ogmios pair and
classify it as confirmed, rolled back, definitely absent, or still ambiguous.
Treat ambiguous as in flight. Never replace a hash or mark a step confirmed by
editing the journal.

Completion: every attempted transaction has an authenticated terminal
observation or recovery remains paused.

## 3. Re-read protocol state

Read the challenged header, computation-thread token, permanent proof token,
operator node, scheduler, correction lock, and state-queue mutation lease from
L1. Re-fetch the retained payload and proof artifacts with the original
deployment fingerprint.

Completion: the next action follows from L1 plus immutable journal state. A
manifest mismatch, missing evidence, provider disagreement, or active foreign
lease keeps recovery paused.

## 4. Resume through the owner

Restart the production watcher with the same journal directory. Its supervisor
must reconstruct the admitted decision, funding permit, and exact workflow
checkpoint, then call the category runner in `resume` mode. A completed proof
that has not removed the header resumes at removal; a confirmed removal resumes
at terminal verification.

The manual `resume-workflow` CLI is diagnostic unless invoked through the
application owner that supplies the admitted permits and runtime registry.

Completion: the resumed journal advances without duplicating a confirmed
transaction or acquiring an unfenced mutation lease.

## 5. Escalation cases

- Missing retained data after an authenticated attestation: retain all local
  evidence and keep readiness failed. The current availability-challenge
  script cannot be published within the 16,384-byte L1 envelope, so no current
  manifest may authenticate that capability; do not attempt or claim this
  recovery path until the validator is redesigned and redeployed.
- L1 rollback: rewind to the last common chain point, invalidate observations
  above it, and let the watcher replay before resubmission.
- Provider disagreement: preserve both raw responses and wait for the required
  finality agreement.
- Concurrent challenger win: verify the target is already removed and the bond
  paid, release this workflow's reservation, and reconcile as an idempotent
  terminal result.
- Reference-script or deployment mismatch: stop. Repair by deploying a coherent
  identity; never substitute a locally rebuilt hash into an existing manifest.

Completion: recovery ends only with independently verified removal/correction
or an explicit fail-closed readiness state with all evidence preserved.
