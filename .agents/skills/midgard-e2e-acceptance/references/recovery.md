# Recovery and Stop Routing

Read this reference completely before rerunning `e2e-stack` after it stopped,
was interrupted, or left a service unhealthy.

## Contents

1. [How the stack protects a rerun](#how-the-stack-protects-a-rerun)
2. [Preserve and diagnose](#preserve-and-diagnose)
3. [Route the stop message](#route-the-stop-message)
4. [A signed nonce that can never land](#a-signed-nonce-that-can-never-land)
5. [Recovery completion](#recovery-completion)

## How the stack protects a rerun

- **Controller lock.** One controller per node directory, held with `flock` on
  `<nodeRoot>/logs/full-stack-controller.lock`. Interrupting the controller
  releases it; child commands do not inherit the descriptor.
- **Command lock.** One stack command at a time, on
  `<nodeRoot>/logs/full-stack-command.lock`. A child blocked without output
  survives an interrupted controller and keeps this lock. A rerun's first
  command then does not start, and its journal record is left as it was.
- **Journal write-ahead.** Before a step executes, `stack-journal.json` records
  it as `running`. What the step submitted is saved before its confirmation is
  checked, and a rerun reconciles that record against Cardano first. An
  ambiguous submission stops the run instead of building a new one.
- **Nonce write-ahead.** The node records the signed hub-oracle nonce, with its
  bytes, in `deployment-run-state.json` before first submitting it, and never
  builds a second nonce once that run state exists. A rerun completes a nonce
  that landed or resubmits exactly those bytes while their inputs are unspent.
- **Saved intents.** Reference, deposit and withdrawal intents and the signed
  transfer bytes are saved before their first submission and reused on every
  retry.
- **Identity binding.** The journal carries the configuration's identity
  digest; the node directory records which run directory and identity own it;
  Postgres and the node directory carry a storage marker. Any mismatch stops
  the run without resetting anything.

So a rerun of the same command is the recovery action once the cause of a stop
is understood. The only other state-changing action is a deliberate new
deployment identity (see [below](#a-signed-nonce-that-can-never-land)).

## Preserve and diagnose

Before rerunning, record the stop message, the last `<step>: running` line, and
the paths of:

- `stack-journal.json` and the step's most recent `attempts/<id>-<uuid>.log`
  and `.json` (the stop message names the raw log for a failed command);
- `deployment-run-state.json`, the deployment manifest in `deploymentInfo/`,
  and `deploymentInfo/full-stack-intent.json`; and
- any `cycle-N-*.json` receipt the step wrote.

Diagnose read-only: the raw attempt logs, the stack's observation attempts
(`*-observation-*`, `provider-confirmation-*`, `deployment-status-*`), the
service logs through the operator Compose wrapper
([live-acceptance.md](live-acceptance.md#operate-the-running-stack)), and the
endpoints the stack polls. Never delete, edit or move the journal, run state,
receipts, lock files or volumes. Never run `init`, a nonce, reference,
operator, deposit, transfer or withdrawal command by hand to get past a stop.

## Route the stop message

| Stop message (prefix)                                                                                                                                                     | Safe next action                                                                                                                                           |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A configuration refusal (for example `Require Preprod with preprod-testing profile, local Kupmios, ...` or the Postgres port refusal)                                     | Fix the configuration or `.env`, confirm with `--check`, then rerun.                                                                                       |
| `Another stack controller holds <lock>`                                                                                                                                   | Another run is live on this node directory. Let it finish or stop that controller; never remove the lock file.                                             |
| `<id> did not start: another stack command holds <lock>`                                                                                                                  | A child of an interrupted run still holds the command lock. Nothing changed. Wait until it exits, then rerun.                                              |
| `This node directory already belongs to another stack configuration`                                                                                                      | Use the configuration that created this deployment. A new identity needs a separate linked worktree.                                                       |
| `Saved stack identity differs from this configuration; preserve the run directory`                                                                                        | An identity field changed. Restore its original value; do not delete the journal.                                                                          |
| `Malformed stack checkpoint`                                                                                                                                              | The journal is corrupt. Stop and preserve it; do not edit it by hand. Escalate.                                                                            |
| `<step>: previous submission remains ambiguous; no transaction was repeated`                                                                                              | A submission may still land. Wait for Cardano to settle it (confirmation, or its validity interval passing), then rerun; reconciliation decides.           |
| `<step>: completion has not been confirmed`                                                                                                                               | The command returned but Cardano does not yet show the result. Its submission is saved; wait, then rerun to verify it.                                     |
| `<label> did not complete within <ms>ms; saved state was preserved`                                                                                                       | Diagnose why the awaited condition (provider sync, readiness, absorption, commitment, finality, payout) has not matured. `timeoutMs` may be raised; rerun. |
| `<id> failed; inspect <raw log>`                                                                                                                                          | Read the raw log. A failure before any submission is fixed locally and rerun. Nonce failures are routed [below](#a-signed-nonce-that-can-never-land).      |
| `Finalized initialization is not at the Cardano tip and is pending re-inclusion; wait, then rerun`                                                                        | A rollback removed a finalized `init` from the tip. Preserve its data, wait, then rerun.                                                                   |
| `Finalized deployment manifest disagrees with Cardano`, `Recorded initialization transaction disagrees with Cardano`, `Existing deployment identity/state does not match` | Deployment identity drift. Stop, preserve everything, and apply `docs/agents/state-reset.md`; escalate before any new deployment.                          |
| `Postgres deployment marker is missing or changed`, `Durable node storage identity changed`, `Storage identity differs from this deployment`                              | Local storage no longer matches the deployment. Do not attach value flows. Preserve it and apply `docs/agents/state-reset.md`.                             |
| `Existing deployment cannot attach to missing or mismatched local event history`                                                                                          | Storage was wiped or swapped under a deployment. Stop; a complete fresh deployment in a separate linked worktree is the only way forward.                  |
| `Fresh deployment cannot reuse populated node db directory`                                                                                                               | Run a fresh deployment from a separate linked worktree; do not empty this one.                                                                             |
| `127.0.0.1:<port> does not reach this stack's Postgres`                                                                                                                   | Another Postgres answers on that port. Fix `MIDGARD_POSTGRES_HOST_PORT` or stop the other listener.                                                        |
| `Wallet <role> is below its configured funding budget`, `... needs a plain ADA output`                                                                                    | Fund that wallet or create the plain output; never substitute another role's wallet.                                                                       |
| `Automatic settlement is unhealthy`                                                                                                                                       | Inspect node `/readyz` settlement state and node logs. Fix the settlement worker; never settle by hand.                                                    |
| `Transfer rejected: <reason>`                                                                                                                                             | The node refused the saved transfer. Diagnose the reason code against the node's validation; do not build a replacement transfer by hand.                  |
| `Public DA retrieval failed`, `No public DA source`, `Saved public DA bytes changed`                                                                                      | Inspect the committee, public retained DA and producer logs. Never merge or finalize around missing or changed DA.                                         |
| A receipt or intent identity error (`Deposit receipt lacks its exact event identity`, `Saved transfer differs from its exact transaction identity or fee`)                | A saved record is inconsistent. Stop, preserve it and escalate; do not rewrite it.                                                                         |

A local Kupmios provider failure is repaired locally (Cardano node, Ogmios,
Kupo); never switch to a remote provider or add failover.

## A signed nonce that can never land

The `nonce` step's raw log names one of two node errors when the recorded
bytes cannot land:

- `SignedNonceConflictError`: another transaction spent the nonce's inputs; it
  lists them.
- `SignedNonceRejectedError`: the ledger rejects the bytes while their inputs
  are unspent, with the ledger's reason (for example a fee made too small by a
  protocol-parameter change).

Rerunning stops the same way. The deployment identity must be replaced, which
is an owner decision. Either start a fresh deployment from a separate linked
worktree, or, deliberately replacing this deployment's identity under
`docs/agents/state-reset.md`, run from the node directory:

```bash
node dist/index.js prepare-hub-oracle-one-shot-nonce \
  --run-state "<runDirectory>/deployment-run-state.json" \
  --fresh-redeploy \
  --fresh-redeploy-reason "<reason>"
```

then rerun `e2e-stack`, which adopts the confirmed replacement nonce.

## Recovery completion

Recovery is complete only when a rerun of the same command reaches the
acceptance contract in `SKILL.md`. Report every stop, its message, the
diagnosis, the evidence paths and the rerun that followed. A working endpoint
by itself does not close an ambiguous submission, a DA failure or a finality
failure.
