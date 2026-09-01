# Independent Challenger Runbook

Use this runbook for a watcher-driven or manual challenge against an existing
deployment. It does not create or reset protocol state.

## 1. Admit one deployment identity

Work from a clean build of the repository commit named by the deployment
manifest. Keep the manifest, `onchain/aiken/plutus.json`, deployment-info file,
catalogue root, reference-script outrefs, and protocol-parameter snapshot as
one immutable evidence set.

Run the fail-closed contract inspection before starting a watcher:

```bash
node demo/midgard-fault-proofs/dist/bin.js inspect-contracts \
  --blueprint onchain/aiken/plutus.json \
  --deployment-info "$MIDGARD_DEPLOYMENT_INFO" \
  --network Preprod
```

Completion: inspection succeeds and its catalogue root, script hashes, and
network agree with the signed deployment manifest. Stop on any disagreement.

## 2. Start public evidence authorities

Start the DA committee and watcher from their signed manifests. Follow
`demo/da-committee-node/docs/da-committee-node-architecture.md` and
`demo/midgard-watcher/README.md` for configuration rather than copying
environment variables here.

The challenger path requires:

- local Kupo/Ogmios L1 authority with no provider failover;
- public libp2p retained-payload and proof-artifact protocols;
- the exact deployment fingerprint on every request;
- a durable watcher decision/workflow journal;
- an action-specific funding reservation and admitted actuation permit;
- every enabled category reported ready by the installed application registry.

Completion: DA retention has completed a successful check, the watcher is
ready, and its installed category list exactly equals the enabled launch
scope. A missing runner, reference script, manifest binding, or evidence source
is a readiness failure.

## 3. Detect and prosecute

The production watcher is the normal entry point. It obtains the challenged
HeaderV1 from authenticated L1 state, retrieves retained evidence through the
public DA protocols, records the classifier decision, reserves funding, and
runs or resumes the manifest-bound category workflow.

For diagnosis, the package readiness command may be inspected without
submitting transactions:

```bash
node demo/midgard-fault-proofs/dist/bin.js workflow-readiness
```

Do not invoke `run-workflow` as an authority bypass. Production workflow
execution requires the watcher-created decision and funding permits plus its
application-installed runtime registry.

Completion: the workflow journal contains confirmed init and step
transactions, a permanent category-plus-header proof token, confirmed
state-queue removal, exact slash/reward observations, and final
reconciliation. Submitted or unknown transactions are not completion.

## 4. Verify independently

Re-read the final state from L1 and public DA, not from the workflow's own
success flag. Verify:

1. the target header and every removed descendant are absent;
2. the permanent proof token remains at the fault-proof address;
3. the operator bond was consumed once and the authenticated prover was paid
   exactly once;
4. retained evidence and immutable journal records hash to the recorded
   deployment identity;
5. the watcher has no unfinished mutation, funding, or reconciliation job.

Completion: all five observations agree at a final chain point.

## Recovery branch

If any submission may have reached L1, switch to
[`manual-recovery-runbook.md`](manual-recovery-runbook.md). Preserve the
journal and reconcile before retrying.
