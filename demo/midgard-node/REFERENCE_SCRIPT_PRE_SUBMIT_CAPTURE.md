# Reference-script diagnostic pre-submit capture

The reference-script deployment command has an explicit diagnostic mode that
builds and signs the normal publication transactions, durably records their
exact signed CBOR and reference-script payloads, and then stops without calling
a provider submit API.

This mode is for inspecting the exact transactions that the configured
deployment identity would produce. It is not a deployment, dry-run estimate,
or transaction recovery mechanism. Some captured transactions are independently
submit-ready, so keep the capture directory private and never broadcast its
contents as a batch.

## Preconditions

- Build the Aiken `testnet` blueprint and the node with Node 22.
- Use the deployment's existing run-state file. Its persisted network, hub
  one-shot out-ref, and reference-script auth policy must match the resolved
  node configuration exactly.
- Fund the reference-script wallet before capture. Diagnostic mode refuses to
  run the automatic funding-wallet top-up path.
- Create the parent of the capture directory, but leave the capture directory
  itself absent. The command creates it with mode `0700` and refuses to reuse an
  existing or non-canonical path.

For example:

```sh
cd demo/midgard-node
MIDGARD_REAL_BLUEPRINT_PATH=/absolute/path/to/onchain/aiken/plutus.json \
  node dist/index.js deploy-reference-script-node-runtime \
  --run-state /absolute/path/to/deployment-run-state.json \
  --capture-signed-tx-pre-submit /absolute/path/to/new-capture-directory
```

Capture mode cannot be combined with `--plan-only` or `--fresh-redeploy`.
Unlike normal publication, it does not persist deployment run-state changes,
query for newly published reference scripts, or write a deployment manifest.

## Safety boundary

The capture path is separate from the generic sign-and-submit helper:

1. The normal reference-script builder completes each transaction with its
   existing strict local-evaluation behavior.
2. `signTransactionForPreSubmitCapture` signs once and extracts that signed
   object's exact CBOR. Every vkey witness signature is verified over the exact
   transaction body hash. The helper has no provider submit or confirmation
   branch.
3. The generic submit helper rejects diagnostic capture options, preventing a
   captured-not-submitted result from being treated as a successful submit.
4. Multi-batch publication uses an in-memory wallet shadow. Later batches
   identify inputs derived from earlier unsubmitted batches as
   `synthetic_change`; no live provider resolution is attempted for them.
5. Any required wallet replenishment, identity mismatch, malformed script,
   missing lineage, duplicate coverage, or artifact-write failure aborts the
   command without a completion marker.

The CLI and transaction helper both enforce the aborting diagnostic invocation.
This duplication is intentional: callers cannot bypass the safety boundary by
calling the lower-level helper directly.

## Artifacts and completion

Each captured transaction produces files with mode `0600`:

- `.CAPTURE_SESSION.json`: the prepared directory, run-state, blueprint hash,
  network, one-shot, auth policy, and command identity;
- `signed-<tx-hash>.cbor`: the binary exact signed transaction CBOR;
- `signed-<tx-hash>.cbor.json`: transaction, session, target, input-lineage,
  output, payload-hash, and signed-CBOR hash metadata;
- `payload-<tx-hash>-<output-index>.cbor`: the exact ledger-serialized script
  payload for each declared reference-script output.

Every artifact is flushed and atomically installed without overwriting an
existing path before it is reported. Finalization re-reads all signed CBOR and
payload files, recomputes their hashes and transaction body hashes, re-verifies
vkey signatures, re-inspects canonical CBOR and complete Flat decoding, checks
script hashes, reconstructs the synthetic-change dependency graph, requires
contiguous batch ordinals and exact target coverage, rejects orphan or temporary
files, and verifies the same persisted session identity across the bundle.

Only after those checks pass does the command atomically install
`COMPLETE.json`, fsync it, and fsync the directory. A directory without that
marker is incomplete and must not be treated as an accepted capture bundle.

## What capture does not prove

Capture proves what was signed, the declared script/payload identities, and the
internal batch lineage. It does not prove provider acceptance, ledger inclusion,
or that the synthetic multi-batch chain was submitted. Use the normal deployment
and confirmation path for publication. Never rename or add a completion marker
manually.
