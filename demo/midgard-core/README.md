# midgard-core

Shared Midgard protocol primitives.

This package owns the Midgard native transaction v1 codec used by
`midgard-node` and `lucid-midgard`. It also owns the stable DA libp2p transport
envelopes, protocol identifiers, and deployment manifest types used by the node
and DA committee service. Change codec semantics here first and verify them
through the shared conformance tests; pre-launch legacy codec compatibility is
not a package goal.

## Requirements

Node.js 22.12.0 or newer, and TypeScript 5.8 or newer for CommonJS consumers.

## Deployment manifests

`@al-ft/midgard-core/deployment-manifest-identity` owns the complete
`DeploymentManifest` and its contract-entry type.
`verifyFinalizedDeploymentManifest(unknown)` returns that structural type after
checking the document's root keys and profile bindings, contract and reference
records, protocol configuration, and required deployment steps.
`verifyDeploymentManifestIdentity` performs only the root-key and profile
checks and retains its less specific return type. A deployment is identified by
its hub-oracle policy id, which `deploymentManifestHubOraclePolicyId` reads
from the manifest's `hubOracleMint` contract entry.

The finalized verifier returns the original object, including on a cache hit.
It does not clone, freeze, or brand the document. Existing caller-owned snapshots
and checks across asynchronous boundaries remain the caller's responsibility.
Consumers still check their selected deployment, local configuration, signer,
and current chain state where required.

The type describes the existing verifier rather than strengthening its rules.
For example, deployment step statuses remain JSON values because that verifier
checks their string representation; the node parser retains its stricter string
check and its additional transport constraints.

Direct finalization, rejection, ownership, and cache revalidation checks live in
`tests/deployment-manifest-identity.test.ts`.
