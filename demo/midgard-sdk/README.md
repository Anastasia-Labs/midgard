# Midgard Off-Chain SDK

Status: Internal/workspace package under active development. The package name is
not evidence of a public registry release; the instructions below use a local
tarball intentionally.

Midgard TypeScript library for building operator and watcher transactions. Each
transaction family has a reference page in the
[documentation site](../../docs-site/content/docs/sdk/midgard-sdk).

## How to use

1. Use the Node.js and pnpm versions declared in `demo/package.json`. If your
   Node installation provides Corepack, enable it to select the pinned pnpm:

```sh
corepack enable
```

2. Install the dependencies:

```sh
cd demo/midgard-sdk
pnpm install
```

3. Bundle the package into a tarball:

```sh
pnpm repack
```

4. Inside this monorepo, use `"@al-ft/midgard-sdk": "workspace:*"`. For an
   external consumer, the SDK tarball also requires the matching
   `@al-ft/midgard-core` package and its declared peers (`@lucid-evolution/lucid`
   and `effect`). Package the core workspace too, then reference both local
   tarballs; the SDK tarball alone is not a standalone installation:

```json
{
  "dependencies": {
    "@al-ft/midgard-sdk": "file:./path/to/al-ft-midgard-sdk-0.1.0.tgz",
    "@al-ft/midgard-core": "file:./path/to/al-ft-midgard-core-0.1.0.tgz"
  }
}
```

## Structure of the Package

The modules themselves are organized vertically. However, all names are globally
unique. For `midgard-node`, the SDK is used as such:

```ts
import * as SDK from "@al-ft/midgard-sdk";
```

Midgard SDK itself is built with `effect`, but it doesn't impose this on
dependent packages. Having said that, it does offer function variants for
projects that use `effect` as well.

The convention is the same as Lucid Evolution, i.e. functions that return
`Effect` blueprints are postfixed with `Program` (e.g.
`incompleteInitializationTxProgram`).

Note that this SDK is under ongoing development and these conventions may not
hold for every single function yet.

## Availability challenge transactions

The availability lifecycle uses the completed `buildOpenDaAvailabilityChallengeTxProgram`,
`buildPublishDaAvailabilityChunkTxProgram`, `buildSettleDaAvailabilityTrancheTxProgram`,
`buildCloseDaAvailabilityChallengeTxProgram`, and `buildTimeoutDaAvailabilityChallengeTxProgram`
builders. A timeout with descendants continues through
`buildPruneDaUnavailableBlockDescendantTxProgram` and `buildRemoveDaUnavailableHeadTxProgram`.
Each builder takes a `DaAvailabilityDeployment`, explicit protocol inputs, a bounded
validity interval, a fee within the authenticated ceiling, and separate wallet
collateral. It evaluates locally and returns the unsigned transaction with its
spent, reference, collateral, and expected output metadata.

Opening requires one plain ADA input containing exactly the challenger bond plus
opening fee, as required by the validator. Use the shared funding preparation
transaction when the actor wallet needs that denomination. Publication and
settlement fees come from the protected challenger bond; removal continuations
accept explicit fee funding and return any spendable change. Opening also checks
that the release economics cover all publication fees and conservative carrier
and thread minimum ADA at the live ledger parameters.

`daAvailabilityChallengeSnapshotFromUtxos` authenticates a caller's coherent L1
snapshot and resolves each tranche's exact current carrier. Services with native
chain provenance should supply their admitted point's UTxOs. The convenience
`fetchDaAvailabilityChallengeSnapshot` reads provider address state; its caller
must establish a stable canonical boundary. Use `runDaAvailabilityOperation` and
the durable operation journal for signing, reservation, submission, and recovery.
See the [operational lifecycle plan](../../docs/fault-proofs/availability-challenge-operations.md)
for the service authority and acceptance requirements.

Retention uses `deriveDaAvailabilityRetentionEvidence` only after the native chain
source admits a finalized queue transition and its exact consumed output. A
consumed `Published` datum proves closed availability; final unavailable removal
must bind the challenged header and challenge through the correction lock. The
evidence commits to the authenticated header's end time and exact transition.
`parseDaAvailabilityRetentionEvidence` revalidates durable evidence before
pruning. Deployed roles alone report `deployed_unobserved`; absent challenge
UTxOs, generic terminal queue status, and missing active-set entries never prove
inactivity. Revoke terminal evidence with its transition on rollback.
