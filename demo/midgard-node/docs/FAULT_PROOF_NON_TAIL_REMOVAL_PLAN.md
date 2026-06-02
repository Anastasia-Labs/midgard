# Non-Tail Fraudulent State Commitment Removal Plan

## Goal

Update `midgard-fault-proofs remove-fraudulent-block` so a completed fault proof
can remove a challenged state-queue commitment even when later state
commitments already exist.

The command must preserve production L2 guarantees:

- prove removal against the permanent fraud-proof token for the challenged
  header hash;
- remove only live, authenticated state-queue nodes;
- preserve linked-list integrity after every transaction;
- slash each removed block's operator exactly according to current directory
  state;
- avoid racing live commitment or merge workers;
- make every submitted transaction and state transition auditable.

## Implementation Readiness

This plan is ready for implementation in two explicit scopes:

- Standalone CLI removal: implement strict topology loading, successor pruning,
  per-removed-block slashing, confirmation/refetch between transactions, and an
  auditable transaction log.
- Live-node coordination: add a node mutation lease before using multi-step
  removal while block-commitment or merge workers are running against the same
  persistent deployment.

The standalone CLI scope can be implemented without Aiken changes because the
existing state-queue redeemer already supports both successor pruning and final
tail removal. The live-node coordination scope remains required before treating
multi-step removal as an unattended production operation.

## Pre-Implementation Behavior

Before this change, the CLI submitted a single `RemoveLastFraudulentBlock`
transaction through `incompleteRemoveLastFraudulentBlockHeaderTxProgram`.

That was narrower than "latest commitment" support. The CLI passed the
state-queue root as the removal anchor, so it only worked when the challenged
block is both:

- the state-queue tail, with `next = Empty`; and
- the direct successor of the confirmed-state root.

If the challenged block was latest but had a predecessor block, the CLI did not
select the correct predecessor anchor. If the challenged block had any successor,
the on-chain `RemoveLastFraudulentBlock` branch rejected it because the removed
node's link is not empty.

## Review Findings Incorporated

1. `RemoveFraudulentBlocksLink` does not remove the global tail. It removes the
   immediate successor of the proved-bad node and reproduces the proved-bad node
   with its link spliced forward.

2. Every successor-removal transaction still references the fraud-proof token
   for the original challenged header hash. The successor itself does not need
   its own fraud-proof token.

3. The `fraudulent_operator` redeemer field is operator-specific for the node
   being removed. For successor pruning, it must be the successor block's
   operator, not necessarily the original challenged block's operator.

4. Slashing must be resolved independently for each removed block. The same
   operator may be active, retired, or already absent by the time a later removal
   transaction is built.

5. Multi-transaction pruning must refetch and revalidate on-chain topology after
   every confirmation. Cached queue shape is not authoritative.

6. Live block-commitment and merge workers can race this operation. A production
   implementation must provide an explicit mutation-exclusion mechanism before
   enabling multi-step removal against persistent deployments.

## On-Chain Model To Use

No Aiken change is expected for the basic capability. The existing state-queue
mint redeemer already has both branches:

- `RemoveFraudulentBlocksLink`: spend the challenged node and its immediate
  successor, burn the successor node NFT, and reproduce the challenged node with
  `next = successor.next`.
- `RemoveLastFraudulentBlock`: spend the predecessor anchor and challenged node,
  burn the challenged node NFT, and reproduce the predecessor with `next = Empty`.

For every removal transaction:

- `fraudulent_blocks_header_hash` must remain the originally proved header hash;
- `fraud_proof_ref_input_index` must reference the fraud-proof token whose asset
  name encodes that header hash;
- `fraudulent_node_input_index` is the original challenged node for
  `RemoveFraudulentBlocksLink`;
- `fraudulent_node_input_index` is the challenged node itself for
  `RemoveLastFraudulentBlock`;
- `fraudulent_operator` must equal the operator of the node burned in that
  transaction.

## Implementation Phases

### 1. Add Strict State-Queue Topology Loading

Add a topology loader in the SDK or fault-proof CLI package that fetches all
authentic state-queue UTxOs and returns a fully validated view:

```ts
type StateQueueTopology = {
  readonly root: StateQueueUTxO;
  readonly ordered: readonly StateQueueUTxO[];
  readonly nodeByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly predecessorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly successorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
};
```

Validation requirements:

- exactly one confirmed-state root exists;
- every non-root node key matches its block-token asset-name suffix;
- every non-root node key matches the hash computed from the header datum;
- the chain from root reaches every authentic non-root state-queue node;
- no duplicate header hashes exist;
- no missing links or cycles exist;
- the requested challenged header hash is present and unmerged.

The existing `sortStateQueueUTxOs` traversal can be reused, but it should be
wrapped with stricter checks that compare the traversed chain against the full
authentic UTxO set.

### 2. Add Operator Directory Planning

Extract the current active-operator removal planning in
`remove-fraudulent-block.ts` into reusable helpers and add equivalent retired
and already-slashed planning.

For each operator that appears on a removed block, resolve one of:

- `SlashActiveOperator`: find the active-operators node and predecessor, build
  active-operators `SlashOperator`, preserve scheduler consistency, and burn the
  active operator node NFT.
- `SlashRetiredOperator`: find the retired-operators node and predecessor, build
  retired-operators `SlashOperator`, and burn the retired operator node NFT.
- `OperatorAlreadySlashed`: select reference inputs from both active and retired
  ordered lists that prove the operator is not a member.

Do not assume that the original challenged block operator is the only operator
to handle. Successor blocks may have different operators, and repeated operators
must naturally transition from active or retired slashing to
`OperatorAlreadySlashed` after the first successful slash.

Retired operator support likely needs additional SDK ergonomics:

- typed retired-operators `SlashOperator` redeemer construction using the shared
  `SlashingArguments` schema;
- retired list node lookup by operator key;
- retired list predecessor discovery;
- retired list non-membership witness discovery.

### 3. Add SDK Builder For Successor Pruning

Add a production-shaped transaction builder for `RemoveFraudulentBlocksLink`.

Proposed builder:

```ts
incompleteRemoveFraudulentBlocksLinkTxProgram(lucid, config, {
  challengedBlockUTxO,
  removedSuccessorUTxO,
  provedFraudulentHeaderHash,
  fraudProofRefInput,
  slashing,
  additionalInputs,
  additionalRefInputs,
  validFrom,
  validTo,
  stateQueueSpendingScript,
  stateQueueMintingScript,
  referenceScripts,
});
```

The builder should:

- assert off-chain that `challengedBlockUTxO.datum.next` points to
  `removedSuccessorUTxO`;
- build the continued challenged-node datum with `next =
removedSuccessorUTxO.datum.next`;
- spend the challenged node and removed successor node;
- burn only the removed successor state-queue node NFT under the state-queue
  policy;
- reference the fraud-proof token for `provedFraudulentHeaderHash`;
- encode `RemoveFraudulentBlocksLink` with the continued challenged-node output
  index and removed successor input index;
- attach or reference the same scripts as the existing tail-removal builder;
- always complete with `localUPLCEval: true` at call sites.

Keep or rename the existing tail builder based on current package conventions,
but its behavior must support any valid predecessor anchor, not only the root.

### 4. Replace Single-Shot CLI With A Removal Orchestrator

Change `submitRemoveFraudulentBlock` from a single transaction builder into a
bounded orchestrator:

1. Resolve contracts and reference scripts.
2. Resolve and validate the fraud-proof token for the challenged header hash.
3. Load strict state-queue topology.
4. Locate the challenged node.
5. If the challenged node has successors, require confirmation/refetch mode.
6. Repeatedly remove the challenged node's current immediate successor:
   - read the successor header;
   - resolve slashing for the successor operator;
   - build and submit `RemoveFraudulentBlocksLink`;
   - wait for confirmation;
   - refetch topology;
   - assert the removed successor is gone and the challenged node is still live.
7. Once the challenged node has no successor:
   - locate its current predecessor from freshly loaded topology;
   - resolve slashing for the challenged node's operator;
   - build and submit `RemoveLastFraudulentBlock` with that predecessor;
   - wait for confirmation if requested;
   - verify the challenged node is gone.

The command should prune from the challenged node forward:

```text
root -> A -> challenged -> B -> C
root -> A -> challenged -> C       after removing B
root -> A -> challenged            after removing C
root -> A                          after removing challenged
```

It should not try to remove the tail first. That is not the shape of the
existing on-chain `RemoveFraudulentBlocksLink` branch.

### 5. Add Mutation Exclusion For Live Nodes

Multi-step removal must not run concurrently with block commitment or merge
workers.

Preferred production path:

- add a node-admin operation that acquires the existing state-queue mutation
  lease for fault-proof removal;
- make block-commitment and merge workers respect that lease;
- have the CLI require a node URL or lease token before submitting a multi-step
  removal on persistent networks;
- release the lease only after final confirmation or explicit failure handling.

Minimum safe standalone behavior:

- allow single-transaction tail removal as today;
- for multi-step removal, reject `--no-await-confirmation`;
- after every transaction, refetch topology from chain and abort on unexpected
  topology changes;
- include clear output stating that no local node database was rewritten.

Do not silently reset, rewrite, or reconcile local node state from this CLI.
Any local recovery after on-chain removal must be explicit and separately
auditable.

### 6. Update CLI Result Shape

Return a transaction log instead of a single `txHash`.

Example:

```json
{
  "fraudulentHeaderHash": "abc...",
  "proofTokenOutRef": "tx#0",
  "initialPosition": {
    "predecessorHeaderHash": "aaa...",
    "successorHeaderHash": "bbb..."
  },
  "transactions": [
    {
      "kind": "remove-successor",
      "txHash": "tx1",
      "removedHeaderHash": "bbb...",
      "removedOperator": "op1...",
      "slashingApproach": "SlashActiveOperator"
    },
    {
      "kind": "remove-target",
      "txHash": "tx2",
      "removedHeaderHash": "abc...",
      "removedOperator": "op2...",
      "slashingApproach": "OperatorAlreadySlashed"
    }
  ],
  "awaitedConfirmation": true
}
```

For one-transaction removals, still return the same `transactions` array with a
single `remove-target` entry. Avoid adding compatibility-only duplicate fields
unless a current caller in the repo requires them.

### 7. Add Tests

Add focused unit tests for topology and planning:

- root plus one block;
- target latest with predecessor not root;
- target with one successor;
- target with multiple successors;
- duplicate node key rejected;
- asset-name/header-datum hash mismatch rejected;
- unreachable authentic state-queue node rejected;
- missing link rejected;
- active, retired, and already-slashed operator plan selection.

Add SDK/emulator tests:

- `RemoveFraudulentBlocksLink` removes the challenged node's immediate
  successor and preserves the challenged node with the successor's old link;
- `RemoveLastFraudulentBlock` works when the predecessor is not root;
- successor removal uses the successor operator in `fraudulent_operator`;
- repeated operator removal uses `OperatorAlreadySlashed` after the first slash;
- retired operator slashing works for state-queue removal.

Add CLI/integration tests:

- one-block queue preserves current behavior;
- non-root latest target is removed in one transaction with the correct
  predecessor;
- target with one successor submits two transactions;
- target with multiple successors submits one transaction per successor plus the
  final target removal;
- stale topology after a submitted transaction causes a clear abort and does not
  submit the next transaction.

Recommended verification commands:

```bash
pnpm --dir demo/midgard-sdk typecheck
pnpm --dir demo/midgard-sdk test -- state-queue.test.ts
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-fault-proofs test
```

Run broader node emulator coverage after the CLI path is wired to live
deployment metadata.

## Acceptance Criteria

- `remove-fraudulent-block` can remove a challenged block with no successors,
  even when its predecessor is another block rather than the root.
- `remove-fraudulent-block` can remove a challenged block with one or more
  successors by submitting a confirmed sequence of successor-pruning
  transactions followed by final target removal.
- Every transaction references the original fraud-proof token.
- Every removed block burns exactly that block's state-queue node NFT.
- The final state queue is linked from the challenged block's predecessor to the
  first post-target link that existed after all removed successors, or to
  `Empty` when there are no remaining later blocks.
- Active, retired, and already-slashed operator cases are handled explicitly.
- Multi-step removal cannot run in fire-and-forget mode.
- The command output is sufficient to audit which headers, operators, slashing
  approaches, and transaction hashes were involved.
- No local durable node state is reset or silently rewritten.

## Out Of Scope

- Adding new fraud-proof categories.
- Changing the on-chain fraud-proof token minting model.
- Rewriting local Midgard node databases after on-chain removal.
- Supporting historical pre-launch runtime formats or old CLI result shapes
  that are not used by current in-repo callers.
