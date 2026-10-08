/**
 * The watcher's user events over an in-memory L1 follower store, as the
 * runtime wires them (`watcher-runtime.create-watcher-runtime.ts`): one
 * store registering the watcher projection (with every followed script) and
 * the deployment's event projection, its raw reads and proof retention, and
 * `createWatcherFollowerUserEvents` over them. The deployment is the
 * synthetic origin deployment (`user-event-origin-fixture`); blocks are its
 * `buildBlock` frames, fed straight to the store. No node, transport or
 * subprocess: the follower's own suites pin how blocks reach a store.
 */
import {
  decodeBlock,
  type FactStore,
  openSqliteFactStore,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { watcherFollowedScripts } from "../../src/l1-follower/deployment-follower.js";
import { watcherFollowerProjections } from "../../src/l1-follower/follower-runtime.js";
import { readWatcherObservation } from "../../src/l1-follower/observation.js";
import { watcherUnitHistoryPolicies } from "../../src/l1-follower/projection.js";
import { createWatcherProofRetention } from "../../src/l1-follower/proof-retention.js";
import { createFollowerRawReads } from "../../src/l1-follower/raw-reads.js";
import { createWatcherFollowerUserEvents } from "../../src/l1-follower/user-events.js";
import {
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentProtocolScriptAuthority,
} from "../../src/runtime/deployment-identity.js";
import { buildBlock } from "./user-event-origin-fixture.build-block.js";
import {
  blueprintBytes,
  INITIALIZATION,
  makeOriginDeployment,
  type Point,
  pointAt,
  type SyntheticUserEventBlock,
} from "./user-event-origin-fixture.make-config.js";

/** The synthetic origin deployment and what its follower reads with. */
export const followerUserEventsDeployment = (ruleBundleCommitment?: string) => {
  const followed = followerUserEventsDeploymentOf(
    makeOriginDeployment(ruleBundleCommitment),
  );
  return Object.freeze({
    ...followed,
    /** The hub-oracle output the activation transaction created. */
    hubOutRef: activationHubOutRef(followed.scripts.hub),
  });
};

/** What a follower of `deployment` reads with. */
export const followerUserEventsDeploymentOf = (
  deployment: ReturnType<typeof makeOriginDeployment>,
) => {
  const deploymentIdentity = deployment.result;
  const scripts = readWatcherUserEventScriptBinding({
    binding: verifyWatcherUserEventScriptBinding({
      deploymentIdentity,
      blueprintBytes,
    }),
    deploymentIdentity,
  });
  const authority =
    watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
  const followedScripts = watcherFollowedScripts({
    contractScriptHashes:
      watcherDeploymentAppliedScriptHashes(deploymentIdentity),
    blueprint: JSON.parse(blueprintBytes.toString("utf8")) as unknown,
  });
  return Object.freeze({
    deployment,
    deploymentIdentity,
    scripts,
    authority,
    followedScripts,
  });
};

export type FollowerUserEventsDeployment = ReturnType<
  typeof followerUserEventsDeployment
>;

export type FollowedUserEventsDeployment = ReturnType<
  typeof followerUserEventsDeploymentOf
>;

const activationHubOutRef = (
  hub: Readonly<{ policyId: string; assetName: string; addressHex: string }>,
): string => {
  const transaction = CML.Transaction.from_cbor_hex(
    INITIALIZATION.transactionCbor,
  );
  const outputs = transaction.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const quantity =
      output
        .amount()
        .multi_asset()
        .get_assets(CML.ScriptHash.from_hex(hub.policyId))
        ?.get(CML.AssetName.from_hex(hub.assetName)) ?? 0n;
    if (output.address().to_hex() === hub.addressHex && quantity === 1n)
      return `${CML.hash_transaction(transaction.body()).to_hex()}#${index.toString()}`;
  }
  throw new Error("the activation transaction created no hub-oracle output");
};

/** A chain point a synthetic chain starts from (the store's origin). */
export const FOLLOWER_USER_EVENTS_ANCHOR: Point = pointAt(
  "ab".repeat(32),
  100n,
  500n,
);

/** Builds a chain of `buildBlock` frames, each on the last. */
export const syntheticChain = (anchor: Point = FOLLOWER_USER_EVENTS_ANCHOR) => {
  const blocks: SyntheticUserEventBlock[] = [];
  const tip = (): Point => blocks.at(-1)?.point ?? anchor;
  return {
    anchor,
    blocks,
    tip,
    next: (transactions: readonly string[] = []): SyntheticUserEventBlock => {
      const block = buildBlock(transactions, tip());
      blocks.push(block);
      return block;
    },
    empties: (count: number): void => {
      for (let index = 0; index < count; index += 1)
        blocks.push(buildBlock([], tip()));
    },
  };
};

const followerPoint = (point: Point) => ({
  slot: Number(point.slot),
  hash: Buffer.from(point.blockHash, "hex"),
});

/**
 * An in-memory follower store for `deployment` from `origin`, with the
 * watcher's user events over it. `k` defaults to the decision driver's
 * release depth plus the recovery slack the SQ observation fixture uses.
 */
export const openFollowerUserEvents = async (
  input: Readonly<{
    deployment: FollowedUserEventsDeployment;
    origin?: Point;
    k?: number;
  }>,
) => {
  const { deployment } = input;
  const origin = input.origin ?? FOLLOWER_USER_EVENTS_ANCHOR;
  const hashes = deployment.authority.protocolScriptHashes;
  const projected = {
    network: deployment.authority.network,
    stateQueueSpend: hashes.stateQueueSpend,
    stateQueueMint: hashes.stateQueueMint,
    correctionLockSpend: hashes.correctionLockSpend,
    hubOracleMint: hashes.hubOracleMint,
    fraudProofSpend: hashes.fraudProofSpend,
    fraudProofMint: hashes.fraudProofMint,
    availabilityChallengeSpend: hashes.availabilityChallengeSpend,
    availabilityChallengeMint: hashes.availabilityChallengeMint,
    daBondPoolSpend: hashes.daBondPoolSpend,
    daAttestationMint: hashes.daAttestationMint,
    followedScripts: deployment.followedScripts,
  };
  const store: FactStore = openSqliteFactStore({
    ...projectionStoreOptions(
      watcherFollowerProjections(projected, deployment.scripts.eventProjection),
      {
        securityParameter: input.k ?? RELEASE_FINALITY_DEPTH + 2,
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "sqlite",
    ),
    path: ":memory:",
  });
  const initialize = async () => {
    const initialized = await store.initialize({
      point: followerPoint(origin),
      height: Number(origin.blockNo),
    });
    if (initialized.kind !== "initialized")
      throw new Error(`follower user-event store origin: ${initialized.kind}`);
  };
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`follower user-event store: ${started.kind}`);
  await initialize();
  const unitHistoryPolicies = watcherUnitHistoryPolicies(projected);
  const rawReads = createFollowerRawReads(store, {
    stateQueuePolicyId: hashes.stateQueueMint,
    unitHistoryPolicies,
  });
  const proofRetention = createWatcherProofRetention(store, {
    unitHistoryPolicies,
    stateQueuePolicyId: hashes.stateQueueMint,
  });
  const userEvents = createWatcherFollowerUserEvents({
    store,
    rawReads,
    proofRetention,
    identity: {
      deploymentManifestId: deployment.deploymentIdentity.manifestId,
      blueprintHash: deployment.deploymentIdentity.blueprintHash,
      network: deployment.deploymentIdentity.network,
    },
    scripts: {
      depositPolicyId: deployment.scripts.deposit.policyId,
      withdrawalPolicyId: deployment.scripts.withdrawal.policyId,
      forcedOrderPolicyId: deployment.scripts.forcedOrder.policyId,
      forcedOrderAddressHex: deployment.scripts.forcedOrder.addressHex,
    },
  });
  let closed = false;
  return Object.freeze({
    store,
    rawReads,
    proofRetention,
    userEvents,
    apply: async (blocks: readonly SyntheticUserEventBlock[]) => {
      for (const block of blocks) {
        const result = await store.applyBlock(
          decodeBlock(Buffer.from(block.nativeBlock.rawBlockCbor, "hex")),
        );
        if (result.kind !== "applied")
          throw new Error(`follower user-event block apply: ${result.kind}`);
      }
    },
    /** Rewinds the store to `point`, keeping it and everything below. */
    rewindTo: async (point: Point) => {
      const result = await store.rewind(followerPoint(point));
      if (result.kind !== "rewound")
        throw new Error(`follower user-event rewind: ${result.kind}`);
    },
    /** A start that finds the tracked-set record gone resets the store. */
    reset: async () => {
      await store.transaction("write", (tx) =>
        tx.query("DELETE FROM l1_follower_tracked_set"),
      );
      const restarted = await store.start();
      if (restarted.kind !== "ready" || restarted.trackedSet.kind !== "reset")
        throw new Error("follower user-event store did not reset");
    },
    pruneAll: async () => {
      for (;;) {
        const result = await store.prune();
        if (!("done" in result))
          throw new Error(`follower user-event prune: ${result.kind}`);
        if (result.done) return;
      }
    },
    /** The SQ observation at `depth`, as the decision driver reads it. */
    observe: async (depth: number = RELEASE_FINALITY_DEPTH) => {
      const read = await readWatcherObservation(store, {
        authority: deployment.authority,
        sourceId: "follower-user-events",
        depth,
        releaseDepth: RELEASE_FINALITY_DEPTH,
      });
      if (read.kind !== "ok")
        throw new Error(
          `follower user-event observation: ${read.reason}: ${read.detail}`,
        );
      return read.observation;
    },
    close: async () => {
      if (closed) return;
      closed = true;
      userEvents.close();
      await store.close();
    },
  });
};

export type FollowerUserEvents = Awaited<
  ReturnType<typeof openFollowerUserEvents>
>;

export {
  ELSEWHERE_ADDRESS_HEX,
  FIXTURE_LIST_INCLUSION_TIME,
  FORCED_ORDER_INCLUSION_TIME,
  forcedOrderBurnTransaction,
  type ForcedOrderPayload,
  forcedOrderTransaction,
  listOrderRetirementTransaction,
  listOrderTransaction,
  PLACEHOLDER_FORCED_PAYLOAD,
  syntheticTransaction,
  transactionHash,
  type UserEventId,
  userEventId,
  userEventIdOf,
} from "./follower-user-events-fixture.transactions.js";
