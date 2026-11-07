/**
 * The node's landed state queue (P1, N2) in the fork simulator: honest
 * state-queue traffic, orphans with valid datums, third-party payments to
 * the queue address, and a check that compares P1 at the tip after every
 * chain-sync event with an independent model replayed from the canonical
 * blocks alone.
 *
 * - The model (`state-queue-sim.model.ts`) decodes the queue outputs itself
 *   and walks them: it shares nothing with P1's decode or walk. The
 *   fixtures are `state-queue-sim.fixtures.ts`.
 * - P1 keeps no tables, so the model being a replay of the canonical chain
 *   is the "equals a fresh replay" check; the runner checks the facts P1
 *   reads against a fresh replay itself.
 */
import {
  type BlockSummary,
  decodeBlock,
  type FollowerProjection,
} from "@al-ft/midgard-l1-follower";
import {
  type ScenarioTraffic,
  SIM_ORIGIN,
  type SimChain,
  type SimOutput,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";

import {
  landedElements,
  type LandedStateQueue,
  readLandedStateQueueFrom,
  stateQueueProjection,
} from "../../src/l1-state-queue/index.js";
import {
  GENESIS_HASH,
  hex32,
  NODE_PREFIX,
  nodeDatum,
  OTHER_POLICY,
  QUEUE_ADDRESS,
  queueOutput,
  ROOT_ASSET,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./state-queue-sim.fixtures.js";
import {
  isQueueOutput,
  type ModelElement,
  modelElement,
  type ModelQueue,
  modelQueue,
  walkModel,
} from "./state-queue-sim.model.js";

/** What the corpus exercised, so a suite can prove each case happened. */
export type StateQueueSimStats = {
  checks: number;
  /** Checks at which the store had pruned rows. */
  prunedChecks: number;
  appends: number;
  attests: number;
  merges: number;
  tailRemovals: number;
  orphans: number;
  thirdPartyPayments: number;
  /** Checks where an orphan with a valid datum made P1 unhealthy (orphan_node). */
  orphanUnhealthy: number;
  /** Rollbacks after which an unhealthy queue is healthy again. */
  healedByRollback: number;
  /** Rollbacks that removed the tail, after which P1 is healthy at the earlier tail. */
  tailRemovedHealthy: number;
  /** Healthy checks with third-party outputs at the address, none in the queue. */
  thirdPartyIgnored: number;
  /** Healthy checks with at least one policy node walked into the queue. */
  policyNodesIncluded: number;
};

export const zeroStateQueueSimStats = (): StateQueueSimStats => ({
  checks: 0,
  prunedChecks: 0,
  appends: 0,
  attests: 0,
  merges: 0,
  tailRemovals: 0,
  orphans: 0,
  thirdPartyPayments: 0,
  orphanUnhealthy: 0,
  healedByRollback: 0,
  tailRemovedHealthy: 0,
  thirdPartyIgnored: 0,
  policyNodesIncluded: 0,
});

const liveQueue = (chain: SimChain) =>
  walkModel(
    chain
      .live()
      .filter((utxo) => isQueueOutput(utxo.output))
      .map((utxo) => modelElement(utxo.outRef, utxo.output)),
  );

const relinked = (element: ModelElement, link: string | null): SimOutput =>
  element.header === null
    ? queueOutput(element.assetName, rootDatum(element.rootHash, link))
    : queueOutput(
        element.assetName,
        nodeDatum(element.header, element.status ?? "Unattested", link),
      );

/**
 * At most one queue transaction per block (the simulator's view is the
 * ledger before the block): initialize the root, append, attest, merge the
 * attested head into the root, remove the tail, or (at `orphanChance`) add
 * a node with a valid datum that nothing links to. Third parties pay to
 * the queue address, with nothing or another policy's token on a datum that
 * reads as a root.
 */
const queueTraffic =
  (
    stats: StateQueueSimStats,
    options: Readonly<{ orphanChance: number }>,
  ): ScenarioTraffic =>
  ({ chain, rng, claim }): SimTx[] => {
    const txs: SimTx[] = [];
    if (rng.chance(0.3)) {
      stats.thirdPartyPayments += 1;
      txs.push({
        inputs: [chain.outsideInput()],
        outputs: [
          rng.chance(0.5)
            ? { address: QUEUE_ADDRESS, lovelace: 2_000_000n }
            : {
                address: QUEUE_ADDRESS,
                lovelace: 2_000_000n,
                assets: new Map([[OTHER_POLICY, new Map([[ROOT_ASSET, 1n]])]]),
                datum: rootDatum(GENESIS_HASH, null),
              },
        ],
        nonce: chain.nonce(),
      });
    }
    const { root, nodes } = liveQueue(chain);
    const nonce = chain.nonce();
    if (root === null)
      return [
        ...txs,
        {
          inputs: [chain.outsideInput()],
          outputs: [queueOutput(ROOT_ASSET, rootDatum(GENESIS_HASH, null))],
          nonce,
        },
      ];
    if (rng.chance(options.orphanChance)) {
      const header = simHeader(nonce, GENESIS_HASH);
      stats.orphans += 1;
      return [
        ...txs,
        {
          inputs: [chain.outsideInput()],
          outputs: [
            queueOutput(
              NODE_PREFIX + SDK.stateQueueHeaderHash(header),
              nodeDatum(header, "Unattested", null),
            ),
          ],
          nonce,
        },
      ];
    }
    const spend = (
      elements: readonly ModelElement[],
      tx: SimTx,
      count: () => void,
    ): SimTx[] => {
      if (!elements.every((element) => claim(element.outRef))) return txs;
      count();
      return [...txs, tx];
    };
    const tail = nodes.at(-1) ?? root;
    const roll = rng.next();
    if (roll < 0.5 || nodes.length === 0) {
      const header = simHeader(
        nonce,
        tail.header === null ? tail.rootHash : tail.key!,
      );
      const hash = SDK.stateQueueHeaderHash(header);
      return spend(
        [tail],
        {
          inputs: [tail.outRef],
          outputs: [
            relinked(tail, hash),
            queueOutput(
              NODE_PREFIX + hash,
              nodeDatum(header, "Unattested", null),
            ),
          ],
          nonce,
        },
        () => (stats.appends += 1),
      );
    }
    const unattested = nodes.filter((node) => node.status === "Unattested");
    if (roll < 0.7 && unattested.length > 0) {
      const node = rng.pick(unattested);
      return spend(
        [node],
        {
          inputs: [node.outRef],
          outputs: [
            queueOutput(
              node.assetName,
              nodeDatum(
                node.header!,
                { Attested: { commitment_hash: hex32(nonce) } },
                node.link,
              ),
            ),
          ],
          nonce,
        },
        () => (stats.attests += 1),
      );
    }
    const head = nodes[0]!;
    if (roll < 0.85 && head.status !== "Unattested")
      return spend(
        [root, head],
        {
          inputs: [root.outRef, head.outRef],
          outputs: [queueOutput(ROOT_ASSET, rootDatum(head.key!, head.link))],
          nonce,
        },
        () => (stats.merges += 1),
      );
    const before = nodes.at(-2) ?? root;
    return spend(
      [before, tail],
      {
        inputs: [before.outRef, tail.outRef],
        outputs: [relinked(before, null)],
        nonce,
      },
      () => (stats.tailRemovals += 1),
    );
  };

const firstDifference = (projected: unknown, model: unknown): string | null => {
  const left = JSON.stringify(projected);
  const right = JSON.stringify(model);
  return left === right
    ? null
    : `P1 ${left.slice(0, 600)} vs model ${right.slice(0, 600)}`;
};

const summary = (queue: LandedStateQueue) => ({
  healthy: queue.healthy,
  reason: queue.reason,
  outRefs: landedElements(queue).map((element) => element.outRef),
  policyOutputs: queue.policyOutputCount,
});

/**
 * P1 with traffic and the model check. One per scenario run: the stats
 * accumulate across runs.
 */
export const stateQueueSimProjection = (
  stats: StateQueueSimStats,
  options: Readonly<{ orphanChance: number }> = { orphanChance: 0.06 },
): FollowerProjection => {
  // The check runs after each event; it keeps the canonical blocks itself.
  const canonical: BlockSummary[] = [];
  let previous: ModelQueue | null = null;
  const check: FollowerProjection["check"] = async ({ store, step }) => {
    stats.checks += 1;
    const { event } = step;
    if (event.kind === "roll_forward") canonical.push(decodeBlock(event.block));
    else {
      const target =
        event.point.kind === "point" ? event.point.hash.toLowerCase() : null;
      while (
        canonical.length > 0 &&
        canonical[canonical.length - 1]!.point.hash.toString("hex") !== target
      )
        canonical.pop();
    }
    const read = await readLandedStateQueueFrom(store, SIM_QUEUE_CONFIG);
    const model = modelQueue(canonical);
    if (read.kind === "not_initialized" && canonical.length === 0) return null;
    if (read.kind !== "ok") return `P1 read refused: ${read.kind}`;
    const tip = canonical.at(-1)?.point.slot ?? SIM_ORIGIN.point.slot;
    if (read.queue.view.point.slot !== tip)
      return `P1 read at slot ${read.queue.view.point.slot.toString()}, tip ${tip.toString()}`;
    const difference = firstDifference(summary(read.queue), {
      healthy: model.healthy,
      reason: model.healthy || model.policyOutputs === 0 ? null : model.reason,
      outRefs: model.outRefs,
      policyOutputs: model.policyOutputs,
    });
    if (difference !== null) return difference;
    // Case counters (P1 agreed with the model, so these describe P1).
    if (
      ((await store.cursor())?.prunedThroughSlot ?? SIM_ORIGIN.point.slot) >
      SIM_ORIGIN.point.slot
    )
      stats.prunedChecks += 1;
    if (model.reason === "orphan_node") stats.orphanUnhealthy += 1;
    if (model.healthy && model.thirdParty > 0) stats.thirdPartyIgnored += 1;
    if (model.healthy && model.outRefs.length > 1)
      stats.policyNodesIncluded += 1;
    if (event.kind === "roll_backward" && previous !== null && model.healthy) {
      if (!previous.healthy && previous.policyOutputs > 0)
        stats.healedByRollback += 1;
      const before = previous.outRefs.at(-1);
      if (
        previous.healthy &&
        before !== undefined &&
        !model.outRefs.includes(before) &&
        model.outRefs.length > 0
      )
        stats.tailRemovedHealthy += 1;
    }
    previous = model;
    return null;
  };
  return {
    ...stateQueueProjection(SIM_QUEUE_CONFIG),
    traffic: queueTraffic(stats, options),
    check,
    // Queue outputs are script-locked on L1: only the queue traffic spends them.
    protects: (output) => isQueueOutput(output),
  };
};
