/**
 * The watcher's independent per-step checks for the fork-simulator gate
 * (ticket W1): the queue view, the tip block's checkpoints and the ruling-2
 * pinned reads, each against the chain oracle.
 */
import { type FactStore } from "@al-ft/midgard-l1-follower";
import { type ForkStep } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";

import { readWatcherTransactionCheckpoint } from "../../src/l1-follower/checkpoints.read.js";
import { WATCHER_QUEUE_CHECKPOINTS_TABLE } from "../../src/l1-follower/projection.js";
import { createFollowerRawReads } from "../../src/l1-follower/raw-reads.js";
import { rawPointOf } from "../../src/l1-follower/raw-reads.types.js";
import { readWatcherQueueView } from "../../src/l1-follower/view.js";
import {
  type ChainModel,
  createChainFollower,
  label,
  modelOf,
  modelQueue,
  type OracleEntry,
} from "./l1-follower-chain-oracle.js";
import { SIM_WATCHER_DEPLOYMENT } from "./l1-follower-state-queue-traffic.js";

const D = SIM_WATCHER_DEPLOYMENT;
const NODE_UNIT = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
const DAAT_UNIT = `${D.daAttestationMint}${SDK.DA_ATTESTATION_ASSET_NAME_PREFIX}`;

/** What the independent checks saw over runs (so a pass is not vacuous). */
export type Coverage = {
  steps: number;
  queuedHeaders: number;
  locks: number;
  checkpointRows: number;
  transitionsMatched: number;
  pinnedHistoryReads: number;
  /** Pinned reads of txs at or below the pruned window. */
  pinnedBelowWindow: number;
  attestationReads: number;
  mergedHeaderReads: number;
  mergedHeadersPruned: number;
};

export const coverage = (): Coverage => ({
  steps: 0,
  queuedHeaders: 0,
  locks: 0,
  checkpointRows: 0,
  transitionsMatched: 0,
  pinnedHistoryReads: 0,
  pinnedBelowWindow: 0,
  attestationReads: 0,
  mergedHeaderReads: 0,
  mergedHeadersPruned: 0,
});

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

/** Whether a valid tx touches the state queue (mint, output or spent input with the policy). */
const touchesQueue = (model: ChainModel, entry: OracleEntry): boolean =>
  entry.tx.isValid &&
  (entry.tx.mint.has(D.stateQueueMint) ||
    entry.tx.outputs.some((output) => output.assets.has(D.stateQueueMint)) ||
    entry.tx.inputs.some(
      (outRef) =>
        model.created
          .get(label(outRef.txHash, outRef.index))
          ?.output.assets.has(D.stateQueueMint) === true,
    ));

const sameTxs = (
  read: readonly Readonly<{ txHash: string; inclusionPoint: unknown }>[],
  oracle: readonly OracleEntry[],
): boolean =>
  JSON.stringify(
    read.map((entry) => [entry.txHash, entry.inclusionPoint]).sort(),
  ) ===
  JSON.stringify(
    oracle
      .map((entry) => [
        entry.tx.hash.toString("hex"),
        rawPointOf({ ...entry.block.point, height: entry.block.height }),
      ])
      .sort(),
  );

/**
 * The watcher's own checks at every step, against the independent chain
 * model: the queue view, the tip block's checkpoints, and (ruling 2) every
 * read a live decision needs past k: the unit history of each queued header,
 * each history tx's body and checkpoint, and each DAAT-creating tx.
 */
export const watcherCheck = (seen: Coverage) => {
  const follower = createChainFollower();
  return async ({
    store,
    step,
  }: Readonly<{ store: FactStore; step: ForkStep }>): Promise<
    string | null
  > => {
    const blocks = follower.follow(step);
    const cursor = await store.cursor();
    if (cursor === null) return null;
    seen.steps += 1;
    const model = modelOf(blocks);
    const expected = modelQueue(model, D);

    const view = await store.transaction("read", (tx) =>
      readWatcherQueueView(tx, cursor.point.slot),
    );
    if (!view.healthy) return `view unhealthy: ${view.reason}: ${view.detail}`;
    if (JSON.stringify(view.queue) !== JSON.stringify(expected.queue))
      return `queue ${JSON.stringify(view.queue)} != model ${JSON.stringify(expected.queue)}`;
    if ((view.correctionLock?.outRef ?? null) !== expected.lock)
      return `lock ${view.correctionLock?.outRef ?? "none"} != model ${expected.lock ?? "none"}`;
    seen.queuedHeaders += view.headers.length;
    if (view.correctionLock !== null) seen.locks += 1;
    for (const header of view.headers) {
      const minted = model.unitHistory.get(`${NODE_UNIT}${header.headerHash}`);
      const mint = minted?.find(({ tx }) =>
        tx.mint
          .get(D.stateQueueMint)
          ?.has(
            `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header.headerHash}`,
          ),
      );
      if (
        mint === undefined ||
        header.observedTransactionHash !== mint.tx.hash.toString("hex") ||
        header.observedBlockHash !== mint.block.point.hash.toString("hex")
      )
        return `header ${header.headerHash} is not anchored at its mint`;
    }

    const tip = blocks.at(-1);
    if (tip !== undefined && tip.point.hash.equals(cursor.point.hash)) {
      const rows = await store.transaction("read", (tx) =>
        tx.query(
          `SELECT tx_hash, transition, failure FROM ${WATCHER_QUEUE_CHECKPOINTS_TABLE} WHERE block_slot = ? AND block_hash = ? ORDER BY tx_index`,
          [tip.point.slot, tip.point.hash],
        ),
      );
      const wanted = tip.txs
        .filter((tx) => touchesQueue(model, { tx, block: tip }))
        .map((tx) => tx.hash.toString("hex"));
      const got = rows.map((row) => hex(row.tx_hash));
      // At or below the pruned window, unpinned rows are gone (the pins are checked below).
      const kept =
        tip.point.slot <= cursor.prunedThroughSlot
          ? wanted.filter((hash) => got.includes(hash))
          : wanted;
      if (JSON.stringify(got) !== JSON.stringify(kept))
        return `checkpoint rows ${JSON.stringify(got)} != queue txs ${JSON.stringify(wanted)}`;
      seen.checkpointRows += rows.length;
      const last = rows.at(-1);
      if (
        last !== undefined &&
        got.length === wanted.length &&
        rows.every((row) => row.failure === null) &&
        (expected.queue.length > 0 || last.transition !== null)
      ) {
        const next = (
          JSON.parse(last.transition as string) as {
            nextQueue: readonly { headerHash: string | null; outRef: string }[];
          }
        ).nextQueue;
        if (JSON.stringify(next) !== JSON.stringify(expected.queue))
          return `last transition's next queue ${JSON.stringify(next)} != model`;
        seen.transitionsMatched += 1;
      }
    }

    const reads = createFollowerRawReads(store, {
      stateQueuePolicyId: D.stateQueueMint,
    });
    const at = rawPointOf({ ...cursor.point, height: cursor.height });
    const queued = new Set(view.headers.map(({ headerHash }) => headerHash));
    const pointOf = (entry: OracleEntry) =>
      rawPointOf({ ...entry.block.point, height: entry.block.height });
    for (const header of queued) {
      const oracle = model.unitHistory.get(`${NODE_UNIT}${header}`) ?? [];
      const history = await reads.unitHistoryAtPoint(
        `${NODE_UNIT}${header}`,
        at,
      );
      if (history.kind !== "ok")
        return `queued header ${header}: unit history ${history.reason}: ${history.detail}`;
      if (!sameTxs(history.value.transactions, oracle))
        return `queued header ${header}: unit history differs from the model`;
      for (const entry of oracle) {
        const txHash = entry.tx.hash.toString("hex");
        const raw = await reads.rawTransaction(txHash, pointOf(entry));
        if (raw.kind !== "ok")
          return `queued header ${header}: history tx ${txHash}: ${raw.reason}`;
        const checkpoint = await store.transaction("read", (tx) =>
          readWatcherTransactionCheckpoint(tx, entry.tx.hash, {
            deploymentIdentityDigest: "00".repeat(32),
            stateQueuePolicyId: D.stateQueueMint,
            finalityDepth: 1,
            prunedThroughSlot: cursor.prunedThroughSlot,
            originSlot: cursor.origin.slot,
          }),
        );
        if (checkpoint.kind !== "ok" && checkpoint.kind !== "failed")
          return `queued header ${header}: checkpoint of ${txHash}: ${checkpoint.kind}`;
        seen.pinnedHistoryReads += 1;
        if (entry.block.point.slot <= cursor.prunedThroughSlot)
          seen.pinnedBelowWindow += 1;
      }
      const daat = (
        model.unitHistory.get(`${DAAT_UNIT}${header}`) ?? []
      ).filter(({ tx }) =>
        tx.outputs.some((output) =>
          output.assets
            .get(D.daAttestationMint)
            ?.has(`${SDK.DA_ATTESTATION_ASSET_NAME_PREFIX}${header}`),
        ),
      );
      for (const entry of daat) {
        const txHash = entry.tx.hash.toString("hex");
        const inclusion = await reads.transactionInclusion(txHash);
        if (
          inclusion.kind !== "ok" ||
          inclusion.value?.pointId !== pointOf(entry).pointId
        )
          return `queued header ${header}: DAAT tx ${txHash} inclusion ${JSON.stringify(inclusion)}`;
        const raw = await reads.rawTransaction(txHash, pointOf(entry));
        if (raw.kind !== "ok")
          return `queued header ${header}: DAAT tx ${txHash}: ${raw.reason}`;
        seen.attestationReads += 1;
        if (entry.block.point.slot <= cursor.prunedThroughSlot)
          seen.pinnedBelowWindow += 1;
      }
    }
    // A merged header's history is complete, or refused once pruned.
    for (const [unit, oracle] of model.unitHistory) {
      if (!unit.startsWith(NODE_UNIT)) continue;
      if (queued.has(unit.slice(NODE_UNIT.length))) continue;
      const history = await reads.unitHistoryAtPoint(unit, at);
      if (history.kind === "refused") {
        if (history.reason !== "beyond_retention")
          return `merged ${unit}: ${history.reason}`;
        seen.mergedHeadersPruned += 1;
        continue;
      }
      if (!sameTxs(history.value.transactions, oracle))
        return `merged ${unit}: unit history differs from the model`;
      seen.mergedHeaderReads += 1;
    }
    return null;
  };
};
