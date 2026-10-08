/**
 * The proof-retention fixture on the fork simulator with pruning on: one
 * header committed on an untracked operator UTxO and then removed as
 * fraudulent, with the proof-retention and tx-input seams over its store.
 */
import type { OutRef } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  type WatcherProjectionDeployment,
  watcherUnitHistoryPolicies,
} from "../../src/l1-follower/projection.js";
import { createWatcherProofRetention } from "../../src/l1-follower/proof-retention.js";
import {
  type LedgerOutputsQuery,
  ledgerOutputsQueryFromTransport,
} from "../../src/l1-follower/raw-reads.ledger.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import { createTxInputsResolver } from "../../src/l1-follower/tx-inputs.js";
import { D, harness, K, SEED, X } from "./l1-follower-raw-reads-fixture.js";
import { removeTailTx } from "./l1-follower-state-queue-removal.js";
import {
  commitTx,
  initTx,
  queueState,
} from "./l1-follower-state-queue-traffic.js";

const FOLLOWED = "70".repeat(28);
export const FOLLOWING: WatcherProjectionDeployment = {
  ...D,
  followedScripts: [FOLLOWED],
};
export const FOLLOWED_UNIT = `${FOLLOWED}aa`;
const scriptAddress = (hash: string): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]);
const one = (policy: string, name: string, quantity = 1n) =>
  new Map([[policy, new Map([[name, quantity]])]]);
export const nodeUnit = (header: string): string =>
  `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;

const closers: (() => Promise<void>)[] = [];
/** Closes every fixture `removedHeader` opened; run it after each test. */
export const closeRemovedHeaders = async (): Promise<void> => {
  for (const close of closers.splice(0).reverse()) await close();
};

type Ledger = Readonly<{
  query: LedgerOutputsQuery;
  down: (on: boolean) => void;
}>;

/**
 * A follower with the proof-retention and tx-input seams, and a chain with
 * one header committed on an untracked operator UTxO (U#0, paid to X by a
 * tx the follower never stores) and then removed as fraudulent.
 */
export const removedHeader = async (
  options: Readonly<{
    pin: boolean;
    resolveAtIngest: boolean;
    followUnit?: boolean;
    /** Resolve the init's inputs while its parent is in the window. */
    resolveInit?: boolean;
  }>,
) => {
  const deployment = options.followUnit === true ? FOLLOWING : D;
  const h = await harness(deployment);
  closers.push(() => h.store.close());
  const real = ledgerOutputsQueryFromTransport(h.node);
  let isDown = false;
  const ledger: Ledger = {
    query: async (point, outRefs) =>
      isDown
        ? { kind: "unavailable", detail: "the node is down" }
        : await real(point, outRefs),
    down: (on) => {
      isDown = on;
    },
  };
  const resolver = createTxInputsResolver({
    store: h.store,
    ledger: ledger.query,
  });
  closers.push(() => resolver.close());
  const retention = createWatcherProofRetention(h.store, {
    unitHistoryPolicies: watcherUnitHistoryPolicies(deployment),
  });

  const funding: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [{ address: X, lovelace: 4_000_000n }],
    nonce: h.chain.nonce(),
  };
  const [u] = (await h.forward([funding])).hashes as [string];
  const operatorUtxo: OutRef = { txHash: Buffer.from(u, "hex"), index: 0 };
  // The init spends the wallet seed: an input the model ledger holds.
  await h.forward([initTx(D, undefined, SEED.outRef)]);
  // The follower is within the node's window at the init: its seed input
  // resolves from the ledger.
  if (options.resolveInit !== false) expect(await resolver.step()).toEqual([]);
  const commit = commitTx(queueState(h.chain, D)!, D, operatorUtxo);
  const header = [
    ...(commit.mint?.get(D.stateQueueMint)?.keys() ?? []),
  ][0]!.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
  const committed = await h.forward([commit]);
  const commitHash = committed.hashes[0]!;
  if (options.resolveAtIngest) expect(await resolver.step()).toEqual([]);
  const target = { category: "doubleSpend", headerHash: header };
  if (options.pin)
    expect(await retention.pin(target)).toEqual({ kind: "pinned" });

  let unitTxs: string[] = [];
  if (options.followUnit === true) {
    // A computation-thread stand-in: minted, then burned, so its history
    // closes and goes k deep like the removed header's.
    const minted = (
      await h.forward([
        {
          inputs: [{ txHash: Buffer.alloc(32, 0xd1), index: 0 }],
          outputs: [
            {
              address: scriptAddress(FOLLOWED),
              lovelace: 2_000_000n,
              assets: one(FOLLOWED, "aa"),
            },
          ],
          mint: one(FOLLOWED, "aa"),
          nonce: h.chain.nonce(),
        },
      ])
    ).hashes[0]!;
    const burned = (
      await h.forward([
        {
          inputs: [{ txHash: Buffer.from(minted, "hex"), index: 0 }],
          outputs: [{ address: X, lovelace: 2_000_000n }],
          mint: one(FOLLOWED, "aa", -1n),
          nonce: h.chain.nonce(),
        },
      ])
    ).hashes[0]!;
    unitTxs = [minted, burned];
  }
  const removal = await h.forward([removeTailTx(queueState(h.chain, D)!, D)]);

  const count = async (table: string, column: string, value: string) =>
    Number(
      (
        await h.store.transaction("read", (tx) =>
          tx.query(`SELECT COUNT(*) AS n FROM ${table} WHERE ${column} = ?`, [
            Buffer.from(value, "hex"),
          ]),
        )
      )[0]!.n,
    );
  /** Moves the tip `blocks` past the removal and prunes everything k deep. */
  const passK = async (blocks = K + 4) => {
    for (let i = 0; i < blocks; i += 1) await h.forward([]);
    await h.pruneAll();
  };
  /**
   * Moves the tip `K + 4` blocks past the removal and runs one prune step
   * of `budget` rows per table: a budget-cut step, whose siblings wait.
   */
  const passKOneStep = async (budget = 1) => {
    for (let i = 0; i < K + 4; i += 1) await h.forward([]);
    const step = await h.store.prune(budget);
    if ("kind" in step) throw new Error(`prune: ${step.kind}`);
    expect(step.done).toBe(false);
  };
  return {
    h,
    ledger,
    resolver,
    retention,
    target,
    header,
    commitHash,
    commitPoint: committed.point,
    removalPoint: removal.point,
    removalHash: removal.hashes[0]!,
    operatorUtxo: `${u}#0`,
    unitTxs,
    count,
    passK,
    passKOneStep,
  };
};

export type Removed = Awaited<ReturnType<typeof removedHeader>>;

export const historyRows = (r: Removed) =>
  r.count(WATCHER_QUEUE_UNIT_HISTORY_TABLE, "header_hash", r.header);
export const departedRows = (r: Removed) =>
  r.count(WATCHER_DEPARTED_HEADERS_TABLE, "header_hash", r.header);
export const txRows = (r: Removed, txHash: string) =>
  r.count("l1_txs", "tx_hash", txHash);
