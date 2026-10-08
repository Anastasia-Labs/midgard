/**
 * Proof retention on the fork simulator with pruning on (E1 ruling): a
 * removed header's L1 history is held past k while a proof objective over
 * it is open and pruned on schedule once released (or when never pinned);
 * the inputs of every tx a unit history records are stored at ingest, so a
 * commit funded by an untracked operator UTxO still resolves after the
 * node's ledger window has moved past its inclusion block.
 */
import type { OutRef } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import { createWatcherProofRetention } from "../../src/l1-follower/proof-retention.js";
import {
  type LedgerOutputsQuery,
  ledgerOutputsQueryFromTransport,
} from "../../src/l1-follower/raw-reads.ledger.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_TX_INPUTS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import {
  createTxInputsResolver,
  L1_TX_INPUTS_UNRESOLVABLE,
  L1_TX_INPUTS_UNRESOLVED,
} from "../../src/l1-follower/tx-inputs.js";
import {
  D,
  harness,
  K,
  okValue,
  reasonOf,
  SEED,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import { removeTailTx } from "../support/l1-follower-state-queue-removal.js";
import {
  commitTx,
  initTx,
  queueState,
} from "../support/l1-follower-state-queue-traffic.js";

const FOLLOWED = "70".repeat(28);
const FOLLOWING: WatcherProjectionDeployment = {
  ...D,
  followedScripts: [FOLLOWED],
};
const FOLLOWED_UNIT = `${FOLLOWED}aa`;
const scriptAddress = (hash: string): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]);
const one = (policy: string, name: string, quantity = 1n) =>
  new Map([[policy, new Map([[name, quantity]])]]);
const nodeUnit = (header: string): string =>
  `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;

const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

type Ledger = Readonly<{
  query: LedgerOutputsQuery;
  down: (on: boolean) => void;
}>;

/**
 * A follower with the proof-retention and tx-input seams, and a chain with
 * one header committed on an untracked operator UTxO (U#0, paid to X by a
 * tx the follower never stores) and then removed as fraudulent.
 */
const removedHeader = async (
  options: Readonly<{
    pin: boolean;
    resolveAtIngest: boolean;
    followUnit?: boolean;
    /** Resolve the init's inputs while its parent is in the window. */
    resolveInit?: boolean;
  }>,
) => {
  const h = await harness(options.followUnit === true ? FOLLOWING : D);
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
  const retention = createWatcherProofRetention(h.store);

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
  if (options.pin) await retention.pin(target);

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
  };
};

type Removed = Awaited<ReturnType<typeof removedHeader>>;

const historyRows = (r: Removed) =>
  r.count(WATCHER_QUEUE_UNIT_HISTORY_TABLE, "header_hash", r.header);
const departedRows = (r: Removed) =>
  r.count(WATCHER_DEPARTED_HEADERS_TABLE, "header_hash", r.header);
const txRows = (r: Removed, txHash: string) =>
  r.count("l1_txs", "tx_hash", txHash);

describe("proof retention: a removed header's history past k", () => {
  it("holds the pinned history, txs and departed row while the proof runs past K blocks", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    expect(await departedRows(r)).toBe(1);
    // The proof runs: the tip moves well past K, pruning runs on schedule.
    await r.passK(2 * K + 4);

    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
    expect(await txRows(r, r.commitHash)).toBe(1);
    expect(await txRows(r, r.removalHash)).toBe(1);
    const reads = r.h.reads(true);
    const history = okValue(
      await reads.unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
    );
    expect(history.transactions.map(({ txHash }) => txHash)).toEqual([
      r.commitHash,
      r.removalHash,
    ]);
    for (const { txHash, inclusionPoint } of history.transactions)
      expect(
        okValue(await reads.rawTransaction(txHash, inclusionPoint)).transaction
          .txHash,
      ).toBe(txHash);
  });

  it("releases the pin once the objective completes, and the rows prune", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBeGreaterThan(0);

    await r.retention.release(r.target);
    expect(await r.retention.pinned()).toEqual([]);
    await r.passK(1);
    expect(await historyRows(r)).toBe(0);
    expect(await departedRows(r)).toBe(0);
    expect(await txRows(r, r.commitHash)).toBe(0);
    expect(
      reasonOf(
        await r.h
          .reads()
          .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ),
    ).toBe("beyond_retention");
    // The stored inputs go with the tx they belong to.
    expect(await r.resolver.step()).toEqual([]);
    expect(
      await r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", r.commitHash),
    ).toBe(0);
  });

  it("prunes an unpinned removed header's history on schedule (negative control)", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBe(0);
    expect(await departedRows(r)).toBe(0);
    expect(await txRows(r, r.commitHash)).toBe(0);
    expect(
      reasonOf(
        await r.h
          .reads()
          .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ),
    ).toBe("beyond_retention");
  });

  it("holds the followed units a capture names while the header is pinned, and releases them with it", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.retention.holdUnits(r.header, [FOLLOWED_UNIT]);
    await r.passK();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(2);
    const history = okValue(
      await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
    );
    expect(history.transactions.map(({ txHash }) => txHash)).toEqual(r.unitTxs);

    await r.retention.release(r.target);
    await r.passK(1);
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
  });

  it("writes no unit hold for an unpinned header", async () => {
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: true,
      followUnit: true,
    });
    await r.retention.holdUnits(r.header, [FOLLOWED_UNIT]);
    await r.passK();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
  });
});

describe("tx inputs stored at ingest (facet 2)", () => {
  it("resolves a commit's untracked operator input after its parent is more than k deep", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    // The node can no longer acquire the commit's parent.
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs).toEqual([]);
    expect(raw.unresolvedReferenceInputs).toEqual([]);
    expect(
      raw.transaction.resolvedInputs.map(({ outRef }) => outRef),
    ).toContain(r.operatorUtxo);
  });

  it("leaves the operator input unresolved when nothing stored it at ingest", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: false });
    await r.passK();
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs.map(({ outRef }) => outRef)).toEqual([
      r.operatorUtxo,
    ]);
  });

  it("holds a named retrying reason while the node is down, pinned or not, and clears it once resolved", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: false });
    r.ledger.down(true);
    const unresolved = await r.resolver.step();
    expect(
      unresolved.map(({ txHash, cause, permanent }) => [
        txHash,
        cause,
        permanent,
      ]),
    ).toContainEqual([r.commitHash, "unavailable", false]);
    const held = await r.resolver.assess();
    expect(held.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVED,
    ]);
    expect(held.readiness[0]!.detail).toContain("(retrying)");
    expect(held.degradations).toEqual([]);

    r.ledger.down(false);
    expect(await r.resolver.step()).toEqual([]);
    expect(await r.resolver.assess()).toEqual({
      readiness: [],
      degradations: [],
    });
    expect(
      await r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", r.commitHash),
    ).toBeGreaterThan(0);
  });
});

describe("permanently unresolvable inputs: degraded unless a proof pin holds them", () => {
  const permanentOf = async (r: Removed, txHash: string) =>
    (await r.resolver.step()).find((entry) => entry.txHash === txHash);

  it("reports a deep catch-up's unresolvable inputs in unpinned histories as a degradation and stays ready", async () => {
    // Caught up from the origin past k: neither the init nor the commit was
    // resolved while its parent was in the node's window.
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: false,
      resolveInit: false,
    });
    for (let i = 0; i < K + 1; i += 1) await r.h.forward([]);
    expect(await permanentOf(r, r.commitHash)).toMatchObject({
      cause: "too_old",
      permanent: true,
    });
    const deep = await r.resolver.assess();
    expect(deep.readiness).toEqual([]);
    expect(deep.degradations).toMatchObject([
      { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 2 },
    ]);
    expect(deep.degradations[0]!.detail).toContain("no open proof needs them");
    // Never skipped: it holds across passes, and the commit's history pruning
    // drops it while the init's still-open hub-oracle history keeps the init.
    await r.passK();
    expect(
      (await r.resolver.step()).map(({ cause, permanent }) => [
        cause,
        permanent,
      ]),
    ).toEqual([["too_old", true]]);
    expect(await r.resolver.assess()).toMatchObject({
      readiness: [],
      degradations: [{ reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 }],
    });
  });

  it("fails readiness by name while a proof pin holds the tx's header, and clears when the pin releases", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: false });
    for (let i = 0; i < K + 1; i += 1) await r.h.forward([]);
    expect(await permanentOf(r, r.commitHash)).toMatchObject({
      cause: "too_old",
      permanent: true,
    });
    const pinned = await r.resolver.assess();
    expect(pinned.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVABLE,
    ]);
    expect(pinned.readiness[0]!.detail).toContain(r.commitHash);
    expect(pinned.degradations).toEqual([]);

    // The release clears the reason at once, with no pass and no restart.
    await r.retention.release(r.target);
    const released = await r.resolver.assess();
    expect(released.readiness).toEqual([]);
    expect(released.degradations).toMatchObject([
      { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 },
    ]);
    await r.passK();
    expect(await r.resolver.step()).toEqual([]);
    expect(await r.resolver.assess()).toEqual({
      readiness: [],
      degradations: [],
    });
  });

  it("fails readiness while a pinned header's unit hold names the tx's unit, and clears when the pin releases", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    const [minted] = r.unitTxs as [string, string];
    // The mint spends an outside input no ledger state holds.
    expect(await permanentOf(r, minted)).toMatchObject({
      cause: "absent_at_parent",
      permanent: true,
    });
    expect((await r.resolver.assess()).readiness).toEqual([]);
    await r.retention.holdUnits(r.header, [FOLLOWED_UNIT]);
    const held = await r.resolver.assess();
    expect(held.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVABLE,
    ]);
    expect(held.readiness[0]!.detail).toContain(minted);

    await r.retention.release(r.target);
    expect(await r.resolver.assess()).toMatchObject({
      readiness: [],
      degradations: [{ reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 }],
    });
  });
});
