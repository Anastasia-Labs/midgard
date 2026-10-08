/**
 * The tx-input sweep over a store reset (lane SI, ruling SI-R3; lane
 * SI-fix, ruling SIFIX-R1): it waits for the replay to end, after a
 * tracked-set reset and a manual `reset --to-origin` alike, so the inputs
 * stored at ingest (class C) survive it; and the watcher's wallets stay out
 * of the tracked-set record.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeBlock,
  openSqliteBackend,
  resetToOrigin,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { openWatcherFollowerRuntime } from "../../src/l1-follower/follower-runtime.js";
import { WATCHER_TX_INPUTS_TABLE } from "../../src/l1-follower/tables.js";
import { createTxInputsResolver } from "../../src/l1-follower/tx-inputs.js";
import { watcherFollowerStarted } from "../../src/runtime/watcher-runtime.decision-driver.js";
import { okValue } from "../support/l1-follower-raw-reads-fixture.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  chainEvents,
  D,
  dropRecord,
  openStore,
  RECOVERY_DEPTH,
  scriptedTransport,
  until,
  WALLET,
} from "../support/l1-follower-store-reset.js";
import {
  closeRemovedHeaders,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

const scratch = mkdtempSync(
  join(tmpdir(), "watcher-tracked-set-reset-inputs-"),
);
const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
  await closeRemovedHeaders();
});
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

describe("tx inputs over a tracked-set reset", () => {
  it("keeps the inputs stored at ingest through the replay with no ledger read, and sweeps again once the replay reached the tip", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    const store = r.h.store;
    const inputs = (txHash: string) =>
      r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", txHash);
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
    // A row of a tx no fact names: what the sweep is for.
    const stray = "ef".repeat(32);
    await store.transaction("write", (tx) =>
      tx.query(
        `INSERT INTO ${WATCHER_TX_INPUTS_TABLE} (tx_hash, out_tx_hash, out_index, output_cbor) VALUES (?, ?, ?, ?)`,
        [Buffer.from(stray, "hex"), Buffer.alloc(32, 0x01), 0, Buffer.of(0xa0)],
      ),
    );
    const blocks = r.h.chain.rawBlocks();

    await dropRecord(store);
    expect(await store.start()).toMatchObject({
      trackedSet: { kind: "reset" },
      replaying: true,
    });
    // The node can serve none of the deep parents: any read would fail.
    r.ledger.down(true);
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    // Mid-replay, before the commit is back in l1_txs.
    expect((await store.applyBlock(decodeBlock(blocks[0]!))).kind).toBe(
      "applied",
    );
    expect(await store.txByHash(Buffer.from(r.commitHash, "hex"))).toBeNull();
    await r.resolver.step();
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
    expect(await inputs(stray)).toBe(1);
    for (const raw of blocks.slice(1))
      expect((await store.applyBlock(decodeBlock(raw))).kind).toBe("applied");
    const unresolved = await r.resolver.step();
    expect(unresolved.map(({ txHash }) => txHash)).not.toContain(r.commitHash);
    expect(await inputs(stray)).toBe(1);
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs).toEqual([]);
    expect(
      raw.transaction.resolvedInputs.map(({ outRef }) => outRef),
    ).toContain(r.operatorUtxo);

    // The first report at the tip ends the replay: the sweep runs again.
    expect(await store.endTrackedSetReplay()).toBe("ended");
    await r.resolver.step();
    expect(await inputs(stray)).toBe(0);
    expect(await inputs(r.commitHash)).toBeGreaterThan(0);
  });
});

describe("tx inputs over a manual reset --to-origin", () => {
  it("keeps the inputs stored at ingest through the replay, and sweeps again once the replay ended", async () => {
    const path = join(scratch, "manual-reset.db");
    const events = chainEvents(6);
    const first = openStore(path);
    expect((await first.start()).kind).toBe("ready");
    expect((await first.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(first, events);
    const stored = (
      await first.transaction("read", (tx) =>
        tx.query("SELECT tx_hash FROM l1_txs ORDER BY tx_hash LIMIT 1"),
      )
    ).map((row) => Buffer.from(row.tx_hash as Uint8Array).toString("hex"))[0];
    if (stored === undefined) throw new Error("no tx stored");
    // A tx the facts name, and a stray row of a tx no fact names.
    const stray = "ef".repeat(32);
    for (const txHash of [stored, stray])
      await first.transaction("write", (tx) =>
        tx.query(
          `INSERT INTO ${WATCHER_TX_INPUTS_TABLE} (tx_hash, out_tx_hash, out_index, output_cbor) VALUES (?, ?, ?, ?)`,
          [
            Buffer.from(txHash, "hex"),
            Buffer.alloc(32, 0x01),
            0,
            Buffer.of(0xa0),
          ],
        ),
      );
    await first.close();

    const backend = openSqliteBackend(path);
    try {
      expect(await resetToOrigin(backend)).toMatchObject({ kind: "reset" });
    } finally {
      await backend.close();
    }

    const store = openStore(path);
    // The node can serve nothing: any read would fail.
    const resolver = createTxInputsResolver({
      store,
      ledger: () =>
        Promise.resolve({ kind: "unavailable", detail: "the node is down" }),
    });
    opened.push(async () => {
      await resolver.close();
      await store.close();
    });
    const inputs = async (txHash: string) =>
      Number(
        (
          await store.transaction("read", (tx) =>
            tx.query(
              `SELECT COUNT(*) AS n FROM ${WATCHER_TX_INPUTS_TABLE} WHERE tx_hash = ?`,
              [Buffer.from(txHash, "hex")],
            ),
          )
        )[0]!.n,
      );
    expect(await store.start()).toMatchObject({
      kind: "ready",
      cursor: null,
      // The reset left no cursor: the record is checked once one is back.
      trackedSet: { kind: "unchecked" },
      replaying: true,
    });
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await resolver.step();
    expect([await inputs(stored), await inputs(stray)]).toEqual([1, 1]);
    await applyAll(store, events);
    await resolver.step();
    expect([await inputs(stored), await inputs(stray)]).toEqual([1, 1]);

    expect(await store.endTrackedSetReplay()).toBe("ended");
    await resolver.step();
    expect([await inputs(stored), await inputs(stray)]).toEqual([1, 0]);
  });
});

describe("the watcher's tracked-set record", () => {
  it("holds none of the watcher's wallets: they are seeded, not tracked", async () => {
    const path = join(scratch, "wallet-record.db");
    const events = chainEvents(2);
    const wallet = WALLET;
    const details = getAddressDetails(wallet);
    const follower = openWatcherFollowerRuntime({
      deployment: D,
      storePath: path,
      automaticRecoveryMaxDepth: RECOVERY_DEPTH,
      origin: {
        origin: SIM_ORIGIN.point,
        hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
      },
      node: { binaryPath: "unused", socketPath: "unused", networkMagic: 42 },
      walletAddresses: [wallet],
      unsafeTransportForTest: scriptedTransport(events, { acked: 0 }),
    });
    opened.push(() => follower.close());
    await until("the follower's start", () =>
      watcherFollowerStarted(follower.status()),
    );
    const record = await follower.store.trackedSetRecord();
    if (record === null) throw new Error("no tracked-set record");
    // The record holds the protocol set (not vacuous) and no wallet item.
    expect(record.trackedSet.policies.length).toBeGreaterThan(0);
    expect(record.trackedSet.addresses).not.toContain(
      details.address.hex.toLowerCase(),
    );
    expect(record.trackedSet.paymentCredentials).not.toContain(
      details.paymentCredential?.hash.toLowerCase(),
    );
  });
});
