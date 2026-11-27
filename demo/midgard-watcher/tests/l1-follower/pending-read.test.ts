import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import { applyChainSyncEvent } from "@al-ft/midgard-l1-follower";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import {
  encodeSimTx,
  encodeUtxoAnswer,
  type SimTx,
  simTxHash,
} from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import {
  createWatcherPendingRead,
  withNodeTipRetries,
} from "../../src/l1-follower/pending-read.js";
import {
  harness,
  hex,
  SEED,
  T,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * The watcher's read of the node's mempool at the follower's view
 * (`pending-read.ts`): one read lists the mempool, then the node's tip,
 * then the follower's view, and holds only at the node's tip, so a
 * transaction of ours is in the listing or in the facts at every read.
 */

type Tip = Readonly<{ slot: number; hash: string }>;
const tipOf = (point: Readonly<{ slot: number; hash: Buffer }>): Tip => ({
  slot: point.slot,
  hash: hex(point.hash),
});

/** A wallet step: spends `input`, pays T. */
const step = (
  input: SimTx["inputs"][number],
  lovelace: bigint,
  nonce: number,
): SimTx => ({
  inputs: [input],
  outputs: [{ address: T, lovelace }],
  nonce,
});

/**
 * A read whose node answers come from `node`, recording the order the read
 * asks them in.
 */
const readOver = (
  store: Parameters<typeof createWatcherPendingRead>[0]["store"],
  node: Readonly<{
    mempoolTxs: () => Promise<readonly SimTx[]>;
    nodeTip: () => Promise<Tip>;
  }>,
) => {
  const calls: string[] = [];
  const read = createWatcherPendingRead({
    store,
    node: {
      mempoolTxs: async () => {
        calls.push("mempoolTxs");
        return (await node.mempoolTxs()).map(encodeSimTx);
      },
    },
    nodeTip: async () => {
      calls.push("nodeTip");
      return await node.nodeTip();
    },
  });
  return { read, calls };
};

const inFacts = async (
  store: Parameters<typeof createWatcherPendingRead>[0]["store"],
  tx: SimTx,
): Promise<boolean> => (await store.txByHash(simTxHash(tx))) !== null;

describe("watcher pending read", () => {
  it("refuses a follower one block behind the node's tip, retries, and reads the landed transaction from the facts", async () => {
    const h = await harness();
    const P = step(SEED.outRef, 4_000_000n, h.chain.nonce());
    // The node applied the block including P; the follower has not yet.
    const { event, encoded } = h.chain.forward([P]);
    const nodeTip = tipOf(h.chain.tip.point);
    expect(hex(encoded.txHashes[0]!)).toBe(hex(simTxHash(P)));
    let attempts = 0;
    const seen: { listed: boolean; landed: boolean }[] = [];
    const { read, calls } = readOver(h.store, {
      mempoolTxs: async () => {
        attempts += 1;
        // The follower applies the block before the second read lists.
        if (attempts === 2)
          expect((await applyChainSyncEvent(h.store, event)).result.kind).toBe(
            "applied",
          );
        return [];
      },
      nodeTip: async () => nodeTip,
    });
    const record = async () => {
      const at = await read.atTip();
      seen.push({
        listed: at.pending.some((tx) => tx.txHash === hex(simTxHash(P))),
        landed: await inFacts(h.store, P),
      });
      return at;
    };

    const at = await withNodeTipRetries(record);

    expect(calls).toEqual(["mempoolTxs", "nodeTip", "mempoolTxs", "nodeTip"]);
    expect(at.view.point.slot).toBe(nodeTip.slot);
    expect(hex(at.view.point.hash)).toBe(nodeTip.hash);
    expect(at.pending).toEqual([]);
    // The refused first read returned nothing; the read that held shows P.
    expect(seen).toEqual([{ listed: false, landed: true }]);
  });

  it("names a transaction listed before its block landed once, as the facts' transaction", async () => {
    const h = await harness();
    const P = step(SEED.outRef, 4_000_000n, h.chain.nonce());
    const { event } = h.chain.forward([P]);
    const nodeTip = tipOf(h.chain.tip.point);
    const { read } = readOver(h.store, {
      // The listing still names P; the block lands before the tip is read.
      mempoolTxs: async () => [P],
      nodeTip: async () => {
        expect((await applyChainSyncEvent(h.store, event)).result.kind).toBe(
          "applied",
        );
        return nodeTip;
      },
    });

    const at = await read.atTip();

    expect(at.pending).toEqual([]);
    expect(await inFacts(h.store, P)).toBe(true);
  });

  it("lists a transaction the node holds and no block includes", async () => {
    const h = await harness();
    const P = step(SEED.outRef, 4_000_000n, h.chain.nonce());
    const { read } = readOver(h.store, {
      mempoolTxs: async () => [P],
      nodeTip: async () => tipOf(h.chain.tip.point),
    });

    const at = await read.atTip();

    expect(at.pending.map((tx) => tx.txHash)).toEqual([hex(simTxHash(P))]);
    expect(await inFacts(h.store, P)).toBe(false);
    expect(await read.latest()).toBe(at.pending);
  });

  it("is unavailable while the follower stays behind the node's tip", async () => {
    const h = await harness();
    h.chain.forward([]);
    const nodeTip = tipOf(h.chain.tip.point);
    const { read, calls } = readOver(h.store, {
      mempoolTxs: async () => [],
      nodeTip: async () => nodeTip,
    });

    await expect(read.latest()).rejects.toBeInstanceOf(
      FraudProofL1UnavailableError,
    );
    // The first read and the checkpoint retries, each refused.
    expect(calls).toEqual([
      "mempoolTxs",
      "nodeTip",
      "mempoolTxs",
      "nodeTip",
      "mempoolTxs",
      "nodeTip",
    ]);
  });

  it("builds the next step of a chain in the mempool from its last output after a restart, at once and without the earlier steps", async () => {
    const h = await harness();
    const step1 = step(SEED.outRef, 4_000_000n, h.chain.nonce());
    const step2 = step(
      { txHash: simTxHash(step1), index: 0 },
      3_000_000n,
      h.chain.nonce(),
    );
    const mempool: SimTx[] = [step1, step2];
    const nodeTip = { current: tipOf(h.chain.tip.point) };
    // A restarted process: a new read and provider, nothing remembered.
    const { read } = readOver(h.store, {
      mempoolTxs: async () => [...mempool],
      nodeTip: async () => nodeTip.current,
    });
    const provider = new L1FollowerProvider({
      store: h.store,
      // Ledger reads (outputs the facts lack) answer empty.
      transport: {
        withLedgerState: async (
          _at: unknown,
          use: (session: { query: () => Promise<Uint8Array> }) => unknown,
        ) => await use({ query: async () => encodeUtxoAnswer([]) }),
      } as unknown as L1NodeTransport,
      pending: read.latest,
    });
    const outRef = (tx: SimTx | null, index = 0) =>
      tx === null
        ? { txHash: hex(SEED.outRef.txHash), outputIndex: SEED.outRef.index }
        : { txHash: hex(simTxHash(tx)), outputIndex: index };
    const live = async () =>
      (
        await provider.getUtxosByOutRef([
          outRef(null),
          outRef(step1),
          outRef(step2),
        ])
      ).map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`);

    expect((await read.atTip()).pending.map((tx) => tx.txHash)).toEqual([
      hex(simTxHash(step1)),
      hex(simTxHash(step2)),
    ]);
    expect(await live()).toEqual([`${hex(simTxHash(step2))}#0`]);

    // Step 1 lands while step 2 waits: the answer is the same coin, once.
    const { event } = h.chain.forward([step1]);
    expect((await applyChainSyncEvent(h.store, event)).result.kind).toBe(
      "applied",
    );
    mempool.splice(0, 1);
    nodeTip.current = tipOf(h.chain.tip.point);
    expect((await read.atTip()).pending.map((tx) => tx.txHash)).toEqual([
      hex(simTxHash(step2)),
    ]);
    expect(await live()).toEqual([`${hex(simTxHash(step2))}#0`]);
  });
});
