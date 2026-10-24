import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  type OutputSummary,
  type OutRef,
  readWalletViewIn,
  walletView,
} from "../src/index.js";
import { SIM_ORIGIN, type SimTx, simTxHash } from "../src/testing/index.js";
import { type Harness, open, spend, u } from "./support/intents-harness.js";

const ref = (fill: number, index = 0): OutRef => ({
  txHash: Buffer.alloc(32, fill),
  index,
});

const output = (address: Buffer, lovelace: bigint): OutputSummary => ({
  address,
  paymentCredential: null,
  stakeCredential: null,
  lovelace,
  assets: new Map(),
  datumHash: null,
  datum: null,
  scriptRef: null,
});

const own = u.trackedAddress;
const other = u.untrackedAddress;

const keys = (outRefs: readonly OutRef[]): string[] =>
  outRefs.map((r) => `${r.txHash.toString("hex")}#${r.index}`);

/** The facts come in the store's order: compare them as a set. */
const sorted = (outRefs: readonly OutRef[]): string[] => keys(outRefs).sort();

describe("walletView (§8.5), the pure formula", () => {
  it("keeps the own facts no live intent holds and adds the live intents' own outputs", () => {
    const view = walletView(
      [
        { outRef: ref(1), output: output(own, 5n) },
        { outRef: ref(2), output: output(own, 6n) },
        { outRef: ref(3), output: output(own, 7n) },
        { outRef: ref(4), output: output(other, 8n) },
      ],
      [
        {
          txHash: Buffer.alloc(32, 9),
          inputs: [ref(1)],
          collaterals: [ref(2)],
          outputs: [
            { index: 0, output: output(own, 4n) },
            { index: 1, output: output(other, 1n) },
          ],
        },
      ],
      own,
    );
    expect(keys(view.available.map((e) => e.outRef))).toEqual(
      keys([ref(3), ref(9, 0)]),
    );
    expect(view.available.map((e) => e.predictedBy)).toEqual([
      null,
      Buffer.alloc(32, 9),
    ]);
    // The input and the collateral are held, whatever address they are at.
    expect(sorted(view.held)).toEqual(sorted([ref(1), ref(2)]));
  });

  it("does not offer a live intent's predicted output that another live intent spends", () => {
    const first = Buffer.alloc(32, 9);
    const view = walletView(
      [{ outRef: ref(1), output: output(own, 5n) }],
      [
        {
          txHash: first,
          inputs: [ref(1)],
          collaterals: [],
          outputs: [{ index: 0, output: output(own, 4n) }],
        },
        {
          txHash: Buffer.alloc(32, 10),
          inputs: [{ txHash: first, index: 0 }],
          collaterals: [],
          outputs: [{ index: 0, output: output(own, 3n) }],
        },
      ],
      own,
    );
    expect(keys(view.available.map((e) => e.outRef))).toEqual(
      keys([ref(10, 0)]),
    );
    expect(sorted(view.held)).toEqual(
      sorted([ref(1), { txHash: first, index: 0 }]),
    );
  });

  it("with no live intent is exactly the own facts", () => {
    const facts = [
      { outRef: ref(1), output: output(own, 5n) },
      { outRef: ref(2), output: output(other, 5n) },
    ];
    expect(
      keys(walletView(facts, [], own).available.map((e) => e.outRef)),
    ).toEqual(keys([ref(1)]));
  });
});

describe("readWalletViewIn over the journal and the facts", () => {
  let h: Harness;

  beforeEach(async () => {
    h = await open();
    await h.store.initialize(SIM_ORIGIN);
  });

  afterEach(async () => {
    await h.store.close();
  });

  const read = () =>
    h.store.transaction("read", (sql) =>
      readWalletViewIn(sql, h.store.dialect, own),
    );
  const available = async () =>
    sorted((await read()).view.available.map((e) => e.outRef));
  const out = (tx: SimTx, index: number): OutRef => ({
    txHash: simTxHash(tx),
    index,
  });

  it("holds a live intent's input and collateral, offers its change, and follows the head through landing, rollback and death", async () => {
    const input = await h.fund();
    const collateral = await h.fund();
    const spare = await h.fund();
    const tx = spend(h.chain, input, { collaterals: [collateral] });
    expect(await h.record(tx)).toMatchObject({ kind: "recorded" });

    // Live: the input and the collateral are held back, the change is offered.
    const live = await read();
    expect(keys(live.view.available.map((e) => e.outRef))).toEqual(
      keys([spare, out(tx, 0)]),
    );
    expect(live.view.available[0]?.predictedBy).toBeNull();
    expect(live.view.available[1]?.predictedBy).toEqual(simTxHash(tx));
    expect(live.view.available[1]?.output.lovelace).toBe(4_000_000n);
    expect(sorted(live.view.held)).toEqual(sorted([input, collateral]));

    // Lands: the change is a fact now, no longer a prediction.
    await h.forward([tx]);
    const landed = await read();
    expect(sorted(landed.view.available.map((e) => e.outRef))).toEqual(
      sorted([collateral, spare, out(tx, 0)]),
    );
    expect(landed.view.available.every((e) => e.predictedBy === null)).toBe(
      true,
    );
    expect(landed.view.held).toEqual([]);

    // Rolled back: live again, so held and predicted again.
    await h.backward(1);
    expect(await available()).toEqual(sorted([spare, out(tx, 0)]));

    // Dies: another tx spends its collateral (conflicted). Its input is
    // offered again on the next read; its change is gone.
    const foreign: SimTx = {
      inputs: [collateral],
      outputs: [{ address: other, lovelace: 1_000_000n }],
      nonce: h.chain.nonce(),
    };
    await h.forward([foreign]);
    expect(
      (await h.statuses()).get(simTxHash(tx).toString("hex")),
    ).toMatchObject({ kind: "conflicted" });
    expect(await available()).toEqual(sorted([input, spare]));
    expect((await read()).view.held).toEqual([]);
  });

  it("chains: a second intent spending the first's change holds it, and offers its own change", async () => {
    const input = await h.fund();
    const first = spend(h.chain, input);
    expect(await h.record(first)).toMatchObject({ kind: "recorded" });
    const second = spend(h.chain, out(first, 0), {
      outputs: [{ address: own, lovelace: 3_000_000n }],
    });
    expect(await h.record(second)).toMatchObject({ kind: "recorded" });
    expect(await available()).toEqual(sorted([out(second, 0)]));

    // The first lands: its change is a fact the live second still holds.
    await h.forward([first]);
    expect(await available()).toEqual(sorted([out(second, 0)]));

    // Both land.
    await h.forward([second]);
    expect(await available()).toEqual(sorted([out(second, 0)]));
    expect((await read()).view.available[0]?.predictedBy).toBeNull();
  });

  it("a dead first intent takes its dependent down, and the first's input comes back", async () => {
    const input = await h.fund();
    const first = spend(h.chain, input);
    expect(await h.record(first)).toMatchObject({ kind: "recorded" });
    const second = spend(h.chain, out(first, 0));
    expect(await h.record(second)).toMatchObject({ kind: "recorded" });
    const foreign: SimTx = {
      inputs: [input],
      outputs: [{ address: other, lovelace: 1_000_000n }],
      nonce: h.chain.nonce(),
    };
    await h.forward([foreign]);
    const statuses = await h.statuses();
    expect(statuses.get(simTxHash(first).toString("hex"))?.kind).toBe(
      "conflicted",
    );
    expect(statuses.get(simTxHash(second).toString("hex"))?.kind).toBe(
      "dependency_dead",
    );
    expect(await available()).toEqual([]);
  });
});
