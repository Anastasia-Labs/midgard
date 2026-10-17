/**
 * Derived-status, S6 and retention edges of the intent journal (I1-fix),
 * over the same harness as `intents.test.ts`.
 */
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  deriveIntentStatusesIn,
  deriveIntentStatusIn,
  type OutRef,
  readIntentEventsIn,
} from "../src/index.js";
import { encodeSimTx, SIM_ORIGIN, simTxHash } from "../src/testing/index.js";
import { modelStatuses, observedOf } from "./support/intent-sim.model.js";
import {
  actionOf,
  type Harness,
  hex,
  K,
  open,
  s6,
  spend,
  u,
} from "./support/intents-harness.js";
import { recordAtCurrentView } from "./support/record-at-view.js";

let h: Harness;

beforeEach(async () => {
  h = await open();
});

afterEach(async () => {
  await h.store.close();
});

describe("derived status edges (I1-fix)", () => {
  it("a child spending only a failed parent's collateral return is not dependency_dead; one spending its regular output is", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const failing = spend(h.chain, await h.fund(), {
      isValid: false,
      collaterals: [await h.fund()],
      collateralReturn: { address: u.trackedAddress, lovelace: 3_000_000n },
    });
    await h.record(failing);
    const onReturn = spend(h.chain, { txHash: simTxHash(failing), index: 2 });
    const onOutput = spend(h.chain, { txHash: simTxHash(failing), index: 0 });
    expect((await h.record(onReturn)).kind).toBe("recorded");
    expect((await h.record(onOutput)).kind).toBe("recorded");
    await h.forward([failing]);
    const statuses = await h.statuses();
    expect(statuses.get(hex(failing))).toMatchObject({
      kind: "failed_landed",
    });
    expect(statuses.get(hex(onReturn))).toEqual({
      kind: "live",
      inputsAvailable: true,
    });
    expect(statuses.get(hex(onOutput))).toMatchObject({
      kind: "dependency_dead",
    });
  });

  it("reports the earliest conflicting spend by slot whatever the input order, as the oracle does", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const a = await h.fund();
    const b = await h.fund();
    const both = spend(h.chain, a, { inputs: [a, b] });
    const recorded = await h.record(both);
    if (recorded.kind !== "recorded") throw new Error(recorded.kind);
    // The journaled (ledger) input order: the later-spent input comes first.
    const [late, early] = recorded.intent.inputs as [OutRef, OutRef];
    const ownReplacement = spend(h.chain, early);
    const foreign = spend(h.chain, late);
    await h.record(ownReplacement);
    await h.forward([ownReplacement]);
    await h.forward([foreign]);
    const status = (await h.statuses()).get(hex(both));
    expect(status).toMatchObject({
      kind: "conflicted",
      ownSpender: true,
      spender: simTxHash(ownReplacement),
    });
    const intents = (await h.states()).values();
    const model = modelStatuses(
      h.blocks,
      h.chain.tip.point.slot,
      [...intents].map((state) => state.intent),
      new Set(),
    );
    expect(model.get(hex(both))).toEqual(observedOf(status!));
  });

  it("a child whose dependency left the journal without landing is dependency_dead and terminal at once", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const parent = spend(h.chain, await h.fund());
    await h.record(parent);
    const child = spend(h.chain, { txHash: simTxHash(parent), index: 0 });
    await h.record(child);
    expect((await h.statuses()).get(hex(child))).toEqual({
      kind: "live",
      inputsAvailable: false,
    });
    // As a prune removes a dead parent: neither journaled nor landed.
    await h.store.transaction("write", (sql) =>
      sql.query("DELETE FROM l1_intents WHERE tx_hash = ?", [
        simTxHash(parent),
      ]),
    );
    const state = (await h.states()).get(hex(child));
    expect(state).toMatchObject({
      status: { kind: "dependency_dead", dependency: simTxHash(parent) },
      terminalSlot: 0,
    });
  });

  it("one intent's status, read by key over its dependency chain, equals its entry in the whole-journal read", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const shared = await h.fund();
    const original = spend(h.chain, shared);
    const replacement = spend(h.chain, shared);
    const parent = spend(h.chain, await h.fund(), {
      invalidAfter: h.chain.nextSlot() + 1,
    });
    const child = spend(h.chain, { txHash: simTxHash(parent), index: 0 });
    const grandchild = spend(h.chain, { txHash: simTxHash(child), index: 0 });
    const landing = spend(h.chain, await h.fund());
    for (const tx of [
      original,
      replacement,
      parent,
      child,
      grandchild,
      landing,
    ])
      await h.record(tx);
    await h.forward([replacement, landing]);
    await h.forward();
    const all = await h.states();
    expect(all.get(hex(original))?.status).toMatchObject({
      kind: "conflicted",
      ownSpender: true,
    });
    expect(all.get(hex(grandchild))?.status).toMatchObject({
      kind: "dependency_dead",
    });
    for (const tx of [original, parent, child, grandchild, landing]) {
      const one = await h.store.transaction("read", (sql) =>
        deriveIntentStatusIn(sql, h.store.dialect, simTxHash(tx)),
      );
      expect(one.state).toEqual(all.get(hex(tx)));
    }
    const missing = await h.store.transaction("read", (sql) =>
      deriveIntentStatusIn(sql, h.store.dialect, Buffer.alloc(32, 9)),
    );
    expect(missing.state).toBeNull();
  });

  it("the status reads never select the signed bytes", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const tx = spend(h.chain, await h.fund());
    await h.record(tx);
    const seen: string[] = [];
    await h.store.transaction("read", async (sql) => {
      const spy = {
        ...sql,
        query: (text: string, params?: Parameters<typeof sql.query>[1]) => {
          seen.push(text);
          return sql.query(text, params);
        },
      };
      await deriveIntentStatusesIn(spy, h.store.dialect);
      await deriveIntentStatusIn(spy, h.store.dialect, simTxHash(tx));
    });
    expect(seen.length).toBeGreaterThan(0);
    expect(seen.filter((text) => text.includes("tx_cbor"))).toEqual([]);
  });

  it("a rollback deeper than the confirmation depth over a landed availability-challenge registration makes it live again with no write", async () => {
    const confirmationDepth = 1;
    await h.store.initialize(SIM_ORIGIN);
    const registration = spend(h.chain, await h.fund());
    await h.store.transaction("write", (sql) =>
      recordAtCurrentView(sql, h.store.dialect, {
        family: "script_reward_registration",
        workflowKey:
          "script_reward_registration:availability_challenge:register",
        txCbor: encodeSimTx(registration),
        isOwnOutput: (output) => output.address.equals(u.trackedAddress),
      }),
    );
    await h.forward([registration]);
    await h.forward();
    const landed = (await h.statuses()).get(hex(registration));
    expect(landed).toMatchObject({ kind: "landed", depth: 2 });
    expect((landed as { depth: number }).depth).toBeGreaterThan(
      confirmationDepth,
    );
    const events = () =>
      h.store.transaction("read", (sql) => readIntentEventsIn(sql));
    const before = await events();
    await h.backward(confirmationDepth + 1);
    expect((await h.statuses()).get(hex(registration))).toEqual({
      kind: "live",
      inputsAvailable: true,
    });
    expect(await events()).toEqual(before);
  });
});

describe("S6 edges (I1-fix)", () => {
  it("follows a landing up to depth k and calls it terminal only deeper than k", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const tx = spend(h.chain, await h.fund());
    await h.record(tx);
    const reconciler = s6(h.store);
    await h.forward([tx]);
    expect(await actionOf(reconciler, tx)).toBe("follow");
    for (let i = 1; i < K; i += 1) await h.forward();
    expect(await actionOf(reconciler, tx)).toBe("follow");
    await h.forward();
    expect(await actionOf(reconciler, tx)).toBe("terminal");
  });

  it("writes no abandon when the tip moved between the status read and the abandon write; at a still tip it abandons", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const tx = spend(h.chain, await h.fund());
    await h.forward();
    await h.record(tx);
    let rewind = true;
    const reconciler = s6(h.store, {
      wanted: async () => {
        if (rewind) await h.backward(1);
        return false;
      },
    });
    expect(await actionOf(reconciler, tx)).toBe("wait_tip_moved");
    expect((await h.statuses()).get(hex(tx))).toEqual({
      kind: "live",
      inputsAvailable: true,
    });
    const kinds = async () =>
      (
        await h.store.transaction("read", (sql) =>
          readIntentEventsIn(sql, simTxHash(tx)),
        )
      ).map((e) => e.kind);
    expect(await kinds()).toEqual(["signed"]);
    rewind = false;
    expect(await actionOf(reconciler, tx)).toBe("abandon");
    expect(await kinds()).toEqual(["signed", "abandoned"]);
  });
});

describe("retention edges (I1-fix)", () => {
  it("an abandoned intent is terminal from its abandon tip and prunes k blocks after it, not before", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const tx = spend(h.chain, await h.fund());
    await h.record(tx);
    await h.forward();
    await s6(h.store, { wanted: () => Promise.resolve(false) }).reconcile();
    const abandonSlot = h.chain.tip.point.slot;
    expect((await h.states()).get(hex(tx))).toMatchObject({
      status: { kind: "abandoned" },
      terminalSlot: abandonSlot,
    });
    for (let i = 1; i < K; i += 1) await h.forward();
    await h.store.prune();
    expect((await h.statuses()).has(hex(tx))).toBe(true);
    await h.forward();
    expect(await h.store.prune()).toMatchObject({
      deleted: { l1_intents: 1 },
    });
    expect((await h.statuses()).has(hex(tx))).toBe(false);
  });
});
