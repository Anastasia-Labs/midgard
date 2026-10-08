import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  createIntentReconciler,
  type FactStore,
  type Intent,
  type IntentHead,
  type IntentReconcilerOptions,
  readIntentEventsIn,
  readIntentsByContentIn,
  readIntentsByWorkflowIn,
} from "../src/index.js";
import {
  cbor as c,
  encodeSimTx,
  encodeTxBody,
  SIM_ORIGIN,
  type SimTx,
  simTxHash,
} from "../src/testing/index.js";
import {
  type Harness,
  hex,
  K,
  open,
  spend,
} from "./support/intents-harness.js";

let h: Harness;

beforeEach(async () => {
  h = await open();
});

afterEach(async () => {
  await h.store.close();
});

describe("recording an intent (§8.2)", () => {
  it("needs a view: nothing is recorded before the follower is initialized", async () => {
    const tx = spend(h.chain, h.chain.outsideInput());
    expect(await h.record(tx)).toMatchObject({ kind: "no_view" });
  });

  it("records the exact signed bytes once, with its own outputs and a signed event", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const input = await h.fund();
    const tx = spend(h.chain, input, { invalidAfter: 99_999 });
    const recorded = await h.record(tx, {
      contentRef: Buffer.alloc(32, 7),
      workflowKey: "commit:block-7",
    });
    expect(recorded).toMatchObject({ kind: "recorded" });
    if (recorded.kind !== "recorded") return;
    const intent = recorded.intent;
    expect(intent.txCbor.equals(encodeSimTx(tx))).toBe(true);
    expect(intent.txHash.equals(simTxHash(tx))).toBe(true);
    expect(intent.inputs).toEqual([input]);
    expect(intent.validToSlot).toBe(99_999);
    expect(intent.ownOutputs.map((o) => o.index)).toEqual([0]);
    expect(intent.dependsOn).toEqual([]);
    const cursor = await h.store.cursor();
    expect(intent.built.point.hash.equals(cursor!.point.hash)).toBe(true);
    const events = await h.store.transaction("read", (sql) =>
      readIntentEventsIn(sql, intent.txHash),
    );
    expect(events.map((e) => [e.kind, e.tipSlot])).toEqual([
      ["signed", cursor!.point.slot],
    ]);
    const byWorkflow = await h.store.transaction("read", (sql) =>
      readIntentsByWorkflowIn(sql, h.store.dialect, "commit:block-7"),
    );
    const byContent = await h.store.transaction("read", (sql) =>
      readIntentsByContentIn(sql, h.store.dialect, Buffer.alloc(32, 7)),
    );
    expect(byWorkflow.map((i) => i.txHash)).toEqual([intent.txHash]);
    expect(byContent.map((i) => i.txHash)).toEqual([intent.txHash]);

    // Idempotent: the first bytes stand, even under another witness set.
    expect(await h.record(tx)).toMatchObject({
      kind: "already_recorded",
      identical: true,
    });
    const rewitnessed = c.array(
      encodeTxBody(tx),
      c.map([c.uint(7), c.array()]),
      c.bool(true),
      c.nul,
    );
    const again = await h.record(tx, { txCbor: rewitnessed });
    expect(again).toMatchObject({ kind: "already_recorded", identical: false });
    if (again.kind === "already_recorded")
      expect(again.intent.txCbor.equals(encodeSimTx(tx))).toBe(true);
  });

  it("refuses bytes it cannot decode and a bare body", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const input = await h.fund();
    expect(
      await h.record(spend(h.chain, input), { txCbor: Buffer.of(1, 2, 3) }),
    ).toMatchObject({ kind: "undecodable" });
    const tx = spend(h.chain, input);
    expect(await h.record(tx, { txCbor: encodeTxBody(tx) })).toMatchObject({
      kind: "undecodable",
    });
  });

  it("records a validity bound up to the safe slot range and refuses one past it", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const safe = BigInt(Number.MAX_SAFE_INTEGER);
    const kept = await h.record(
      spend(h.chain, await h.fund(), { invalidAfter: safe }),
    );
    expect(kept).toMatchObject({ kind: "recorded" });
    if (kept.kind === "recorded")
      expect(kept.intent.validToSlot).toBe(Number.MAX_SAFE_INTEGER);
    expect(
      await h.record(
        spend(h.chain, await h.fund(), { invalidAfter: safe + 1n }),
      ),
    ).toMatchObject({ kind: "undecodable" });
    expect(
      await h.record(
        spend(h.chain, await h.fund(), { invalidBefore: 2n ** 64n - 1n }),
      ),
    ).toMatchObject({ kind: "undecodable" });
  });

  it("refuses an input that is neither a fact nor a recorded intent's output", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const outside = h.chain.outsideInput();
    const refused = await h.record(spend(h.chain, outside));
    expect(refused).toMatchObject({ kind: "input_untracked" });
    if (refused.kind === "input_untracked")
      expect(refused.untracked).toEqual([outside]);
    // A recorded parent's output is accepted, and makes it a dependency.
    const parent = spend(h.chain, await h.fund());
    expect(await h.record(parent)).toMatchObject({ kind: "recorded" });
    const child = spend(h.chain, { txHash: simTxHash(parent), index: 1 });
    const recorded = await h.record(child);
    expect(recorded).toMatchObject({ kind: "recorded" });
    if (recorded.kind === "recorded")
      expect(recorded.intent.dependsOn).toEqual([simTxHash(parent)]);
    // An index the parent does not create is refused.
    expect(
      await h.record(spend(h.chain, { txHash: simTxHash(parent), index: 2 })),
    ).toMatchObject({ kind: "input_untracked" });
  });
});

describe("derived status (§8.2)", () => {
  it("lands, un-lands on a rollback with no write, and lands again", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const tx = spend(h.chain, await h.fund());
    await h.record(tx);
    expect((await h.statuses()).get(hex(tx))).toEqual({
      kind: "live",
      inputsAvailable: true,
    });
    await h.forward([tx]);
    await h.forward();
    expect((await h.statuses()).get(hex(tx))).toMatchObject({
      kind: "landed",
      depth: 2,
    });
    const events = () =>
      h.store.transaction("read", (sql) => readIntentEventsIn(sql));
    const before = await events();
    await h.backward(2);
    expect((await h.statuses()).get(hex(tx))).toEqual({
      kind: "live",
      inputsAvailable: true,
    });
    expect(await events()).toEqual(before);
    await h.forward([tx]);
    expect((await h.statuses()).get(hex(tx))).toMatchObject({
      kind: "landed",
      depth: 1,
    });
  });

  it("is conflicted by a foreign spend and superseded by its own replacement", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const input = await h.fund();
    const original = spend(h.chain, input);
    const replacement = spend(h.chain, input);
    const foreign = spend(h.chain, input);
    await h.record(original);
    await h.record(replacement);
    await h.forward([replacement]);
    let statuses = await h.statuses();
    expect(statuses.get(hex(original))).toMatchObject({
      kind: "conflicted",
      ownSpender: true,
    });
    await h.backward(1);
    await h.forward([foreign]);
    statuses = await h.statuses();
    expect(statuses.get(hex(original))).toMatchObject({
      kind: "conflicted",
      ownSpender: false,
    });
    expect(statuses.get(hex(replacement))).toMatchObject({
      kind: "conflicted",
      ownSpender: false,
    });
  });

  it("expires at its validity end, kills its dependants, and dies phase-2 failed", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const input = await h.fund();
    const collateral = await h.fund();
    const parent = spend(h.chain, input, {
      invalidAfter: h.chain.nextSlot() + 1,
    });
    await h.record(parent);
    const child = spend(h.chain, { txHash: simTxHash(parent), index: 0 });
    await h.record(child);
    const failing = spend(h.chain, await h.fund(), {
      isValid: false,
      collaterals: [collateral],
    });
    await h.record(failing);
    await h.forward([failing]);
    await h.forward();
    const statuses = await h.statuses();
    expect(statuses.get(hex(parent))).toMatchObject({ kind: "expired" });
    expect(statuses.get(hex(child))).toMatchObject({
      kind: "dependency_dead",
    });
    expect(statuses.get(hex(failing))).toMatchObject({
      kind: "failed_landed",
    });
  });
});

describe("S6 reconciliation (§8.3)", () => {
  const reconciler = (
    store: FactStore,
    overrides: Partial<IntentReconcilerOptions> & { sent: Intent[] },
  ) =>
    createIntentReconciler({
      dialect: store.dialect,
      transaction: (mode, run) => store.transaction(mode, run),
      securityParameter: K,
      inMempool: () => Promise.resolve(false),
      wanted: () => Promise.resolve(true),
      submit: (intent) => {
        overrides.sent.push(intent);
        return Promise.resolve({ kind: "accepted" });
      },
      ...overrides,
    });

  it("resubmits a live intent's exact bytes once per tip and never a dead one", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const input = await h.fund();
    const live = spend(h.chain, await h.fund());
    const dead = spend(h.chain, input);
    await h.record(live);
    await h.record(dead);
    await h.forward([spend(h.chain, input)]);
    const sent: Intent[] = [];
    const s6 = reconciler(h.store, { sent });
    const first = await s6.reconcile();
    expect(
      first.intents.map((i) => [i.intent.txHash.toString("hex"), i.action]),
    ).toEqual(
      expect.arrayContaining([
        [hex(live), "resubmit"],
        [hex(dead), "dead"],
      ]),
    );
    expect(sent.map((i) => i.txCbor)).toEqual([encodeSimTx(live)]);
    const second = await s6.reconcile();
    expect(
      second.intents.find((i) => i.intent.txHash.equals(simTxHash(live)))
        ?.action,
    ).toBe("wait_attempted");
    expect(sent).toHaveLength(1);
    await h.forward();
    await s6.reconcile();
    expect(sent.map((i) => i.txCbor)).toEqual([
      encodeSimTx(live),
      encodeSimTx(live),
    ]);
    const events = await h.store.transaction("read", (sql) =>
      readIntentEventsIn(sql, simTxHash(live)),
    );
    expect(events.map((e) => e.kind)).toEqual([
      "signed",
      "submit_attempt",
      "submit_attempt",
    ]);
  });

  it("waits in the mempool, abandons on a false predicate, records refusals, and survives transient failures", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const pooled = spend(h.chain, await h.fund());
    const unwanted = spend(h.chain, await h.fund());
    const refused = spend(h.chain, await h.fund());
    const flaky = spend(h.chain, await h.fund());
    for (const tx of [pooled, unwanted, refused, flaky]) await h.record(tx);
    const sent: Intent[] = [];
    const is = (intent: IntentHead, tx: SimTx) =>
      intent.txHash.equals(simTxHash(tx));
    const s6 = reconciler(h.store, {
      sent,
      inMempool: (intent) => Promise.resolve(is(intent, pooled)),
      wanted: (state) => Promise.resolve(!is(state.intent, unwanted)),
      submit: (intent) => {
        if (is(intent, flaky))
          return Promise.reject(new Error("socket closed"));
        sent.push(intent);
        return Promise.resolve(
          is(intent, refused)
            ? { kind: "rejected", detail: "BadInputsUTxO" }
            : { kind: "accepted" },
        );
      },
    });
    const report = await s6.reconcile();
    const action = (tx: SimTx) => report.intents.find((i) => is(i.intent, tx));
    expect(action(pooled)?.action).toBe("wait_in_mempool");
    expect(action(unwanted)?.action).toBe("abandon");
    expect(action(refused)?.action).toBe("resubmit");
    expect(action(flaky)).toMatchObject({
      action: "wait_transient",
      error: "socket closed",
    });
    const statuses = await h.statuses();
    expect(statuses.get(hex(unwanted))).toEqual({ kind: "abandoned" });
    expect(statuses.get(hex(refused))).toMatchObject({ kind: "live" });
    const refusedEvents = await h.store.transaction("read", (sql) =>
      readIntentEventsIn(sql, simTxHash(refused)),
    );
    expect(refusedEvents.map((e) => [e.kind, e.detail])).toEqual([
      ["signed", null],
      ["submit_attempt", { generation: 0 }],
      ["submit_rejected", { rejection: "BadInputsUTxO" }],
    ]);
    // Abandoned is dead: never sent again.
    sent.length = 0;
    await h.forward();
    await s6.reconcile();
    expect(sent.some((i) => is(i, unwanted))).toBe(false);
  });

  it("sends a not-yet-valid intent's exact bytes again at each tip until the ledger takes them and they land, and never sends one at or past its validity end", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const early = await h.fund();
    const closingInput = await h.fund();
    const tip = () => h.chain.tip.point.slot;
    const notYet = spend(h.chain, early, { invalidBefore: tip() + 5 });
    const closing = spend(h.chain, closingInput, { invalidAfter: tip() + 3 });
    await h.record(notYet);
    await h.record(closing);
    const sent: Intent[] = [];
    const sentAt: Array<Readonly<{ tx: string; slot: number }>> = [];
    const s6 = reconciler(h.store, {
      sent,
      // The ledger refuses a body below its lower bound, as a node does.
      submit: (intent) => {
        sent.push(intent);
        sentAt.push({ tx: intent.txHash.toString("hex"), slot: tip() });
        return Promise.resolve(
          intent.txHash.equals(simTxHash(notYet)) &&
            tip() < notYet.invalidBefore!
            ? { kind: "rejected", detail: "OutsideValidityInterval" }
            : { kind: "accepted" },
        );
      },
    });
    let tips = 0;
    while (tip() < notYet.invalidBefore!) {
      await s6.reconcile();
      tips += 1;
      await h.forward();
    }
    await s6.reconcile();
    const notYetSends = sentAt.filter((s) => s.tx === hex(notYet));
    expect(notYetSends).toHaveLength(tips + 1);
    expect(
      sent
        .filter((i) => i.txHash.equals(simTxHash(notYet)))
        .every((i) => i.txCbor.equals(encodeSimTx(notYet))),
    ).toBe(true);
    const closingSends = sentAt.filter((s) => s.tx === hex(closing));
    expect(closingSends.length).toBeGreaterThan(0);
    expect(closingSends.every((s) => s.slot < closing.invalidAfter!)).toBe(
      true,
    );
    await h.forward([notYet]);
    sent.length = 0;
    await s6.reconcile();
    await h.forward();
    await s6.reconcile();
    expect(sent).toEqual([]);
    const statuses = await h.statuses();
    expect(statuses.get(hex(notYet))).toMatchObject({ kind: "landed" });
    expect(statuses.get(hex(closing))).toMatchObject({ kind: "expired" });
  });
});

describe("retention", () => {
  it("prunes an intent k blocks after it is terminal, with its events, and keeps a live one", async () => {
    await h.store.initialize(SIM_ORIGIN);
    const landed = spend(h.chain, await h.fund());
    const live = spend(h.chain, await h.fund());
    await h.record(landed);
    await h.record(live);
    await h.forward([landed]);
    // Depth k: not final, kept.
    for (let i = 1; i < K; i += 1) await h.forward();
    const kept = await h.store.prune();
    expect(kept).toMatchObject({ done: true });
    expect([...(await h.statuses()).keys()].sort()).toEqual(
      [hex(landed), hex(live)].sort(),
    );
    // Depth k + 1: final, pruned with its events.
    await h.forward();
    const pruned = await h.store.prune();
    expect(pruned).toMatchObject({ deleted: { l1_intents: 1 } });
    expect([...(await h.statuses()).keys()]).toEqual([hex(live)]);
    const events = await h.store.transaction("read", (sql) =>
      readIntentEventsIn(sql, simTxHash(landed)),
    );
    expect(events).toEqual([]);
  });
});
