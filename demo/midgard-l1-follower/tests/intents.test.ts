import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  createIntentReconciler,
  decodeBlock,
  deriveIntentStatusesIn,
  type FactStore,
  type Intent,
  intentJournalProjection,
  type IntentReconcilerOptions,
  type IntentStatus,
  openSqliteFactStore,
  type OutRef,
  projectionStoreOptions,
  readIntentEventsIn,
  readIntentsByContentIn,
  readIntentsByWorkflowIn,
  recordIntentIn,
  type RecordIntentResult,
} from "../src/index.js";
import {
  cbor as c,
  encodeSimTx,
  encodeTxBody,
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simTxHash,
  simUniverse,
} from "../src/testing/index.js";

const K = 3;
const u = simUniverse();

type Harness = {
  store: FactStore;
  chain: SimChain;
  /** Appends a block of `txs` to the model and applies it to the store. */
  forward: (txs?: readonly SimTx[]) => Promise<void>;
  /** Rolls the model and the store back `depth` blocks. */
  backward: (depth: number) => Promise<void>;
  record: (
    tx: SimTx,
    extra?: Partial<{
      txCbor: Buffer;
      contentRef: Buffer;
      workflowKey: string;
    }>,
  ) => Promise<RecordIntentResult>;
  statuses: () => Promise<Map<string, IntentStatus>>;
  /** A funded tracked output, created in its own block. */
  fund: () => Promise<OutRef>;
};

const open = async (): Promise<Harness> => {
  const store = openSqliteFactStore({
    ...projectionStoreOptions(
      [intentJournalProjection],
      { securityParameter: K, trackedSet: u.tracked },
      "sqlite",
    ),
    path: ":memory:",
  });
  await store.start();
  const chain = new SimChain(u, SIM_ORIGIN);
  const harness: Harness = {
    store,
    chain,
    forward: async (txs = []) => {
      const { encoded } = chain.forward(txs);
      const applied = await store.applyBlock(decodeBlock(encoded.raw));
      expect(applied.kind).toBe("applied");
    },
    backward: async (depth) => {
      chain.backward(depth);
      const rewound = await store.rewind(chain.tip.point);
      expect(rewound.kind).toBe("rewound");
    },
    record: (tx, extra = {}) =>
      store.transaction("write", (sql) =>
        recordIntentIn(sql, store.dialect, {
          family: "commit",
          workflowKey:
            extra.workflowKey ?? `commit:${simTxHash(tx).toString("hex")}`,
          txCbor: extra.txCbor ?? encodeSimTx(tx),
          isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          ...(extra.contentRef === undefined
            ? {}
            : { contentRef: extra.contentRef }),
        }),
      ),
    statuses: async () =>
      new Map(
        (
          await store.transaction("read", (sql) =>
            deriveIntentStatusesIn(sql, store.dialect),
          )
        ).states.map((s) => [s.intent.txHash.toString("hex"), s.status]),
      ),
    fund: async () => {
      const funding: SimTx = {
        inputs: [chain.outsideInput()],
        outputs: [{ address: u.trackedAddress, lovelace: 10_000_000n }],
        nonce: chain.nonce(),
      };
      await harness.forward([funding]);
      return { txHash: simTxHash(funding), index: 0 };
    },
  };
  return harness;
};

const spend = (
  chain: SimChain,
  input: OutRef,
  extra: Partial<SimTx> = {},
): SimTx => ({
  inputs: [input],
  outputs: [
    { address: u.trackedAddress, lovelace: 4_000_000n },
    { address: u.untrackedAddress, lovelace: 1_000_000n },
  ],
  nonce: chain.nonce(),
  ...extra,
});

const hex = (tx: SimTx): string => simTxHash(tx).toString("hex");

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
    const is = (intent: Intent, tx: SimTx) =>
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
