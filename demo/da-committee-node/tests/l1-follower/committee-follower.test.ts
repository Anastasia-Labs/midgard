import {
  type FactStore,
  FOLLOWER_APPLY_STUCK,
  FOLLOWER_CATCHING_UP,
  FOLLOWER_MIGRATION_FAILED,
  FOLLOWER_NODE_UNAVAILABLE,
  FOLLOWER_PRUNE_FAILING,
  FOLLOWER_TRACKED_SET_CHANGED,
  FOLLOWER_WAITING,
  type FollowStatus,
  openSqliteFactStore,
  type PointStatus,
  projectionStoreOptions,
  WALLET_SEED_PENDING,
  type WalletSeeder,
  type WalletSeedStatus,
} from "@al-ft/midgard-l1-follower";
import { describe, expect, it } from "vitest";

import {
  committeeL1InterventionReason,
  committeeL1Source,
  L1_FOLLOWER_UNCONFIGURED,
  untilCommitteeL1SourceReady,
} from "../../src/l1/follower/l1-follower.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import { SIM_QUEUE, SIM_SLOT_TIME } from "./queue-sim.js";
const status = (fields: Partial<FollowStatus>): FollowStatus =>
  ({ readiness: [], cursor: null, ...fields }) as FollowStatus;

const fakeSeeder = (
  answer: WalletSeedStatus,
): WalletSeeder & { steps: number; seeded: boolean } => {
  const seeder = {
    steps: 0,
    seeded: false,
    owed: () => [],
    ready: () => seeder.seeded,
    addWallets: () => undefined,
    step: async () => {
      seeder.steps += 1;
      if (answer.kind === "ready") seeder.seeded = true;
      return answer;
    },
    close: () => undefined,
  };
  return seeder;
};

describe("the committee's L1 source over the follower", () => {
  const source = async (
    current: () => FollowStatus | null,
    seeder?: WalletSeeder,
  ) => {
    const store = openSqliteFactStore({
      ...projectionStoreOptions(
        [committeeProjection(SIM_QUEUE)],
        {
          securityParameter: 4,
          trackedSet: {
            addresses: new Set(),
            paymentCredentials: new Set(),
            policies: new Set(),
          },
        },
        "sqlite",
      ),
      path: ":memory:",
    });
    expect(await store.start()).toMatchObject({ kind: "ready" });
    return {
      store,
      l1: committeeL1Source({
        store,
        parameters: { confirmationDepth: 2, securityParameter: 4 },
        status: current,
        slotTime: async () => SIM_SLOT_TIME,
        ...(seeder === undefined ? {} : { seeder }),
      }),
    };
  };

  it("is catching up before the follower's first status, with no cursor", async () => {
    const { l1, store } = await source(() => null);
    try {
      expect(l1.readiness()).toEqual([
        {
          reason: FOLLOWER_CATCHING_UP,
          detail: "the follower has not started",
        },
      ]);
      expect(l1.cursorSlot()).toBeNull();
    } finally {
      await store.close();
    }
  });

  it("carries the follower's readiness and cursor", async () => {
    let current = status({
      readiness: [{ reason: "rollback_beyond_k", detail: "depth 5 > k 4" }],
      cursor: { slot: 77 } as FollowStatus["cursor"],
    });
    const { l1, store } = await source(() => current);
    try {
      expect(l1.readiness()).toEqual([
        { reason: "rollback_beyond_k", detail: "depth 5 > k 4" },
      ]);
      expect(l1.cursorSlot()).toBe(77);
      current = status({ cursor: { slot: 78 } as FollowStatus["cursor"] });
      expect(l1.readiness()).toEqual([]);
      expect(l1.cursorSlot()).toBe(78);
    } finally {
      await store.close();
    }
  });

  it("owes the wallet seed until a view read at a ready follower settles it", async () => {
    const pending = fakeSeeder({
      kind: "pending",
      reason: "seed_query_failed" as never,
      detail: "ledger state unavailable",
      wallets: [],
    });
    let current: FollowStatus | null = null;
    const { l1, store } = await source(() => current, pending);
    try {
      expect(l1.readiness()).toContainEqual({
        reason: WALLET_SEED_PENDING,
        detail: "own wallets not seeded yet",
      });
      // The follower is not ready: the seed waits for it.
      await expect(
        l1.readView({ signed: [], exitsOf: [] }),
      ).resolves.toBeNull();
      expect(pending.steps).toBe(0);

      current = status({});
      await l1.readView({ signed: [], exitsOf: [] });
      expect(pending.steps).toBe(1);
      expect(l1.readiness()).toEqual([
        {
          reason: WALLET_SEED_PENDING,
          detail: "seed_query_failed: ledger state unavailable",
        },
      ]);
    } finally {
      await store.close();
    }

    const settling = fakeSeeder({ kind: "ready" });
    const ready = await source(() => status({}), settling);
    try {
      await ready.l1.readView({ signed: [], exitsOf: [] });
      expect(settling.steps).toBe(1);
      expect(ready.l1.readiness()).toEqual([]);
    } finally {
      await ready.store.close();
    }
  });

  it("asks the store where a point stands, by its block hash bytes", async () => {
    const asked: unknown[] = [];
    const answer = { kind: "canonical" } as unknown as PointStatus;
    const l1 = committeeL1Source({
      store: {
        pointStatus: async (point: unknown) => {
          asked.push(point);
          return answer;
        },
      } as unknown as FactStore,
      parameters: { confirmationDepth: 2, securityParameter: 4 },
      status: () => null,
      slotTime: async () => SIM_SLOT_TIME,
    });
    await expect(
      l1.pointStatus({ slot: 9, blockHash: "ab".repeat(32) }),
    ).resolves.toBe(answer);
    expect(asked).toEqual([
      { slot: 9, hash: Buffer.from("ab".repeat(32), "hex") },
    ]);
  });
});

describe("waiting for the committee's L1 source", () => {
  const scripted = (...answers: { reason: string; detail: string }[][]) => {
    let calls = 0;
    return {
      calls: () => calls,
      readiness: () => {
        const answer = answers[Math.min(calls, answers.length - 1)]!;
        calls += 1;
        return answer;
      },
    };
  };

  it("resolves at once when nothing but an owed seed holds it", async () => {
    const source = scripted([{ reason: WALLET_SEED_PENDING, detail: "owed" }]);
    await expect(
      untilCommitteeL1SourceReady(source, { pollMs: 1 }),
    ).resolves.toBeUndefined();
    expect(source.calls()).toBe(1);
  });

  it.each([FOLLOWER_CATCHING_UP, FOLLOWER_WAITING, FOLLOWER_NODE_UNAVAILABLE])(
    "waits while the follower is %s, then resolves",
    async (reason) => {
      const source = scripted(
        [{ reason, detail: "behind" }],
        [{ reason, detail: "behind" }],
        [],
      );
      await expect(
        untilCommitteeL1SourceReady(source, { pollMs: 1 }),
      ).resolves.toBeUndefined();
      expect(source.calls()).toBe(3);
    },
  );

  it.each(["rollback_beyond_k", L1_FOLLOWER_UNCONFIGURED])(
    "holds on %s, which no wait clears, reporting it every poll and never giving up",
    async (reason) => {
      const held = { reason, detail: "named" };
      const source = scripted(
        [{ reason: FOLLOWER_CATCHING_UP, detail: "behind" }],
        [held],
        [held],
        [held],
        // An operator repaired it: the wait resolves, the process never left.
        [],
      );
      const reported: string[] = [];
      await expect(
        untilCommitteeL1SourceReady(source, {
          pollMs: 1,
          onHeld: (reasons) =>
            reported.push(
              committeeL1InterventionReason(reasons)?.reason ?? "transient",
            ),
        }),
      ).resolves.toBeUndefined();
      expect(reported).toEqual(["transient", reason, reason, reason]);
      expect(source.calls()).toBe(5);
    },
  );

  it("stops only when its caller's onHeld throws (a one-shot run)", async () => {
    const source = scripted([{ reason: "rollback_beyond_k", detail: "named" }]);
    await expect(
      untilCommitteeL1SourceReady(source, {
        pollMs: 1,
        onHeld: (reasons) => {
          const stuck = committeeL1InterventionReason(reasons);
          if (stuck !== undefined)
            throw new Error(`${stuck.reason}: ${stuck.detail}`);
        },
      }),
    ).rejects.toThrow("rollback_beyond_k: named");
    expect(source.calls()).toBe(1);
  });

  it("names only reasons no wait clears as interventions", () => {
    expect(
      committeeL1InterventionReason([
        { reason: FOLLOWER_CATCHING_UP, detail: "a" },
        { reason: FOLLOWER_WAITING, detail: "b" },
        { reason: FOLLOWER_NODE_UNAVAILABLE, detail: "node_unreachable: d" },
        { reason: WALLET_SEED_PENDING, detail: "c" },
      ]),
    ).toBeUndefined();
    expect(
      committeeL1InterventionReason([
        { reason: FOLLOWER_WAITING, detail: "b" },
        { reason: "rollback_beyond_k", detail: "deep" },
      ]),
    ).toEqual({ reason: "rollback_beyond_k", detail: "deep" });
  });

  it.each([
    [FOLLOWER_CATCHING_UP, "transient"],
    [FOLLOWER_WAITING, "transient"],
    [FOLLOWER_NODE_UNAVAILABLE, "transient"],
    [FOLLOWER_TRACKED_SET_CHANGED, "transient"],
    [WALLET_SEED_PENDING, "transient"],
    [FOLLOWER_APPLY_STUCK, "intervention"],
    [FOLLOWER_MIGRATION_FAILED, "intervention"],
    [FOLLOWER_PRUNE_FAILING, "intervention"],
    [L1_FOLLOWER_UNCONFIGURED, "intervention"],
    ["rollback_beyond_k", "intervention"],
    ["intersection_outside_history", "intervention"],
    ["origin_after_protocol_init", "intervention"],
    ["origin_not_on_chain", "intervention"],
    ["origin_mismatch", "intervention"],
    ["store_integrity", "intervention"],
  ] as const)("classifies %s as %s", (reason, kind) => {
    const found = committeeL1InterventionReason([{ reason, detail: "d" }]);
    expect(found === undefined ? "transient" : "intervention").toBe(kind);
  });
});
