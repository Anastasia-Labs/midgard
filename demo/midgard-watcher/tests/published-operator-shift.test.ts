import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { OperatorFundingShortfall } from "midgard-node/transactions/operators/funding-preflight";
import { describe, expect, it } from "vitest";

import {
  awaitOperatorShift,
  continuedActiveOperatorDatum,
  operatorLifecycleStep,
  type OperatorShiftAppointment,
} from "./support/published-block-actor.operator-shift.js";

const SHIFT_MS = 3_600_000n;
const COVER_MS = 60_000;
const OPERATOR_2 = "02".repeat(28);
const OUR_NODE = "01".repeat(28);

type Refresh = (
  target: bigint,
  now: number,
) =>
  | { readonly appointment: OperatorShiftAppointment }
  | { readonly notDueForMs: number }
  | { readonly fails: Error; readonly appointment?: OperatorShiftAppointment };

/**
 * The scheduler as L1 holds it, on a ledger clock that moves only when the
 * operator waits. A refresh answers by the script `refresh`; by default it
 * appoints operator-2 from the refresh's own time, as the node's does.
 */
const scheduler = (
  appointment: OperatorShiftAppointment,
  start: number,
  refresh: Refresh = (_target, now) => ({
    appointment: { operator: OPERATOR_2, startTime: BigInt(now) },
  }),
) => {
  let clock = start;
  let current = appointment;
  const refreshes: bigint[] = [];
  let reads = 0;
  const stages: string[] = [];
  return {
    refreshes,
    stages,
    now: () => clock,
    appoint: (next: OperatorShiftAppointment) => {
      current = next;
    },
    wait: () =>
      awaitOperatorShift(OPERATOR_2, COVER_MS, {
        readAppointment: async () => {
          // An operator that reads without end never waits for its shift.
          if (++reads > 200) throw new Error("read the scheduler without end");
          return current;
        },
        refresh: async (target) => {
          refreshes.push(target);
          const answer = refresh(target, clock);
          if ("notDueForMs" in answer) return answer;
          if (answer.appointment !== undefined) current = answer.appointment;
          if ("fails" in answer) throw answer.fails;
          return { kind: "aligned" };
        },
        now: () => clock,
        awaitLedgerTime: async (timeMs) => {
          clock = Math.max(clock, timeMs);
        },
        shiftDurationMs: SHIFT_MS,
        onStage: (name) => stages.push(name),
      }),
  };
};

describe("an operator sharing the scheduler", () => {
  it("waits out our node's shift, then refreshes the scheduler into its own", async () => {
    const ourShiftEnd = 10_000_000 + Number(SHIFT_MS);
    const l1 = scheduler(
      { operator: OUR_NODE, startTime: 10_000_000n },
      10_600_000,
    );
    await l1.wait();
    expect(l1.refreshes).toEqual([BigInt(ourShiftEnd + COVER_MS)]);
    expect(l1.now()).toBe(ourShiftEnd + 1);
    expect(l1.stages[0]).toMatch(/awaiting the end of .* shift/u);
  });

  it("commits at once in its own shift while it covers the commit", async () => {
    const l1 = scheduler(
      { operator: OPERATOR_2, startTime: 10_000_000n },
      10_600_000,
    );
    await l1.wait();
    expect(l1.refreshes).toEqual([]);
    expect(l1.now()).toBe(10_600_000);
  });

  it("waits for its own appointment that starts later", async () => {
    const l1 = scheduler(
      { operator: OPERATOR_2, startTime: 11_000_000n },
      10_600_000,
    );
    await l1.wait();
    expect(l1.refreshes).toEqual([]);
    expect(l1.now()).toBe(11_000_001);
  });

  it("waits for a refresh that is not due yet, then submits it", async () => {
    let asked = 0;
    const l1 = scheduler(
      { operator: OUR_NODE, startTime: 0n },
      10_600_000,
      (_target, now) =>
        asked++ === 0
          ? { notDueForMs: 30_000 }
          : { appointment: { operator: OPERATOR_2, startTime: BigInt(now) } },
    );
    await l1.wait();
    expect(l1.refreshes).toHaveLength(2);
    expect(l1.now()).toBe(10_630_001);
  });

  it("waits again when our node took the next shift first", async () => {
    let asked = 0;
    const ourNextShift = BigInt(10_600_000);
    const l1 = scheduler(
      { operator: OUR_NODE, startTime: ourNextShift - SHIFT_MS },
      10_600_000,
      (_target, now) =>
        asked++ === 0
          ? {
              fails: new Error("scheduler input already spent"),
              appointment: { operator: OUR_NODE, startTime: ourNextShift },
            }
          : { appointment: { operator: OPERATOR_2, startTime: BigInt(now) } },
    );
    await l1.wait();
    expect(l1.refreshes).toHaveLength(2);
    expect(l1.now()).toBe(Number(ourNextShift + SHIFT_MS) + 1);
    expect(l1.stages.some((stage) => stage.includes("waiting again"))).toBe(
      true,
    );
  });

  it("fails on a refresh failure no other refresh explains", async () => {
    const refused = new Error("not the next scheduled operator");
    const l1 = scheduler(
      { operator: OUR_NODE, startTime: 0n },
      10_600_000,
      () => ({
        fails: refused,
      }),
    );
    await expect(l1.wait()).rejects.toBe(refused);
    expect(l1.refreshes).toHaveLength(1);
  });

  it("refuses work that no single shift can cover", async () => {
    const reads: string[] = [];
    await expect(
      awaitOperatorShift(OPERATOR_2, Number(SHIFT_MS) + 1, {
        readAppointment: async () => {
          reads.push("read");
          if (reads.length > 200)
            throw new Error("read the scheduler without end");
          return { operator: OPERATOR_2, startTime: 0n };
        },
        refresh: async () => ({ kind: "aligned" }),
        now: () => 0,
        awaitLedgerTime: async () => {},
        shiftDurationMs: SHIFT_MS,
        onStage: () => {},
      }),
    ).rejects.toThrow(/shift covers/u);
    expect(reads).toEqual([]);
  });

  it("fails when the scheduler does not move for two whole shifts", async () => {
    const l1 = scheduler(
      { operator: OUR_NODE, startTime: 0n },
      10_600_000,
      () => ({
        notDueForMs: 600_000,
      }),
    );
    await expect(l1.wait()).rejects.toThrow(/for two shifts/u);
    expect(l1.now()).toBeGreaterThan(10_600_000 + 2 * Number(SHIFT_MS));
    expect(l1.now()).toBeLessThanOrEqual(
      10_600_000 + 2 * Number(SHIFT_MS) + 600_000,
    );
  });
});

describe("an operator's header commit", () => {
  const policyId = "ab".repeat(28);
  const activeNode = (data: {
    bond_unlock_time: bigint | null;
    inactivity_strikes: bigint;
  }): UTxO => ({
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address: "addr_test1wpfq9z5cjufs4m5f5qhyrqffwyrq2wkyrg3lnsupryeq43ga7swjc",
    assets: {
      lovelace: 5_000_000n,
      [toUnit(
        policyId,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + OPERATOR_2,
      )]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: OPERATOR_2 } },
      next: { Key: { key: OUR_NODE } },
      data: Data.castTo(data, SDK.ActiveOperatorDatum),
    }),
  });
  const continued = async (input: UTxO, blockUnlockTime: bigint) => {
    const view = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO({
        ...input,
        datum: await continuedActiveOperatorDatum(input, blockUnlockTime),
      }),
    );
    return {
      ...view,
      data: Data.castFrom(view.data, SDK.ActiveOperatorDatum),
    };
  };

  it("keeps the operator's link and inactivity strikes, and raises its bond hold to the block's", async () => {
    await expect(
      continued(
        activeNode({ bond_unlock_time: 1_000n, inactivity_strikes: 3n }),
        5_000n,
      ),
    ).resolves.toEqual({
      key: { Key: { key: OPERATOR_2 } },
      next: { Key: { key: OUR_NODE } },
      data: { bond_unlock_time: 5_000n, inactivity_strikes: 3n },
    });
  });

  it.each([
    ["a later hold", 9_000n, 9_000n],
    ["no hold", null, 5_000n],
  ])("keeps the monotonic bond hold over %s", async (_, held, expected) => {
    const { data } = await continued(
      activeNode({ bond_unlock_time: held, inactivity_strikes: 0n }),
      5_000n,
    );
    expect(data.bond_unlock_time).toBe(expected);
  });
});

describe("an operator lifecycle step posting a bond", () => {
  const shortfall = new OperatorFundingShortfall({
    label: "operator registration",
    walletAddress: "addr_test1operator",
    availableLovelace: 100_000_000n,
    tokenChangeLovelace: 0n,
    requiredLovelace: 905_000_000n,
    collateralLovelace: 5_000_000n,
    shortfallLovelace: 805_000_000n,
    sufficient: false,
  });
  /** A wallet holding `balance`, which a bond step needs `needs` of. */
  const wallet = (balance: bigint, needs = 905_000_000n) => {
    let held = balance;
    const funded: bigint[] = [];
    return {
      funded,
      step: Effect.suspend(() =>
        held >= needs ? Effect.succeed("registered") : Effect.fail(shortfall),
      ),
      fund: async (lovelace: bigint) => {
        funded.push(lovelace);
        held += lovelace;
      },
    };
  };

  it("funds the operator's shortfall once and runs the step again", async () => {
    const operator = wallet(100_000_000n);
    await expect(
      operatorLifecycleStep(operator.step, operator.fund),
    ).resolves.toBe("registered");
    expect(operator.funded).toEqual([805_000_000n]);
  });

  it("fails on the shortfall where nobody funds the operator", async () => {
    await expect(
      operatorLifecycleStep(wallet(100_000_000n).step, undefined),
    ).rejects.toBe(shortfall);
  });

  it("funds nothing for a failure other than a shortfall", async () => {
    const funded: bigint[] = [];
    const refused = new Error("registration refused");
    await expect(
      operatorLifecycleStep(Effect.fail(refused), async (lovelace) => {
        funded.push(lovelace);
      }),
    ).rejects.toBe(refused);
    expect(funded).toEqual([]);
  });
});
