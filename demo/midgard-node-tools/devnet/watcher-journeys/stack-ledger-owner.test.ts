import { describe, expect, it } from "vitest";

import { awaitStackLedgerOwnerFunded } from "./stack-ledger-owner.js";

const POLL_MS = 30_000;
const ABSORB_WITHIN_MS = 600_000;

/**
 * An attached stack on a clock that moves only while the drill pauses. Its
 * node absorbs a deposit `absorbsAfterMs` after the deposit's inclusion time;
 * `pending` is a deposit an earlier run recorded.
 */
const stack = (input: {
  funded?: boolean;
  pending?: number;
  absorbsAfterMs?: number;
}) => {
  let clock = 1_000_000;
  let pending = input.pending;
  const deposits: number[] = [];
  const stages: string[] = [];
  return {
    deposits,
    stages,
    pending: () => pending,
    now: () => clock,
    fund: () =>
      awaitStackLedgerOwnerFunded({
        ownerHoldsOutput: async () => {
          if (input.funded === true) return true;
          const due = pending ?? deposits[0];
          return (
            due !== undefined &&
            input.absorbsAfterMs !== undefined &&
            clock >= due + input.absorbsAfterMs
          );
        },
        readPendingDeposit: async () => pending,
        deposit: async () => {
          const inclusionTime = clock + 120_000;
          deposits.push(inclusionTime);
          pending = inclusionTime;
          return inclusionTime;
        },
        clearPendingDeposit: async () => {
          pending = undefined;
        },
        now: () => clock,
        pause: async () => {
          clock += POLL_MS;
        },
        absorbWithinMs: ABSORB_WITHIN_MS,
        onStage: (name) => stages.push(name),
      }),
  };
};

describe("funding the drill's ledger owner on an attached stack", () => {
  it("deposits nothing for an owner that already holds an output, and forgets an earlier deposit", async () => {
    const l2 = stack({ funded: true, pending: 900_000 });
    await l2.fund();
    expect(l2.deposits).toEqual([]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBe(1_000_000);
  });

  it("deposits once and waits for the stack to absorb it", async () => {
    const l2 = stack({ absorbsAfterMs: 240_000 });
    await l2.fund();
    expect(l2.deposits).toEqual([1_120_000]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBeGreaterThanOrEqual(1_120_000 + 240_000);
    expect(l2.stages[0]).toMatch(/due at 1120000/u);
  });

  it("awaits an earlier run's deposit rather than depositing again", async () => {
    const l2 = stack({ pending: 1_050_000, absorbsAfterMs: 90_000 });
    await l2.fund();
    expect(l2.deposits).toEqual([]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBeGreaterThanOrEqual(1_140_000);
  });

  it("fails, naming the deposit's due time, when the stack never absorbs it", async () => {
    const l2 = stack({});
    await expect(l2.fund()).rejects.toThrow(
      /did not absorb the ledger owner's deposit, due at 1120000/u,
    );
    expect(l2.deposits).toEqual([1_120_000]);
    expect(l2.pending()).toBe(1_120_000);
    expect(l2.now()).toBeGreaterThan(1_120_000 + ABSORB_WITHIN_MS);
    expect(l2.now()).toBeLessThanOrEqual(
      1_120_000 + ABSORB_WITHIN_MS + POLL_MS,
    );
  });
});
