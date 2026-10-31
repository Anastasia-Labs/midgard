import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  planDaAvailabilityTimeout,
  selectDaAvailabilityCollateral,
} from "../src/availability-challenge-transactions.js";
import {
  BOND,
  FLOOR,
  KEY_ADDRESS,
  PARAMETERS,
  PENALTY,
  RECORD,
  refusal,
} from "./availability-challenge-transactions.challenge-record-min-ada.js";

describe("timeout output arithmetic", () => {
  const REMAINING = 9_000_000n;
  const C = 400_000n;
  const conserved = (pool: bigint, c: bigint) => {
    const plan = planDaAvailabilityTimeout({
      poolLovelace: pool,
      remainingChallengerLovelace: REMAINING,
      challengerFeeLovelace: c,
      parameters: PARAMETERS,
    });
    // Record + terminal + pool in == challenger + pool out + fee.
    expect(RECORD + REMAINING + pool).toBe(
      plan.challengerOutputLovelace +
        plan.poolOutputLovelace +
        plan.feeLovelace,
    );
    expect(plan.feeLovelace).toBe(plan.feePart + c);
    expect(plan.challengerOutputLovelace).toBe(
      REMAINING - c + RECORD + plan.payout,
    );
    return plan;
  };

  it("full pool: takes one da_bond, burns the penalty, pays the rest", () => {
    const pool = FLOOR + 3n * BOND;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(BOND);
    expect(plan.feePart).toBe(PENALTY);
    expect(plan.payout).toBe(BOND - PENALTY);
    expect(plan.poolOutputLovelace).toBe(pool - BOND);
    expect(conserved(pool, 0n).feeLovelace).toBe(PENALTY);
  });

  it("partial pool above the penalty: takes the backing, pays taken - penalty", () => {
    const pool = FLOOR + PENALTY + 7n;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(PENALTY + 7n);
    expect(plan.feePart).toBe(PENALTY);
    // A dust payout merges into the challenger output; it is not refused.
    expect(plan.payout).toBe(7n);
    expect(plan.poolOutputLovelace).toBe(FLOOR);
  });

  it("partial pool below the penalty: all backing is fee, no payout", () => {
    const pool = FLOOR + PENALTY / 2n;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(PENALTY / 2n);
    expect(plan.feePart).toBe(PENALTY / 2n);
    expect(plan.payout).toBe(0n);
    expect(plan.poolOutputLovelace).toBe(FLOOR);
  });

  it("empty pool: nothing taken, the challenger pays the whole fee", () => {
    for (const pool of [FLOOR, FLOOR - 1n, 0n]) {
      const plan = conserved(pool, C);
      expect(plan.taken).toBe(0n);
      expect(plan.feePart).toBe(0n);
      expect(plan.payout).toBe(0n);
      expect(plan.poolOutputLovelace).toBe(pool);
      expect(plan.feeLovelace).toBe(C);
    }
    expect(() =>
      planDaAvailabilityTimeout({
        poolLovelace: FLOOR,
        remainingChallengerLovelace: REMAINING,
        challengerFeeLovelace: 0n,
        parameters: PARAMETERS,
      }),
    ).toThrow(/fee must be positive/);
  });

  it("refuses a contribution above the remaining challenger reserve", () => {
    expect(() =>
      planDaAvailabilityTimeout({
        poolLovelace: FLOOR,
        remainingChallengerLovelace: C - 1n,
        challengerFeeLovelace: C,
        parameters: PARAMETERS,
      }),
    ).toThrow(/remaining challenger reserve/);
  });
});

describe("collateral selection", () => {
  const coin = (index: number, lovelace: bigint): UTxO => ({
    txHash: "aa".repeat(32),
    outputIndex: index,
    address: KEY_ADDRESS,
    assets: { lovelace },
  });

  it("takes the largest coins first, at most three", () => {
    const picked = selectDaAvailabilityCollateral({
      candidates: [
        coin(0, 2_000_000n),
        coin(1, 9_000_000n),
        coin(2, 3_000_000n),
      ],
      requiredLovelace: 10_000_000n,
      minimumReturnLovelace: 1_000_000n,
    });
    expect(picked.map((u) => u.outputIndex)).toEqual([1, 2]);
  });

  it("accepts an exact cover and refuses a sub-minimum return", () => {
    expect(
      selectDaAvailabilityCollateral({
        candidates: [coin(0, 5_000_000n)],
        requiredLovelace: 5_000_000n,
        minimumReturnLovelace: 1_000_000n,
      }),
    ).toHaveLength(1);
    refusal(
      () =>
        selectDaAvailabilityCollateral({
          candidates: [coin(0, 5_500_000n)],
          requiredLovelace: 5_000_000n,
          minimumReturnLovelace: 1_000_000n,
        }),
      "collateral-insufficient",
    );
  });

  it("refuses when three coins cannot cover the requirement", () => {
    refusal(
      () =>
        selectDaAvailabilityCollateral({
          candidates: [1, 2, 3, 4].map((i) => coin(i, 2_000_000n)),
          requiredLovelace: 7_000_000n,
          minimumReturnLovelace: 1_000_000n,
        }),
      "collateral-insufficient",
    );
  });
});
