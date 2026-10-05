import { describe, expect, it } from "vitest";

import { committeePromiseCausalProbability } from "../src/availability/promise-causal-model.js";
import { evaluateCommitteePromiseTiming } from "../src/availability/promise-causal-timing.js";

// Independent architecture calculation: explicit controlled conditional model,
// not an active production policy or an empirical chain inclusion guarantee.
const model = {
  activeSlotProbability: 0.05,
  eligibleFutureSlots: 50,
  initialReadySlots: 45,
  includedToNextReadySlots: 37,
  expiryToReplacementReadySlots: 52,
  aggregateAllowedFailedAttempts: 5,
};
const probability = (
  remainingActions: number,
  remainingHorizonSlots: number,
  usedFailedAttempts = 0,
) =>
  committeePromiseCausalProbability({
    model,
    remainingActions,
    remainingHorizonSlots,
    usedFailedAttempts,
  });

describe("conditional committee promise causal model", () => {
  it("keeps full protected history while reusing only a certified healthy scheduling slot", () => {
    const candidate = {
      headerHash: "ab".repeat(28),
      commitmentDigest: "cd".repeat(32),
      kind: "potential" as const,
      remainingPublications: 1,
      remainingSettlements: 1,
      remainingCloses: 1,
      responseWindowMs: 720000,
    };
    const older = {
      ...candidate,
      headerHash: "ef".repeat(28),
      commitmentDigest: "12".repeat(32),
    };
    const input = {
      model,
      target: { numerator: 999, denominator: 1000 },
      slotLengthMs: 1000,
      upperNetworkTimeMs: 1000000,
      maximumCurrentSchedulingPromises: 1,
      fullProtectedObligations: [older],
      currentCommitmentDigests: new Set<string>(),
      currentProgress: [],
      candidate,
      usedFailedAttempts: new Map([
        [candidate.commitmentDigest, 0],
        [older.commitmentDigest, 0],
      ]),
    };
    expect(evaluateCommitteePromiseTiming(input)).toMatchObject({
      status: "sufficient",
      protectedPromises: 2,
      schedulingPromises: 1,
    });
    expect(input.fullProtectedObligations).toEqual([older]);
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        currentCommitmentDigests: new Set([older.commitmentDigest]),
      }),
    ).toMatchObject({
      status: "insufficient",
      protectedPromises: 2,
      schedulingPromises: 2,
    });
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        maximumCurrentSchedulingPromises: 2,
      }).status,
    ).toBe("unknown");
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        currentProgress: [
          { ...older, kind: "active", responseDeadlineMs: 1720000 },
        ],
      }),
    ).toMatchObject({
      status: "insufficient",
      protectedPromises: 2,
      schedulingPromises: 2,
    });
  });

  it("uses actual active prefix and upper clock horizon without renewing durable retries", () => {
    const candidate = {
      headerHash: "ab".repeat(28),
      commitmentDigest: "cd".repeat(32),
      kind: "potential" as const,
      remainingPublications: 2,
      remainingSettlements: 1,
      remainingCloses: 1,
      responseWindowMs: 720000,
    };
    const input = {
      model,
      target: { numerator: 999, denominator: 1000 },
      slotLengthMs: 1000,
      upperNetworkTimeMs: 1000000,
      maximumCurrentSchedulingPromises: 1,
      fullProtectedObligations: [candidate],
      currentCommitmentDigests: new Set([candidate.commitmentDigest]),
      currentProgress: [
        { ...candidate, kind: "active" as const, responseDeadlineMs: 1675000 },
      ],
      candidate,
      usedFailedAttempts: new Map([[candidate.commitmentDigest, 0]]),
    };
    expect(evaluateCommitteePromiseTiming(input).status).toBe("sufficient");
    expect(
      evaluateCommitteePromiseTiming({ ...input, upperNetworkTimeMs: 1001000 })
        .status,
    ).toBe("insufficient");
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        currentProgress: [
          {
            ...input.currentProgress[0]!,
            remainingPublications: 1,
            responseDeadlineMs: 1575000,
          },
        ],
      }).status,
    ).toBe("sufficient");
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        usedFailedAttempts: new Map([[candidate.commitmentDigest, 5]]),
      }).status,
    ).toBe("insufficient");
    expect(
      evaluateCommitteePromiseTiming({
        ...input,
        usedFailedAttempts: new Map(),
      }).status,
    ).toBe("unknown");
  });
  it.each([
    [3, 720, 0.9999187201135058],
    [4, 720, 0.9995221747625154],
    [3, 840, 0.9999898348681454],
    [4, 840, 0.9999356491293164],
  ])("pins %s remaining actions in %s slots", (actions, horizon, expected) => {
    const result = probability(actions, horizon);
    expect(result.successProbability).toBeCloseTo(expected, 11);
    expect(
      result.successProbability +
        result.deadlineMissProbability +
        result.retryExhaustionProbability,
    ).toBeCloseTo(1, 11);
  });
  it.each([
    [3, 575],
    [4, 675],
  ])(
    "requires the actual strict remaining horizon for %s actions",
    (actions, minimum) => {
      expect(probability(actions, minimum).successProbability).toBeGreaterThan(
        0.999,
      );
      expect(probability(actions, minimum - 1).successProbability).toBeLessThan(
        0.999,
      );
    },
  );
  it("never renews the failed-attempt allowance on the next action", () => {
    const fresh = probability(4, 720);
    const exhausted = probability(4, 720, 5);
    expect(exhausted.retryExhaustionProbability).toBeGreaterThan(0.25);
    expect(exhausted.successProbability).toBeLessThan(fresh.successProbability);
    expect(() => probability(4, 720, 6)).toThrow("supported domain");
  });
  it("excludes inclusion at the exact deadline and handles a late first attempt", () => {
    const certain = {
      ...model,
      activeSlotProbability: 1,
      initialReadySlots: 0,
    };
    expect(
      committeePromiseCausalProbability({
        model: certain,
        remainingActions: 1,
        remainingHorizonSlots: 1,
        usedFailedAttempts: 0,
      }).successProbability,
    ).toBe(0);
    expect(
      committeePromiseCausalProbability({
        model: certain,
        remainingActions: 1,
        remainingHorizonSlots: 2,
        usedFailedAttempts: 0,
      }).successProbability,
    ).toBe(1);
    expect(probability(3, 45).deadlineMissProbability).toBe(1);
  });
  it.each([0, NaN, Infinity, -1, 1.01])(
    "refuses unsupported inclusion probability %s",
    (p) => {
      expect(() =>
        committeePromiseCausalProbability({
          model: { ...model, activeSlotProbability: p },
          remainingActions: 4,
          remainingHorizonSlots: 720,
          usedFailedAttempts: 0,
        }),
      ).toThrow("supported domain");
    },
  );
});
