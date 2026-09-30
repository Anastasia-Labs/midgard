import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  FAKE_SHIFT_START,
  FAKE_TAIL_END_TIME,
  fakeUtxo,
} from "./operator-inactivity-emulator.stalled-operator-strike-and-takeover.js";

const fakeNode = (
  label: string,
  key: string | null,
  next: string | null,
  active: SDK.ActiveOperatorDatum | null,
): SDK.ActiveOperatorNode => ({
  utxo: fakeUtxo(label),
  assetName: label,
  datum: {
    key: key === null ? "Empty" : { Key: { key } },
    next: next === null ? "Empty" : { Key: { key: next } },
    data: null,
  } as unknown as SDK.LinkedListNodeView,
  active,
});

const OP_A = "aa".repeat(28);

const OP_B = "bb".repeat(28);

const OP_C = "cc".repeat(28);

const fakeSnapshot = ({
  operators,
  currentOperator,
  strikes = 0n,
}: {
  readonly operators: readonly string[];
  readonly currentOperator: string;
  readonly strikes?: bigint;
}): SDK.InactivityDirectoryView => {
  const datumFor = (key: string): SDK.ActiveOperatorDatum => ({
    bond_unlock_time: null,
    inactivity_strikes: key === currentOperator ? strikes : 0n,
  });
  const active: SDK.ActiveOperatorNode[] = [
    fakeNode("root", null, operators[0] ?? null, null),
    ...operators.map((key, index) =>
      fakeNode(
        `active-${index}`,
        key,
        operators[index + 1] ?? null,
        datumFor(key),
      ),
    ),
  ];
  return {
    active,
    // Only the root: every operator has already activated, so no registration
    // is pending and the rewind witness is unconstrained.
    registered: [
      { ...fakeNode("registered-root", null, null, null), registered: null },
    ],
    scheduler: {
      utxo: fakeUtxo("scheduler"),
      assetName: "scheduler",
      datum: {
        ActiveOperator: {
          operator: currentOperator,
          start_time: FAKE_SHIFT_START,
        },
      },
    },
    stateQueueTail: {
      utxo: fakeUtxo("tail"),
      datum: fakeNode("tail", null, null, null).datum,
      endTime: FAKE_TAIL_END_TIME,
      isRoot: true,
    },
  } as unknown as SDK.InactivityDirectoryView;
};

describe("inactivity threshold", () => {
  const params = SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS;

  it("takes the later of the new-shift grace period and the commitment gap", () => {
    // The gap term wins when the queue has been quiet since long before the
    // shift started plus the grace period.
    const gapDominates = SDK.computeInactivityThreshold({
      shiftStartMs: FAKE_SHIFT_START,
      stateQueueTailEndTimeMs:
        FAKE_SHIFT_START + params.newShiftInactivityGracePeriodMs,
    });
    expect(gapDominates).toMatchObject({
      kind: "threshold",
      source: "block-commitment-gap",
      thresholdMs:
        FAKE_SHIFT_START +
        params.newShiftInactivityGracePeriodMs +
        params.maxInactivityBetweenBlockCommitmentsMs,
    });

    const graceDominates = SDK.computeInactivityThreshold({
      shiftStartMs: FAKE_SHIFT_START,
      stateQueueTailEndTimeMs: FAKE_TAIL_END_TIME,
    });
    expect(graceDominates).toMatchObject({
      kind: "threshold",
      source: "new-shift-grace-period",
      thresholdMs: FAKE_SHIFT_START + params.newShiftInactivityGracePeriodMs,
    });
  });

  it("dates all three neglected variants from inclusion_time", () => {
    // The event term lands midway between the grace threshold and the shift
    // end, so it both wins and stays satisfiable under any profile timing.
    const inclusionTimeMs =
      FAKE_SHIFT_START +
      (params.newShiftInactivityGracePeriodMs + params.shiftDurationMs) / 2n -
      params.userEventsNegligenceTimeoutMs;
    for (const kind of ["Deposit", "Withdrawal", "TxOrder"] as const) {
      expect(
        SDK.computeInactivityThreshold({
          shiftStartMs: FAKE_SHIFT_START,
          stateQueueTailEndTimeMs: FAKE_TAIL_END_TIME,
          neglectedEvent: { kind, inclusionTimeMs },
        }),
      ).toMatchObject({
        kind: "threshold",
        source: "neglected-user-event",
        thresholdMs: inclusionTimeMs + params.userEventsNegligenceTimeoutMs,
      });
    }
  });

  it("rejects a neglected event that precedes the state-queue tail", () => {
    expect(
      SDK.computeInactivityThreshold({
        shiftStartMs: FAKE_SHIFT_START,
        stateQueueTailEndTimeMs: FAKE_TAIL_END_TIME,
        neglectedEvent: {
          kind: "Deposit",
          inclusionTimeMs: FAKE_TAIL_END_TIME - 1n,
        },
      }),
    ).toMatchObject({
      kind: "unsatisfiable",
      reason: "neglected-event-precedes-state-queue-tail",
    });
  });

  it("rejects a threshold that does not fall before the end of the shift", () => {
    expect(
      SDK.computeInactivityThreshold({
        shiftStartMs: FAKE_SHIFT_START,
        stateQueueTailEndTimeMs: FAKE_TAIL_END_TIME,
        neglectedEvent: {
          kind: "Deposit",
          inclusionTimeMs: FAKE_SHIFT_START + params.shiftDurationMs,
        },
      }),
    ).toMatchObject({
      kind: "unsatisfiable",
      reason: "threshold-not-before-shift-end",
    });
  });
});

describe("inactivity takeover planner", () => {
  const params = SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS;
  const thresholdMs = FAKE_SHIFT_START + params.newShiftInactivityGracePeriodMs;

  it("reports no shift when the scheduler has no active operator", () => {
    const snapshot = fakeSnapshot({
      operators: [OP_A],
      currentOperator: OP_A,
    });
    expect(
      SDK.planInactivityTakeover({
        snapshot: {
          ...snapshot,
          scheduler: {
            ...snapshot.scheduler,
            datum: "NoActiveOperators",
          } as SDK.OperatorDirectorySnapshot["scheduler"],
        },
        nowMs: thresholdMs + 1n,
      }),
    ).toStrictEqual({ kind: "no-shift" });
  });

  it("is not-yet at the threshold and ready one millisecond later", () => {
    const snapshot = fakeSnapshot({
      operators: [OP_A, OP_B, OP_C],
      currentOperator: OP_C,
    });
    expect(
      SDK.planInactivityTakeover({ snapshot, nowMs: thresholdMs }),
    ).toMatchObject({
      kind: "not-yet",
      thresholdMs,
      thresholdSource: "new-shift-grace-period",
    });
    const ready = SDK.planInactivityTakeover({
      snapshot,
      nowMs: thresholdMs + 1n,
    });
    expect(ready.kind).toBe("ready");
    if (ready.kind !== "ready") {
      return;
    }
    // `interval.is_entirely_after(threshold)` with an inclusive lower bound
    // means `threshold < valid_from`.
    expect(ready.validity.validFrom).toBe(thresholdMs + 1n);
    expect(ready.validity.validTo).toBe(
      thresholdMs + 1n + SDK.DEFAULT_STRIKE_VALIDITY_WINDOW_MS,
    );
    expect(ready.newStartTime).toBe(ready.validity.validTo - 1n);
    expect(ready.newStartTime - ready.validity.validFrom).toBeLessThanOrEqual(
      params.maxValidityRangeLengthMs,
    );
  });

  it("picks GoToNext for a non-head operator and Rewind for the head", () => {
    const operators = [OP_A, OP_B, OP_C];
    const goToNext = SDK.planInactivityTakeover({
      snapshot: fakeSnapshot({ operators, currentOperator: OP_C }),
      nowMs: thresholdMs + 1n,
    });
    expect(goToNext).toMatchObject({
      kind: "ready",
      tier: "GoToNext",
      newOperatorKey: OP_B,
    });

    // `OP_A` is the element the root links to, so its shift rewinds to the
    // tail.
    const rewind = SDK.planInactivityTakeover({
      snapshot: fakeSnapshot({ operators, currentOperator: OP_A }),
      nowMs: thresholdMs + 1n,
    });
    expect(rewind).toMatchObject({
      kind: "ready",
      tier: "Rewind",
      newOperatorKey: OP_C,
    });
    if (rewind.kind === "ready" && rewind.witnesses.tier === "Rewind") {
      expect(rewind.witnesses.activeTailNode).not.toBeNull();
    }
  });

  it("rewinds a sole operator onto itself without referencing its own node", () => {
    const plan = SDK.planInactivityTakeover({
      snapshot: fakeSnapshot({ operators: [OP_A], currentOperator: OP_A }),
      nowMs: thresholdMs + 1n,
    });
    expect(plan).toMatchObject({
      kind: "ready",
      tier: "Rewind",
      newOperatorKey: OP_A,
    });
    if (plan.kind === "ready" && plan.witnesses.tier === "Rewind") {
      expect(plan.witnesses.activeTailNode).toBeNull();
    }
  });

  it("reports strikes-exhausted instead of planning a sixth strike", () => {
    const plan = SDK.planInactivityTakeover({
      snapshot: fakeSnapshot({
        operators: [OP_A, OP_B, OP_C],
        currentOperator: OP_C,
        strikes: SDK.MAX_INACTIVITY_STRIKES,
      }),
      nowMs: thresholdMs + 1n,
    });
    expect(plan).toMatchObject({
      kind: "strikes-exhausted",
      currentOperator: OP_C,
      inactivityStrikes: SDK.MAX_INACTIVITY_STRIKES,
      maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES,
    });
  });

  it("refuses an alignment hook that moves the lower bound backwards", () => {
    expect(
      SDK.planInactivityTakeover({
        snapshot: fakeSnapshot({
          operators: [OP_A, OP_B, OP_C],
          currentOperator: OP_C,
        }),
        nowMs: thresholdMs + 1n,
        alignValidFrom: (candidate) => candidate - 1n,
      }),
    ).toMatchObject({
      kind: "blocked",
      reason: "validity-alignment-regressed",
    });
  });
});
