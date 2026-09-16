/**
 * Stalled-operator strike and takeover, against the real compiled validators.
 *
 * The emulator does not run phase-2 scripts, so every positive case here is
 * proven by submitting the transaction (`localUPLCEval: true` evaluates the
 * deployed scheduler and active-operators scripts while the transaction is
 * completed) and every negative case is proven by that same evaluation
 * refusing to complete it. `expectInactivityStrikeRefusal` additionally checks
 * the refusal did not come from one of the builder's own pre-flight guards, so
 * a refusal always means an on-chain check fired.
 *
 * Scheduler rotation, which decides the whole topology of these tests: the
 * next shift belongs to the active-operators element whose `next` link points
 * at the current operator, so the shift walks the key-ascending list
 * *backwards*, and rewinds to the tail once it reaches the element the root
 * links to. `AppointFirstOperator` can therefore only appoint the tail — the
 * greatest key hash — and the fixture's operators are sorted ascending, so
 * `operators[n-1]` starts the rotation and `operators[0]` is the element that
 * rewinds.
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  activeOperatorNodeUnit,
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  expectInactivityStrikeRefusal,
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  prepareInactivityStrike,
  strikeOperatorToMaxStrikes,
  submitInactivityStrike,
  submitNeglectedDeposit,
} from "./helpers/operator-inactivity.js";

const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;

const requireOperator = (
  fixture: OperatorInactivityFixture,
  index: number,
): string => {
  const operator = fixture.operators[index];
  if (operator === undefined) {
    throw new Error(`Fixture has no operator at index ${index}`);
  }
  return operator.keyHash;
};

const activeNodeDatum = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<SDK.ActiveOperatorDatum> => {
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const node = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
  if (node === undefined || node.active === null) {
    throw new Error(`Operator ${operatorKeyHash} has no active node`);
  }
  return node.active;
};

const activeNodeUtxo = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<UTxO> => {
  const [utxo] = await fixture.lucid.utxosAtWithUnit(
    fixture.contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit(fixture.contracts, operatorKeyHash),
  );
  if (utxo === undefined) {
    throw new Error(`Operator ${operatorKeyHash} has no active node UTxO`);
  }
  return utxo;
};

const requireActiveOperatorShift = async (
  fixture: OperatorInactivityFixture,
): Promise<{ readonly operator: string; readonly startTime: bigint }> => {
  const datum = await fetchSchedulerDatum(fixture);
  if (datum === "NoActiveOperators") {
    throw new Error("The scheduler holds no active operator");
  }
  return {
    operator: datum.ActiveOperator.operator,
    startTime: datum.ActiveOperator.start_time,
  };
};

describe("stalled-operator strike and takeover", () => {
  it("refuses a strike whose validity range starts before the inactivity threshold", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const appointed = await appointFirstSchedulerOperator(fixture);
    expect(appointed.operatorKeyHash).toBe(requireOperator(fixture, 2));

    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: appointed.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
    });
    expect(threshold.kind).toBe("threshold");

    // Still inside the shift's grace period: the planner says so, and the
    // validator says so too when the range is dated anyway.
    const earlyPlan = SDK.planInactivityTakeover({
      snapshot,
      nowMs: BigInt(fixture.emulator.now()),
    });
    expect(earlyPlan.kind).toBe("not-yet");

    const validFrom = BigInt(fixture.emulator.now());
    const message = await expectInactivityStrikeRefusal(fixture, {
      skipThresholdWait: true,
      // Plans as if the threshold had passed, purely to obtain the witnesses,
      // then dates the range where it really is.
      planNowMs:
        threshold.kind === "threshold" ? threshold.thresholdMs + 2n : 0n,
      validity: { validFrom, validTo: validFrom + 120_000n },
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);

    // The scheduler and the node are untouched.
    const shift = await requireActiveOperatorShift(fixture);
    expect(shift.operator).toBe(appointed.operatorKeyHash);
    expect(shift.startTime).toBe(appointed.startTime);
    expect(
      (await activeNodeDatum(fixture, appointed.operatorKeyHash))
        .inactivity_strikes,
    ).toBe(0n);
  }, 420_000);

  it("hands the shift to the successor and strikes the skipped operator once", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const skipped = requireOperator(fixture, 2);
    const successor = requireOperator(fixture, 1);
    expect(appointed.operatorKeyHash).toBe(skipped);

    const nodeBefore = await activeNodeUtxo(fixture, skipped);
    const datumBefore = await activeNodeDatum(fixture, skipped);
    expect(datumBefore.inactivity_strikes).toBe(0n);

    const submission = await submitInactivityStrike(fixture);
    expect(submission.plan.tier).toBe("GoToNext");
    expect(submission.plan.newOperatorKey).toBe(successor);
    expect(submission.plan.thresholdSource).toBe("new-shift-grace-period");
    expect(submission.result.struckInactivityStrikes).toBe(1n);

    const shift = await requireActiveOperatorShift(fixture);
    expect(shift.operator).toBe(successor);
    expect(shift.startTime).toBe(submission.plan.newStartTime);
    expect(shift.startTime).toBe(submission.plan.validity.validTo - 1n);

    const datumAfter = await activeNodeDatum(fixture, skipped);
    expect(datumAfter.inactivity_strikes).toBe(1n);
    expect(datumAfter.bond_unlock_time).toStrictEqual(
      datumBefore.bond_unlock_time,
    );

    // The strike neither seizes the bond nor skims the node's lovelace.
    const nodeAfter = await activeNodeUtxo(fixture, skipped);
    expect(nodeAfter.assets).toStrictEqual(nodeBefore.assets);
    expect(nodeAfter.assets["lovelace"]).toBe(
      nodeBefore.assets["lovelace"] ?? 0n,
    );
    expect(nodeBefore.assets["lovelace"]).toBeGreaterThanOrEqual(
      EMULATOR_REQUIRED_BOND_LOVELACE,
    );

    // The successor was only witnessed, never struck.
    expect((await activeNodeDatum(fixture, successor)).inactivity_strikes).toBe(
      0n,
    );
  }, 420_000);

  it("hops shift by shift through dead successors and then rewinds to the tail", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const tail = requireOperator(fixture, 2);
    const middle = requireOperator(fixture, 1);
    const rewindPoint = requireOperator(fixture, 0);
    await appointFirstSchedulerOperator(fixture);

    const first = await submitInactivityStrike(fixture);
    expect(first.plan.currentOperator).toBe(tail);
    expect(first.plan.newOperatorKey).toBe(middle);
    // The intermediate scheduler datum is the handover this test is about.
    const afterFirst = await requireActiveOperatorShift(fixture);
    expect(afterFirst.operator).toBe(middle);
    expect(afterFirst.startTime).toBe(first.plan.newStartTime);

    const second = await submitInactivityStrike(fixture);
    expect(second.plan.tier).toBe("GoToNext");
    expect(second.plan.currentOperator).toBe(middle);
    expect(second.plan.newOperatorKey).toBe(rewindPoint);
    expect(second.plan.shiftStartMs).toBe(afterFirst.startTime);
    const afterSecond = await requireActiveOperatorShift(fixture);
    expect(afterSecond.operator).toBe(rewindPoint);

    // `operators[0]` is the element the root links to, so its shift rewinds.
    const third = await submitInactivityStrike(fixture);
    expect(third.plan.tier).toBe("Rewind");
    expect(third.plan.currentOperator).toBe(rewindPoint);
    expect(third.plan.newOperatorKey).toBe(tail);
    if (third.result.layout.tier !== "Rewind") {
      throw new Error("Expected a Rewind layout");
    }
    expect(third.result.layout.activeTailRefInputIndex).not.toBeNull();
    expect((await requireActiveOperatorShift(fixture)).operator).toBe(tail);

    expect((await activeNodeDatum(fixture, tail)).inactivity_strikes).toBe(1n);
    expect((await activeNodeDatum(fixture, middle)).inactivity_strikes).toBe(
      1n,
    );
    expect(
      (await activeNodeDatum(fixture, rewindPoint)).inactivity_strikes,
    ).toBe(1n);
  }, 600_000);

  it("rewinds a single-operator set onto the struck operator itself", async () => {
    const fixture = await initOperatorInactivityFixture(1);
    const only = requireOperator(fixture, 0);
    await appointFirstSchedulerOperator(fixture);

    const submission = await submitInactivityStrike(fixture);
    expect(submission.plan.tier).toBe("Rewind");
    expect(submission.plan.newOperatorKey).toBe(only);
    if (submission.result.layout.tier !== "Rewind") {
      throw new Error("Expected a Rewind layout");
    }
    // The struck node is being spent, so it cannot also be a reference input.
    expect(submission.result.layout.activeTailRefInputIndex).toBeNull();

    const shift = await requireActiveOperatorShift(fixture);
    expect(shift.operator).toBe(only);
    expect(shift.startTime).toBe(submission.plan.newStartTime);
    expect((await activeNodeDatum(fixture, only)).inactivity_strikes).toBe(1n);
  }, 420_000);

  it("strikes for a neglected deposit and refuses a mis-indexed one", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const neglected = await submitNeglectedDeposit(fixture);

    // Under `env/testnet.ak` a neglected event can never license an *earlier*
    // strike than the plain commitment gap: on-chain `inclusion_time >=
    // last_state_queue_elements_end_time` and
    // `user_events_negligence_timeout (300_000) >
    // max_inactivity_between_block_commitments (60_000)`, so the neglected
    // term dominates. This deposit is therefore the binding term.
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    expect(neglected.inclusionTimeMs).toBeGreaterThanOrEqual(
      snapshot.stateQueueTail.endTime,
    );
    const withEvent = SDK.computeInactivityThreshold({
      shiftStartMs: appointed.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
      neglectedEvent: neglected,
    });
    const withoutEvent = SDK.computeInactivityThreshold({
      shiftStartMs: appointed.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
    });
    if (withEvent.kind !== "threshold" || withoutEvent.kind !== "threshold") {
      throw new Error("Both thresholds should be satisfiable");
    }
    expect(withEvent.thresholdMs).toBeGreaterThanOrEqual(
      withoutEvent.thresholdMs,
    );
    expect(withEvent.source).toBe("neglected-user-event");

    // A mis-indexed reference input is refused: the redeemer points the
    // validator at the hub oracle instead of the deposit.
    const honest = await prepareInactivityStrike(fixture, {
      neglectedEvent: neglected,
    });
    const honestResult = await Effect.runPromise(
      SDK.buildStrikeInactiveOperatorTxProgram(honest.config),
    );
    expect(honestResult.layout.neglectedUserEventRefInputIndex).not.toBe(
      undefined,
    );
    const wrongIndexMessage = await expectInactivityStrikeRefusal(fixture, {
      neglectedEvent: neglected,
      adversarialOverrides: {
        neglectedUserEventRefInputIndex:
          honestResult.layout.hubOracleRefInputIndex,
      },
    });
    expect(wrongIndexMessage).toMatch(/failed script execution Spend\[\d+\]/);

    const submission = await submitInactivityStrike(fixture, {
      neglectedEvent: neglected,
    });
    expect(submission.plan.thresholdSource).toBe("neglected-user-event");
    expect(submission.plan.neglectedEvent?.utxo.txHash).toBe(
      neglected.utxo.txHash,
    );
    expect(submission.result.layout.neglectedUserEventRefInputIndex).not.toBe(
      undefined,
    );
    expect(submission.result.struckInactivityStrikes).toBe(1n);
    expect((await requireActiveOperatorShift(fixture)).operator).toBe(
      requireOperator(fixture, 1),
    );
  }, 600_000);

  it("accumulates five strikes on one node and refuses the sixth", async () => {
    // Only a single-operator set rotates the shift back onto the same
    // operator, which is what lets one node collect every strike.
    const fixture = await initOperatorInactivityFixture(1);
    const only = requireOperator(fixture, 0);
    await appointFirstSchedulerOperator(fixture);

    const struck = await strikeOperatorToMaxStrikes(fixture, only);
    expect(struck.inactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
    expect(struck.txHashes).toHaveLength(Number(SDK.MAX_INACTIVITY_STRIKES));
    expect((await activeNodeDatum(fixture, only)).inactivity_strikes).toBe(
      SDK.MAX_INACTIVITY_STRIKES,
    );

    // The honest planner refuses to plan a sixth strike.
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const shift = await requireActiveOperatorShift(fixture);
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: shift.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
    });
    if (threshold.kind !== "threshold") {
      throw new Error("The threshold should be satisfiable");
    }
    advanceEmulatorPastUnixTime(fixture.emulator, threshold.thresholdMs);
    const exhausted = SDK.planInactivityTakeover({
      snapshot,
      nowMs: BigInt(fixture.emulator.now()),
    });
    expect(exhausted.kind).toBe("strikes-exhausted");
    if (exhausted.kind === "strikes-exhausted") {
      expect(exhausted.inactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
      expect(exhausted.maxInactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
    }

    // And the validator refuses it too, when a caller plans with a
    // deliberately wrong strike cap to build it anyway.
    const message = await expectInactivityStrikeRefusal(fixture, {
      params: {
        ...SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS,
        maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES + 1n,
      },
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);
    expect((await activeNodeDatum(fixture, only)).inactivity_strikes).toBe(
      SDK.MAX_INACTIVITY_STRIKES,
    );
  }, 900_000);

  it("drives a named operator in a multi-operator set to the strike cap", async () => {
    // The helper the forced-retirement tests reuse: reaching the cap in a
    // three-operator set means rotating the shift through all of them, so each
    // round costs three strikes and every operator ends up capped.
    const fixture = await initOperatorInactivityFixture(3);
    await appointFirstSchedulerOperator(fixture);
    const target = requireOperator(fixture, 0);

    const struck = await strikeOperatorToMaxStrikes(fixture, target);
    expect(struck.inactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
    expect(struck.txHashes).toHaveLength(
      3 * Number(SDK.MAX_INACTIVITY_STRIKES),
    );
    for (const index of [0, 1, 2]) {
      expect(
        (await activeNodeDatum(fixture, requireOperator(fixture, index)))
          .inactivity_strikes,
      ).toBe(SDK.MAX_INACTIVITY_STRIKES);
    }
  }, 900_000);

  it("refuses a strike that misstates the struck node's link", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    await appointFirstSchedulerOperator(fixture);
    // The scheduled operator is the list tail, so its honest link is `Empty`.
    const message = await expectInactivityStrikeRefusal(fixture, {
      adversarialOverrides: { activeNodeLink: requireOperator(fixture, 0) },
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);
    expect(
      (await activeNodeDatum(fixture, requireOperator(fixture, 2)))
        .inactivity_strikes,
    ).toBe(0n);
  }, 420_000);

  it("refuses a strike that hands the shift past the successor", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    await appointFirstSchedulerOperator(fixture);
    // The successor is `operators[1]`; naming `operators[0]` skips a hop.
    const message = await expectInactivityStrikeRefusal(fixture, {
      newOperatorKeyHash: requireOperator(fixture, 0),
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);
    expect((await requireActiveOperatorShift(fixture)).operator).toBe(
      requireOperator(fixture, 2),
    );
  }, 420_000);

  it("refuses a strike whose new start_time is not the validity upper bound", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    await appointFirstSchedulerOperator(fixture);
    const prepared = await prepareInactivityStrike(fixture);
    const message = await expectInactivityStrikeRefusal(fixture, {
      // Inside the range, but one millisecond short of its inclusive upper
      // bound, so `start_time` is not the bound the scheduler recomputes.
      newStartTime: prepared.plan.validity.validTo - 2n,
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);
    expect((await requireActiveOperatorShift(fixture)).operator).toBe(
      requireOperator(fixture, 2),
    );
  }, 420_000);
});

// ---------------------------------------------------------------------------
// Planner unit tests (no emulator)
// ---------------------------------------------------------------------------

const FAKE_SHIFT_START = 1_700_000_000_000n;
const FAKE_TAIL_END_TIME = FAKE_SHIFT_START - 1_000_000n;

const fakeUtxo = (label: string): UTxO =>
  ({
    txHash: label.padStart(64, "0"),
    outputIndex: 0,
    address: "addr_test_fake",
    assets: { lovelace: 5_000_000n },
    datum: null,
    datumHash: null,
    scriptRef: null,
  }) as unknown as UTxO;

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
    const inclusionTimeMs = FAKE_SHIFT_START + 600_000n;
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
