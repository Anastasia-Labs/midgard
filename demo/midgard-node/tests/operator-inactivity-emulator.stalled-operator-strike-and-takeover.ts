import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  expectInactivityStrikeRefusal,
  fetchInactivityDirectorySnapshot,
  fetchNeglectedUserEvent,
  initOperatorInactivityFixture,
  prepareInactivityStrike,
  STRIKE_VALIDITY_WINDOW_MS,
  strikeOperatorToMaxStrikes,
  submitInactivityStrike,
  submitNeglectedDeposit,
} from "./helpers/operator-inactivity.js";
import {
  activeNodeDatum,
  activeNodeUtxo,
  EMULATOR_REQUIRED_BOND_LOVELACE,
  expectNeglectedEventStrike,
  requireActiveOperatorShift,
  requireOperator,
} from "./operator-inactivity-emulator.expect-neglected-event-strike.js";

describe("stalled-operator strike and takeover", () => {
  it("refuses a strike whose validity range starts before the inactivity threshold", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const appointed = await appointFirstSchedulerOperator(fixture);
    expect(appointed.operatorKeyHash).toBe(requireOperator(fixture, 2));
    const neglectedEvent = await submitNeglectedDeposit(fixture);

    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: appointed.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
      neglectedEvent,
    });
    expect(threshold.kind).toBe("threshold");

    // Still before the threshold: the planner says so, and the validator says
    // so too when the range is dated anyway.
    const earlyPlan = SDK.planInactivityTakeover({
      snapshot,
      nowMs: BigInt(fixture.emulator.now()),
      neglectedEvent,
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

  it("never strikes an idle network, however long the shift has been quiet", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const params = SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS;
    // Late in the shift: past its grace period and far past the tail's end
    // time, with no deposit, withdrawal or tx order anywhere on the ledger.
    const lateMs =
      appointed.startTime +
      params.shiftDurationMs -
      4n * STRIKE_VALIDITY_WINDOW_MS;
    advanceEmulatorPastUnixTime(fixture.emulator, lateMs);
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    expect(BigInt(fixture.emulator.now())).toBeGreaterThan(
      snapshot.stateQueueTail.endTime + params.userEventsNegligenceTimeoutMs,
    );
    expect(await fetchNeglectedUserEvent(fixture, snapshot)).toBeNull();
    expect(
      SDK.planInactivityTakeover({
        snapshot,
        nowMs: BigInt(fixture.emulator.now()),
        neglectedEvent: null,
      }),
    ).toStrictEqual({
      kind: "no-neglected-event",
      currentOperator: appointed.operatorKeyHash,
    });

    // A strike citing something that is not a user event, here the
    // registered-operators root claimed as a deposit, is refused by the
    // scheduler's authentication of the cited Order node.
    const registeredRoot = SDK.findRootNode(snapshot.registered);
    if (registeredRoot === undefined) {
      throw new Error("The registered-operators list has no root");
    }
    const message = await expectInactivityStrikeRefusal(fixture, {
      neglectedEvent: {
        kind: "Deposit",
        utxo: registeredRoot.utxo,
        inclusionTimeMs: snapshot.stateQueueTail.endTime + 1n,
      },
    });
    expect(message).toMatch(/failed script execution Spend\[\d+\]/);
    expect((await requireActiveOperatorShift(fixture)).operator).toBe(
      appointed.operatorKeyHash,
    );
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

    const neglected = await submitNeglectedDeposit(fixture);
    const nodeBefore = await activeNodeUtxo(fixture, skipped);
    const datumBefore = await activeNodeDatum(fixture, skipped);
    expect(datumBefore.inactivity_strikes).toBe(0n);

    // The strike cites the undelivered deposit read back from the ledger.
    const submission = await submitInactivityStrike(fixture);
    expect(submission.plan.tier).toBe("GoToNext");
    expect(submission.plan.newOperatorKey).toBe(successor);
    expect(submission.plan.neglectedEvent.utxo.txHash).toBe(
      neglected.utxo.txHash,
    );
    // The deposit arrives just after the shift starts, so its negligence
    // timeout ends after the shift's grace period.
    expect(submission.plan.thresholdSource).toBe("neglected-user-event");
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
    // One undelivered deposit licenses every hop: nothing delivers it.
    await submitNeglectedDeposit(fixture);

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
    await submitNeglectedDeposit(fixture);

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

  it("strikes for a neglected deposit and refuses early, mis-indexed, wrong-family and unauthenticated claims", async () => {
    await expectNeglectedEventStrike("Deposit");
  }, 600_000);

  it("strikes for a neglected withdrawal and refuses early, mis-indexed, wrong-family and unauthenticated claims", async () => {
    await expectNeglectedEventStrike("Withdrawal");
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
    const neglectedEvent = await fetchNeglectedUserEvent(fixture, snapshot);
    if (neglectedEvent === null) {
      throw new Error("The strikes cite an undelivered deposit");
    }
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: shift.startTime,
      stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
      neglectedEvent,
    });
    if (threshold.kind !== "threshold") {
      throw new Error("The threshold should be satisfiable");
    }
    advanceEmulatorPastUnixTime(fixture.emulator, threshold.thresholdMs);
    const exhausted = SDK.planInactivityTakeover({
      snapshot,
      nowMs: BigInt(fixture.emulator.now()),
      neglectedEvent,
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
    await submitNeglectedDeposit(fixture);
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
    await submitNeglectedDeposit(fixture);
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
    await submitNeglectedDeposit(fixture);
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

export const FAKE_SHIFT_START = 1_700_000_000_000n;

export const FAKE_TAIL_END_TIME = FAKE_SHIFT_START - 1_000_000n;

export const fakeUtxo = (label: string): UTxO =>
  ({
    txHash: label.padStart(64, "0"),
    outputIndex: 0,
    address: "addr_test_fake",
    assets: { lovelace: 5_000_000n },
    datum: null,
    datumHash: null,
    scriptRef: null,
  }) as unknown as UTxO;
