/**
 * The validation case at the consensus maximum: a 2^32 - 1 step trace, a
 * defense, every bisection round, then the timeout, the thread close and the
 * case close with its bond. Each depth runs as its own case so a measurement
 * can start small and double.
 *
 * The case opens through the VerifySource seam of the case scene. The
 * one-step resolver chain is not on this base, so a conviction here comes
 * from a defender that lets its clock run out.
 */
import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  buildCloseCase,
  buildCloseTimedOutThread,
  closeTimedOutThread,
} from "./support/validation-case-close.js";
import { caseUnit, fraudProofUnit } from "./support/validation-case-moves.js";
import {
  bondLovelace,
  defenseDeadline,
  submitSigned,
  treasuryAddress,
} from "./support/validation-case-scene.js";
import {
  deepDivergentTraces,
  sparseTrace,
} from "./support/validation-game-deep-traces.js";
import {
  beforeDefenderReveal,
  lovelaceOf,
  type MaxShape,
  playReveals,
  report,
  setUpMaxShape,
  spendWholeClock,
} from "./support/validation-game-max-shape-scene.js";
import { gameMove } from "./support/validation-game-moves.js";
import {
  nextReveal,
  options,
  passDeadline,
  passOperatorDeadline,
  timeoutMove,
} from "./support/validation-game-play.js";
import {
  buildMove,
  cap,
  createdAt,
  type Move,
  tagOf,
  type Thread,
} from "./support/validation-game-scene.js";
import {
  machineState,
  type TraceSide,
} from "./support/validation-game-traces.js";

const maxBisectionRounds =
  SELECTED_DEPLOYMENT_PROFILE.limits.max_bisection_rounds;

/** The thread handed to the timeout once its side to move ran out. */
const timeOut = async (shape: MaxShape, thread: Thread) => {
  const { hash, budget } = await submitSigned(
    shape.world,
    await buildMove(shape.world, timeoutMove(shape.world, thread)),
  );
  return {
    thread: { ...thread, utxo: await createdAt(shape.world, hash, 0) },
    budget,
  };
};

/** The thread closed into its case, the thread paying the capped fee. */
const closeThreadAlone = async (shape: MaxShape, thread: Thread) => {
  const { built, next } = buildCloseTimedOutThread(
    shape.world,
    shape.opened,
    thread,
    { lone: true, fee: cap },
  );
  const { hash, budget } = await submitSigned(shape.world, await built);
  return { opened: await next(hash), budget };
};

describe("sparse trace", () => {
  it("commits exactly what the core trace tree commits, for every index", () => {
    const states = [
      machineState(0, "canonicalDecode"),
      machineState(1, "cek"),
      machineState(2, "terminal"),
      machineState(3, "cek"),
    ];
    // 37 steps: the last run is padded up to 64 leaves.
    const runs = [
      { from: 0, state: states[0]! },
      { from: 1, state: states[1]! },
      { from: 22, state: states[2]! },
      { from: 23, state: states[3]! },
    ];
    const stepCount = 37;
    const listed = Array.from({ length: stepCount + 1 }, (_, index) =>
      hashMidgardValidationMachineState(
        index >= 23
          ? states[3]!
          : index >= 22
            ? states[2]!
            : index >= 1
              ? states[1]!
              : states[0]!,
      ),
    );
    const tree = buildMidgardValidationTraceTree(listed, "accepted");
    const sparse: TraceSide = sparseTrace(runs, stepCount);
    expect(sparse.descriptor).toEqual(tree.descriptor);
    tree.proofs.forEach((proof, index) => {
      expect(sparse.proofAt(index)).toEqual(proof);
    });
  });
});

describe.each([4, 8, 16, 32])("validation case at trace depth %i", (depth) => {
  it(`depth ${depth} convicts a defender that stops at its last turn, and refunds the bond`, async () => {
    const traces = deepDivergentTraces(depth);
    const shape = await setUpMaxShape(traces);
    const { world } = shape;
    expect(traces.stepCount).toBe(2 ** depth - 1);
    report("verify-source", depth, shape.budgets.open);
    report("defend", depth, shape.budgets.defend);

    const { thread: last, heaviest } = await playReveals(
      world,
      traces,
      shape.thread,
      beforeDefenderReveal(depth),
    );
    expect(last.dispute.turn.type).toBe("awaitingOperator");
    report("reveal", depth, heaviest);

    // The defender's clock has not run out yet: the timeout is refused.
    await expectOnchainRefusal(() =>
      buildMove(world, timeoutMove(world, last)),
    );
    passOperatorDeadline(world, last);
    const timedOut = await timeOut(shape, last);
    report("enter-timeout", depth, timedOut.budget);

    const closed = await closeThreadAlone(shape, timedOut.thread);
    report("thread-close-challenger-wins", depth, closed.budget);
    expect(closed.opened.datum.open_threads).toBe(0n);

    passDeadline(
      world,
      defenseDeadline(Number(closed.opened.datum.started_at)),
    );
    const close = await submitSigned(
      world,
      await buildCloseCase(world, closed.opened),
    );
    report("case-close-convicted", depth, close.budget);
    const [record] = await world.lucid.utxosAtWithUnit(
      world.fraudProofAddress,
      fraudProofUnit(world, closed.opened),
    );
    expect(lovelaceOf(record!)).toBeGreaterThan(0n);
    const refund = (await world.lucid.utxosAt(world.proverAddress)).filter(
      (utxo) => utxo.datum === tagOf(closed.opened.bond),
    );
    expect(refund.map(lovelaceOf)).toEqual([bondLovelace]);
  });

  it(`depth ${depth} never convicts a defender whose challenger stops, and forfeits the bond`, async () => {
    const traces = deepDivergentTraces(depth);
    const shape = await setUpMaxShape(traces);
    const { world } = shape;

    // Play to the challenger's last reveal and stop there.
    const { thread } = await playReveals(
      world,
      traces,
      shape.thread,
      (_, next) => next.turn.type === "readyForOneStep",
    );
    expect(thread.dispute.turn.type).toBe("awaitingChallenger");
    expect(thread.dispute.round).toBe(depth - 1);

    const challengerMove = (from: number, corrupt: boolean): Move => {
      const { action, next } = nextReveal(traces, thread.dispute, from);
      if (action.kind !== "RevealChallenger") throw new Error("not a reveal");
      const siblings = [...action.proof.siblings];
      // The sibling next to the root: the one only a full-depth tree has.
      const top = Buffer.from(siblings[siblings.length - 1]!);
      top[0] = top[0]! ^ 1;
      if (corrupt) siblings[siblings.length - 1] = top;
      return gameMove(
        world,
        thread,
        { kind: "RevealChallenger", proof: { ...action.proof, siblings } },
        next,
        options(from),
      );
    };
    // The honest reveal is accepted; the same reveal with one sibling of
    // the proof changed is refused at the proof check.
    await buildMove(world, challengerMove(world.emulator.now(), false));
    await expectOnchainRefusal(() =>
      buildMove(world, challengerMove(world.emulator.now(), true)),
    );

    // The challenger lets its clock run out; the thread closes as the
    // defender's win.
    passDeadline(
      world,
      thread.dispute.turnStartedAt + thread.dispute.challengerClockMs,
    );
    const timedOut = await timeOut(shape, thread);
    const closed = await closeTimedOutThread(
      world,
      shape.opened,
      timedOut.thread,
    );
    report("thread-close-defender-wins", depth, closed.budget);
    expect(closed.opened.datum.defender_won).toBe(true);

    const close = await submitSigned(
      world,
      await buildCloseCase(world, closed.opened),
    );
    report("case-close-defender-won", depth, close.budget);
    const forfeited = (await world.lucid.utxosAt(treasuryAddress())).filter(
      (utxo) => utxo.datum === tagOf(closed.opened.bond),
    );
    expect(forfeited.map(lovelaceOf)).toEqual([bondLovelace]);
    expect(
      await world.lucid.utxosAtWithUnit(
        world.caseValidator.address,
        caseUnit(world, closed.opened),
      ),
    ).toEqual([]);
  });
});

/**
 * The latest a validation-case conviction can land: a case at the maximum
 * depth where each side spends its whole clock and the defender then stops.
 * The case close lands within the maximum dispute duration of the case's
 * start, so before the defender's bond hold ends.
 */
describe("validation case at the consensus maximum, with both clocks spent", () => {
  it("max-depth case with both clocks spent convicts within the maximum dispute duration", async () => {
    const depth = maxBisectionRounds;
    const traces = deepDivergentTraces(depth);
    const shape = await setUpMaxShape(traces);
    const { world } = shape;
    const startedAt = Number(shape.opened.datum.started_at);

    const { thread: last } = await playReveals(
      world,
      traces,
      shape.thread,
      beforeDefenderReveal(depth),
      spendWholeClock(world),
    );
    passOperatorDeadline(world, last);
    const timedOut = await timeOut(shape, last);
    const closed = await closeThreadAlone(shape, timedOut.thread);
    passDeadline(world, defenseDeadline(startedAt));
    const closedFrom = world.emulator.now();
    await submitSigned(world, await buildCloseCase(world, closed.opened));
    expect(
      await world.lucid.utxosAtWithUnit(
        world.fraudProofAddress,
        fraudProofUnit(world, closed.opened),
      ),
    ).toHaveLength(1);

    // The case close's validity upper bound: the latest the record can exist.
    const convictedBy =
      closedFrom + MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS - 1;
    expect(convictedBy - startedAt).toBeLessThanOrEqual(
      MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
    );
    console.log(
      `max-shape timing depth=${depth} case_ms=${convictedBy - startedAt} ` +
        `max_duration_ms=${MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS} ` +
        `slack_ms=${MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS - (convictedBy - startedAt)}`,
    );
  });
});
