/** The validation-trace dispute runner's choice among the threads of our
 * case when more than one can move at once. Our reveal comes first, since
 * our clock runs on it; then a timeout of a defender whose clock ran out;
 * then the close of a thread timed out on its defender's side. Among reveals,
 * and among timeouts, the earliest deadline goes first, whatever the order
 * of the threads' tokens. A thread
 * timed out on our side is reported lost, and a thread on our turn that
 * commits a trace this workflow does not hold is held, never revealed. */
import { describe, expect, it } from "vitest";

import {
  decideValidationTraceThreads,
  VALIDATION_TRACE_MATERIAL_HOLD,
  VALIDATION_TRACE_THREAD_LOST,
  type ValidationTraceThread,
} from "../src/validation-trace-dispute/threads.js";

const ROOT = "a1".repeat(32);
const STEPS = 12n;
const NOW = 1_000_000;
const CLOCK = 100_000n;
const material = { challengerRoot: ROOT, stepCount: STEPS } as never;

const thread = (
  label: string,
  at: "game" | "timeout",
  turn: unknown,
  startedAt: bigint,
  root = ROOT,
): ValidationTraceThread =>
  ({
    utxo: {},
    outRef: `${label.repeat(64)}#0`,
    unit: label.repeat(60),
    at,
    thread: {
      turn,
      turn_started_at: startedAt,
      defender_clock_ms: CLOCK,
      challenger_clock_ms: CLOCK,
      challenger_root: root,
      step_count: STEPS,
      low_index: 0n,
      high_index: STEPS,
    },
  }) as never;

/** On our turn, our clock still running. */
const ours = (root = ROOT) =>
  thread(
    "1",
    "game",
    { AwaitingChallenger: { midpoint: 6n, operator_midpoint_hash: "00" } },
    BigInt(NOW) - CLOCK / 2n,
    root,
  );
/** On its defender's turn, the defender's clock spent. */
const silent = () =>
  thread(
    "2",
    "game",
    { AwaitingOperator: { midpoint: 6n } },
    BigInt(NOW) - CLOCK - 1n,
  );
/** At the timeout validator, timed out on its defender's side. */
const timedOut = () =>
  thread("3", "timeout", { AwaitingOperator: { midpoint: 6n } }, 0n);
/** At the timeout validator, timed out on our side. */
const lostOnOurSide = () =>
  thread(
    "4",
    "timeout",
    { AwaitingChallenger: { midpoint: 6n, operator_midpoint_hash: "00" } },
    0n,
  );

const decide = (threads: readonly ValidationTraceThread[]) =>
  decideValidationTraceThreads({
    threads,
    caseUnit: "c".repeat(64),
    caseOutRef: `${"c".repeat(64)}#0`,
    caseDatum: { open_threads: BigInt(threads.length) } as never,
    now: NOW,
    material,
  });

describe("validation-trace dispute runner's choice among threads", () => {
  it("reveals on our turn before timing out a silent defender, and times out before closing a timed-out thread", () => {
    const [r, s, c] = [ours(), silent(), timedOut()];
    expect(decide([c, s, r])).toStrictEqual({
      kind: "move",
      stage: "reveal",
      thread: r,
    });
    expect(decide([c, s])).toStrictEqual({
      kind: "move",
      stage: "timeout",
      thread: s,
    });
    expect(decide([c])).toStrictEqual({
      kind: "move",
      stage: "close_thread",
      thread: c,
    });
  });

  it("reveals first on the thread whose clock of ours ends first, and times out first the defender whose clock ended first, whatever their tokens' order", () => {
    // Thread "9" sorts after thread "1" by token, but its deadline is earlier.
    const later = thread(
      "1",
      "game",
      { AwaitingChallenger: { midpoint: 6n, operator_midpoint_hash: "00" } },
      BigInt(NOW) - CLOCK / 4n,
    );
    const sooner = thread(
      "9",
      "game",
      { AwaitingChallenger: { midpoint: 6n, operator_midpoint_hash: "00" } },
      BigInt(NOW) - CLOCK / 2n,
    );
    expect(later.unit < sooner.unit).toBe(true);
    for (const order of [
      [later, sooner],
      [sooner, later],
    ])
      expect(decide(order)).toStrictEqual({
        kind: "move",
        stage: "reveal",
        thread: sooner,
      });
    const endedLater = thread(
      "1",
      "game",
      { AwaitingOperator: { midpoint: 6n } },
      BigInt(NOW) - CLOCK - 1n,
    );
    const endedFirst = thread(
      "9",
      "game",
      { AwaitingOperator: { midpoint: 6n } },
      BigInt(NOW) - CLOCK - 2n,
    );
    for (const order of [
      [endedLater, endedFirst],
      [endedFirst, endedLater],
    ])
      expect(decide(order)).toStrictEqual({
        kind: "move",
        stage: "timeout",
        thread: endedFirst,
      });
  });

  it("reports a thread timed out on our side as lost, never closing it", () => {
    const lost = lostOnOurSide();
    expect(decide([lost])).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_THREAD_LOST}: thread ${lost.unit} at ${lost.outRef} timed out on the challenger's side`,
    });
  });

  it("holds a thread on our turn that commits a trace this workflow does not hold, never revealing on it", () => {
    const foreign = ours("b2".repeat(32));
    expect(decide([foreign])).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_MATERIAL_HOLD}: thread ${foreign.unit} at ${foreign.outRef} commits a trace this workflow does not hold`,
    });
  });
});
