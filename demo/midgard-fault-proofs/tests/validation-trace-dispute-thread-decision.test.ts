/** The validation-trace dispute runner's choice among the threads of our
 * case when more than one can move at once. Our reveal comes first, since
 * our clock runs on it; then a timeout of a defender whose clock ran out;
 * then the close of a thread timed out on its defender's side. Among reveals,
 * and among timeouts, the earliest deadline goes first, whatever the order
 * of the threads' tokens. A thread
 * timed out on our side is reported lost, and a thread on our turn that
 * commits a trace this workflow does not hold is held, never revealed.
 *
 * A thread ReadyForOneStep enters its one-step resolution on our turn,
 * ordered with our reveals by our deadline, only on a step the runner plans;
 * a thread in its resolution moves on before any close, and a thread at the
 * award closes into the case. A case a defender won stops; a case with no
 * open thread settles. */
import { describe, expect, it } from "vitest";

import type { ValidationTraceOneStepPlanning } from "../src/validation-trace-dispute/one-step-plan.js";
import {
  decideValidationTraceThreads,
  VALIDATION_TRACE_CASE_CONTESTED,
  VALIDATION_TRACE_DEFENDER_WON,
  VALIDATION_TRACE_MATERIAL_HOLD,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  VALIDATION_TRACE_THREAD_LOST,
  type ValidationTraceResolvingThread,
  type ValidationTraceThread,
} from "../src/validation-trace-dispute/threads.js";

const ROOT = "a1".repeat(32);
const STEPS = 12n;
const NOW = 1_000_000;
const CLOCK = 100_000n;
const material = { challengerRoot: ROOT, stepCount: STEPS } as never;

const thread = (
  label: string,
  at: ValidationTraceThread["at"],
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

/** ReadyForOneStep, our clock still running from `startedAt`. */
const ready = (label: string, startedAt = BigInt(NOW) - CLOCK / 2n) =>
  thread(label, "game", "ReadyForOneStep", startedAt);
/** A thread of ours in its one-step resolution at `holder`. */
const resolving = (
  label: string,
  holder: ValidationTraceResolvingThread["holder"],
): ValidationTraceResolvingThread =>
  ({
    utxo: {},
    outRef: `${label.repeat(64)}#0`,
    unit: label.repeat(60),
    defenderVkh: "d".repeat(56),
    holder,
  }) as never;
const PLANNED: ValidationTraceOneStepPlanning = {
  kind: "plan",
  plan: {} as never,
};

const decide = (
  threads: readonly ValidationTraceThread[],
  {
    resolving = [],
    plannings = new Map<string, ValidationTraceOneStepPlanning>(),
    caseDatum = {},
    held = material,
  }: {
    resolving?: readonly ValidationTraceResolvingThread[];
    plannings?: ReadonlyMap<string, ValidationTraceOneStepPlanning>;
    caseDatum?: object;
    /** The workflow's trace; null for none. */
    held?: typeof material | null;
  } = {},
) =>
  decideValidationTraceThreads({
    threads,
    resolving,
    plannings,
    caseUnit: "c".repeat(64),
    caseOutRef: `${"c".repeat(64)}#0`,
    caseDatum: {
      open_threads: BigInt(threads.length + resolving.length),
      defender_won: false,
      neither_claim_valid: false,
      ...caseDatum,
    } as never,
    now: NOW,
    material: held ?? undefined,
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

  it("enters the one-step resolution of a planned ReadyForOneStep thread in order of our deadlines with reveals, before any timeout", () => {
    const [s, c] = [silent(), timedOut()];
    const sooner = ready("5", BigInt(NOW) - (CLOCK * 3n) / 4n);
    const later = ready("6", BigInt(NOW) - CLOCK / 4n);
    const plannings = new Map([
      [sooner.outRef, PLANNED],
      [later.outRef, PLANNED],
    ]);
    // Our reveal's clock ends between the two entries'.
    const reveal = ours();
    expect(decide([c, s, later, reveal, sooner], { plannings })).toStrictEqual({
      kind: "move",
      stage: "enter_resolution",
      thread: sooner,
    });
    expect(decide([c, s, later, reveal], { plannings })).toStrictEqual({
      kind: "move",
      stage: "reveal",
      thread: reveal,
    });
    expect(decide([c, s, later], { plannings })).toStrictEqual({
      kind: "move",
      stage: "enter_resolution",
      thread: later,
    });
  });

  it("waits under one-step-required on a ReadyForOneStep thread whose resolution is not installed or whose step it does not drive, never entering it", () => {
    const step = ready("5");
    const label = `thread ${step.unit} at ${step.outRef} is ReadyForOneStep between states 0 and 12`;
    expect(decide([step])).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: ${label}; the one-step resolution is not installed, so it waits`,
    });
    const plannings = new Map([
      [step.outRef, { kind: "unsupported" as const, reason: "a step apart" }],
    ]);
    expect(decide([step], { plannings })).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: ${label}; a step apart, so it waits`,
    });
  });

  it("reports a ReadyForOneStep thread past our clock as lost, and holds one on a trace it does not hold, never entering either", () => {
    const spent = ready("5", BigInt(NOW) - CLOCK - 1n);
    const plannings = new Map([[spent.outRef, PLANNED]]);
    expect(decide([spent], { plannings })).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_THREAD_LOST}: thread ${spent.unit} at ${spent.outRef} ran out of the challenger's clock`,
    });
    const foreign = thread(
      "5",
      "game",
      "ReadyForOneStep",
      BigInt(NOW),
      "b2".repeat(32),
    );
    expect(
      decide([foreign], { plannings: new Map([[foreign.outRef, PLANNED]]) }),
    ).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_MATERIAL_HOLD}: thread ${foreign.unit} at ${foreign.outRef} commits a trace this workflow does not hold`,
    });
  });

  it("moves a thread in its resolution on after timeouts and before closes, then closes the awarded thread into the case", () => {
    const [s, c] = [silent(), timedOut()];
    const boundary = thread("7", "boundary", "ReadyForOneStep", 0n);
    const core = resolving("8", "core");
    const award = resolving("a", "award");
    expect(
      decide([c, s, boundary], { resolving: [award, core] }),
    ).toStrictEqual({ kind: "move", stage: "timeout", thread: s });
    expect(decide([c, boundary], { resolving: [award, core] })).toStrictEqual({
      kind: "move",
      stage: "resolve",
      thread: boundary,
    });
    expect(decide([c], { resolving: [award, core] })).toStrictEqual({
      kind: "move",
      stage: "resolve",
      thread: core,
    });
    expect(decide([c], { resolving: [award] })).toStrictEqual({
      kind: "move",
      stage: "close_thread",
      thread: c,
    });
    expect(decide([], { resolving: [award] })).toStrictEqual({
      kind: "move",
      stage: "close_awarded",
      thread: award,
    });
  });

  it("holds a thread in its resolution without our trace, still closing an awarded one", () => {
    const core = resolving("8", "core");
    const award = resolving("a", "award");
    expect(decide([], { resolving: [core, award], held: null })).toStrictEqual({
      kind: "move",
      stage: "close_awarded",
      thread: award,
    });
    expect(decide([], { resolving: [core], held: null })).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_MATERIAL_HOLD}: thread ${core.unit} at ${core.outRef} is in its one-step resolution without our trace`,
    });
  });

  it("stops under defender-won on a case a defender won, whatever its threads offer", () => {
    expect(
      decide([ours(), timedOut()], {
        resolving: [resolving("a", "award")],
        caseDatum: { defender_won: true },
      }),
    ).toStrictEqual({
      kind: "pending",
      reason: `${VALIDATION_TRACE_DEFENDER_WON}: a defender won a thread of case ${"c".repeat(64)} at ${"c".repeat(64)}#0, which cannot convict, so the run stops`,
    });
  });

  it("settles a case with no open thread, and waits contested on one with no valid claim", () => {
    expect(decide([])).toStrictEqual({ kind: "settle" });
    expect(decide([], { caseDatum: { neither_claim_valid: true } })).toEqual({
      kind: "pending",
      reason: expect.stringMatching(
        new RegExp(`^${VALIDATION_TRACE_CASE_CONTESTED}: `, "u"),
      ),
    });
    // A thread the case counts but discovery did not find is never settled.
    expect(decide([], { caseDatum: { open_threads: 1n } })).not.toStrictEqual({
      kind: "settle",
    });
  });
});
