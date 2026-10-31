/**
 * A header whose classification keeps failing on this watcher's own replay
 * (`ReplayLocalFault`) is deferred and held on readiness under
 * `replayed_ledger_local_fault`, and classified again once its backoff (1 s
 * doubling to 60 s) passed, for as long as the fault recurs. The
 * `LOCAL_FAULT_RUN_LIMIT`th fault in a row with no classification of the
 * header in between names the hold `replayed_ledger_local_fault_stuck` and
 * logs one error-level alarm; the pass never fails on it and the header is
 * still retried; the faults count whatever their messages say, so one whose
 * message varies reaches stuck too. A classification of the header releases
 * the hold; a deferral for any other cause neither counts nor releases it.
 */
import {
  FraudProofL1UnavailableError,
  LOCAL_FAULT_RUN_LIMIT,
  ReplayLocalFault,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  WATCHER_REPLAY_LOCAL_FAULT,
  WATCHER_REPLAY_LOCAL_FAULT_STUCK,
} from "../../src/fault-proofs/replay-local-fault-bound.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import { ATTESTED } from "./fault-decision-bridge.released-fixture.js";

/** Inside every fixture header's challengeability window (they end at 2 ms). */
const IN_WINDOW = () => 3n;

const FAULT = "the script evaluator failed locally";

/** `varied_fault` fails locally with a message naming its attempt. */
type Attempt = "local_fault" | "varied_fault" | "l1_unavailable" | "healthy";

afterEach(() => {
  vi.restoreAllMocks();
});

/** One header, classified by `attempts` in order, on the clock `clock.ms`. */
const scene = (attempts: readonly Attempt[]) => {
  const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
  const [header] = current.finalizedHeaders;
  const warn = vi.fn();
  const clock = { ms: 0 };
  const alarms: Record<string, unknown>[] = [];
  vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
    const text = String(chunk);
    if (text.includes(WATCHER_REPLAY_LOCAL_FAULT_STUCK))
      alarms.push(JSON.parse(text) as Record<string, unknown>);
    return true;
  });
  let next = 0;
  const h = harness({
    current,
    categoryByHeader: { [header!.headerHash]: "doubleSpend" },
    nowMs: IN_WINDOW,
    monotonicNowMs: () => clock.ms,
    warn,
    classifyOverride: (fresh) => {
      const attempt = attempts[next];
      next += 1;
      if (attempt === "local_fault")
        throw new ReplayLocalFault({ message: FAULT, cause: undefined });
      if (attempt === "varied_fault")
        throw new ReplayLocalFault({
          message: `${FAULT} at attempt ${next.toString()}`,
          cause: undefined,
        });
      if (attempt === "l1_unavailable")
        throw new FraudProofL1UnavailableError("the node did not answer");
      if (attempt === "healthy") return { ...fresh, decision: "healthy" };
      throw new Error("the scene ran out of attempts");
    },
  });
  const pass = () => h.bridge.reconcileAndDispatch(current);
  /** One pass per attempt, each after the longest backoff; the reasons the
   * bridge's readiness names after each. */
  const passes = async () => {
    const reasons: string[][] = [];
    for (let attempt = 0; attempt < attempts.length; attempt += 1) {
      clock.ms += 60_000;
      expect((await pass()).target).toBeNull();
      reasons.push(h.bridge.readiness().map(({ reason }) => reason));
    }
    return reasons;
  };
  return { h, header: header!, warn, clock, alarms, pass, passes };
};

const faults = (count: number): Attempt[] =>
  Array.from({ length: count }, () => "local_fault" as const);

describe("the fault decision bridge's local-fault hold", () => {
  it("holds a header past the bound, names it stuck with one error-level alarm, and keeps classifying it", async () => {
    const s = scene([...faults(LOCAL_FAULT_RUN_LIMIT + 2), "healthy"]);
    const reasons = await s.passes();
    expect(reasons).toEqual([
      ...Array.from({ length: LOCAL_FAULT_RUN_LIMIT - 1 }, () => [
        WATCHER_REPLAY_LOCAL_FAULT,
      ]),
      [WATCHER_REPLAY_LOCAL_FAULT_STUCK],
      [WATCHER_REPLAY_LOCAL_FAULT_STUCK],
      [WATCHER_REPLAY_LOCAL_FAULT_STUCK],
      [],
    ]);
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(
      LOCAL_FAULT_RUN_LIMIT + 3,
    );
    expect(s.alarms).toEqual([
      expect.objectContaining({
        level: "error",
        detail: `${s.header.headerHash}: ${FAULT}`,
      }),
    ]);
    expect(s.h.appended).toEqual([]);
    expect(s.h.enqueued).toEqual([]);
    expect(s.warn).toHaveBeenCalledExactlyOnceWith({
      event: "classification_deferred",
      headerHash: s.header.headerHash,
      cause: "replay_local_fault",
    });
  });

  it("names a header stuck after the bound even when every fault's message differs", async () => {
    const s = scene(
      Array.from(
        { length: LOCAL_FAULT_RUN_LIMIT + 1 },
        () => "varied_fault" as const,
      ),
    );
    const reasons = await s.passes();
    expect(reasons.slice(LOCAL_FAULT_RUN_LIMIT - 2)).toEqual([
      [WATCHER_REPLAY_LOCAL_FAULT],
      [WATCHER_REPLAY_LOCAL_FAULT_STUCK],
      [WATCHER_REPLAY_LOCAL_FAULT_STUCK],
    ]);
    expect(s.alarms).toEqual([
      expect.objectContaining({
        level: "error",
        detail: `${s.header.headerHash}: ${FAULT} at attempt ${LOCAL_FAULT_RUN_LIMIT.toString()}`,
      }),
    ]);
  });

  it("does not classify a held header again before its backoff passed", async () => {
    const s = scene(faults(3));
    await s.pass();
    await s.pass();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(1);
    expect(s.h.bridge.readiness()).toEqual([
      { reason: WATCHER_REPLAY_LOCAL_FAULT, detail: FAULT },
    ]);
    s.clock.ms += 999;
    await s.pass();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(1);
    s.clock.ms += 1;
    await s.pass();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(2);
    s.clock.ms += 1_999;
    await s.pass();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(2);
    s.clock.ms += 1;
    await s.pass();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(3);
  });

  it("keeps counting across a deferral for another cause", async () => {
    const s = scene([
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "l1_unavailable",
      "local_fault",
    ]);
    const reasons = await s.passes();
    expect(reasons.at(-1)).toEqual([WATCHER_REPLAY_LOCAL_FAULT_STUCK]);
    expect(s.alarms).toHaveLength(1);
  });

  it("starts over once the header is classified", async () => {
    const s = scene([
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "healthy",
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "healthy",
    ]);
    const reasons = await s.passes();
    expect(reasons.flat()).not.toContain(WATCHER_REPLAY_LOCAL_FAULT_STUCK);
    expect(reasons.at(-1)).toEqual([]);
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(
      2 * LOCAL_FAULT_RUN_LIMIT,
    );
    expect(s.alarms).toEqual([]);
    expect(s.h.appended).toEqual([]);
  });
});
