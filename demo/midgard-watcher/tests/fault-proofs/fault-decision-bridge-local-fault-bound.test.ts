/**
 * A header whose classification keeps failing on this watcher's own replay
 * (`ReplayLocalFault`) is deferred and retried, but not without bound: the
 * `LOCAL_FAULT_RUN_LIMIT`th attempt in a row that fails that way with no
 * classification of the header in between fails the pass with
 * `WatcherReplayLocalFaultExhaustedError`, which ends the watcher. A
 * classification of the header starts its count over; a deferral for any
 * other cause neither counts nor starts it over.
 */
import {
  FraudProofL1UnavailableError,
  LOCAL_FAULT_RUN_LIMIT,
  ReplayLocalFault,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { WatcherReplayLocalFaultExhaustedError } from "../../src/fault-proofs/replay-local-fault-bound.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import { ATTESTED } from "./fault-decision-bridge.released-fixture.js";

/** Inside every fixture header's challengeability window (they end at 2 ms). */
const IN_WINDOW = () => 3n;

type Attempt = "local_fault" | "l1_unavailable" | "healthy";

/** One header, classified by `attempts` in order, one per pass. */
const scene = (attempts: readonly Attempt[]) => {
  const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
  const [header] = current.finalizedHeaders;
  const warn = vi.fn();
  let next = 0;
  const h = harness({
    current,
    categoryByHeader: { [header!.headerHash]: "doubleSpend" },
    nowMs: IN_WINDOW,
    warn,
    classifyOverride: (fresh) => {
      const attempt = attempts[next];
      next += 1;
      if (attempt === "local_fault")
        throw new ReplayLocalFault({
          message: "the script evaluator failed locally",
          cause: undefined,
        });
      if (attempt === "l1_unavailable")
        throw new FraudProofL1UnavailableError("the node did not answer");
      if (attempt === "healthy") return { ...fresh, decision: "healthy" };
      throw new Error("the scene ran out of attempts");
    },
  });
  /** Runs one pass per attempt; the last pass's outcome is returned. */
  const passes = async () => {
    for (let pass = 1; pass < attempts.length; pass += 1)
      await h.bridge.reconcileAndDispatch(current);
    return h.bridge.reconcileAndDispatch(current);
  };
  return { h, header: header!, warn, passes };
};

const faults = (count: number): Attempt[] =>
  Array.from({ length: count }, () => "local_fault" as const);

describe("the fault decision bridge's local-fault bound", () => {
  it("exits on a header's limit-th local fault in a row, after deferring the ones before it", async () => {
    const s = scene(faults(LOCAL_FAULT_RUN_LIMIT));
    const outcome = s.passes();
    await expect(outcome).rejects.toBeInstanceOf(
      WatcherReplayLocalFaultExhaustedError,
    );
    await expect(outcome).rejects.toThrow(
      `replayed_ledger_local_fault: ${s.header.headerHash}: the script evaluator failed locally`,
    );
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(
      LOCAL_FAULT_RUN_LIMIT,
    );
    expect(s.h.appended).toEqual([]);
    expect(s.h.enqueued).toEqual([]);
    expect(s.warn).toHaveBeenCalledExactlyOnceWith({
      event: "classification_deferred",
      headerHash: s.header.headerHash,
      cause: "replay_local_fault",
    });
  });

  it("defers every local fault before the limit", async () => {
    const s = scene(faults(LOCAL_FAULT_RUN_LIMIT - 1));
    expect((await s.passes()).target).toBeNull();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(
      LOCAL_FAULT_RUN_LIMIT - 1,
    );
  });

  it("keeps counting across a deferral for another cause", async () => {
    const s = scene([
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "l1_unavailable",
      "local_fault",
    ]);
    await expect(s.passes()).rejects.toBeInstanceOf(
      WatcherReplayLocalFaultExhaustedError,
    );
  });

  it("starts the count over once the header is classified", async () => {
    const s = scene([
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "healthy",
      ...faults(LOCAL_FAULT_RUN_LIMIT - 1),
      "healthy",
    ]);
    expect((await s.passes()).target).toBeNull();
    expect(s.h.application.classifyHeader).toHaveBeenCalledTimes(
      2 * LOCAL_FAULT_RUN_LIMIT,
    );
    expect(s.h.appended).toEqual([]);
  });
});
