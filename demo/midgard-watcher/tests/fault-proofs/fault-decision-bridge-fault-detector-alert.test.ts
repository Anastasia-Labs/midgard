/**
 * The `fault_detector_failed` alert of a header clears once the header leaves
 * the observed queue, as the `proof_family_coverage_gap` alert does: a
 * rollback that drops the header records no outcome for it, so nothing else
 * would clear it before a restart. A header still queued keeps its alert
 * until it is decided.
 */
import { describe, expect, it } from "vitest";

import { createWatcherFaultDetectorAlert } from "../../src/fault-proofs/fault-detector-isolation.js";
import { watcherFaultDetectorFailedAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import { operations } from "./fault-decision-bridge.released-fixture.js";

const detectorFailures = (observability: ReturnType<typeof operations>) =>
  observability.api
    .status()
    .activeAlerts.filter(({ code }) => code === "fault_detector_failed")
    .map(({ subjectDigest }) => subjectDigest);

const detectorFailureRecords = (observability: ReturnType<typeof operations>) =>
  observability.api
    .diagnostics({ kind: "alert" })
    .records.filter(
      (record) => "code" in record && record.code === "fault_detector_failed",
    );

/** A queue of two headers, and the same queue after a rollback dropped the
 * second. A detector failed on both, and neither is decided: replaying the
 * first fails, which postpones the second. */
const rolledBackQueue = () => {
  const before = observation([headerFixture("01"), headerFixture("02")]);
  const after = observation([headerFixture("01")]);
  const [kept, dropped] = before.finalizedHeaders.map(
    ({ headerHash }) => headerHash,
  );
  const observability = operations();
  const alert = createWatcherFaultDetectorAlert(observability.sink);
  const failure = new Error("replay of the block threw");
  let decides = false;
  const h = harness({
    current: before,
    categoryByHeader: { [kept!]: "doubleSpend", [dropped!]: "doubleSpend" },
    operationsSink: observability.sink,
    faultDetectorAlert: alert,
    classifyOverride: (fresh) => {
      if (!decides) throw failure;
      return { ...fresh, decision: "healthy" as const };
    },
  });
  h.admitted.add(after);
  alert.failed(kept!);
  alert.failed(dropped!);
  return {
    before,
    after,
    kept: kept!,
    dropped: dropped!,
    observability,
    failure,
    h,
    decide: () => {
      decides = true;
    },
  };
};

describe("fault decision bridge on a header a fault detector failed on", () => {
  it("clears the alert of a header a rollback drops and keeps the alert of a header still queued", async () => {
    const { before, after, kept, dropped, observability, failure, h } =
      rolledBackQueue();

    await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);
    expect(detectorFailures(observability)).toEqual(
      [
        watcherFaultDetectorFailedAlertSubject(kept),
        watcherFaultDetectorFailedAlertSubject(dropped),
      ].sort(),
    );

    await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
    expect(detectorFailures(observability)).toEqual([
      watcherFaultDetectorFailedAlertSubject(kept),
    ]);
  });

  it("records no second clear when a header decided after its failure leaves the queue", async () => {
    const { before, after, observability, h, decide } = rolledBackQueue();
    decide();

    // Both headers are decided healthy, which clears both alerts.
    await h.bridge.reconcileAndDispatch(before);
    expect(detectorFailures(observability)).toEqual([]);
    const recorded = detectorFailureRecords(observability).length;

    await h.bridge.reconcileAndDispatch(after);
    expect(detectorFailureRecords(observability)).toHaveLength(recorded);
  });
});
