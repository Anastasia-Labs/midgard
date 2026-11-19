/**
 * The watcher's policy for a fault detector that fails during a decision
 * pass: a failure the pass retries, defers or settles on its own terms still
 * fails the pass, while any other failure withholds only that detector's
 * result. A withheld result raises the informational `fault_detector_failed`
 * alert for the header it failed on and logs the family, header and error at
 * error level; the alert stands until that header is decided.
 */
import { ReplayLocalFault } from "@al-ft/midgard-fault-proofs/block-replay";
import { RetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs/transition-trace/fetch";
import { ValidationTraceReplayHeldError } from "@al-ft/midgard-fault-proofs/validation-trace-dispute/replay-hold";
import {
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherFaultDetectorAlert,
  watcherFaultDetectorIsolation,
} from "../../src/fault-proofs/fault-detector-isolation.js";
import { watcherFaultDetectorFailedAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { WatcherRetainedDaTransportUnavailableError } from "../../src/storage/retained-da-transport-unavailable.js";
import { operations } from "./fault-decision-bridge.released-fixture.js";

const HEADER = "ab".repeat(28);
const OTHER_HEADER = "cd".repeat(28);

const detectorFailures = (observability: ReturnType<typeof operations>) =>
  observability.api
    .status()
    .activeAlerts.filter(({ code }) => code === "fault_detector_failed")
    .map(({ subjectDigest }) => subjectDigest);

const alertRecords = (observability: ReturnType<typeof operations>) =>
  observability.api
    .diagnostics({ kind: "alert" })
    .records.filter(
      (record) => "code" in record && record.code === "fault_detector_failed",
    );

const decided = (
  observability: ReturnType<typeof operations>,
  headerHash: string,
  outcome: "fault_detected" | "failed",
) =>
  observability.sink.recordVerification({
    subjectDigest: headerHash.padEnd(64, "0"),
    headerHash,
    queuedAtMs: "1000",
    startedAtMs: "1000",
    completedAtMs: "2000",
    elapsedMs: "1000",
    outcome,
  });

const withheldOn = (
  isolation: ReturnType<typeof watcherFaultDetectorIsolation>,
  family: FraudProofCatalogueCategoryName,
  headerHash: string,
) =>
  isolation.withheld({
    launchScope: [family],
    headerHash,
    error: new Error("accepted source custody differs"),
  });

const quietStderr = () => {
  const written: string[] = [];
  vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
    written.push(String(chunk));
    return true;
  });
  return written;
};

describe("watcher fault detector isolation", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it.each([
    [
      "an L1 source that did not answer",
      new FraudProofL1UnavailableError("tip unavailable"),
    ],
    [
      "an L1 source that refused the read",
      new FraudProofL1RefusedError("rollback", "tip moved"),
    ],
    [
      "a local replay fault",
      new ReplayLocalFault({ message: "replay failed", cause: undefined }),
    ],
    [
      "a held script re-execution",
      new ValidationTraceReplayHeldError(HEADER, [], 0, []),
    ],
    [
      "a retained-DA transport that is not up",
      new WatcherRetainedDaTransportUnavailableError(new Error("down"), 0),
    ],
    [
      "a retained-DA payload that was not served",
      new RetainedDaPayloadUnavailableError(HEADER, "payload not found"),
    ],
    [
      "a store connection that dropped",
      Object.assign(new Error("terminating connection"), { code: "57P01" }),
    ],
  ])("fails the pass on %s rather than withholding it", (_case, error) => {
    expect(watcherFaultDetectorIsolation().failsPass(error)).toBe(true);
  });

  it("withholds a failure the pass would only repeat", () => {
    expect(
      watcherFaultDetectorIsolation().failsPass(
        new Error("direct script derivation: accepted source custody differs"),
      ),
    ).toBe(false);
  });

  it("alerts on the header it failed on and logs the family", () => {
    const written = quietStderr();
    const observability = operations();
    const isolation = watcherFaultDetectorIsolation(
      createWatcherFaultDetectorAlert(observability.sink),
    );

    withheldOn(isolation, "missingRedeemer", HEADER);

    expect(detectorFailures(observability)).toEqual([
      watcherFaultDetectorFailedAlertSubject(HEADER),
    ]);
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    expect(written.map((line) => JSON.parse(line))).toEqual([
      expect.objectContaining({
        level: "error",
        reason: "fault_detector_failed",
        family: "missingRedeemer",
        headerHash: HEADER,
        detail: "accepted source custody differs",
      }),
    ]);
  });

  it("keeps the alert standing until the header it failed on is decided", () => {
    quietStderr();
    const observability = operations();
    const isolation = watcherFaultDetectorIsolation(
      createWatcherFaultDetectorAlert(observability.sink),
    );

    withheldOn(isolation, "missingRedeemer", HEADER);
    // Other headers replay and are decided, the same family among them.
    decided(observability, OTHER_HEADER, "fault_detected");
    // The header itself fails its pass: it is not decided.
    decided(observability, HEADER, "failed");
    expect(detectorFailures(observability)).toEqual([
      watcherFaultDetectorFailedAlertSubject(HEADER),
    ]);

    decided(observability, HEADER, "fault_detected");
    expect(detectorFailures(observability)).toEqual([]);
  });

  it("records a header's failure once while it stands, whatever fails on it", () => {
    quietStderr();
    const observability = operations();
    const isolation = watcherFaultDetectorIsolation(
      createWatcherFaultDetectorAlert(observability.sink),
    );

    withheldOn(isolation, "missingRedeemer", HEADER);
    withheldOn(isolation, "missingRedeemer", HEADER);
    withheldOn(isolation, "unusedRedeemer", HEADER);
    withheldOn(isolation, "missingRedeemer", OTHER_HEADER);

    expect(detectorFailures(observability)).toEqual(
      [
        watcherFaultDetectorFailedAlertSubject(HEADER),
        watcherFaultDetectorFailedAlertSubject(OTHER_HEADER),
      ].sort(),
    );
    expect(alertRecords(observability)).toHaveLength(2);
  });
});
