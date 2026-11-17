/**
 * The watcher's policy for a fault detector that fails during a decision
 * pass: a failure the pass already retries (an L1 source that did not
 * answer) still fails the pass, while any other failure withholds only that
 * detector's result. A withheld result raises the informational
 * `fault_detector_failed` alert for the family and logs the family, header
 * and error at error level; the family's next replay clears the alert.
 */
import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherFaultDetectorAlert,
  watcherFaultDetectorIsolation,
} from "../../src/fault-proofs/fault-detector-isolation.js";
import { watcherFaultDetectorFailedAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { operations } from "./fault-decision-bridge.released-fixture.js";

const HEADER = "ab".repeat(32);

const detectorFailures = (observability: ReturnType<typeof operations>) =>
  observability.api
    .status()
    .activeAlerts.filter(({ code }) => code === "fault_detector_failed")
    .map(({ subjectDigest }) => subjectDigest);

describe("watcher fault detector isolation", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("fails the pass on a failure the pass retries, and withholds any other", () => {
    const isolation = watcherFaultDetectorIsolation();
    expect(
      isolation.failsPass(new FraudProofL1UnavailableError("tip unavailable")),
    ).toBe(true);
    expect(
      isolation.failsPass(
        new Error("direct script derivation: accepted source custody differs"),
      ),
    ).toBe(false);
  });

  it("alerts and logs the failed family, and clears once the family replays again", () => {
    const written: string[] = [];
    vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
      written.push(String(chunk));
      return true;
    });
    const observability = operations();
    const isolation = watcherFaultDetectorIsolation(
      createWatcherFaultDetectorAlert(observability.sink),
    );

    isolation.withheld({
      launchScope: ["missingRedeemer"],
      headerHash: HEADER,
      error: new Error("accepted source custody differs"),
    });
    expect(detectorFailures(observability)).toEqual([
      watcherFaultDetectorFailedAlertSubject("missingRedeemer"),
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

    isolation.replayed(["l2TxMistag"]);
    expect(detectorFailures(observability)).toHaveLength(1);
    isolation.replayed(["missingRedeemer"]);
    expect(detectorFailures(observability)).toEqual([]);
  });
});
