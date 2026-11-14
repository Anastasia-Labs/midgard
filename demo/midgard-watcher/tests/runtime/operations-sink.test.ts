/**
 * Startup creates the operations sink before any module that reports to it,
 * and the view only once the supervisor exists: what was reported in between
 * is served by the view as it was recorded.
 */
import { describe, expect, it } from "vitest";

import {
  createWatcherOperationsSink,
  createWatcherOperationsView,
} from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

const MANIFEST = "11".repeat(32);
const RESERVATION = "ab".repeat(32);

describe("the watcher operations sink", () => {
  it("serves an alert raised before the view exists in status().activeAlerts", () => {
    let now = 5_000n;
    const recorder = createWatcherOperationsSink({
      deploymentFingerprint: MANIFEST,
      nowMs: () => now,
    });
    recorder.sink.setAlert({
      code: "proof_submission_failure",
      subjectDigest: RESERVATION,
      active: true,
      observedAtMs: "4000",
    });

    now = 9_000n;
    const operations = createWatcherOperationsView(recorder, {
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 32,
        requiredCategoryCount: 32,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "4000",
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    });

    const status = operations.api.status();
    expect(status.deploymentFingerprint).toBe(MANIFEST);
    expect(status.activeAlerts).toEqual([
      {
        code: "proof_submission_failure",
        subjectDigest: RESERVATION,
        observedAtMs: "4000",
      },
    ]);
    expect(status.readinessReasons).toContain("active_alert");
    expect(operations.sink).toBe(recorder.sink);
    expect(operations.api.diagnostics({ kind: "alert" }).records).toMatchObject(
      [
        {
          kind: "alert",
          code: "proof_submission_failure",
          observedAtMs: "4000",
          sequence: "1",
        },
      ],
    );
  });
});
