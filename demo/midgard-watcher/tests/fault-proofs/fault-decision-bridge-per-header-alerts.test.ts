/**
 * The `validation_trace_replay_stuck` and `da_fetch_failure` alerts of a
 * header clear once the header leaves the observed queue, as the
 * `proof_family_coverage_gap` and `fault_detector_failed` alerts do: a
 * rollback that drops the header records no outcome for it, so nothing else
 * would clear them before a restart. A header still queued keeps its alerts
 * until it is decided.
 */
import { describe, expect, it } from "vitest";

import type { PerHeaderAlert } from "../../src/fault-proofs/per-header-alert.js";
import { createWatcherValidationTraceReplayStuckAlert } from "../../src/fault-proofs/validation-trace-replay-stuck-alert.js";
import {
  watcherDaFetchAlertSubject,
  watcherValidationTraceReplayStuckAlertSubject,
} from "../../src/runtime/operations-observability.alert-book.js";
import type { WatcherAlertCode } from "../../src/runtime/operations-observability.js";
import {
  bindWatcherRetainedDaOperations,
  createWatcherRetainedDaRuntime,
} from "../../src/storage/retained-da-runtime.js";
import {
  admittedConfig,
  transportFactory,
} from "../storage/retained-da-runtime.fixtures.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import { operations } from "./fault-decision-bridge.released-fixture.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

const active = (
  observability: ReturnType<typeof operations>,
  code: WatcherAlertCode,
) =>
  observability.api
    .status()
    .activeAlerts.filter((alert) => alert.code === code)
    .map(({ subjectDigest }) => subjectDigest);

/** A queue of two headers, and the same queue after a rollback dropped the
 * second. Neither is decided: replaying the first fails, which postpones
 * the second. */
const rolledBackQueue = (
  observability: ReturnType<typeof operations>,
  alert: Pick<PerHeaderAlert, "reconcile">,
) => {
  const before = observation([headerFixture("01"), headerFixture("02")]);
  const after = observation([headerFixture("01")]);
  const [kept, dropped] = before.finalizedHeaders.map(
    ({ headerHash }) => headerHash,
  );
  const failure = new Error("replay of the block threw");
  const h = harness({
    current: before,
    categoryByHeader: { [kept!]: "doubleSpend", [dropped!]: "doubleSpend" },
    operationsSink: observability.sink,
    perHeaderAlerts: [alert],
    classifyOverride: () => {
      throw failure;
    },
  });
  h.admitted.add(after);
  return { before, after, kept: kept!, dropped: dropped!, failure, h };
};

describe("fault decision bridge on the per-header alerts raised outside it", () => {
  it("clears the stuck-replay alert of a header a rollback drops and keeps the alert of a header still queued", async () => {
    const observability = operations();
    const alert = createWatcherValidationTraceReplayStuckAlert(
      observability.sink,
    );
    const { before, after, kept, dropped, failure, h } = rolledBackQueue(
      observability,
      alert,
    );
    alert.stuck({ headerHash: kept });
    alert.stuck({ headerHash: dropped });

    await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);
    expect(active(observability, "validation_trace_replay_stuck")).toEqual(
      [
        watcherValidationTraceReplayStuckAlertSubject(kept),
        watcherValidationTraceReplayStuckAlertSubject(dropped),
      ].sort(),
    );

    await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
    expect(active(observability, "validation_trace_replay_stuck")).toEqual([
      watcherValidationTraceReplayStuckAlertSubject(kept),
    ]);
  });

  it("clears the failed DA fetch alert of a header a rollback drops and keeps the alert of a header still queued", async () => {
    const observability = operations();
    const binding = bindWatcherRetainedDaOperations({
      deploymentIdentity: AUTHORITY.result,
      sink: observability.sink,
    });
    const fake = transportFactory();
    fake.request.mockRejectedValue(new Error("public DA peer did not answer"));
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: admittedConfig(),
      deploymentIdentity: AUTHORITY.result,
      unsafeTransportFactoryForTest: fake.factory,
    });
    try {
      const { before, after, kept, dropped, failure, h } = rolledBackQueue(
        observability,
        binding,
      );
      for (const headerHash of [kept, dropped])
        expect(
          (await runtime.sources[0]!.fetchPayloadByHeaderHash(headerHash)).ok,
        ).toBe(false);

      await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual(
        [
          watcherDaFetchAlertSubject(kept),
          watcherDaFetchAlertSubject(dropped),
        ].sort(),
      );

      await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(kept),
      ]);
    } finally {
      await runtime.close();
      binding.close();
    }
  });
});
