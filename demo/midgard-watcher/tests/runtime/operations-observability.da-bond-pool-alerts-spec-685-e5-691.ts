import "./operations-observability.production-operations-observability-v1.js";

import { describe, expect, it } from "vitest";

import type { WatcherDaBondPoolObservation } from "../../src/availability/pool-observation.js";
import {
  createWatcherOperationsObservability,
  WATCHER_ALERT_CODES,
  WATCHER_INFORMATIONAL_ALERT_CODES,
  type WatcherOperationsStatus,
} from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

describe("DA bond pool alerts (spec #685 E5, #691)", () => {
  const MANIFEST = "11".repeat(32);
  const healthy = () => {
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: MANIFEST,
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 32,
        requiredCategoryCount: 32,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "99000",
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      nowMs: () => 100_000n,
    });
    observability.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "100000",
    });
    return observability;
  };
  const readout = (
    state: WatcherDaBondPoolObservation["state"],
    underBacked: boolean,
  ): WatcherDaBondPoolObservation => ({
    state,
    ...(state === "missing"
      ? {}
      : {
          lovelace: underBacked ? "1500" : "3000",
          backing: underBacked ? "500" : "2000",
        }),
    requiredBacking: "2000",
    belowBond: underBacked,
    ...(state === "withdrawing"
      ? { unlockAt: "1900000000000", unlockable: false }
      : {}),
    alerts: { underBacked, withdrawing: state === "withdrawing" },
  });
  const activeAlertCodes = (status: WatcherOperationsStatus) =>
    status.activeAlerts.map(({ code }) => code);

  it("lists the pool codes as alert codes that never block readiness", () => {
    expect(WATCHER_ALERT_CODES).toEqual(
      expect.arrayContaining([
        "da_bond_pool_under_backed",
        "da_bond_pool_withdrawing",
      ]),
    );
    expect([...WATCHER_INFORMATIONAL_ALERT_CODES].sort()).toEqual([
      "da_bond_pool_under_backed",
      "da_bond_pool_withdrawing",
    ]);
  });

  it("fires under-backed on a drained pool without making the watcher not_ready, and clears it after a top-up", () => {
    const observability = healthy();
    expect(observability.api.status().daBondPool).toBeNull();
    observability.sink.recordDaBondPool(
      readout("bonded", true),
      MANIFEST,
      "100000",
    );
    expect(observability.api.status()).toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      activeAlerts: [
        {
          code: "da_bond_pool_under_backed",
          subjectDigest: MANIFEST,
          observedAtMs: "100000",
        },
      ],
      daBondPool: {
        state: "bonded",
        backing: "500",
        belowBond: true,
        observedAtMs: "100000",
      },
    });
    expect(observability.api.metrics().activeAlertCount).toBe("1");
    observability.sink.recordDaBondPool(
      readout("bonded", false),
      MANIFEST,
      "100001",
    );
    expect(observability.api.status()).toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      activeAlerts: [],
      daBondPool: { backing: "2000", belowBond: false },
    });
  });

  it("fires withdrawing on BeginWithdraw without making the watcher not_ready, and clears it after a cancel", () => {
    const observability = healthy();
    observability.sink.recordDaBondPool(
      readout("withdrawing", false),
      MANIFEST,
      "100000",
    );
    expect(observability.api.status()).toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      daBondPool: { state: "withdrawing", unlockAt: "1900000000000" },
    });
    expect(activeAlertCodes(observability.api.status())).toEqual([
      "da_bond_pool_withdrawing",
    ]);
    observability.sink.recordDaBondPool(
      readout("withdrawing", true),
      MANIFEST,
      "100001",
    );
    expect(activeAlertCodes(observability.api.status())).toEqual([
      "da_bond_pool_under_backed",
      "da_bond_pool_withdrawing",
    ]);
    expect(observability.api.status().readinessReasons).toEqual([]);
    observability.sink.recordDaBondPool(
      readout("bonded", false),
      MANIFEST,
      "100002",
    );
    expect(observability.api.status()).toMatchObject({
      readiness: "ready",
      activeAlerts: [],
      daBondPool: { state: "bonded" },
    });
    expect(observability.api.status().daBondPool).not.toHaveProperty(
      "unlockAt",
    );
  });

  it("reports a missing pool as under-backed, and still blocks readiness on any other active alert", () => {
    const observability = healthy();
    observability.sink.recordDaBondPool(
      readout("missing", true),
      MANIFEST,
      "100000",
    );
    expect(activeAlertCodes(observability.api.status())).toEqual([
      "da_bond_pool_under_backed",
    ]);
    expect(observability.api.status().readiness).toBe("ready");
    observability.sink.setAlert({
      code: "chain_rollback",
      subjectDigest: MANIFEST,
      active: true,
      observedAtMs: "100000",
    });
    expect(observability.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: ["active_alert"],
    });
  });

  it("appends an alert diagnostic only when a pool alert changes state", () => {
    const observability = healthy();
    const alertRecords = () =>
      observability.api
        .diagnostics({ kind: "alert" })
        .records.map((record) =>
          record.kind === "alert" ? [record.code, record.active] : [],
        );
    for (const observedAtMs of ["100000", "100001", "100002"])
      observability.sink.recordDaBondPool(
        readout("bonded", true),
        MANIFEST,
        observedAtMs,
      );
    observability.sink.recordDaBondPool(
      readout("bonded", false),
      MANIFEST,
      "100003",
    );
    expect(alertRecords()).toEqual([
      ["da_bond_pool_under_backed", true],
      ["da_bond_pool_withdrawing", false],
      ["da_bond_pool_under_backed", false],
    ]);
    // The active alert keeps the time it fired; the readout keeps the latest read.
    observability.sink.recordDaBondPool(
      readout("bonded", true),
      MANIFEST,
      "100004",
    );
    observability.sink.recordDaBondPool(
      readout("bonded", true),
      MANIFEST,
      "100005",
    );
    expect(observability.api.status()).toMatchObject({
      activeAlerts: [{ observedAtMs: "100004" }],
      daBondPool: { observedAtMs: "100005" },
    });
  });

  it("refuses a pool record whose subject is not a 32-byte digest", () => {
    const observability = healthy();
    expect(() =>
      observability.sink.recordDaBondPool(
        readout("bonded", true),
        "deployment",
        "100000",
      ),
    ).toThrow("DA bond pool subject digest is invalid");
    expect(observability.api.status().daBondPool).toBeNull();
  });

  it("serves the pool readout on /v1/status", async () => {
    const observability = healthy();
    observability.sink.recordDaBondPool(
      readout("withdrawing", true),
      MANIFEST,
      "100000",
    );
    const response = await observability.handleHttpRequest(
      new Request("http://127.0.0.1/v1/status"),
    );
    await expect(response.json()).resolves.toMatchObject({
      readiness: "ready",
      daBondPool: {
        state: "withdrawing",
        lovelace: "1500",
        backing: "500",
        requiredBacking: "2000",
        belowBond: true,
        unlockAt: "1900000000000",
        unlockable: false,
        alerts: { underBacked: true, withdrawing: true },
        observedAtMs: "100000",
      },
    });
  });

  it("serves a failed pool read on /v1/status without touching readiness, the last good readout or its alerts, until the next good read", async () => {
    const observability = healthy();
    expect(observability.api.status().daBondPoolReadFailure).toBeNull();
    observability.sink.recordDaBondPool(
      readout("withdrawing", true),
      MANIFEST,
      "100000",
    );
    observability.sink.recordDaBondPoolReadFailure(
      `kupo unreachable ${"ab".repeat(64)}`,
      "100007",
    );
    const response = await observability.handleHttpRequest(
      new Request("http://127.0.0.1/v1/status"),
    );
    await expect(response.json()).resolves.toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      daBondPool: { state: "withdrawing", observedAtMs: "100000" },
      daBondPoolReadFailure: {
        error: "kupo unreachable [hex omitted]",
        failedAtMs: "100007",
      },
    });
    expect(activeAlertCodes(observability.api.status()).sort()).toEqual([
      "da_bond_pool_under_backed",
      "da_bond_pool_withdrawing",
    ]);
    observability.sink.recordDaBondPool(
      readout("bonded", false),
      MANIFEST,
      "100008",
    );
    expect(observability.api.status()).toMatchObject({
      daBondPool: { state: "bonded", observedAtMs: "100008" },
      daBondPoolReadFailure: null,
    });
  });
});
