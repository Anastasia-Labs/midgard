import "./store-retention.retention-candidates-v1.js";

import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { retentionReadinessFromDeadlines } from "../src/committee-service.js";
import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { retentionCycleOptions } from "../src/store/retention.js";
import { runRetentionCycle } from "../src/store/retention.scan.js";
import {
  retentionCandidates,
  retentionDeadlineReport,
} from "./helpers/retention-whole-set.js";
import {
  FINGERPRINT,
  hashOf,
  HEAD,
  LIVE_A,
  LIVE_B,
  NOW,
  openStore,
  PROMISED_RETENTION_MS,
  REQUIRED_RETENTION_MS,
  retentionOptions,
  seed,
} from "./store-retention.header-record.js";

describe("retentionDeadlineReportV1", () => {
  // Opt-in alert (owner ruling 2026-09-26): only an operator threshold
  // (DA_RETENTION_ALERT_THRESHOLD_MS) turns the deadline into an alert.
  const THRESHOLD_MS = 60_000;

  it("keeps a catch-up payload and keys its deadline alert on the authenticated commit time", async () => {
    const store = await openStore();
    const headerHash = hashOf(37);
    const blockTimeMs = NOW - REQUIRED_RETENTION_MS + THRESHOLD_MS;
    await seed(store, [
      {
        headerHash,
        endTimeMs: blockTimeMs - 6 * 60 * 60 * 1000,
        blockTimeMs,
        status: "attested",
      },
    ]);
    const cycle = await runRetentionCycle(
      store,
      {
        ...retentionOptions(),
        alertThresholdMs: THRESHOLD_MS,
      },
      {},
    );
    expect(cycle.prune.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(headerHash)).toBeDefined();
    expect(cycle.deadlines.entries).toEqual([
      {
        headerHash,
        reasonCode: "still_challengeable",
        challengeableUntilMs: blockTimeMs + REQUIRED_RETENTION_MS,
        remainingMs: THRESHOLD_MS,
        headroomMs: 0,
        alerting: true,
      },
    ]);
    expect(
      (
        await runRetentionCycle(
          store,
          retentionOptions(blockTimeMs + REQUIRED_RETENTION_MS),
          {},
        )
      ).prune.prunedHeaderHashes,
    ).toEqual([]);
    expect(
      (
        await runRetentionCycle(
          store,
          retentionOptions(blockTimeMs + PROMISED_RETENTION_MS + 1),
          {},
        )
      ).prune.prunedHeaderHashes,
    ).toEqual([headerHash]);
  });

  it("raises no alert and leaves readiness ok without an operator threshold", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(39),
        endTimeMs: NOW - REQUIRED_RETENTION_MS + 1,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.alertThresholdMs).toBeNull();
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate?.decision).toMatchObject({
      reasonCode: "still_challengeable",
      remainingMs: 1,
    });
    expect(report.entries).toEqual([]);
    expect(report.alerting).toBe(0);
    expect(retentionReadinessFromDeadlines(report)).toMatchObject({
      status: "ok",
      alerting: 0,
    });
  });

  it.each([
    { threshold: THRESHOLD_MS, alerting: 1 },
    { threshold: undefined, alerting: 0 },
  ] as const)(
    "carries the configured threshold ($threshold) through the runtime cycle to the alert count ($alerting), never to readiness",
    async ({ threshold, alerting }) => {
      const store = await openStore();
      await seed(store, [
        {
          headerHash: hashOf(38),
          endTimeMs: NOW - REQUIRED_RETENTION_MS + THRESHOLD_MS,
          status: "attested",
        },
      ]);
      const config = {
        ...(threshold === undefined
          ? {}
          : { retentionAlertThresholdMs: threshold }),
        deploymentFingerprint: FINGERPRINT,
        automaticRecoveryMaxDepth: 2160,
        daTransport: { retentionDays: LIBP2P_DA_MIN_RETENTION_DAYS },
      };
      const { deadlines, prune } = await runRetentionCycle(
        store,
        retentionCycleOptions(
          config,
          {
            confirmedHeadHash: HEAD,
            liveQueueHeaderHashes: new Set([LIVE_A, LIVE_B]),
            finalBlockTimeMs: NOW,
          },
          NOW,
        ),
        {},
      );
      expect(prune.prunedHeaderHashes).toEqual([]);
      expect(deadlines.alertThresholdMs).toBe(threshold ?? null);
      expect(deadlines.alerting).toBe(alerting);
      expect(retentionReadinessFromDeadlines(deadlines)).toMatchObject({
        status: "ok",
        alerting,
      });
    },
  );

  it("reports derived window arithmetic and alerts on burned headroom, without affecting readiness", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(40),
        endTimeMs: NOW - REQUIRED_RETENTION_MS + THRESHOLD_MS,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, {
      ...retentionOptions(),
      alertThresholdMs: THRESHOLD_MS,
    });
    expect(report.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    expect(report.deployedRetentionMs).toBe(1_296_000_000);
    expect(report.marginMs).toBe(MIDGARD_RETENTION_WINDOW.marginMs);
    expect(report.alertThresholdMs).toBe(THRESHOLD_MS);
    expect(report.entries[0]).toMatchObject({ headroomMs: 0, alerting: true });
    expect(report.alerting).toBe(1);
    // The alert is informational: the cycle's readiness stays `ok` and only
    // carries the count.
    expect(retentionReadinessFromDeadlines(report)).toEqual({
      status: "ok",
      checkedAt: new Date(NOW).toISOString(),
      scanned: 1,
      retained: 1,
      prunable: 0,
      alerting: 1,
    });
  });

  it("reports the retention period in force, not the derived minimum", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(40), status: "attested" }]);
    const { deadlines } = await runRetentionCycle(
      store,
      { ...retentionOptions(), retentionDays: 120 },
      {},
    );
    expect(deadlines.deployedRetentionMs).toBe(120 * RETENTION_MS_PER_DAY);
    expect(deadlines.marginMs).toBe(
      120 * RETENTION_MS_PER_DAY - MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
  });

  it("does not alert one millisecond above the threshold", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(41),
        endTimeMs: NOW - REQUIRED_RETENTION_MS + THRESHOLD_MS + 1,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, {
      ...retentionOptions(),
      alertThresholdMs: THRESHOLD_MS,
    });
    expect(report.entries).toEqual([]);
    expect(report.alerting).toBe(0);
    expect(retentionReadinessFromDeadlines(report).status).toBe("ok");
  });

  it("never alerts on a record that is not still challengeable", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(44), status: "removed" }]);
    const options = {
      ...retentionOptions(),
      alertThresholdMs: REQUIRED_RETENTION_MS,
    };
    const report = await retentionDeadlineReport(store, options);
    const [candidate] = await retentionCandidates(store, options);
    expect(candidate?.decision.reasonCode).toBe("removed_header");
    expect(report.entries).toEqual([]);
    expect(report.alerting).toBe(0);
  });

  it("does not alert on a record past its horizon on the wall clock but not on the release clock", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(45),
        endTimeMs: NOW - REQUIRED_RETENTION_MS,
        status: "attested",
      },
    ]);
    const options = {
      ...retentionOptions(NOW + 1),
      finalBlockTimeMs: NOW,
      alertThresholdMs: THRESHOLD_MS,
    };
    const report = await retentionDeadlineReport(store, options);
    const [candidate] = await retentionCandidates(store, options);
    expect(candidate?.decision.reasonCode).toBe("still_challengeable");
    expect(report.entries).toEqual([]);
    expect(report.alerting).toBe(0);
  });

  it("computes a deadline for a payload with no header row from its receipt time", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(42), withoutHeader: true }]);
    // A threshold of the whole horizon alerts from receipt, naming the entry.
    const report = await retentionDeadlineReport(store, {
      ...retentionOptions(),
      alertThresholdMs: REQUIRED_RETENTION_MS,
    });
    expect(report.entries).toEqual([
      {
        headerHash: hashOf(42),
        reasonCode: "still_challengeable",
        challengeableUntilMs: NOW + REQUIRED_RETENTION_MS,
        remainingMs: REQUIRED_RETENTION_MS,
        headroomMs: 0,
        alerting: true,
      },
    ]);
  });

  it("rejects malformed alert thresholds", async () => {
    const store = await openStore();
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      await expect(
        retentionDeadlineReport(store, {
          ...retentionOptions(),
          alertThresholdMs: bad,
        }),
      ).rejects.toThrow(/alertThresholdMs/u);
    }
  });
});

describe("runRetentionCycleV1", () => {
  it("reports and deletes a removed header in one cycle", async () => {
    const store = await openStore();
    const headerHash = hashOf(43);
    await seed(store, [{ headerHash, status: "removed" }]);

    const cycle = await runRetentionCycle(store, retentionOptions(), {});
    expect(cycle.deadlines).toMatchObject({
      scanned: 1,
      retained: 0,
      prunable: 1,
      alerting: 0,
    });
    expect(cycle.prune).toEqual({
      scanned: 1,
      prunedHeaderHashes: [headerHash],
      retained: 0,
    });
    expect(await store.getDaPayload(headerHash)).toBeUndefined();
  });

  it.each([
    ["without", undefined],
    ["with", REQUIRED_RETENTION_MS],
  ] as const)(
    "prunes the same set %s an alert threshold and keeps still-challengeable evidence",
    async (_label, alertThresholdMs) => {
      const store = await openStore();
      const imminent = hashOf(46);
      const expired = hashOf(47);
      const removed = hashOf(48);
      await seed(store, [
        {
          headerHash: imminent,
          endTimeMs: NOW - REQUIRED_RETENTION_MS,
          status: "attested",
        },
        {
          headerHash: expired,
          endTimeMs: NOW - PROMISED_RETENTION_MS - 1,
          status: "attested",
        },
        { headerHash: removed, status: "removed" },
      ]);

      const cycle = await runRetentionCycle(
        store,
        {
          ...retentionOptions(),
          ...(alertThresholdMs === undefined ? {} : { alertThresholdMs }),
        },
        {},
      );
      expect([...cycle.prune.prunedHeaderHashes].sort()).toEqual(
        [expired, removed].sort(),
      );
      expect(await store.getDaPayload(imminent)).toBeDefined();
      expect(cycle.deadlines.alerting).toBe(
        alertThresholdMs === undefined ? 0 : 1,
      );
    },
  );

  it("never deletes a concurrent payload absent from the preceding report", async () => {
    const store = await openStore();
    const reported = hashOf(44);
    const concurrent = hashOf(45);
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: reported, endTimeMs: expired, status: "merged" },
    ]);
    let injected = false;
    const wrapped = new Proxy(store, {
      get(target, property, receiver) {
        if (property === "getStateQueueHeaders") {
          return async (headerHashes: readonly string[]) => {
            if (!injected) {
              injected = true;
              await seed(store, [
                {
                  headerHash: concurrent,
                  endTimeMs: expired,
                  status: "removed",
                },
              ]);
            }
            return store.getStateQueueHeaders(headerHashes);
          };
        }
        const value = Reflect.get(target, property, receiver) as unknown;
        return typeof value === "function" ? value.bind(target) : value;
      },
    });

    const cycle = await runRetentionCycle(wrapped, retentionOptions(), {});
    // The report counted the reported payload alone.
    expect(cycle.deadlines.scanned).toBe(1);
    expect(cycle.prune.prunedHeaderHashes).toEqual([reported]);
    expect(await store.getDaPayload(concurrent)).toBeDefined();
  });
});
