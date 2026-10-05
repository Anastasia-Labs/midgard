import { describe, expect, it } from "vitest";

import {
  createWatcherAlertBook,
  WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS,
  watcherDaFetchAlertSubject,
} from "../../src/runtime/operations-observability.alert-book.js";
import type { WatcherAlertDiagnostic } from "../../src/runtime/operations-observability.js";

const HEADER = "ab".repeat(28);
const OTHER = "cd".repeat(28);

const book = (daFetchMaximumAgeMs = 1_000) => {
  const appended: Omit<WatcherAlertDiagnostic, "sequence">[] = [];
  const alerts = createWatcherAlertBook({
    append: (record) => {
      appended.push(record);
      return Object.freeze({
        ...record,
        sequence: appended.length.toString(),
      }) as WatcherAlertDiagnostic;
    },
    daFetchMaximumAgeMs,
  });
  return { alerts, appended };
};

const daFetch = (headerHash: string, active: boolean, observedAtMs: string) =>
  Object.freeze({
    code: "da_fetch_failure" as const,
    subjectDigest: watcherDaFetchAlertSubject(headerHash),
    active,
    observedAtMs,
  });

describe("watcher alert book", () => {
  it("keys a DA fetch failure by header, whatever the case of the hash", () => {
    expect(watcherDaFetchAlertSubject(HEADER.toUpperCase())).toBe(
      watcherDaFetchAlertSubject(HEADER),
    );
    expect(watcherDaFetchAlertSubject(HEADER)).not.toBe(
      watcherDaFetchAlertSubject(OTHER),
    );
    expect(watcherDaFetchAlertSubject(HEADER)).toMatch(/^[0-9a-f]{64}$/u);
    expect(WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS).toBe(180_000);
  });

  it("clears a header's DA fetch failure once the header has a terminal outcome", () => {
    const { alerts } = book();
    alerts.set(daFetch(HEADER, true, "100"));
    alerts.set(daFetch(OTHER, true, "100"));
    alerts.settleHeader(HEADER, "pending_da", "150");
    alerts.settleHeader(HEADER, "failed", "150");
    expect(alerts.active()).toHaveLength(2);
    alerts.settleHeader(HEADER, "unverified_merged", "200");
    expect(alerts.active()).toEqual([
      expect.objectContaining({
        subjectDigest: watcherDaFetchAlertSubject(OTHER),
      }),
    ]);
    expect(alerts.holdsReadiness(200n)).toBe(true);
  });

  it("stops holding readiness for a failure that is never repeated, and renews one that is", () => {
    const { alerts } = book(1_000);
    alerts.set(daFetch(HEADER, true, "100"));
    expect(alerts.holdsReadiness(1_100n)).toBe(true);
    expect(alerts.holdsReadiness(1_101n)).toBe(false);
    // Still listed for the operator, only not a readiness reason.
    expect(alerts.active()).toHaveLength(1);
    alerts.set(daFetch(HEADER, true, "1100"));
    expect(alerts.holdsReadiness(1_101n)).toBe(true);
  });

  it("keeps every other non-informational alert holding readiness however old", () => {
    const { alerts } = book(1_000);
    alerts.set({
      code: "root_mismatch",
      subjectDigest: "11".repeat(32),
      active: true,
      observedAtMs: "1",
    });
    expect(alerts.holdsReadiness(10_000_000n)).toBe(true);
    alerts.set({
      code: "da_bond_pool_under_backed",
      subjectDigest: "22".repeat(32),
      active: true,
      observedAtMs: "1",
    });
    alerts.set({
      code: "root_mismatch",
      subjectDigest: "11".repeat(32),
      active: false,
      observedAtMs: "2",
    });
    expect(alerts.holdsReadiness(10_000_000n)).toBe(false);
    expect(alerts.active()).toHaveLength(1);
  });

  it("lists an L1 rollback for the operator without making the watcher not ready", () => {
    const { alerts } = book();
    const source = "44".repeat(32);
    alerts.set({
      code: "chain_rollback",
      subjectDigest: source,
      active: true,
      observedAtMs: "1",
    });
    expect(alerts.active()).toEqual([
      expect.objectContaining({
        code: "chain_rollback",
        subjectDigest: source,
      }),
    ]);
    expect(alerts.holdsReadiness(2n)).toBe(false);
    alerts.set({
      code: "chain_rollback",
      subjectDigest: source,
      active: false,
      observedAtMs: "2",
    });
    expect(alerts.active()).toEqual([]);
  });

  it("drops inactive per-header records but keeps the deduplicated bond-pool state", () => {
    const { alerts } = book();
    alerts.set(daFetch(HEADER, true, "1"));
    alerts.set(daFetch(HEADER, false, "2"));
    expect(
      alerts.state("da_fetch_failure", watcherDaFetchAlertSubject(HEADER)),
    ).toBeUndefined();
    alerts.set({
      code: "da_bond_pool_withdrawing",
      subjectDigest: "33".repeat(32),
      active: false,
      observedAtMs: "3",
    });
    expect(alerts.state("da_bond_pool_withdrawing", "33".repeat(32))).toBe(
      false,
    );
  });

  it("bounds the DA fetch failures it keeps, evicting the oldest first", () => {
    const { alerts } = book();
    const hashes = Array.from({ length: 1_001 }, (_, index) =>
      index.toString(16).padStart(56, "0"),
    );
    for (const hash of hashes) alerts.set(daFetch(hash, true, "1"));
    const kept = new Set(
      alerts.active().map(({ subjectDigest }) => subjectDigest),
    );
    expect(kept.size).toBe(1_000);
    expect(kept.has(watcherDaFetchAlertSubject(hashes[0]!))).toBe(false);
    expect(kept.has(watcherDaFetchAlertSubject(hashes.at(-1)!))).toBe(true);
  });

  it("refuses an invalid alert and an invalid age bound", () => {
    const { alerts } = book();
    expect(() =>
      alerts.set({ ...daFetch(HEADER, true, "1"), subjectDigest: "zz" }),
    ).toThrow();
    expect(() => book(0)).toThrow(/age bound/u);
  });
});
