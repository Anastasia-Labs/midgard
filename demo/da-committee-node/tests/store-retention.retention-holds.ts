import "./store-retention.retention-deadline-report-v1.js";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { assertLibp2pDaRetentionDays } from "../src/config.js";
import type { StateQueueHeaderRecord } from "../src/domain.js";
import { finalityHeldHeaderHashes } from "../src/store/retention.js";

describe("finality-held header hashes", () => {
  it("finds the newest final merge across every stored header record", () => {
    // Stored header records are never pruned, so a long-running committee
    // holds far more than one call can take as spread arguments.
    const merged = (headerHash: string, blockHeight: number) =>
      ({
        headerHash,
        status: "merged",
        finalized: true,
        observedChainPoint: {
          providerSource: "authenticated_state_queue_transition_v1",
          blockHeight,
        },
      }) as unknown as StateQueueHeaderRecord;
    const records = Array.from({ length: 400_000 }, (_, index) =>
      merged(index.toString(16).padStart(56, "0"), index),
    );
    expect(finalityHeldHeaderHashes(["aa".repeat(28)], records)).toEqual([
      "aa".repeat(28),
      (399_999).toString(16).padStart(56, "0"),
    ]);
  });
});

describe("assertLibp2pDaRetentionDaysV1", () => {
  it("accepts the canonical 15-day window matching the manifest", () => {
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
        manifestRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      }),
    ).toBe(15);
    expect(LIBP2P_DA_MIN_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays,
    );
  });

  it("rejects 14 days and accepts 15 at the boundary", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 14,
        manifestRetentionDays: 14,
      }),
    ).toThrow(/must be at least 15 days/u);
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 15,
        manifestRetentionDays: 15,
      }),
    ).toBe(15);
  });

  it("rejects a runtime window that differs from the manifest window", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 16,
        manifestRetentionDays: 15,
      }),
    ).toThrow(/must exactly equal the verified deployment manifest/u);
  });

  it("rejects malformed runtime retention days", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        assertLibp2pDaRetentionDays({
          runtimeRetentionDays: bad,
          manifestRetentionDays: 15,
        }),
      ).toThrow(/da_transport\.retention_days/u);
    }
  });
});
