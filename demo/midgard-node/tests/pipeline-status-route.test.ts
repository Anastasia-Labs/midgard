import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  encodePipelineStatusOldestActive,
  PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
  type PipelineStatusOldestActiveRow,
} from "@/commands/listen-router.js";
import { PendingBlockFinalizationsDB } from "@/database/index.js";

const ACTIVE_STATUSES = [
  PendingBlockFinalizationsDB.Status.PendingSubmission,
  PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
  PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
  PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
] as const;

describe("GET /pipeline-status pending-finalization reporting", () => {
  it("filters the oldest-active query with the canonical active status enum", async () => {
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).toEqual(
      ACTIVE_STATUSES,
    );
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).not.toContain(
      PendingBlockFinalizationsDB.Status.Finalized,
    );
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).not.toContain(
      PendingBlockFinalizationsDB.Status.Abandoned,
    );

    const source = await readFile(
      new URL("../src/commands/listen-router.ts", import.meta.url),
      "utf8",
    );
    const handler = source.slice(
      source.indexOf("const getPipelineStatusHandler"),
      source.indexOf("const getProtocolInfoHandler"),
    );
    expect(handler).toContain(
      "PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES",
    );
    expect(handler).toContain("PendingBlockFinalizationsDB.Columns.STATUS");
    expect(handler).not.toMatch(
      /WHERE\s+status\s+IN\s+\('prepared',\s*'submitted',\s*'confirmed'\)/u,
    );
  });

  it.each(ACTIVE_STATUSES)(
    "encodes an active %s row instead of reporting no active journal",
    (status) => {
      const row: PipelineStatusOldestActiveRow = {
        header_hash: "11".repeat(28),
        submitted_tx_hash: "22".repeat(32),
        status,
        created_at: new Date("2026-07-29T00:00:00.000Z"),
        updated_at: new Date("2026-07-29T00:00:01.000Z"),
        observed_confirmed_at_ms:
          status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability
            ? Date.parse("2026-07-29T00:00:02.000Z")
            : null,
      };

      expect(
        encodePipelineStatusOldestActive(
          row,
          new Date("2026-07-29T00:00:05.000Z"),
        ),
      ).toMatchObject({
        headerHash: row.header_hash,
        submittedTxHash: row.submitted_tx_hash,
        status,
        ageMs: 5_000,
      });
    },
  );

  it("reports null only when the active query returns no row", () => {
    expect(
      encodePipelineStatusOldestActive(
        undefined,
        new Date("2026-07-29T00:00:05.000Z"),
      ),
    ).toBeNull();
  });
});
