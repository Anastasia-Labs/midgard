import { describe, expect, it } from "vitest";

import { selectAuthenticatedForeignBaseCandidate } from "@/workers/commit-block-header.js";

describe("authenticated foreign commit-base rebinding", () => {
  it("reuses an authenticated parent snapshot when the foreign tip keeps its UTxO root", () => {
    const root = "11".repeat(32);
    expect(
      selectAuthenticatedForeignBaseCandidate({
        foreignUtxosRoot: root,
        requireEntries: true,
        candidates: [
          { source: "foreign-parent-journal", root, hasEntries: true },
          {
            source: "persistent-ledger-mpf",
            root,
            hasEntries: false,
          },
        ],
      }),
    ).toEqual({ type: "Ready", source: "foreign-parent-journal" });
  });

  it("fails closed when a changed foreign root has no authenticated finalization", () => {
    expect(
      selectAuthenticatedForeignBaseCandidate({
        foreignUtxosRoot: "22".repeat(32),
        requireEntries: true,
        candidates: [
          {
            source: "foreign-parent-journal",
            root: "11".repeat(32),
            hasEntries: true,
          },
          {
            source: "confirmed-ledger",
            root: "11".repeat(32),
            hasEntries: true,
          },
        ],
      }),
    ).toEqual({
      type: "AwaitingForeignLedger",
      reason: "foreign UTxO root differs from every authenticated local base",
    });
  });

  it("reuses a matching durable marker after restart only when entries are not required", () => {
    const root = "33".repeat(32);
    const candidates = [
      { source: "persistent-ledger-mpf", root, hasEntries: false },
    ] as const;
    expect(
      selectAuthenticatedForeignBaseCandidate({
        foreignUtxosRoot: root,
        requireEntries: false,
        candidates,
      }),
    ).toEqual({ type: "Ready", source: "persistent-ledger-mpf" });
    expect(
      selectAuthenticatedForeignBaseCandidate({
        foreignUtxosRoot: root,
        requireEntries: true,
        candidates,
      }),
    ).toEqual({
      type: "AwaitingForeignLedger",
      reason: "matching durable root has no authenticated entry snapshot",
    });
  });
});
