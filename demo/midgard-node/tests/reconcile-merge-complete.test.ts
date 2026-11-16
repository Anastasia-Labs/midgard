import { Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  type MergeCompletionObservation,
  mergeCompletionVerdict,
  parseReconciliationResult,
  RECONCILIATION_SCHEMA_VERSION,
} from "../src/commands/reconcile.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";

const HEADER_HASH = "11".repeat(28);
const OTHER_HEADER_HASH = "22".repeat(28);
const Status = PendingBlockFinalizationsDB.OwnBlockStatus;

const observed = (
  override: Partial<MergeCompletionObservation> = {},
): MergeCompletionObservation => ({
  canonicalHeaders: [OTHER_HEADER_HASH],
  canonical: false,
  txCount: 0,
  status: Status.Folded,
  journal: Option.none(),
  ...override,
});

describe("merge-complete reconciliation verdict", () => {
  it("is satisfied only once the header left the queue and its block is folded", () => {
    expect(mergeCompletionVerdict(observed())).toEqual({
      status: "satisfied",
      nextAction: null,
    });
  });

  it("keeps a still-queued header pending whatever the local rows say", () => {
    for (const txCount of [0, 3])
      for (const status of [
        undefined,
        ...PendingBlockFinalizationsDB.OWN_BLOCK_STATUSES,
      ])
        expect(
          mergeCompletionVerdict(
            observed({
              canonicalHeaders: [HEADER_HASH],
              canonical: true,
              txCount,
              status,
            }),
          ).status,
        ).toBe("pending");
  });

  it("reads an unqueued header's block rows as neither completion nor its absence", () => {
    // The block's rows stay until its fold is final, after its merge
    // completed: only the derived status decides.
    expect(
      mergeCompletionVerdict(observed({ txCount: 3, status: Status.Unlanded }))
        .status,
    ).toBe("ambiguous");
    expect(mergeCompletionVerdict(observed({ txCount: 3 }))).toEqual({
      status: "satisfied",
      nextAction: null,
    });
  });

  it("does not treat a header without a journal here as proof", () => {
    // Another operator built it, or a block without L2 transactions never
    // had rows.
    expect(
      mergeCompletionVerdict(observed({ status: undefined })),
    ).toMatchObject({
      status: "ambiguous",
      nextAction: expect.stringContaining("holds no journal"),
    });
  });

  it("is pending on the landed fold while an unqueued header's block is landed or applied", () => {
    for (const status of [Status.Landed, Status.Applied])
      expect(mergeCompletionVerdict(observed({ status }))).toMatchObject({
        status: "pending",
        nextAction: expect.stringMatching(
          new RegExp(
            `^pending: landed fold not yet applied\\..*its block is ${status}`,
          ),
        ),
      });
  });

  it("names the scheduled merge fiber and permissionless merging for a queued header", () => {
    const { nextAction } = mergeCompletionVerdict(
      observed({ canonicalHeaders: [HEADER_HASH], canonical: true }),
    );
    expect(nextAction).toContain("scheduled merge fiber");
    expect(nextAction).toContain("permissionless");
  });

  it("refuses a result that claims the deleted merge repair", () => {
    const repairedBy = (action: string) => ({
      schemaVersion: RECONCILIATION_SCHEMA_VERSION,
      milestone: "merge-complete",
      target: { headerHash: HEADER_HASH },
      status: "repaired",
      safeToRetryOriginalStep: true,
      evidence: [],
      nextAction: null,
      repairActions: [action],
    });
    // Another milestone's repair still parses; `merge_action` no longer does.
    expect(
      parseReconciliationResult(repairedBy("backfill_missing_da_payload")),
    ).toEqual(repairedBy("backfill_missing_da_payload"));
    expect(() => parseReconciliationResult(repairedBy("merge_action"))).toThrow(
      "repairActions[0] must be one of",
    );
  });
});
