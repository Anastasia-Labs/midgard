import { Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  type MergeCompletionObservation,
  mergeCompletionVerdict,
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

  it("blocks while an unqueued header's block is landed or applied but not folded", () => {
    for (const status of [Status.Landed, Status.Applied])
      expect(mergeCompletionVerdict(observed({ status }))).toMatchObject({
        status: "blocked",
        nextAction: expect.stringContaining(`its block is ${status}`),
      });
  });
});
