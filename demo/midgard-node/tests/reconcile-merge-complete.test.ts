import { Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  type MergeCompletionObservation,
  mergeCompletionVerdict,
} from "../src/commands/reconcile.js";
import { MutationJobsDB } from "../src/database/index.js";

const HEADER_HASH = "11".repeat(28);
const OTHER_HEADER_HASH = "22".repeat(28);

const job = (status: MutationJobsDB.Status): MutationJobsDB.Entry => ({
  [MutationJobsDB.Columns.JOB_ID]:
    MutationJobsDB.confirmedMergeFinalizationJobId(HEADER_HASH),
  [MutationJobsDB.Columns.KIND]: MutationJobsDB.Kind.ConfirmedMergeFinalization,
  [MutationJobsDB.Columns.STATUS]: status,
  [MutationJobsDB.Columns.PLAN_HASH]: null,
  [MutationJobsDB.Columns.PAYLOAD]: {},
  [MutationJobsDB.Columns.ATTEMPTS]: 1,
  [MutationJobsDB.Columns.LAST_ERROR]:
    status === MutationJobsDB.Status.Failed ? "finalization failed" : null,
  [MutationJobsDB.Columns.CREATED_AT]: new Date(0),
  [MutationJobsDB.Columns.UPDATED_AT]: new Date(0),
  [MutationJobsDB.Columns.COMPLETED_AT]:
    status === MutationJobsDB.Status.Completed ? new Date(0) : null,
});

const observed = (
  override: Partial<MergeCompletionObservation> = {},
): MergeCompletionObservation => ({
  canonicalHeaders: [OTHER_HEADER_HASH],
  canonical: false,
  txCount: 0,
  job: job(MutationJobsDB.Status.Completed),
  journal: Option.none(),
  ...override,
});

describe("merge-complete reconciliation verdict", () => {
  it("is satisfied only once the header left the queue and its confirmed-merge finalization completed", () => {
    expect(mergeCompletionVerdict(observed())).toEqual({
      status: "satisfied",
      nextAction: null,
    });
  });

  it("keeps a still-queued header pending whatever the local rows say", () => {
    for (const txCount of [0, 3])
      expect(
        mergeCompletionVerdict(
          observed({
            canonicalHeaders: [HEADER_HASH],
            canonical: true,
            txCount,
            job: undefined,
          }),
        ).status,
      ).toBe("pending");
  });

  it("never reads surviving block rows of an unqueued header as a completed merge", () => {
    // The merge's local finalization clears the block's rows in the same
    // transaction that folds its ledger delta, so rows that survive are the
    // opposite of completion.
    expect(
      mergeCompletionVerdict(observed({ txCount: 3, job: undefined })).status,
    ).toBe("ambiguous");
    expect(mergeCompletionVerdict(observed({ txCount: 3 })).status).toBe(
      "ambiguous",
    );
  });

  it("does not treat an empty row set without a finalization job as proof", () => {
    // A block without L2 transactions never had rows.
    expect(mergeCompletionVerdict(observed({ job: undefined }))).toMatchObject({
      status: "ambiguous",
      nextAction: expect.stringContaining("no confirmed-merge finalization"),
    });
  });

  it("blocks when the confirmed-merge finalization failed or never finished", () => {
    for (const status of [
      MutationJobsDB.Status.Failed,
      MutationJobsDB.Status.Running,
    ])
      expect(
        mergeCompletionVerdict(observed({ job: job(status) })),
      ).toMatchObject({
        status: "blocked",
        nextAction: expect.stringContaining(status),
      });
  });
});
