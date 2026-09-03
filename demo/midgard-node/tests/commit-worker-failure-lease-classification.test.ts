import { readFile } from "node:fs/promises";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  classifyCommitWorkerOutputForMutationLease,
  type CommitWorkerFailureJournalEvidence,
} from "../src/fibers/commit-worker-failure-classification.js";
import { WorkerError } from "../src/workers/utils/common.js";

const failureOutput = {
  type: "FailureOutput",
  error: "provider unavailable before witness assembly",
} as const;

const journalEvidence = ({
  status,
  submittedTxHash = null,
}: {
  readonly status: string;
  readonly submittedTxHash?: Buffer | null;
}): CommitWorkerFailureJournalEvidence => ({
  headerHash: Buffer.from("11".repeat(28), "hex"),
  submittedTxHash,
  status,
});

describe("commit worker failure mutation-lease classification", () => {
  it("keeps both commit paths journaled before sign-and-submit can run", async () => {
    const source = await readFile(
      new URL(
        "../src/workers/commit-block-header/submission.ts",
        import.meta.url,
      ),
      "utf8",
    );
    const depositOnly = source.slice(
      source.indexOf("export const submitDepositOnlyCommit"),
      source.indexOf("export const submitTxBackedCommit"),
    );
    const txBacked = source.slice(
      source.indexOf("export const submitTxBackedCommit"),
      source.indexOf(
        "export const deferProcessedCommitPayloadUntilConfirmation",
      ),
    );

    for (const commitPath of [depositOnly, txBacked]) {
      const prepare = commitPath.indexOf(
        "PendingBlockFinalizationsDB.preparePendingSubmission",
      );
      const submit = commitPath.indexOf(
        "Effect.andThen(signAndSubmitProgram)",
        prepare,
      );
      expect(prepare).toBeGreaterThanOrEqual(0);
      expect(submit).toBeGreaterThanOrEqual(0);
      expect(prepare).toBeLessThan(submit);
    }
  });

  it("returns a typed failure normally when no journal exists for the lease token", async () => {
    const result = await Effect.runPromise(
      classifyCommitWorkerOutputForMutationLease({
        output: failureOutput,
        stateQueueLeaseToken: "block_commitment:no-journal",
        retrieveJournalEvidence: () => Effect.succeed([]),
      }),
    );

    expect(result).toStrictEqual(failureOutput);
  });

  for (const evidence of [
    journalEvidence({ status: "pending_submission" }),
    journalEvidence({ status: "abandoned" }),
    journalEvidence({
      status: "submitted_unconfirmed",
      submittedTxHash: Buffer.from("22".repeat(32), "hex"),
    }),
  ]) {
    it(`fails closed for ${evidence.status} durable journal evidence`, async () => {
      const result = await Effect.runPromise(
        classifyCommitWorkerOutputForMutationLease({
          output: failureOutput,
          stateQueueLeaseToken: `block_commitment:${evidence.status}`,
          retrieveJournalEvidence: () => Effect.succeed([evidence]),
        }).pipe(Effect.either),
      );

      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left).toBeInstanceOf(WorkerError);
        expect(result.left.message).toContain(
          "durable mutation preparation may have started",
        );
      }
    });
  }

  it("fails closed when durable journal evidence cannot be queried", async () => {
    const lookupFailure = new Error("database unavailable");
    const result = await Effect.runPromise(
      classifyCommitWorkerOutputForMutationLease({
        output: failureOutput,
        stateQueueLeaseToken: "block_commitment:lookup-failure",
        retrieveJournalEvidence: () => Effect.fail(lookupFailure),
      }).pipe(Effect.either),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left).toBeInstanceOf(WorkerError);
      expect(result.left.message).toContain("safety classification failed");
      expect(result.left.cause).toStrictEqual({
        workerFailure: failureOutput.error,
        journalLookupFailure: lookupFailure,
      });
    }
  });

  it("fails closed when a typed failure has no lease token", async () => {
    let queried = false;
    const result = await Effect.runPromise(
      classifyCommitWorkerOutputForMutationLease({
        output: failureOutput,
        retrieveJournalEvidence: () => {
          queried = true;
          return Effect.succeed([]);
        },
      }).pipe(Effect.either),
    );

    expect(result._tag).toBe("Left");
    expect(queried).toBe(false);
    if (result._tag === "Left") {
      expect(result.left.message).toContain(
        "without a state-queue lease token",
      );
    }
  });

  it("does not classify non-failure worker output", async () => {
    let queried = false;
    const output = { type: "NothingToCommitOutput" } as const;
    const result = await Effect.runPromise(
      classifyCommitWorkerOutputForMutationLease({
        output,
        stateQueueLeaseToken: "block_commitment:not-a-failure",
        retrieveJournalEvidence: () => {
          queried = true;
          return Effect.succeed([]);
        },
      }),
    );

    expect(result).toStrictEqual(output);
    expect(queried).toBe(false);
  });
});
