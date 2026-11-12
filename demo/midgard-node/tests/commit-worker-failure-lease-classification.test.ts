import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

/**
 * Journal-before-submit ordering is observed by running the real commit
 * submission programs. Only the collaborators that would need L1, a live
 * wallet, or a full ledger are replaced; the sequencing under test is the
 * production one in `src/workers/commit-block-header/submission.ts`.
 */
const submissionTrace = vi.hoisted(() => ({
  calls: [] as string[],
  failJournalPreparation: false,
  /** The pending row holds the commit's signed intent (`recordSignedIntent`). */
  intentJournaled: false,
}));

vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
    DepositsDB: {
      ...actual.DepositsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
    ForcedTransactionsDB: {
      ...actual.ForcedTransactionsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      discardUnsignedJournal: vi.fn(() => Effect.void),
      preparePendingSubmission: vi.fn(() =>
        Effect.suspend(() => {
          submissionTrace.calls.push("prepare-journal");
          return submissionTrace.failJournalPreparation
            ? Effect.fail(new Error("journal insert refused"))
            : Effect.succeed({ kind: "prepared" as const });
        }),
      ),
      retrieveByHeaderHash: vi.fn(() =>
        Effect.sync(() =>
          Option.some({
            [actual.PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
              "33".repeat(32),
            [actual.PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH]:
              submissionTrace.intentJournaled
                ? Buffer.from("22".repeat(32), "hex")
                : null,
            utxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
          }),
        ),
      ),
    },
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      moveFromMempool: vi.fn(() => Effect.void),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
    },
    WithdrawalsDB: {
      ...actual.WithdrawalsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
  };
});

// The commit anchor caps no planned end time.
vi.mock("../src/services/history-commit-window.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/services/history-commit-window.js")
  >()),
  commitEventHorizon: () =>
    Effect.succeed({
      horizonMs: Number.MAX_SAFE_INTEGER,
      anchor: { hash: Buffer.alloc(32), height: 0, slot: 0 },
    }),
}));
vi.mock("../src/forced-orders/horizon.js", () => ({
  forcedOrderHorizon: Effect.succeed(null),
}));
vi.mock("../src/e2e/commit-crash-checkpoint.js", () => ({
  reachCommitCrashCheckpoint: vi.fn(() => Effect.void),
}));
vi.mock(
  "../src/workers/commit-block-header/build-unsigned-tx.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../src/workers/commit-block-header/build-unsigned-tx.js")
    >()),
    buildUnsignedCommitTx: vi.fn(),
  }),
);
vi.mock("../src/workers/commit-block-header/event-roots.js", () => ({
  resolveDepositsRoot: vi.fn(() =>
    Effect.succeed(Option.some("66".repeat(32))),
  ),
  resolveForcedTransactionsRoot: vi.fn(() => Effect.succeed(Option.none())),
  resolveWithdrawalsRoot: vi.fn(() => Effect.succeed(Option.none())),
}));
vi.mock("../src/workers/commit-block-header/pending-journal.js", () => ({
  assertLiveTailCommitBase: vi.fn(() => Effect.void),
  assertPendingJournalCompleteness: vi.fn(() => Effect.void),
  buildPendingJournalMetadata: vi.fn(() =>
    Effect.succeed({ baseRoots: { utxosRoot: "33".repeat(32) } }),
  ),
  resolveLiveTailCommitBase: vi.fn((_contracts: unknown, latest: unknown) =>
    Effect.succeed(latest),
  ),
  resolvePendingJournalLedgerState: vi.fn(() =>
    Effect.succeed({ ledgerDelta: { spent: [], produced: [] } }),
  ),
  revalidateStateQueueLease: vi.fn(() => Effect.void),
}));
vi.mock("../src/workers/commit-block-header/transition-commitments.js", () => ({
  makeEventCommitments: vi.fn(() =>
    Effect.succeed({
      transitionTraceRoot: "44".repeat(32),
      eventToStepRoot: "55".repeat(32),
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    }),
  ),
}));

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { shouldSkipForRegisteredCommitDueWork } from "../src/fibers/block-commitment.should-skip-for-registered-commit-due-work.js";
import {
  classifyCommitWorkerOutputForMutationLease,
  type CommitWorkerFailureJournalEvidence,
} from "../src/fibers/commit-worker-failure-classification.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { buildUnsignedCommitTx } from "../src/workers/commit-block-header/build-unsigned-tx.js";
import { assertLiveTailCommitBase } from "../src/workers/commit-block-header/pending-journal.js";
import { WorkerError } from "../src/workers/utils/common.js";
import {
  baseCommitArgs,
  commitPaths,
  HEADER_HASH,
  NEW_HEADER,
  SUBMIT_FAILURES,
  TAIL_TX_HASH,
} from "./commit-worker-failure-lease-classification.fixture.js";

const failureOutput = {
  type: "FailureOutput",
  error: "provider unavailable before witness assembly",
} as const;

const journalEvidence = ({
  status = PendingBlockFinalizationsDB.OwnBlockStatus.Unlanded,
  intendedTxHash = null,
  dead = false,
}: {
  readonly status?: PendingBlockFinalizationsDB.OwnBlockStatus;
  readonly intendedTxHash?: Buffer | null;
  readonly dead?: boolean;
}): CommitWorkerFailureJournalEvidence => ({
  headerHash: Buffer.from("11".repeat(28), "hex"),
  intendedTxHash,
  status,
  dead,
});

const COMMIT_TX_HASH = Buffer.from("22".repeat(32), "hex");

describe("commit worker failure mutation-lease classification", () => {
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

  // A commit path discards its unsigned journal on a pre-broadcast
  // failure; a row a later lease holder has not discarded yet was never
  // sent either, dead or not.
  it("returns a typed failure normally when every journal row for the lease is unsigned", async () => {
    const result = await Effect.runPromise(
      classifyCommitWorkerOutputForMutationLease({
        output: failureOutput,
        stateQueueLeaseToken: "block_commitment:unsigned",
        retrieveJournalEvidence: () =>
          Effect.succeed([
            journalEvidence({}),
            journalEvidence({ dead: true }),
          ]),
      }),
    );

    expect(result).toStrictEqual(failureOutput);
  });

  // A row that recorded its signed intent may be on L1 whatever its
  // derived status, dead or not.
  for (const evidence of [
    [journalEvidence({ intendedTxHash: COMMIT_TX_HASH })],
    [journalEvidence({}), journalEvidence({ intendedTxHash: COMMIT_TX_HASH })],
    [journalEvidence({ intendedTxHash: COMMIT_TX_HASH, dead: true })],
    [
      journalEvidence({
        status: PendingBlockFinalizationsDB.OwnBlockStatus.Landed,
        intendedTxHash: COMMIT_TX_HASH,
      }),
    ],
    [
      journalEvidence({
        status: PendingBlockFinalizationsDB.OwnBlockStatus.Folded,
        intendedTxHash: COMMIT_TX_HASH,
      }),
    ],
  ]) {
    const statuses = evidence
      .map(
        ({ status, intendedTxHash, dead }) =>
          `${status}${intendedTxHash !== null ? "(signed)" : ""}${dead ? "(dead)" : ""}`,
      )
      .join("+");
    it(`fails closed for ${statuses} durable journal evidence`, async () => {
      const result = await Effect.runPromise(
        classifyCommitWorkerOutputForMutationLease({
          output: failureOutput,
          stateQueueLeaseToken: `block_commitment:${statuses}`,
          retrieveJournalEvidence: () => Effect.succeed(evidence),
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

describe("commit submission journals before it signs and submits", () => {
  beforeEach(() => {
    submissionTrace.calls.length = 0;
    submissionTrace.failJournalPreparation = false;
    vi.mocked(buildUnsignedCommitTx).mockReturnValue(
      Effect.succeed({
        preparedTxHash: "22".repeat(32),
        newHeaderHash: HEADER_HASH,
        newHeader: NEW_HEADER,
        newHeaderCbor: Buffer.from("header"),
        blockEndTimeMs: baseCommitArgs.endTime.getTime(),
        txValidFromMs: 0,
        txValidToMs: 1,
        signAndSubmitProgram: Effect.sync(() => {
          submissionTrace.calls.push("sign-and-submit");
          return "22".repeat(32);
        }),
        txSize: 1,
      }) as never,
    );
  });

  it.each(commitPaths)(
    "%s commit writes the durable journal row before signing and submitting",
    async (_name, run) => {
      const outcome = await run();

      expect(outcome).toMatchObject({ _tag: "Right" });
      expect(submissionTrace.calls).toEqual([
        "prepare-journal",
        "sign-and-submit",
      ]);
    },
  );

  it.each(commitPaths)(
    "%s commit never signs or submits when the journal row cannot be written",
    async (_name, run) => {
      submissionTrace.failJournalPreparation = true;

      const outcome = await run();

      expect(submissionTrace.calls).toEqual(["prepare-journal"]);
      expect(outcome).toMatchObject({ _tag: "Left" });
    },
  );
});

describe("both commit paths classify a submit failure the same way", () => {
  beforeEach(() => {
    submissionTrace.calls.length = 0;
    submissionTrace.failJournalPreparation = false;
    submissionTrace.intentJournaled = false;
    vi.mocked(PendingBlockFinalizationsDB.discardUnsignedJournal).mockClear();
    vi.mocked(assertLiveTailCommitBase).mockReset();
    vi.mocked(assertLiveTailCommitBase).mockReturnValue(Effect.void);
  });

  const armSubmitFailure = (failure: (typeof SUBMIT_FAILURES)[number]) => {
    submissionTrace.intentJournaled = failure.intentJournaled;
    if (failure.liveTail !== undefined)
      vi.mocked(assertLiveTailCommitBase).mockReturnValue(
        Effect.fail(failure.liveTail) as never,
      );
    vi.mocked(buildUnsignedCommitTx).mockReturnValue(
      Effect.succeed({
        preparedTxHash: TAIL_TX_HASH,
        newHeaderHash: HEADER_HASH,
        newHeader: NEW_HEADER,
        newHeaderCbor: Buffer.from("header"),
        blockEndTimeMs: baseCommitArgs.endTime.getTime(),
        txValidFromMs: 0,
        txValidToMs: 1,
        signAndSubmitProgram: Effect.fail(failure.submit),
        txSize: 1,
      }) as never,
    );
  };

  for (const failure of SUBMIT_FAILURES)
    it.each(commitPaths)(`%s: ${failure.name}`, async (_name, run) => {
      armSubmitFailure(failure);

      const outcome = await run();

      expect(outcome).toMatchObject({ _tag: "Right", right: failure.output });
      expect(
        vi.mocked(PendingBlockFinalizationsDB.discardUnsignedJournal),
      ).toHaveBeenCalledTimes(failure.discarded ? 1 : 0);
    });

  it.each(commitPaths)(
    "%s: the tick after a pre-submit validity defer has no due work to wait on",
    async (_name, run) => {
      armSubmitFailure(
        SUBMIT_FAILURES.find(
          ({ name }) => name === "a pre-submit validity defer",
        )!,
      );

      // The action's arm for this output registers nothing.
      expect(await run()).toMatchObject({
        _tag: "Right",
        right: { type: "NothingToCommitOutput" },
      });
      expect(listSlotAwareDueWork()).toEqual([]);
      expect(
        await Effect.runPromise(
          shouldSkipForRegisteredCommitDueWork as Effect.Effect<boolean>,
        ),
      ).toBe(false);
    },
  );
});
