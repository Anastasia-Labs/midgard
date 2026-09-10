import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
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
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      stampLedgerPayloadAggregate: vi.fn(() => Effect.void),
    },
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      markAbandoned: vi.fn(() => Effect.void),
      markSubmitted: vi.fn(() => Effect.void),
      preparePendingSubmission: vi.fn(() =>
        Effect.suspend(() => {
          submissionTrace.calls.push("prepare-journal");
          return submissionTrace.failJournalPreparation
            ? Effect.fail(new Error("journal insert refused"))
            : Effect.void;
        }),
      ),
      retrieveByHeaderHash: vi.fn(() =>
        Effect.succeed(
          Option.some({
            [actual.PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
              "33".repeat(32),
            utxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
          }),
        ),
      ),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      retrieveProgramMaterialSidecars: vi.fn((txIds: readonly Buffer[]) =>
        Effect.succeed(
          txIds.map((txId) => ({
            txId,
            sidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
          })),
        ),
      ),
    },
    WithdrawalsDB: {
      ...actual.WithdrawalsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
  };
});

vi.mock("../src/fibers/fetch-and-insert-deposit-utxos.js", () => ({
  fetchAndInsertDepositUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/fibers/fetch-and-insert-withdrawal-utxos.js", () => ({
  fetchAndInsertWithdrawalUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/fibers/fetch-and-insert-tx-order-utxos.js", () => ({
  fetchAndInsertTxOrderUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/e2e/pipelined-commit-crash-checkpoint.js", () => ({
  reachPipelinedCommitCrashCheckpoint: vi.fn(() => Effect.void),
}));
vi.mock("../src/operator-wallet-view.js", () => ({
  fetchOperatorWalletView: vi.fn(),
  isPotentiallyStaleOperatorWalletViewError: vi.fn(() => false),
}));
vi.mock("../src/workers/commit-block-header/build-unsigned-tx.js", () => ({
  buildUnsignedCommitTx: vi.fn(),
}));
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
import { TxUtils as TxTable } from "../src/database/index.js";
import {
  classifyCommitWorkerOutputForMutationLease,
  type CommitWorkerFailureJournalEvidence,
} from "../src/fibers/commit-worker-failure-classification.js";
import { Lucid } from "../src/services/index.js";
import { buildUnsignedCommitTx } from "../src/workers/commit-block-header/build-unsigned-tx.js";
import {
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "../src/workers/commit-block-header/submission.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
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

/**
 * A structurally complete V1 header, so the production pre-submit DA sizing
 * runs over real field values instead of dying on an empty stub.
 */
const NEW_HEADER = {
  prevUtxosRoot: "33".repeat(32),
  utxosRoot: "33".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: "66".repeat(32),
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
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "77".repeat(32),
  protocolVersion: 1n,
} as never;

const HEADER_HASH = "11".repeat(28);
const workerInput = {
  data: {
    availableConfirmedBlock: "",
    availableLocalFinalizationBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    forcedValidationSlotConfig: {
      zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
      zeroSlot: 100,
      slotLength: 1_000,
    },
    ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
    localFinalizationPending: false,
    mempoolTxsCountSoFar: 0,
    sizeOfProcessedTxsSoFar: 0,
    baseSnapshotId: "test",
    stateQueueHasUnmergedTail: false,
  },
} as unknown as WorkerInput;
const fakeLucid = {
  api: {},
  switchToOperatorsMainWallet: Effect.void,
} as never;
const fakeSql = Object.assign(
  ((..._args: readonly unknown[]) =>
    Effect.succeed([])) as unknown as SqlClient.SqlClient,
  { array: vi.fn((values: readonly unknown[]) => values) },
) as unknown as SqlClient.SqlClient;

const baseCommitArgs = {
  contracts: {
    stateQueue: {
      spendingScriptAddress: "addr_test1statequeue",
      policyId: "aa".repeat(28),
    },
  } as never,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  deploymentMarker: {
    schemaVersion: "midgard-deployment-marker-v1",
    manifestId: "test-manifest",
  } as never,
  latestBlock: {
    utxo: {
      txHash: "22".repeat(32),
      outputIndex: 0,
      address: "addr_test1statequeue",
      assets: {},
      datum: "datum",
    },
    datum: { key: "Empty" },
  } as never,
  endTime: new Date("2026-01-01T00:07:00.999Z"),
  includedDepositEntries: [],
  includedDepositEventIds: [Buffer.from("01", "hex")],
  includedForcedTransactionEntries: [],
  includedForcedTransactionEventIds: [],
  includedWithdrawalEntries: [],
  includedWithdrawalEventIds: [],
  workerInput,
  blockEndTimeCapMs: undefined,
  utxoRoot: "33".repeat(32),
  txRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "44".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceMembers: [],
  eventToStepMembers: [],
  validationTraceMembers: [],
  transitionStepCount: 0,
  validationTraceCount: 0,
  utxoPayloadEntries: [],
  ledgerDelta: { spent: [], produced: [] },
  utxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
  selectedBaseUtxosRoot: "33".repeat(32),
  implicitGenesisEntries: [],
  beforePendingJournalInsert: () => Effect.void,
  nativeMpfReplay: undefined,
} as const;

/**
 * A real canonical V1 transaction from the transaction-root golden fixture, so
 * the production DA-sizing path decodes genuine transaction bytes rather than
 * being short-circuited by a mock.
 */
const goldenTx = (
  JSON.parse(
    readFileSync(
      fileURLToPath(
        new URL(
          "./fixtures/transaction-root-v1.generated.json",
          import.meta.url,
        ),
      ),
      "utf8",
    ),
  ) as {
    readonly transactions: readonly {
      readonly txIdHex: string;
      readonly canonicalTransactionCborHex: string;
    }[];
  }
).transactions[0]!;
const goldenTxId = Buffer.from(goldenTx.txIdHex, "hex");
const processedMempoolTxs = [
  {
    [TxTable.Columns.TX_ID]: goldenTxId,
    [TxTable.Columns.TX]: Buffer.from(
      goldenTx.canonicalTransactionCborHex,
      "hex",
    ),
    [TxTable.Columns.TIMESTAMPTZ]: new Date("2026-01-01T00:07:00.999Z"),
  },
] as never;

const runDepositOnlyCommit = () =>
  Effect.runPromise(
    Effect.either(
      submitDepositOnlyCommit(
        baseCommitArgs as unknown as Parameters<
          typeof submitDepositOnlyCommit
        >[0],
      ),
    ).pipe(
      Effect.provideService(Lucid, fakeLucid),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
    ) as Effect.Effect<unknown, never, never>,
  );

const runTxBackedCommit = () =>
  Effect.runPromise(
    Effect.either(
      submitTxBackedCommit({
        ...baseCommitArgs,
        transactionsMpf: {} as never,
        processedMempoolTxs,
        mempoolTxHashes: [goldenTxId],
        mempoolTxSourceTable: "mempool",
        sizeOfProcessedTxs: 2,
      } as unknown as Parameters<typeof submitTxBackedCommit>[0]),
    ).pipe(
      Effect.provideService(Lucid, fakeLucid),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
    ) as Effect.Effect<unknown, never, never>,
  );

const commitPaths = [
  ["deposit-only", runDepositOnlyCommit],
  ["tx-backed", runTxBackedCommit],
] as const;

describe("commit submission journals before it signs and submits", () => {
  beforeEach(() => {
    submissionTrace.calls.length = 0;
    submissionTrace.failJournalPreparation = false;
    vi.mocked(PendingBlockFinalizationsDB.markSubmitted).mockClear();
    vi.mocked(buildUnsignedCommitTx).mockReturnValue(
      Effect.succeed({
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
      expect(
        vi.mocked(PendingBlockFinalizationsDB.markSubmitted),
      ).toHaveBeenCalledTimes(1);
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
