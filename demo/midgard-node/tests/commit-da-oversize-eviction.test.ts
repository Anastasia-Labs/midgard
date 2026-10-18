import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Effect, Either, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
import { DepositsDB } from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import type { EntryWithTimeStamp } from "../src/database/utils/tx.js";
import { COMMIT_REJECT_CODE_DA_OVERSIZE } from "../src/mpf/commit-rejection.js";
import { buildOnVerifiedCommitBaseProgram } from "../src/workers/commit-block-header.build-on-verified-base-program.js";
import { databaseOperationsProgram } from "../src/workers/commit-block-header.database-operations-program.js";
import {
  attributeDaFrameRefusal,
  CommitDaFrameRefusedNormalTxError,
  type CommitEviction,
} from "../src/workers/commit-block-header/commit-da-oversize-eviction.js";
import { submitTxBackedCommit } from "../src/workers/commit-block-header/submission.submit-tx-backed-commit.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  blockContentFor,
  header,
  mkCandidate,
  REFUSAL,
  runQuietSubmission,
} from "./helpers/commit-da-frame-fixtures.js";

// Reach the exact size check of the tx-backed submission, then fail the first
// journal preparation step. No transaction is signed or submitted.
vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      assertNoUnreconciledSignedSubmission: Effect.void,
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
    },
  };
});
// The rebuild loop runs under a producer permit the test does not exercise.
vi.mock("../src/services/follower-write-gate.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/services/follower-write-gate.js")
  >("../src/services/follower-write-gate.js");
  return { ...actual, assertFollowerWrite: () => Effect.void };
});
vi.mock("../src/workers/commit-block-header/pending-journal.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/workers/commit-block-header/pending-journal.js")
  >("../src/workers/commit-block-header/pending-journal.js");
  return {
    ...actual,
    revalidateStateQueueLease: () => Effect.void,
    assertPendingJournalCompleteness: () => Effect.void,
    resolveLiveTailCommitBase: () => Effect.succeed({}),
    buildPendingJournalMetadata: () =>
      Effect.fail("later journal preparation failure"),
  };
});
vi.mock("../src/workers/commit-block-header/build-unsigned-tx.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/workers/commit-block-header/build-unsigned-tx.js")
  >("../src/workers/commit-block-header/build-unsigned-tx.js");
  return {
    ...actual,
    buildUnsignedCommitTx: () =>
      Effect.succeed({
        newHeaderHash: "33".repeat(28),
        newHeader: header,
        newHeaderCbor: Buffer.alloc(0),
        blockEndTimeMs: Number(header.endTime),
        signAndSubmitProgram: Effect.die("must not sign"),
        txSize: 1,
      }),
  };
});
// The commit build itself is faked in the rebuild tests.
vi.mock(
  "../src/workers/commit-block-header.build-on-verified-base-program.js",
  () => ({ buildOnVerifiedCommitBaseProgram: vi.fn() }),
);

const LIMIT = maxDaPayloadInnerBytes(readDaHardeningConfig().envelopeMode);
const idHex = (seed: number) => seed.toString(16).padStart(64, "0");

/**
 * The real tx-backed submission of a block of `txs`, each carrying a
 * descriptor `descriptorBytes` long, with one due deposit when `deposit`.
 */
const submit = (
  txs: readonly EntryWithTimeStamp[],
  { descriptorBytes = 146, deposit = false } = {},
) => {
  const content = blockContentFor(txs, { descriptorBytes });
  const depositEntry = {
    [DepositsDB.Columns.ID]: Buffer.alloc(38, 1),
    [DepositsDB.Columns.INFO]: Buffer.from("d87980", "hex"),
    [DepositsDB.Columns.INCLUSION_TIME]: new Date(Number(header.endTime)),
  };
  return runQuietSubmission(
    submitTxBackedCommit({
      contracts: {},
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      deploymentMarker: {},
      latestBlock: {},
      endTime: new Date(Number(header.endTime)),
      ...content,
      includedDepositEntries: deposit ? [depositEntry] : [],
      includedDepositEventIds: deposit
        ? [depositEntry[DepositsDB.Columns.ID]]
        : [],
      includedForcedTransactionEventIds: [],
      includedWithdrawalEventIds: [],
      workerInput: {
        data: { mempoolTxsCountSoFar: 0, sizeOfProcessedTxsSoFar: 0 },
      },
      utxoRoot: header.utxosRoot,
      txRoot: header.transactionsRoot,
      transitionTraceRoot: header.transitionTraceRoot,
      eventToStepRoot: header.eventToStepRoot,
      validationTracesRoot: header.validationTracesRoot,
      transitionStepCount: txs.length + (deposit ? 1 : 0),
      validationTraceCount: txs.length,
      ledgerDelta: { spent: [], produced: [] },
      selectedBaseUtxosRoot: header.prevUtxosRoot,
      implicitGenesisEntries: [],
      nativeMpfReplay: {},
      transactionsMpf: {},
      mempoolTxHashes: [],
      mempoolTxSourceTable: "mempool",
      sizeOfProcessedTxs: 0,
    } as unknown as Parameters<typeof submitTxBackedCommit>[0]),
  );
};

describe("exact DA frame refusal of a lone normal transaction", () => {
  it("names the transaction whose block alone the exact check refuses", async () => {
    const result = await submit([mkCandidate(1)], { descriptorBytes: LIMIT });
    expect(Either.isLeft(result)).toBe(true);
    const error = (result as Either.Left<unknown, unknown>).left;
    expect(error).toBeInstanceOf(CommitDaFrameRefusedNormalTxError);
    expect(error).toMatchObject({ txIdHex: idHex(1) });
  });

  it("names no transaction when the refused block holds more than one", async () => {
    const result = await submit([mkCandidate(1), mkCandidate(2)], {
      descriptorBytes: LIMIT,
    });
    const error = (result as Either.Left<unknown, unknown>).left;
    expect(error).toBeInstanceOf(DatabaseError);
    expect(error).not.toBeInstanceOf(CommitDaFrameRefusedNormalTxError);
  });

  it("names no transaction when the refused block holds a due event", async () => {
    const result = await submit([mkCandidate(1)], {
      descriptorBytes: LIMIT,
      deposit: true,
    });
    const error = (result as Either.Left<unknown, unknown>).left;
    expect(error).toBeInstanceOf(DatabaseError);
    expect(error).not.toBeInstanceOf(CommitDaFrameRefusedNormalTxError);
  });

  it("passes the refusal of a block it names no transaction for through unchanged", () => {
    const refusal = new DatabaseError({
      table: "pending_block_finalizations",
      message: REFUSAL,
      cause: "inner_bytes=2",
    });
    const tx = (seed: number) => ({ tx_id: Buffer.from(idHex(seed), "hex") });
    for (const block of [
      { txs: [tx(1), tx(2)], deposits: [] },
      { txs: [tx(1)], deposits: [{}] },
    ])
      expect(
        Effect.runSync(
          Effect.flip(
            attributeDaFrameRefusal(refusal, {
              processedMempoolTxs: block.txs,
              includedDepositEntries: block.deposits,
              includedForcedTransactionEntries: [],
              includedWithdrawalEntries: [],
            }),
          ),
        ),
      ).toBe(refusal);
  });

  it("admits a lone transaction whose block fits", async () => {
    expect(await submit([mkCandidate(1)])).toEqual(
      Either.left("later journal preparation failure"),
    );
  });
});

type BuildArgs = Parameters<typeof buildOnVerifiedCommitBaseProgram>;

/**
 * One tick of the commit worker's build under the rebuild loop. Each build
 * forks a native context; `outcome` decides the build's result from the
 * evictions it got. A tick that builds more than twice fails the test.
 */
const runRebuilds = async (
  outcome: (
    evictions: ReadonlyMap<string, CommitEviction>,
    attempt: number,
  ) => Effect.Effect<WorkerOutput, unknown>,
) => {
  const discarded: number[] = [];
  const seen: Record<string, string>[] = [];
  const nativeMpfState: {
    context?: { client: unknown; handle: unknown };
    preserve: boolean;
  } = { preserve: false };
  const client = {
    discard: vi.fn(async (handle: { id: number }) => {
      discarded.push(handle.id);
    }),
  };
  vi.mocked(buildOnVerifiedCommitBaseProgram).mockReset();
  vi.mocked(buildOnVerifiedCommitBaseProgram).mockImplementation(((
    ...args: BuildArgs
  ) =>
    Effect.suspend(() => {
      const evictions = args[6] ?? new Map<string, CommitEviction>();
      seen.push(
        Object.fromEntries([...evictions].map(([id, { code }]) => [id, code])),
      );
      if (seen.length > 2) return Effect.die("more than one rebuild in a tick");
      // A build forks only once the previous fork was discarded.
      expect(nativeMpfState.context).toBeUndefined();
      nativeMpfState.context = { client, handle: { id: seen.length } };
      return outcome(evictions, seen.length);
    })) as unknown as typeof buildOnVerifiedCommitBaseProgram);
  const result = await Effect.runPromise(
    Effect.either(
      databaseOperationsProgram(
        { history: undefined } as never,
        {} as never,
        undefined,
        {} as never,
        nativeMpfState as never,
        () => Effect.die("unused"),
      ),
    ).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<Either.Either<WorkerOutput, unknown>>,
  );
  return { result, seen, discarded, nativeMpfState };
};

const refusedFor = (seed: number) =>
  new CommitDaFrameRefusedNormalTxError({
    txIdHex: idHex(seed),
    detail: "inner_bytes=2",
  });

describe("rebuild without a transaction the exact DA frame refuses alone", () => {
  it("rejects the transaction and builds the next block in the same tick", async () => {
    const committed = {
      type: "SubmittedAwaitingConfirmationOutput",
    } as unknown as WorkerOutput;
    const run = await runRebuilds((evictions) =>
      evictions.has(idHex(1))
        ? Effect.succeed(committed)
        : Effect.fail(refusedFor(1)),
    );
    expect(run.result).toEqual(Either.right(committed));
    expect(run.seen).toEqual([
      {},
      { [idHex(1)]: COMMIT_REJECT_CODE_DA_OVERSIZE },
    ]);
    // The refused build's fork was discarded before the rebuild forked.
    expect(run.discarded).toEqual([1]);
  });

  it("rejects one lone refused transaction per tick, and each in turn across ticks", async () => {
    // The pending transactions the DA frame refuses alone; a build rejects
    // its evictions before validation, durably.
    const oversize = [1, 2, 3];
    const rejected = new Set<number>();
    const committed = {
      type: "SubmittedAwaitingConfirmationOutput",
    } as unknown as WorkerOutput;
    const ticks = [];
    for (let tick = 0; tick < oversize.length; tick += 1)
      ticks.push(
        await runRebuilds((evictions) => {
          for (const seed of oversize)
            if (evictions.has(idHex(seed))) rejected.add(seed);
          const refused = oversize.find((seed) => !rejected.has(seed));
          return refused === undefined
            ? Effect.succeed(committed)
            : Effect.fail(refusedFor(refused));
        }),
      );
    expect(ticks.map(({ seen }) => seen)).toEqual(
      oversize.map((seed) => [
        {},
        { [idHex(seed)]: COMMIT_REJECT_CODE_DA_OVERSIZE },
      ]),
    );
    // The second refusal of a tick holds it; the last tick commits.
    expect(ticks.map(({ result }) => result)).toMatchObject([
      { _tag: "Left", left: { txIdHex: idHex(2) } },
      { _tag: "Left", left: { txIdHex: idHex(3) } },
      { _tag: "Right", right: committed },
    ]);
    expect([...rejected]).toEqual(oversize);
  });

  it("holds the build on any other refusal", async () => {
    const refusal = new Error(REFUSAL);
    const run = await runRebuilds(() => Effect.fail(refusal));
    expect(run.result).toEqual(Either.left(refusal));
    expect((run.result as Either.Left<unknown, unknown>).left).toBe(refusal);
    expect(run.seen).toHaveLength(1);
    expect(run.discarded).toEqual([]);
  });

  it("holds the build when an evicted transaction is refused again", async () => {
    const run = await runRebuilds(() => Effect.fail(refusedFor(1)));
    expect(run.result).toMatchObject({
      _tag: "Left",
      left: { txIdHex: idHex(1) },
    });
    expect(run.seen).toHaveLength(2);
  });
});
