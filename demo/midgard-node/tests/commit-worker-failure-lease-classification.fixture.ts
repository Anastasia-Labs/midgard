/**
 * The commit submission programs' inputs for
 * `commit-worker-failure-lease-classification.test.ts`, which mocks their
 * collaborators before this module loads them, and the submit failures fed to
 * both commit paths.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import {
  PendingBlockFinalizationsDB,
  TxUtils as TxTable,
} from "../src/database/index.js";
import { COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE } from "../src/database/pendingBlockFinalizations.assert-canonical-event-members.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import { Lucid, NodeConfig } from "../src/services/index.js";
import {
  NoInlineSubmitDefer,
  TxSignError,
  TxSubmitError,
} from "../src/transactions/utils.js";
import { commitSubmitDeferError } from "../src/workers/commit-block-header/build-unsigned-tx.js";
import {
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "../src/workers/commit-block-header/submission.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

/**
 * A structurally complete V1 header, so the production pre-submit DA sizing
 * runs over real field values instead of dying on an empty stub.
 */
export const NEW_HEADER = {
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

export const HEADER_HASH = "11".repeat(28);
const workerInput = {
  data: {
    availableConfirmedBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    forcedValidationSlotConfig: {
      zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
      zeroSlot: 100,
      slotLength: 1_000,
    },
    ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
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
  {
    array: vi.fn((values: readonly unknown[]) => values),
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;

export const baseCommitArgs = {
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
  ledgerDelta: { spent: [], produced: [] },
  selectedBaseUtxosRoot: "33".repeat(32),
  implicitGenesisEntries: [],
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

/** The final recheck reads the commit-event depth (a fixture has no permit). */
const depthConfig = makeTestNodeConfig({ COMMIT_EVENT_DEPTH: 0 });

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
      Effect.provideService(NodeConfig, depthConfig),
      Effect.provideService(FollowerWriteFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
    ) as Effect.Effect<unknown, never, never>,
  );

const runTxBackedCommit = () =>
  Effect.runPromise(
    Effect.either(
      submitTxBackedCommit({
        ...baseCommitArgs,
        transactionsMpf: {
          diagnostics: () => Effect.succeed({ entries: 0 }),
        } as never,
        processedMempoolTxs,
        mempoolTxHashes: [goldenTxId],
        mempoolTxSourceTable: "mempool",
        sizeOfProcessedTxs: 2,
      } as unknown as Parameters<typeof submitTxBackedCommit>[0]),
    ).pipe(
      Effect.provideService(Lucid, fakeLucid),
      Effect.provideService(NodeConfig, depthConfig),
      Effect.provideService(FollowerWriteFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
    ) as Effect.Effect<unknown, never, never>,
  );

export const commitPaths = [
  ["deposit-only", runDepositOnlyCommit],
  ["tx-backed", runTxBackedCommit],
] as const;

export const TAIL_TX_HASH = "22".repeat(32);

const submitFailure = (cause: unknown) =>
  new TxSubmitError({
    message: "Failed to submit transaction",
    cause,
    txHash: TAIL_TX_HASH,
  });

/** The node's refusal of a commit spending its tail: only a provider answers
 * it, so only once the gate journaled the signed intent. */
const tailSpentRejection = new OgmiosJsonRpcError({
  code: 3117,
  message: "The transaction contains unknown UTxO references as inputs.",
  data: {
    unknownOutputReferences: [{ transaction: { id: TAIL_TX_HASH }, index: 0 }],
  },
  method: "submitTransaction",
  id: null,
});

const preSubmitDefer = new NoInlineSubmitDefer({
  callerLabel: "commit-block",
  kind: "pre_submit_validity",
  key: `commit-block:${HEADER_HASH}`,
  txHash: TAIL_TX_HASH,
  currentSlot: 100,
  targetSlot: 105,
  dueSlot: 105,
  waitMs: 5_000,
  slotSource: "provider",
  dependencyKey: `commit-block:${HEADER_HASH}`,
  invalidationKey: `commit-block:${HEADER_HASH}`,
});

/** One failure fed to both commit paths, and the output each must give. */
export const SUBMIT_FAILURES: readonly {
  readonly name: string;
  /** The submit program's failure, after the live-tail recheck passed. */
  readonly submit?: unknown;
  /** The live-tail recheck's failure, before anything is signed. */
  readonly liveTail?: unknown;
  readonly intentJournaled: boolean;
  readonly output: Readonly<Record<string, unknown>>;
  /** The commit path discards its unsigned journal. */
  readonly discarded: boolean;
}[] = [
  {
    name: "a pre-broadcast gate refusal",
    submit: submitFailure(
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message: COMMIT_ANCHOR_NOT_CANONICAL_MESSAGE,
        cause: HEADER_HASH,
      }),
    ),
    intentJournaled: false,
    output: {
      type: "SkippedSubmissionOutput",
      submitRefusal: expect.objectContaining({
        code: "COMMIT_ANCHOR_NOT_CANONICAL",
      }),
    },
    discarded: true,
  },
  {
    name: "a signing failure",
    submit: new TxSignError({
      message: "Failed to sign transaction",
      cause: new Error("wallet unavailable"),
      txHash: TAIL_TX_HASH,
    }),
    intentJournaled: false,
    output: {
      type: "SkippedSubmissionOutput",
      submitRefusal: expect.objectContaining({ code: "SIGNING_FAILED" }),
    },
    discarded: true,
  },
  {
    name: "a stale tail",
    liveTail: new SDK.StateQueueError({
      message: "Commit base is stale",
      cause: `tail=${TAIL_TX_HASH}#0`,
    }),
    intentJournaled: false,
    output: { type: "NothingToCommitOutput" },
    discarded: true,
  },
  {
    // The tail-naming arm's only trigger: it is retained, never discarded.
    name: "a retained intent the node refused naming the tail",
    submit: submitFailure(tailSpentRejection),
    intentJournaled: true,
    output: {
      type: "FailureOutput",
      error: expect.stringContaining(
        "Signed commit intent retained for canonical reconciliation",
      ),
    },
    discarded: false,
  },
  {
    // Nothing was sent and nothing is registered: the next tick rebuilds it.
    name: "a pre-submit validity defer",
    submit: commitSubmitDeferError(preSubmitDefer, HEADER_HASH),
    intentJournaled: false,
    output: { type: "NothingToCommitOutput" },
    discarded: true,
  },
];
