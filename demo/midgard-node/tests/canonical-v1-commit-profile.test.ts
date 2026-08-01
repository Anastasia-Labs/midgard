import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { TxAdmissionsDB, TxUtils as TxTable } from "@/database/index.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "@/workers/commit-block-header.js";
import { buildUnsignedCommitTx } from "@/workers/commit-block-header/build-unsigned-tx.js";
import {
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "@/workers/commit-block-header/submission.js";
import type { WorkerInput } from "@/workers/utils/commit-block-header.js";
import { processMpfs } from "@/workers/utils/mpf.js";

vi.mock("@/database/index.js", async () => {
  const actual = await vi.importActual<typeof import("@/database/index.js")>(
    "@/database/index.js",
  );
  const workerTx = {
    [actual.TxUtils.Columns.TX_ID]: Buffer.from("02", "hex"),
    [actual.TxUtils.Columns.TX]: Buffer.from("tx"),
    [actual.TxUtils.Columns.TIMESTAMPTZ]: new Date("2026-01-01T00:07:00.999Z"),
  };
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
    MempoolDB: {
      ...actual.MempoolDB,
      retrievePage: vi.fn(() =>
        Effect.succeed({ entries: [], nextCursor: null }),
      ),
    },
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      assertLedgerAuditHealthy: Effect.void,
      revalidateLedgerStoreLease: vi.fn(() => Effect.void),
      stampLedgerPayloadAggregate: vi.fn(() => Effect.void),
      tryWithLedgerStoreLease: vi.fn(
        (
          owner: string,
          program: (activeOwner: string) => Effect.Effect<unknown>,
        ) =>
          program(owner).pipe(
            Effect.map((value) => ({ _tag: "Ran" as const, value })),
          ),
      ),
    },
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      preparePendingSubmission: vi.fn(),
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
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      retrieve: Effect.succeed([workerTx]),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      retrieveProgramMaterialSidecars: vi.fn((txIds: readonly Buffer[]) =>
        Effect.succeed(
          txIds.map((txId) => ({
            txId,
            sidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
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

vi.mock("@/fibers/fetch-and-insert-deposit-utxos.js", () => ({
  fetchAndInsertDepositUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("@/fibers/fetch-and-insert-withdrawal-utxos.js", () => ({
  fetchAndInsertWithdrawalUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("@/fibers/fetch-and-insert-tx-order-utxos.js", () => ({
  fetchAndInsertTxOrderUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("@/e2e/pipelined-commit-crash-checkpoint.js", () => ({
  reachPipelinedCommitCrashCheckpoint: vi.fn(() => Effect.void),
}));
vi.mock("@/operator-wallet-view.js", () => ({
  fetchOperatorWalletView: vi.fn(),
  isPotentiallyStaleOperatorWalletViewError: vi.fn(() => false),
}));
vi.mock("@/workers/commit-block-header/build-unsigned-tx.js", () => ({
  buildUnsignedCommitTx: vi.fn(),
}));
vi.mock("@/workers/commit-block-header/event-roots.js", () => ({
  resolveDepositsRoot: vi.fn(() => Effect.succeed(Option.none())),
  resolveForcedTransactionsRoot: vi.fn(() =>
    Effect.succeed(Option.some("forced-root")),
  ),
  resolveWithdrawalsRoot: vi.fn(() => Effect.succeed(Option.none())),
}));
vi.mock("@/workers/commit-block-header/pending-journal.js", () => ({
  assertLiveTailCommitBase: vi.fn(() => Effect.void),
  assertPendingJournalCompleteness: vi.fn(() => Effect.void),
  buildPendingJournalMetadata: vi.fn(() => Effect.succeed({})),
  resolveLiveTailCommitBase: vi.fn((_contracts: unknown, latest: unknown) =>
    Effect.succeed(latest),
  ),
  resolvePendingJournalLedgerState: vi.fn(() =>
    Effect.succeed({ ledgerDelta: { spent: [], produced: [] } }),
  ),
  revalidateStateQueueLease: vi.fn(() => Effect.void),
}));
vi.mock("@/workers/commit-block-header/transition-commitments.js", () => ({
  makeEventCommitments: vi.fn(() =>
    Effect.succeed({
      transitionTraceRoot: "transition-root",
      eventToStepRoot: "event-root",
      validationTracesRoot: "validation-root",
      withdrawalCount: 0n,
      forcedTransactionCount: 1n,
      l2TransactionCount: 1n,
      depositCount: 0n,
      totalEventCount: 1n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    }),
  ),
}));

vi.mock("@/workers/utils/mpf.js", async () => {
  const actual = await vi.importActual<typeof import("@/workers/utils/mpf.js")>(
    "@/workers/utils/mpf.js",
  );
  const fakeMpf = {
    close: vi.fn(() => Effect.void),
    rootHex: vi.fn(() => Effect.succeed("33".repeat(32))),
    rootIsEmpty: vi.fn(() => Effect.succeed(true)),
    resetToEmpty: vi.fn(() => Effect.void),
  };
  return {
    ...actual,
    configureCommitMpfRuntime: vi.fn(() => Effect.void),
    makeMpfs: Effect.succeed({ ledgerMpf: fakeMpf, transactionsMpf: fakeMpf }),
    processMpfs: vi.fn(() =>
      Effect.fail(new Error("stop at processMpfs observation point")),
    ),
    withMpfRootTransactions: vi.fn(
      (
        _mpfs: readonly unknown[],
        effect: Effect.Effect<unknown, unknown, unknown>,
      ) => effect,
    ),
  };
});

const HEADER_HASH = "11".repeat(28);
const forcedValidationSlotConfig = {
  zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
  zeroSlot: 100,
  slotLength: 1_000,
} as const;
const latestBlock = {
  utxo: {
    txHash: "22".repeat(32),
    outputIndex: 0,
    address: "addr_test1statequeue",
    assets: {},
    datum: "datum",
  },
  datum: { key: "Empty" },
} as never;
const workerInput = {
  data: {
    availableConfirmedBlock: "",
    availableLocalFinalizationBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    forcedValidationSlotConfig,
    ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
    localFinalizationPending: false,
    mempoolTxsCountSoFar: 0,
    sizeOfProcessedTxsSoFar: 0,
    baseSnapshotId: "test",
    stateQueueHasUnmergedTail: false,
  },
} as unknown as WorkerInput;
const contracts = {
  stateQueue: {
    spendingScriptAddress: "addr_test1statequeue",
    policyId: "aa".repeat(28),
  },
} as never;
const deploymentMarker = {
  schemaVersion: "midgard-deployment-marker-v1",
  manifestId: "test-manifest",
} as never;
const fakeLucid = {
  api: {},
  switchToOperatorsMainWallet: Effect.void,
} as never;
const fakeSql = Object.assign(
  ((..._args: readonly unknown[]) =>
    Effect.succeed([])) as unknown as SqlClient.SqlClient,
  { array: vi.fn((values: readonly unknown[]) => values) },
) as unknown as SqlClient.SqlClient;

const runEffect = <A>(effect: Effect.Effect<A, unknown, unknown>) =>
  Effect.runPromise(effect as Effect.Effect<A, unknown, never>);

const forcedEntryMissingMaterial = {
  tx_order_id: Buffer.from("01", "hex"),
  cek_program_material_sidecar_cbor: Buffer.alloc(0),
} as never;

const baseCommitArgs = {
  contracts,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  deploymentMarker,
  latestBlock,
  endTime: new Date("2026-01-01T00:07:00.999Z"),
  includedDepositEntries: [],
  includedDepositEventIds: [],
  includedForcedTransactionEntries: [],
  includedForcedTransactionEventIds: [Buffer.from("01", "hex")],
  includedWithdrawalEntries: [],
  includedWithdrawalEventIds: [],
  workerInput,
  blockEndTimeCapMs: undefined,
  utxoRoot: "33".repeat(32),
  txRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "44".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "66".repeat(32),
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

describe("canonical V1 commit profile", () => {
  it("fails closed on missing forced program material before journal preparation", async () => {
    vi.mocked(buildUnsignedCommitTx).mockReturnValue(
      Effect.succeed({
        newHeaderHash: HEADER_HASH,
        newHeader: {} as never,
        newHeaderCbor: Buffer.from("header"),
        blockEndTimeMs: baseCommitArgs.endTime.getTime(),
        txValidFromMs: 0,
        txValidToMs: 1,
        signAndSubmitProgram: Effect.succeed("22".repeat(32)),
        txSize: 1,
      }),
    );

    const outcome = await runEffect(
      Effect.either(
        submitDepositOnlyCommit({
          ...baseCommitArgs,
          includedForcedTransactionEntries: [forcedEntryMissingMaterial],
        } as unknown as Parameters<typeof submitDepositOnlyCommit>[0]),
      ).pipe(Effect.provideService(Lucid, fakeLucid)),
    );

    expect(outcome._tag).toBe("Left");
    if (outcome._tag !== "Left") return;
    expect(outcome.left).toMatchObject({
      message:
        "Cannot build V1 DA from missing or conflicting forced-transaction program material",
    });
  });

  it("fails closed when a normal transaction has no durable material sidecar", async () => {
    vi.mocked(TxAdmissionsDB.retrieveProgramMaterialSidecars).mockReturnValue(
      Effect.succeed([]),
    );
    vi.mocked(buildUnsignedCommitTx).mockReturnValue(
      Effect.succeed({
        newHeaderHash: HEADER_HASH,
        newHeader: {} as never,
        newHeaderCbor: Buffer.from("header"),
        blockEndTimeMs: baseCommitArgs.endTime.getTime(),
        txValidFromMs: 0,
        txValidToMs: 1,
        signAndSubmitProgram: Effect.succeed("22".repeat(32)),
        txSize: 1,
      }),
    );
    const txId = Buffer.from("02", "hex");
    const processedMempoolTxs = [
      {
        [TxTable.Columns.TX_ID]: txId,
        [TxTable.Columns.TX]: Buffer.from("tx"),
        [TxTable.Columns.TIMESTAMPTZ]: baseCommitArgs.endTime,
      },
    ] as never;

    const outcome = await runEffect(
      Effect.either(
        submitTxBackedCommit({
          ...baseCommitArgs,
          includedForcedTransactionEntries: [],
          includedForcedTransactionEventIds: [],
          transactionsMpf: {} as never,
          processedMempoolTxs,
          mempoolTxHashes: [txId],
          mempoolTxSourceTable: "mempool",
          sizeOfProcessedTxs: 2,
        } as unknown as Parameters<typeof submitTxBackedCommit>[0]),
      ).pipe(
        Effect.provideService(Lucid, fakeLucid),
        Effect.provideService(SqlClient.SqlClient, fakeSql),
      ),
    );

    expect(outcome._tag).toBe("Left");
    if (outcome._tag !== "Left") return;
    expect(outcome.left).toMatchObject({
      message:
        "Cannot build V1 block without one durable program-material sidecar per normal transaction",
    });
  });

  it("fails closed without worker slot mapping and passes the node mapping to processMpfs", async () => {
    const nodeConfig = {
      MPF_ENGINE: "legacy",
      MPF_PAYLOAD_ROOT_CHECK: "off",
      MPF_RECORD_CORPUS: "",
      MEMPOOL_RETRIEVE_PAGE_SIZE: 100,
      COMMIT_BUILD_COST_MODEL: "static",
      COMMIT_MAX_L2_TX_COUNT: 100,
      COMMIT_MAX_LEDGER_OP_COUNT: 100,
      COMMIT_MAX_TRANSITION_STEP_COUNT: 100,
      NETWORK: "Testnet",
      MIN_FEE_A: 0n,
      MIN_FEE_B: 0n,
      VALIDATION_G4_BUCKET_CONCURRENCY: 1,
    } as never;
    const deploymentIdentity = ContractDeploymentIdentity.make({
      kind: "derived",
      deploymentMarker,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    });
    const speculativeBuild = {
      base: {
        headerHash: "aa".repeat(28),
        utxosRoot: "33".repeat(32),
        blockEndTimeMs: Date.parse("2026-01-01T00:06:00.000Z"),
        submittedTxHash: "bb".repeat(32),
      },
      watermarks: {
        depositMs: Date.parse("2026-01-01T00:07:00.999Z"),
        withdrawalMs: Date.parse("2026-01-01T00:07:00.999Z"),
        txOrderMs: Date.parse("2026-01-01T00:07:00.999Z"),
        refreshedAtMs: Date.parse("2026-01-01T00:07:00.999Z"),
      },
      excludedMempoolTxIds: [],
      excludedDepositEventIds: [],
      excludedForcedTransactionEventIds: [],
      excludedWithdrawalEventIds: [],
    } as const;
    const runWorker = (input: typeof workerInput) =>
      runEffect(
        Effect.either(
          runCommitBlockHeaderWorkerProgram(input, () =>
            Effect.succeed({
              type: "InvalidateSpeculativeCandidate",
              reason: "T1",
            }),
          ),
        ).pipe(
          Effect.provideService(NodeConfig, nodeConfig),
          Effect.provideService(ContractDeploymentIdentity, deploymentIdentity),
        ),
      );

    vi.mocked(processMpfs).mockClear();
    const missingConfigOutcome = await runWorker({
      data: {
        ...workerInput.data,
        speculativeBuild,
        forcedValidationSlotConfig: undefined,
      },
    } as never);
    expect(missingConfigOutcome._tag).toBe("Left");
    expect(processMpfs).not.toHaveBeenCalled();
    if (missingConfigOutcome._tag === "Left") {
      expect(String(missingConfigOutcome.left)).toContain(
        "missing its node-selected slot configuration",
      );
    }

    vi.mocked(processMpfs).mockClear();
    const suppliedConfigOutcome = await runWorker({
      data: { ...workerInput.data, speculativeBuild },
    } as never);
    expect(suppliedConfigOutcome._tag).toBe("Left");
    expect(processMpfs).toHaveBeenCalledTimes(1);
    const processConfig = vi.mocked(processMpfs).mock.calls[0]?.[3] as {
      readonly forcedValidation?: {
        readonly slotForUnixTime: (unixTimeMs: number) => bigint;
      };
    };
    expect(processConfig.forcedValidation).toBeDefined();
    expect(
      processConfig.forcedValidation?.slotForUnixTime(
        Date.parse("2026-01-01T00:07:00.999Z"),
      ),
    ).toBe(110n);
  });
});
