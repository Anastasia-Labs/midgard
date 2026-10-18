import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  computeLedgerMpfRootFromLedgerEntries,
  processMpfs,
} from "../src/mpf/index.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";

vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  const workerTx = {
    [actual.TxUtils.Columns.TX_ID]: Buffer.from("02", "hex"),
    [actual.TxUtils.Columns.TX]: Buffer.from("tx"),
    [actual.TxUtils.Columns.TIMESTAMPTZ]: new Date("2026-01-01T00:07:00.999Z"),
  };
  return {
    ...actual,
    // The commit base is an empty confirmed ledger.
    ConfirmedLedgerDB: {
      ...actual.ConfirmedLedgerDB,
      retrieve: Effect.succeed([]),
    },
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
    },
    WithdrawalsDB: {
      ...actual.WithdrawalsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
  };
});

vi.mock(
  "../src/transactions/state-queue/confirmed-ledger-snapshot.js",
  async () => {
    const actual = await vi.importActual<
      typeof import("../src/transactions/state-queue/confirmed-ledger-snapshot.js")
    >("../src/transactions/state-queue/confirmed-ledger-snapshot.js");
    return {
      ...actual,
      materializeConfirmedLedgerSnapshot: vi.fn(() =>
        Effect.succeed({
          entries: [],
          baseRoot: "33".repeat(32),
          root: "33".repeat(32),
          deltaChain: [],
          delta: { spent: [], produced: [] },
        }),
      ),
    };
  },
);

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
// The clock fits, no event is pending, and DA admits the placeholder tx.
vi.mock("../src/workers/commit-block-header.plan-event-window.js", () => {
  const blockTimeMs = Date.parse("2026-01-01T00:07:00.999Z");
  const counts = { deposits: 0, forcedTransactions: 0, withdrawals: 0 };
  const window = { mode: "normal", endTimeMs: blockTimeMs, counts };
  const plan = { kind: "selected", blockTimeMs, groups: [] };
  return {
    commitEventLimitsFromConsensusProfile: () => ({}),
    planHistoryCommitEventWindow: () =>
      Effect.succeed({
        ...plan,
        clockFit: { status: "fits", resolvedEndTime: blockTimeMs + 1 },
        window: { ...window, includedInstantCount: 0 },
      }),
  };
});
vi.mock("../src/workers/commit-block-header.plan-additive-da.js", () => ({
  planAdditiveCommitDa: (input: { window: unknown; candidates: unknown[] }) =>
    Effect.succeed({
      kind: "selected",
      window: input.window,
      candidateCount: input.candidates.length,
      upperBoundBytes: 0n,
    }),
}));
vi.mock("../src/forced-orders/horizon.js", () => ({
  forcedOrderHorizon: Effect.succeed(null),
}));
vi.mock("../src/e2e/commit-crash-checkpoint.js", () => ({
  reachCommitCrashCheckpoint: vi.fn(() => Effect.void),
}));
vi.mock("../src/workers/commit-block-header/build-unsigned-tx.js", () => ({
  buildUnsignedCommitTx: vi.fn(),
}));
vi.mock("../src/workers/commit-block-header/event-roots.js", () => ({
  resolveDepositsRoot: vi.fn(() => Effect.succeed(Option.none())),
  resolveForcedTransactionsRoot: vi.fn(() =>
    Effect.succeed(Option.some("forced-root")),
  ),
  resolveWithdrawalsRoot: vi.fn(() => Effect.succeed(Option.none())),
}));
vi.mock("../src/workers/commit-block-header/pending-journal.js", () => ({
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
vi.mock("../src/workers/commit-block-header/transition-commitments.js", () => ({
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

vi.mock("../src/mpf/index.js", async () => {
  const actual = await vi.importActual<typeof import("../src/mpf/index.js")>(
    "../src/mpf/index.js",
  );
  return {
    ...actual,
    configureCommitMpfRuntime: vi.fn(() => Effect.void),
    processMpfs: vi.fn(() =>
      Effect.fail(new Error("stop at processMpfs observation point")),
    ),
  };
});

// The worker forks the Architecture G owner's root before it processes the
// block; a stub port client stands in for the owner process.
vi.mock("../src/services/mpf-native-owner/client.js", () => ({
  NativeMpfWorkerPortClient: class {
    fork = vi.fn(async () => 1);
    discard = vi.fn(async () => undefined);
    retainForJournal = vi.fn(async () => undefined);
    close = vi.fn();
  },
}));

const forcedValidationSlotConfig = {
  zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
  zeroSlot: 100,
  slotLength: 1_000,
} as const;
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
  {
    array: vi.fn((values: readonly unknown[]) => values),
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;

/** The commit path gates its SQL writes on history authority; these unit
 * runs stand in for a database fixture with no acquired owner. */
const runEffect = <A>(effect: Effect.Effect<A, unknown, unknown>) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provideService(FollowerWriteFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
    ) as Effect.Effect<A, unknown, never>,
  );

describe("canonical V1 commit profile", () => {
  it("fails closed without worker slot mapping and passes the node mapping to processMpfs", async () => {
    const nodeConfig = {
      MPF_PAYLOAD_ROOT_CHECK: "off",
      MPF_RECORD_CORPUS: "",
      MEMPOOL_RETRIEVE_PAGE_SIZE: 100,
      COMMIT_BUILD_COST_MODEL: "static",
      COMMIT_MAX_L2_TX_COUNT: 100,
      COMMIT_MAX_LEDGER_OP_COUNT: 100,
      COMMIT_MAX_TRANSITION_STEP_COUNT: 100,
      BLOCK_EX_UNITS_BUDGET: { memory: 33_000_000n, steps: 20_000_000_000n },
      NETWORK: "Testnet",
      MIN_FEE_A: 0n,
      MIN_FEE_B: 0n,
      VALIDATION_G4_BUCKET_CONCURRENCY: 1,
      COMMIT_EVENT_DEPTH: 0,
    } as never;
    const deploymentIdentity = ContractDeploymentIdentity.make({
      kind: "derived",
      deploymentMarker,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });
    const runWorker = (input: typeof workerInput) =>
      runEffect(
        Effect.either(
          runCommitBlockHeaderWorkerProgram(input, undefined, () =>
            Effect.succeed(fakeLucid),
          ),
        ).pipe(
          Effect.provideService(NodeConfig, nodeConfig),
          Effect.provideService(ContractDeploymentIdentity, deploymentIdentity),
          withoutFollowerJournal,
        ),
      );

    vi.mocked(processMpfs).mockClear();
    const nativeMpf = {
      port: {} as MessagePort,
      durableRoot: await Effect.runPromise(
        computeLedgerMpfRootFromLedgerEntries([]),
      ),
      ownerBinarySha256: "ab".repeat(32),
    };
    const missingConfigOutcome = await runWorker({
      nativeMpf,
      data: {
        ...workerInput.data,
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
      nativeMpf,
      data: workerInput.data,
    } as never);
    expect(suppliedConfigOutcome._tag).toBe("Left");
    expect(processMpfs).toHaveBeenCalledTimes(1);
    const processConfig = vi.mocked(processMpfs).mock.calls[0]?.[2] as {
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
