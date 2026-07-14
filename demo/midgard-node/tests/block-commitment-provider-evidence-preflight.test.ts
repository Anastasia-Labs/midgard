import "./utils.js";

import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { Globals, NodeConfig } from "@/services/index.js";
import { Lucid as LucidService } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import type { StateQueueSnapshot } from "@/services/state-queue-topology.js";

const fetchStateQueueSnapshotProgramMock = vi.hoisted(() => vi.fn());
const resolveEarliestCommitSchedulerDueWorkPlanMock = vi.hoisted(() => vi.fn());
const fetchRealStateQueueWitnessContextMock = vi.hoisted(() => vi.fn());
const tryWithLeaseMock = vi.hoisted(() => vi.fn());

vi.mock("@/services/state-queue-topology.js", () => ({
  fetchStateQueueSnapshotProgram: fetchStateQueueSnapshotProgramMock,
  refreshStateQueueGlobalsFromSnapshot: () => Effect.void,
}));

vi.mock("@/workers/utils/scheduler-refresh.js", () => ({
  fetchRealStateQueueWitnessContext: fetchRealStateQueueWitnessContextMock,
  resolveEarliestCommitSchedulerDueWorkPlan:
    resolveEarliestCommitSchedulerDueWorkPlanMock,
}));

vi.mock("@/workers/utils/commit-end-time.js", () => ({
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS: 1_000,
}));

vi.mock("@/database/index.js", async () => {
  const { Effect: EffectModule } = await import("effect");
  return {
    DaPayloadsDB: {
      retrieveByHeaderHash: () => EffectModule.die("unexpected DA lookup"),
      tableName: "da_payloads",
    },
    DepositsDB: {
      retrievePendingHeaderEntriesUpTo: () => EffectModule.succeed([]),
    },
    ForcedTransactionsDB: {
      retrievePendingHeaderEntriesUpTo: () => EffectModule.succeed([]),
    },
    ForeignTipReconciliationsDB: {
      countAwaiting: EffectModule.succeed(0n),
    },
    MempoolDB: {
      retrieveTxCount: EffectModule.succeed(1n),
    },
    PendingBlockFinalizationsDB: {
      Columns: {
        HEADER_HASH: "header_hash",
        STATUS: "status",
        SUBMITTED_TX_HASH: "submitted_tx_hash",
      },
      Status: {
        ObservedWaitingStability: "observed_waiting_stability",
        SubmittedLocalFinalizationPending:
          "submitted_local_finalization_pending",
        SubmittedUnconfirmed: "submitted_unconfirmed",
      },
      retrieveActive: () => EffectModule.die("unexpected journal lookup"),
    },
    StateQueueMutationLeasesDB: {
      describeActiveLease: () => "holder=test,status=active",
      tryWithLease: tryWithLeaseMock,
    },
    WithdrawalsDB: {
      retrievePendingHeaderEntriesUpTo: () => EffectModule.succeed([]),
    },
  };
});

import { blockCommitmentAction } from "@/fibers/block-commitment.js";
import { slotAwareDueWorkRegistry } from "@/fibers/slot-aware-due-work.js";

const snapshot = {
  snapshotId: "commit-preflight:root#0:tail#0",
  root: {
    outRef: "root#0",
  },
  tailCommitBase: {
    outRef: "tail#0",
    blockEndTimeMs: 0,
  },
} as StateQueueSnapshot;

const alignmentRequiredPlan = {
  status: "proceed" as const,
  reason: "current_operator_already_active",
  dependencyKey: "scheduler=scheduler#0,current_operator=operator",
  invalidationKey: "scheduler=scheduler#0,current_operator=operator",
};

const fakeConfig = {
  SPECULATIVE_COMMIT_BUILD: false,
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: 120_000,
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: 30_000,
};

const switchToOperatorsMainWalletMock = vi.fn();
const fakeLucid = LucidService.make({
  api: {} as never,
  referenceScriptsApi: {} as never,
  operatorMainAddress: "",
  operatorMergeAddress: "",
  referenceScriptsWalletAddress: "",
  referenceScriptsAddress: "addr_test1referencescripts",
  submitSlotSnapshot: () => Effect.die("unexpected submit-slot lookup"),
  switchToOperatorsMainWallet: Effect.sync(() => {
    switchToOperatorsMainWalletMock();
  }),
  switchToOperatorsMergingWallet: Effect.void,
  switchToReferenceScriptWallet: Effect.void,
});

const fakeContracts = {
  stateQueue: {
    policyId: "00".repeat(28),
    spendingScriptAddress: "addr_test1statequeue",
  },
};

const runAction = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* blockCommitmentAction;
      return {
        commitWorkerActive: yield* globals.COMMIT_WORKER_ACTIVE,
        pipelinePhase: yield* globals.COMMIT_PIPELINE_PHASE,
      };
    }).pipe(
      Effect.provideService(NodeConfig, fakeConfig as never),
      Effect.provideService(LucidService, fakeLucid),
      Effect.provideService(MidgardContracts, fakeContracts as never),
      Effect.provide(Globals.Default),
    ) as Effect.Effect<
      {
        readonly commitWorkerActive: boolean;
        readonly pipelinePhase: string;
      },
      unknown,
      never
    >,
  );

describe("block commitment provider-evidence preflight", () => {
  beforeEach(() => {
    slotAwareDueWorkRegistry.clearAll();
    fetchStateQueueSnapshotProgramMock.mockReset();
    resolveEarliestCommitSchedulerDueWorkPlanMock.mockReset();
    fetchRealStateQueueWitnessContextMock.mockReset();
    tryWithLeaseMock.mockReset();
    switchToOperatorsMainWalletMock.mockReset();

    fetchStateQueueSnapshotProgramMock.mockReturnValue(
      Effect.succeed(snapshot),
    );
    resolveEarliestCommitSchedulerDueWorkPlanMock.mockReturnValue(
      Effect.succeed(alignmentRequiredPlan),
    );
    fetchRealStateQueueWitnessContextMock.mockReturnValue(Effect.succeed({}));
    tryWithLeaseMock.mockReturnValue(
      Effect.succeed({ _tag: "Ran", value: undefined }),
    );
  });

  it("skips before the mutation lease when the earliest scheduler preflight provider evidence fails", async () => {
    fetchStateQueueSnapshotProgramMock.mockReturnValue(
      Effect.fail(new Error("state-queue preflight provider unavailable")),
    );

    const result = await runAction();

    expect(fetchStateQueueSnapshotProgramMock).toHaveBeenCalledTimes(1);
    expect(
      resolveEarliestCommitSchedulerDueWorkPlanMock,
    ).not.toHaveBeenCalled();
    expect(fetchRealStateQueueWitnessContextMock).not.toHaveBeenCalled();
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(result).toStrictEqual({
      commitWorkerActive: false,
      pipelinePhase: "idle",
    });
  });

  it("skips before the mutation lease when the scheduler alignment probe provider evidence fails", async () => {
    resolveEarliestCommitSchedulerDueWorkPlanMock
      .mockReturnValueOnce(Effect.succeed(alignmentRequiredPlan))
      .mockReturnValueOnce(
        Effect.fail(new Error("scheduler alignment probe unavailable")),
      );

    const result = await runAction();

    expect(fetchStateQueueSnapshotProgramMock).toHaveBeenCalledTimes(2);
    expect(resolveEarliestCommitSchedulerDueWorkPlanMock).toHaveBeenCalledTimes(
      2,
    );
    expect(fetchRealStateQueueWitnessContextMock).not.toHaveBeenCalled();
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(result).toStrictEqual({
      commitWorkerActive: false,
      pipelinePhase: "idle",
    });
  });

  it("skips before the mutation lease when detailed scheduler alignment provider evidence fails", async () => {
    fetchRealStateQueueWitnessContextMock.mockReturnValue(
      Effect.fail(new Error("detailed scheduler alignment unavailable")),
    );

    const result = await runAction();

    expect(fetchStateQueueSnapshotProgramMock).toHaveBeenCalledTimes(2);
    expect(resolveEarliestCommitSchedulerDueWorkPlanMock).toHaveBeenCalledTimes(
      2,
    );
    expect(fetchRealStateQueueWitnessContextMock).toHaveBeenCalledTimes(1);
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(result).toStrictEqual({
      commitWorkerActive: false,
      pipelinePhase: "idle",
    });
  });

  it("reaches the mutation lease after successful provider evidence and detailed alignment", async () => {
    const result = await runAction();

    expect(fetchStateQueueSnapshotProgramMock).toHaveBeenCalledTimes(2);
    expect(resolveEarliestCommitSchedulerDueWorkPlanMock).toHaveBeenCalledTimes(
      2,
    );
    expect(fetchRealStateQueueWitnessContextMock).toHaveBeenCalledTimes(1);
    expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
    expect(result).toStrictEqual({
      commitWorkerActive: false,
      pipelinePhase: "idle",
    });
  });
});
