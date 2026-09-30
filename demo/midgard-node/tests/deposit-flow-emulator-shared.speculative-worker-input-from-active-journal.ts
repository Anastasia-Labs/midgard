import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import { expect, vi } from "vitest";

import { withdrawalEventIdFromBuildMetadata } from "../src/commands/submit-withdrawal.js";
import {
  DepositsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { type SlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import type { UserEventBarrierWatermarks } from "../src/fibers/speculative-commit-state.js";
import { canonicalSlotConfigForLucid } from "../src/lucid-time.js";
import type { NodeConfigDep } from "../src/services/config.js";
import type {
  EventHistoryOwner,
  HistoryOwnerCoverage,
} from "../src/services/event-history-owner.js";
import { UnownedHistoryFixture } from "../src/services/event-history-producer.js";
import {
  Database,
  Globals,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { type MempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { submitWithdrawalProgram } from "../src/transactions/submit-withdrawal.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.js";
import {
  type WorkerInput as CommitWorkerInput,
  type WorkerOutput as CommitWorkerOutput,
} from "../src/workers/utils/commit-block-header.js";
import { fetchRealStateQueueWitnessContext } from "../src/workers/utils/scheduler-refresh.js";
import {
  type EmulatorFixture,
  runNodeDatabaseEffect,
} from "./deposit-flow-emulator-shared.make-fixture.js";
import {
  advanceHistoryAdmissionClock,
  ensureSeparateCollateralUtxo,
} from "./deposit-flow-emulator-shared.submit-with-wallet.js";
import { deriveEmulatorSubmitSlotSnapshot } from "./helpers/emulator-submit-slot-snapshot.js";

export const submitWithdrawalWithDiagnostics = async (
  fixture: EmulatorFixture,
  config: {
    readonly body: SDK.WithdrawalBody;
    readonly signature: SDK.WithdrawalSignature;
    readonly refundAddress: SDK.AddressData;
    readonly refundDatum?: SDK.CardanoDatum;
  },
): Promise<{
  readonly txHash: string;
  readonly withdrawalEventId: string;
}> => {
  await ensureSeparateCollateralUtxo(fixture.depositorLucid);
  await advanceHistoryAdmissionClock(fixture, "withdrawal");
  const result = await runNodeDatabaseEffect(
    submitWithdrawalProgram(
      fixture.depositorLucid,
      fixture.contracts,
      { ...config, referenceScripts: fixture.referenceScripts.withdrawal },
      `emulator-withdrawal-${randomUUID()}`,
    ),
  );
  return {
    txHash: result.txHash,
    withdrawalEventId: withdrawalEventIdFromBuildMetadata(result.metadata),
  };
};

export const makeLucidRuntimeService = async ({
  emulator,
  operatorLucid,
  referenceScriptsLucid,
  operatorAccount,
  referenceScriptsAccount,
}: Pick<
  EmulatorFixture,
  | "emulator"
  | "operatorLucid"
  | "referenceScriptsLucid"
  | "operatorAccount"
  | "referenceScriptsAccount"
>) => {
  return {
    api: operatorLucid,
    referenceScriptsApi: referenceScriptsLucid,
    referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
    switchToOperatorsMainWallet: Effect.sync(() =>
      operatorLucid.selectWallet.fromSeed(operatorAccount.seedPhrase),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      operatorLucid.selectWallet.fromSeed(operatorAccount.seedPhrase),
    ),
    switchToReferenceScriptWallet: Effect.sync(() =>
      referenceScriptsLucid.selectWallet.fromSeed(
        referenceScriptsAccount.seedPhrase,
      ),
    ),
    submitSlotSnapshot: () =>
      Effect.sync(() =>
        deriveEmulatorSubmitSlotSnapshot({
          currentSlot: emulator.slot,
          observedAtMs: emulator.now(),
        }),
      ),
  };
};

export type ProductionHistoryFixtureRuntime = {
  readonly owner: EventHistoryOwner;
  readonly cache: MempoolLedgerCacheService;
  readonly nodeConfig: NodeConfigDep;
  readonly synchronize: () => Promise<void>;
  readonly onCommitAttempt?: (
    receipt: Readonly<{
      coverage: HistoryOwnerCoverage;
      startedAtMs: number;
      finishedAtMs: number;
      output: CommitWorkerOutput;
    }>,
  ) => void;
};

export type OwnedCommitFixture = ProductionHistoryFixtureRuntime & {
  readonly globals: Globals;
};

export const advanceEmulatorToDueWork = async (
  fixture: Pick<EmulatorFixture, "emulator" | "operatorLucid">,
  dueWork: SlotAwareDueWork,
) => {
  const currentSlot = Number(
    fixture.operatorLucid.unixTimeToSlot(fixture.emulator.now()),
  );
  const slotsToAdvance = Math.max(1, dueWork.dueSlot - currentSlot + 1);
  fixture.emulator.awaitSlot(slotsToAdvance);
  vi.setSystemTime(new Date(fixture.emulator.now()));
};

export const alignCommitSchedulerBeforeTestWorker = async ({
  fixture,
  lucidService,
  targetEndTimeMs,
  maxAttempts = 6,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly targetEndTimeMs: number;
  readonly maxAttempts?: number;
}) => {
  let lastDueWork: SlotAwareDueWork | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    // Production selects a fresh operator wallet before every pre-lease
    // alignment (planPreLeaseCommitSchedulerDueWork), which drops the wallet
    // view a confirmed submission pinned. Without it, a later refresh reads a
    // pin that an earlier no-confirmation refresh already spent.
    await Effect.runPromise(lucidService.switchToOperatorsMainWallet);
    const alignment = await Effect.runPromise(
      fetchRealStateQueueWitnessContext(
        lucidService.api,
        fixture.contracts,
        targetEndTimeMs,
        undefined,
        lucidService.referenceScriptsAddress,
        lucidService.submitSlotSnapshot,
        true,
      ),
    );
    if (!("dueWork" in alignment)) {
      return;
    }
    lastDueWork = alignment.dueWork;
    await advanceEmulatorToDueWork(fixture, alignment.dueWork);
  }
  throw new Error(
    `Unexpected scheduler alignment due work: ${JSON.stringify(lastDueWork)}`,
  );
};

export const commitWorkerProgram = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  workerInput: CommitWorkerInput,
  awaitSpeculativeInstruction?: Parameters<
    typeof runCommitBlockHeaderWorkerProgram
  >[1],
  nodeConfig?: NodeConfigDep,
  commitLucidFactory: Parameters<
    typeof runCommitBlockHeaderWorkerProgram
  >[3] = () => Effect.succeed(lucidService as any),
) => {
  const program = runCommitBlockHeaderWorkerProgram(
    workerInput,
    awaitSpeculativeInstruction,
    undefined,
    commitLucidFactory,
  ).pipe(
    Effect.provideService(MidgardContracts, contracts as any),
    workerInput.history === undefined
      ? Effect.provideService(UnownedHistoryFixture, true)
      : (effect) => effect,
  );
  return nodeConfig === undefined
    ? program.pipe(Effect.provide(NodeConfig.layer))
    : program.pipe(Effect.provideService(NodeConfig, nodeConfig));
};

export const makeGlobalsService = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* Globals;
    }).pipe(Effect.provide(Globals.Default)),
  );

export const speculativeWorkerInputFromActiveJournal = async (
  watermarks: UserEventBarrierWatermarks,
  forcedValidationSlotConfig: ReturnType<typeof canonicalSlotConfigForLucid>,
): Promise<CommitWorkerInput> => {
  const pending = await runNodeDatabaseEffect(
    PendingBlockFinalizationsDB.retrieveActive(),
  );
  if (Option.isNone(pending)) {
    throw new Error("Expected an active submitted journal for speculation");
  }
  const record = pending.value;
  const submittedTxHash =
    record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH];
  if (submittedTxHash === null) {
    throw new Error("Expected the speculative base journal to be submitted");
  }
  const headerHash =
    record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
  return {
    data: {
      availableConfirmedBlock: "",
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME].getTime(),
      forcedValidationSlotConfig,
      ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
      localFinalizationPending: false,
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
      baseSnapshotId: `speculative:${headerHash}`,
      stateQueueHasUnmergedTail: true,
      speculativeBuild: {
        base: {
          headerHash,
          utxosRoot:
            record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
          blockEndTimeMs:
            record[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime(),
          submittedTxHash: submittedTxHash.toString("hex"),
        },
        watermarks,
        excludedMempoolTxIds: record.mempoolTxIds.map((txId) =>
          txId.toString("hex"),
        ),
        excludedDepositEventIds: record.depositEventIds.map((eventId) =>
          eventId.toString("hex"),
        ),
        excludedForcedTransactionEventIds: record.forcedTransactionEventIds.map(
          (eventId) => eventId.toString("hex"),
        ),
        excludedWithdrawalEventIds: record.withdrawalEventIds.map((eventId) =>
          eventId.toString("hex"),
        ),
      },
    },
  };
};

export const assertSpeculativeDepositSnapshotIsMemoryOnly = ({
  baseBlockEndTimeMs,
  candidateEndTimeMs,
}: {
  readonly baseBlockEndTimeMs: number;
  readonly candidateEndTimeMs: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const readyDeposits = yield* DepositsDB.retrievePendingHeaderEntriesUpTo(
      new Date(candidateEndTimeMs),
    );
    const speculativeDeposits = readyDeposits.filter(
      (entry) =>
        entry[DepositsDB.Columns.INCLUSION_TIME].getTime() > baseBlockEndTimeMs,
    );
    expect(speculativeDeposits).toHaveLength(1);
    expect(speculativeDeposits[0]?.[DepositsDB.Columns.STATUS]).toBe(
      DepositsDB.Status.Awaiting,
    );
    expect(
      speculativeDeposits[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
    ).toBeNull();
  });

export type NormalizedT1RecoveryGlobals = {
  readonly availableConfirmedBlockPresent: boolean;
  readonly availableLocalFinalizationBlockPresent: boolean;
  readonly blocksInQueue: number;
  readonly latestLocalBlockBoundaryPresent: boolean;
  readonly localFinalizationPending: boolean;
  readonly unconfirmedSubmittedBlockSinceMs: number;
  readonly unconfirmedSubmittedBlockTxHash: string;
};

/**
 * Extracts the end time from a state-queue datum fixture.
 */
export const getStateQueueDatumEndTime = (datum: SDK.LinkedListNodeView) =>
  Effect.runPromise(
    Effect.gen(function* () {
      if (datum.key === "Empty") {
        const { data: confirmedState } =
          yield* SDK.getConfirmedStateFromStateQueueDatum(datum);
        return Number(confirmedState.endTime);
      }
      const latestHeader = yield* SDK.getHeaderFromStateQueueDatum(datum);
      return Number(latestHeader.endTime);
    }),
  );
