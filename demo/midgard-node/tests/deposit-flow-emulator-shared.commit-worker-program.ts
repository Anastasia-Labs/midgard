import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { vi } from "vitest";

import { withdrawalEventIdFromBuildMetadata } from "../src/commands/submit-withdrawal.js";
import { type SlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import type { NodeConfigDep } from "../src/services/config.js";
import type {
  EventHistoryOwner,
  HistoryOwnerCoverage,
} from "../src/services/event-history-owner.js";
import { UnownedHistoryFixture } from "../src/services/event-history-producer.js";
import {
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
import { runWithoutFollower } from "./helpers/intent-journal.js";

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
    const alignment = await runWithoutFollower(
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
  notifyParent?: Parameters<typeof runCommitBlockHeaderWorkerProgram>[1],
  nodeConfig?: NodeConfigDep,
  commitLucidFactory: Parameters<
    typeof runCommitBlockHeaderWorkerProgram
  >[2] = () => Effect.succeed(lucidService as any),
) => {
  const program = runCommitBlockHeaderWorkerProgram(
    workerInput,
    notifyParent,
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
