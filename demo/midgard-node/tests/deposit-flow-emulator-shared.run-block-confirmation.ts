import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { verifyDaPayloadAgainstHeader } from "da-committee-node/da/payload";
import { Effect, Option, Ref } from "effect";
import { expect } from "vitest";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { buildBlockConfirmationAction } from "../src/fibers/block-confirmation.js";
import type { SpeculativeCandidateSummary } from "../src/fibers/speculative-commit-state.js";
import type { NodeConfigDep } from "../src/services/config.js";
import {
  HistoryProducer,
  UnownedHistoryFixture,
} from "../src/services/event-history-producer.js";
import {
  Database,
  Globals,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { materializeConfirmedLedgerSnapshot } from "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { buildDaPayloadInsert } from "../src/workers/commit-block-header/da-payload.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../src/workers/commit-block-header/transition-roots.js";
import { runConfirmBlockCommitmentsWorkerProgram } from "../src/workers/confirm-block-commitments.js";
import { WorkerError } from "../src/workers/utils/common.js";
import {
  type WorkerInput as ConfirmationWorkerInput,
  type WorkerOutput as ConfirmationWorkerOutput,
} from "../src/workers/utils/confirm-block-commitments.js";
import {
  type EmulatorFixture,
  isEmulatorProvider,
  runNodeDatabaseEffect,
} from "./deposit-flow-emulator-shared.make-fixture.js";
import {
  getStateQueueDatumEndTime,
  makeLucidRuntimeService,
  type NormalizedT1RecoveryGlobals,
  type ProductionHistoryFixtureRuntime,
} from "./deposit-flow-emulator-shared.speculative-worker-input-from-active-journal.js";
import { stripPlutusV3WitnessByHash } from "./deposit-flow-emulator-shared.submit-with-wallet.js";

export const normalizeT1RecoveryGlobals = (
  globals: Globals,
): Promise<NormalizedT1RecoveryGlobals> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const availableConfirmedBlock = yield* Ref.get(
        globals.AVAILABLE_CONFIRMED_BLOCK,
      );
      const availableLocalFinalizationBlock = yield* Ref.get(
        globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      );
      return {
        availableConfirmedBlockPresent: availableConfirmedBlock !== "",
        availableLocalFinalizationBlockPresent:
          availableLocalFinalizationBlock !== "",
        blocksInQueue: yield* Ref.get(globals.BLOCKS_IN_QUEUE),
        latestLocalBlockBoundaryPresent: Number.isFinite(
          yield* Ref.get(globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
        ),
        localFinalizationPending: yield* Ref.get(
          globals.LOCAL_FINALIZATION_PENDING,
        ),
        unconfirmedSubmittedBlockSinceMs: yield* Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
        ),
        unconfirmedSubmittedBlockTxHash: yield* Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
        ),
      };
    }),
  );

export const withEmulatorExtraneousScriptRetry = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
): Promise<A> => {
  const provider = lucid.config().provider;
  if (!isEmulatorProvider(provider)) {
    return run();
  }

  const originalSubmitTx = provider.submitTx;
  provider.submitTx = async (txCbor: string) => {
    try {
      return await originalSubmitTx.call(provider, txCbor);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      const extraneousScriptHash = message.match(
        /Extraneous plutus script\. Script hash: ([0-9a-fA-F]{56})/,
      )?.[1];
      if (extraneousScriptHash === undefined) {
        throw error;
      }
      return originalSubmitTx.call(
        provider,
        stripPlutusV3WitnessByHash({
          txCbor,
          witnessHash: extraneousScriptHash.toLowerCase(),
        }),
      );
    }
  };

  try {
    return await run();
  } finally {
    provider.submitTx = originalSubmitTx;
  }
};

export const runBlockConfirmation = (
  globals: Globals,
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  nodeConfig?: NodeConfigDep,
  production?: ProductionHistoryFixtureRuntime,
) =>
  Effect.runPromise(
    buildBlockConfirmationAction(
      (
        input: ConfirmationWorkerInput,
      ): Effect.Effect<ConfirmationWorkerOutput, WorkerError, never> =>
        runConfirmBlockCommitmentsWorkerProgram(input).pipe(
          Effect.provideService(LucidService, lucidService as any),
          Effect.provideService(MidgardContracts, contracts as any),
          nodeConfig === undefined
            ? Effect.provide(NodeConfig.layer)
            : Effect.provideService(NodeConfig, nodeConfig),
          Effect.catchAllCause((cause) =>
            Effect.fail(
              new WorkerError({
                worker: "confirm-block-commitments",
                message: "Confirmation worker failed.",
                cause,
              }),
            ),
          ),
        ),
    ).pipe(
      (program) =>
        production === undefined
          ? program
          : production.owner.runProducer((token, assertCurrent, coverage) =>
              assertCurrent.pipe(
                Effect.zipRight(
                  Effect.provideService(program, HistoryProducer, {
                    token,
                    coverage,
                  }),
                ),
              ),
            ),
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
      nodeConfig === undefined
        ? Effect.provide(NodeConfig.layer)
        : Effect.provideService(NodeConfig, nodeConfig),
    ),
  );

/**
 * Runs the DA committee member's own payload validator over the payload the
 * node persisted for `headerHash`, against the header read back from L1. The
 * emulator attests with the node's local signers, which never run this check,
 * so without it a node-built payload the committee rejects stays green here.
 */
export const expectDaCommitteeAcceptsPersistedPayload = async ({
  headerHash,
  l1Header,
}: {
  readonly headerHash: string;
  readonly l1Header: SDK.Header;
}) => {
  const row = await runNodeDatabaseEffect(
    DaPayloadsDB.retrieveByHeaderHash(Buffer.from(headerHash, "hex")),
  );
  if (Option.isNone(row))
    throw new Error(`Missing persisted DA payload for ${headerHash}`);
  return verifyDaPayloadAgainstHeader(
    row.value[DaPayloadsDB.Columns.PAYLOAD_CBOR],
    headerHash,
    l1Header,
    { payloadSchemaVersion: 1, stateQueueOutRef: `emulator:${headerHash}` },
  );
};

/**
 * Waits for the submitted commit and stores the header's canonical DA payload
 * from its pending finalization journal, the retention step the node's
 * attestation path requires before it attests.
 */
export const retainSubmittedHeaderPayload = async ({
  fixture,
  headerHash,
  submittedTxHash,
}: {
  readonly fixture: EmulatorFixture;
  readonly headerHash: string;
  readonly submittedTxHash: string;
}) => {
  await fixture.operatorLucid.awaitTx(submittedTxHash);
  await runNodeDatabaseEffect(
    Effect.gen(function* () {
      const pending = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(headerHash, "hex"),
      );
      if (Option.isNone(pending))
        return yield* Effect.fail(
          new Error(`Missing submitted journal ${headerHash}`),
        );
      expect(
        pending.value[
          PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
        ]?.toString("hex"),
      ).toBe(submittedTxHash);
      const snapshot = yield* materializeConfirmedLedgerSnapshot(pending.value);
      const payload = yield* buildDaPayloadInsert({
        record: pending.value,
        utxos: snapshot.entries.map((entry) => ({
          outref: entry.outref,
          output: entry.output,
        })),
      });
      yield* DaPayloadsDB.upsertAvailable(payload);
    }),
  );
};

/**
 * Builds the state-queue fetch configuration for the emulator tests.
 */
export const stateQueueFetchConfig = (contracts: SDK.MidgardValidators) => ({
  stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  stateQueuePolicyId: contracts.stateQueue.policyId,
});

export const fetchLatestCommittedBlock = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.runPromise(
    SDK.fetchLatestCommittedBlockProgram(
      lucid,
      stateQueueFetchConfig(contracts),
    ),
  );

export const advanceEmulatorPastLatestBlockEndTime = async (
  fixture: Pick<EmulatorFixture, "emulator" | "operatorLucid" | "contracts">,
) => {
  const latestCommittedBlock = await fetchLatestCommittedBlock(
    fixture.operatorLucid,
    fixture.contracts,
  );
  const latestBlockEndTime = await getStateQueueDatumEndTime(
    latestCommittedBlock.datum,
  );

  // Fresh deployments anchor the state queue's first commit window in the
  // future. Preprod deposits happen after the node is already live past that
  // genesis boundary, so the realistic harness must advance past it before
  // creating user events; otherwise the worker will correctly exclude the
  // deposit from the first block window.
  while (fixture.emulator.now() <= latestBlockEndTime) {
    fixture.emulator.awaitSlot(1);
  }
};

export const fetchSchedulerDatum = async ({
  operatorLucid,
  contracts,
}: Pick<EmulatorFixture, "operatorLucid" | "contracts">) => {
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const schedulerUtxos = await operatorLucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  expect(schedulerUtxos).toHaveLength(1);
  expect(schedulerUtxos[0]?.datum).toBeDefined();
  return Data.from(schedulerUtxos[0]!.datum!, SDK.SchedulerDatum);
};

export const findUtxoWithUnit = (
  utxos: readonly UTxO[],
  unit: string,
  quantity = 1n,
): UTxO => {
  const utxo = utxos.find((candidate) => candidate.assets[unit] === quantity);
  if (utxo === undefined) {
    throw new Error(
      `Missing UTxO with ${unit} quantity ${quantity.toString()}`,
    );
  }
  return utxo;
};

export const expectedAuthenticatedEventRoot = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> =>
  Effect.runPromise(
    buildAuthenticatedRootFromEncodedEntries(domain, entries).pipe(
      Effect.map((root) => root.root),
    ),
  );

export const expectHeaderRootsToMatchCandidate = (
  header: SDK.Header,
  candidate: SpeculativeCandidateSummary,
): void => {
  expect(header.utxosRoot).toBe(candidate.roots.utxos);
  expect(header.transactionsRoot).toBe(candidate.roots.transactions);
  expect(header.depositsRoot).toBe(candidate.roots.deposits);
  expect(header.forcedTransactionsRoot).toBe(
    candidate.roots.forcedTransactions,
  );
  expect(header.withdrawalsRoot).toBe(candidate.roots.withdrawals);
  expect(header.transitionTraceRoot).toBe(candidate.roots.transitionTrace);
  expect(header.eventToStepRoot).toBe(candidate.roots.eventToStep);
};
