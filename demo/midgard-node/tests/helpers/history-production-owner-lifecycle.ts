import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Exit, Layer, ManagedRuntime, Ref, Scope } from "effect";
import { expect, vi } from "vitest";

import {
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "../../src/commands/listen-startup.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
} from "../../src/l1-event-history-source.js";
import type { HistoryTransportOptions } from "../../src/l1-event-history-transport.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../../src/services/event-history-producer.js";
import { makeProductionEventHistoryOwner } from "../../src/services/event-history-runtime.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  MempoolLedgerCache,
  mempoolLedgerCacheLayer,
} from "../../src/services/mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/index.js";
import { initializeArchitectureGOwner } from "../../src/services/native-mpf-startup.js";
import { WriteBehindLive } from "../../src/services/write-behind.js";
import {
  makeGlobalsService,
  makeNodeConfigForFixture,
  type ProductionHistoryFixtureRuntime,
} from "../deposit-flow-emulator-shared.js";
import { testDatabaseName } from "../test-env.js";
import { historyOutputObservation } from "./history-projection-observations.js";
import {
  makeStreamingHistoryTransport,
  openHistorySourceOwnerLifecycle,
  type RecordedHistoryBatch,
} from "./history-source-owner-emulator.js";
import { nativeOwnerBinaryPath } from "./native-owner-binary.js";

/** One actual deployment, real production reconciliation and native MPF owner.
 * Only the network transport's genesis, point identifiers and ancestry are synthetic. */
type ProductionOwnerFixtureOptions = {
  readonly eventHistoryProtectionDurationMs?: bigint;
  readonly transportFactory?: (
    recorded: Parameters<typeof makeStreamingHistoryTransport>[0],
  ) => Omit<ReturnType<typeof makeStreamingHistoryTransport>, "options"> & {
    readonly options: Omit<HistoryTransportOptions, "signal">;
  };
  readonly afterNativePreparation?: NonNullable<
    Parameters<
      typeof makeProductionEventHistoryOwner<
        unknown,
        SqlClient.SqlClient | Globals | MempoolLedgerCache
      >
    >[0]["prepareCompletion"]
  >;
};

export const openHistoryProductionOwnerLifecycle = async (
  options: ProductionOwnerFixtureOptions = {},
) => {
  const recorded = await openHistorySourceOwnerLifecycle(
    options.eventHistoryProtectionDurationMs,
  );
  const { fixture, lucidService, binding } = recorded;
  const identity = fixture.runtimeOverrides!.deploymentIdentity;
  const binarySha256 = createHash("sha256")
    .update(readFileSync(nativeOwnerBinaryPath))
    .digest("hex");
  const nodeConfig = {
    ...(await makeNodeConfigForFixture(fixture)),
    MPF_ENGINE: "architecture_g" as const,
    MPF_NATIVE_OWNER_BINARY_PATH: nativeOwnerBinaryPath,
    MPF_NATIVE_OWNER_BINARY_SHA256: binarySha256,
  };
  const operatorAddress = await fixture.operatorLucid.wallet().address();
  await recorded.observer.flush();
  const transport = (options.transportFactory ?? makeStreamingHistoryTransport)(
    recorded,
  );
  const stoppedGenerations: unknown[] = [];
  const startRuntime = async (
    globals: Globals,
    generation: number,
    synchronizeOnStart = true,
  ) => {
    const services = Layer.mergeAll(
      Layer.succeed(NodeConfig, nodeConfig),
      Layer.succeed(Globals, globals),
      Layer.succeed(
        Lucid,
        Lucid.make({
          ...lucidService,
          operatorMainAddress: operatorAddress,
          operatorMergeAddress: operatorAddress,
          referenceScriptsWalletAddress: await fixture.referenceScriptsLucid
            .wallet()
            .address(),
        }),
      ),
      Layer.succeed(
        MidgardContracts,
        MidgardContracts.make({
          ...fixture.contracts,
          consensusProfile: identity.consensusProfile,
        }),
      ),
      Layer.succeed(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make(identity),
      ),
      Database.layer,
    );
    const runtime = ManagedRuntime.make(
      Layer.mergeAll(mempoolLedgerCacheLayer, WriteBehindLive).pipe(
        Layer.provideMerge(services),
      ),
    );
    if (generation === 0) {
      // The normal global setup owns this disposable worker shard. Existing shared
      // reset omits the private history journal; clear this scenario's exact FK set.
      expect(nodeConfig.POSTGRES_DB).toBe(testDatabaseName());
      await runtime.runPromise(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<{
            name: string;
          }>`SELECT current_database() AS name`;
          expect(rows[0]?.name).toBe(testDatabaseName());
          yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_replay_receipts, event_history_authority`;
        }),
      );
    }
    const scope = await runtime.runPromise(Scope.make());
    const cache = await runtime.runPromise(MempoolLedgerCache);
    let nativeOwner: NativeMpfOwnerService | undefined;
    const owner = await runtime.runPromise(
      makeProductionEventHistoryOwner({
        transport: transport.options,
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256(
          recorded.genesis,
        ),
        heartbeatIntervalMs: 100,
        retainedPointLimit: 128,
        maximumReceiptBytes: 16 * 1024 * 1024,
        leaseDurationMs: 60_000,
        prepareCompletion: (checkpoint, preparation) =>
          Effect.gen(function* () {
            yield* preparation.assertCurrent;
            if (nativeOwner === undefined) {
              yield* seedLatestLocalBlockBoundaryOnStartup;
              if (generation > 0)
                yield* hydratePendingBlockFinalizationOnStartup;
              nativeOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
              if (nativeOwner === undefined)
                nativeOwner = yield* initializeArchitectureGOwner(
                  globals,
                  nodeConfig,
                  preparation,
                );
              if (nativeOwner === undefined)
                return yield* Effect.fail(
                  new Error("Architecture G owner did not start"),
                );
            }
            yield* preparation.assertCurrent;
            if (options.afterNativePreparation !== undefined)
              yield* options.afterNativePreparation(checkpoint, preparation);
            yield* preparation.assertCurrent;
          }),
      }).pipe(Effect.provideService(Scope.Scope, scope)),
    );
    await runtime.runPromise(Ref.set(globals.EVENT_HISTORY_OWNER, owner));
    const addresses = [
      ...new Set([
        binding.hubAddress,
        ...Object.values(binding.deployments).flatMap(
          ({ address, retentionAddress }) => [address, retentionAddress],
        ),
      ]),
    ];
    const readOutputs = async () =>
      (
        await Promise.all(
          addresses.map((address) => fixture.operatorLucid.utxosAt(address)),
        )
      )
        .flat()
        .map(historyOutputObservation);
    const emptyIntervals: RecordedHistoryBatch[] = [];
    const checkpoints: unknown[] = [];
    type CommitAttempt = Parameters<
      NonNullable<ProductionHistoryFixtureRuntime["onCommitAttempt"]>
    >[0];
    const commitAttempts: CommitAttempt[] = [];
    const synchronizedCoverage = new Map<string, CommitAttempt["coverage"]>();
    const onCommitAttempt: NonNullable<
      ProductionHistoryFixtureRuntime["onCommitAttempt"]
    > = (receipt) => {
      expect(
        synchronizedCoverage.get(receipt.coverage.checkpointRevision),
      ).toEqual(receipt.coverage);
      expect(receipt.coverage.includedThroughMs).toBe(
        fixture.operatorLucid.slotToUnixTime(receipt.coverage.point.slot),
      );
      commitAttempts.push(structuredClone(receipt));
    };
    let stopped = false;
    const requireRunning = () => {
      if (stopped)
        throw new Error("This production fixture runtime generation is closed");
    };
    const synchronize = async (): Promise<void> => {
      requireRunning();
      await recorded.observer.flush();
      if (
        recorded.observer.pendingCount() !== 0 ||
        Object.keys(fixture.emulator.mempool).length !== 0
      )
        throw new Error(
          "Cannot seal a history frontier with pending emulator submissions",
        );
      const last = recorded.batches.at(-1)!;
      if (fixture.emulator.slot > last.observedSlot) {
        if (fixture.emulator.blockHeight <= last.observedHeight)
          fixture.emulator.awaitBlock(1);
        expect(fixture.emulator.slot).toBeGreaterThan(last.observedSlot);
        expect(fixture.emulator.blockHeight).toBeGreaterThan(
          last.observedHeight,
        );
        const empty = {
          observations: [],
          observedSlot: fixture.emulator.slot,
          observedHeight: fixture.emulator.blockHeight,
          outputs: await readOutputs(),
        };
        recorded.batches.push(empty);
        emptyIntervals.push(empty);
      }
      vi.setSystemTime(fixture.emulator.now());
      const tip = transport.appendAccepted();
      const coverage = await runtime.runPromise(owner.awaitReadyAt(tip));
      const checkpoint = await runtime.runPromise(Journal.load(binding));
      if (checkpoint === null) throw new Error("Ready owner has no checkpoint");
      const provider = await runtime.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { id: tip.id, slot: tip.slot },
            addresses,
            outputs: await readOutputs(),
          },
          binding,
        ),
      );
      expect(checkpoint.capture.snapshotDigest).toBe(provider.snapshotDigest);
      expect(coverage.snapshotDigest).toBe(provider.snapshotDigest);
      const ledger = await runtime.runPromise(
        cache.withPhaseBLock(
          Effect.gen(function* () {
            const state = yield* cache.currentState;
            const rows = yield* MempoolLedgerDB.retrieveSpendable;
            const cached = [...state]
              .map(([key, value]) => [key, value.toString("hex")])
              .sort();
            const durable = rows
              .map((row) => [
                row[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
                row[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
              ])
              .sort();
            expect(cached).toEqual(durable);
            return { cached, durable };
          }),
        ),
      );
      synchronizedCoverage.set(
        coverage.checkpointRevision,
        structuredClone(coverage),
      );
      checkpoints.push({
        coverage,
        checkpoint,
        providerSnapshotDigest: provider.snapshotDigest,
        ledger,
      });
    };
    type Services = ManagedRuntime.ManagedRuntime.Context<typeof runtime>;
    const runWithoutSynchronizing = async <A, E>(
      effect: Effect.Effect<A, E, Services | HistoryProducerPermit>,
    ) => {
      requireRunning();
      const result = await runtime.runPromise(
        owner.runProducer((token, assertCurrent, coverage) =>
          Effect.gen(function* () {
            yield* assertCurrent;
            const result = yield* effect.pipe(
              Effect.provideService(HistoryProducer, { token, coverage }),
            );
            yield* assertCurrent;
            return result;
          }),
        ),
      );
      return result;
    };
    const command = async <A, E>(
      effect: Effect.Effect<A, E, Services | HistoryProducerPermit>,
    ) => {
      const result = await runWithoutSynchronizing(effect);
      await synchronize();
      return result;
    };
    const evidence = async () => {
      requireRunning();
      return {
        generation,
        stoppedGenerations: [...stoppedGenerations],
        scope:
          "Actual accepted emulator transactions and observed empty intervals; synthetic network genesis, contiguous transport heights, point identifiers and ancestry. Production materialization, projection, cache and Architecture G startup; no injected reconciliation.",
        binding,
        genesis: recorded.genesis,
        publications: [...recorded.publications],
        points: transport.points,
        requests: transport.requests,
        emptyIntervals,
        checkpoints,
        commitAttempts,
        binary: { path: nativeOwnerBinaryPath, sha256: binarySha256 },
        native: await nativeOwner?.diagnostics(),
        journal: await runtime.runPromise(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const key = Buffer.from(binding.digest, "hex");
            return {
              replay:
                yield* sql`SELECT * FROM event_history_replay_receipts WHERE binding_digest = ${key} ORDER BY block_height`,
              applications:
                yield* sql`SELECT * FROM event_history_block_applications WHERE binding_digest = ${key} ORDER BY application_revision`,
            };
          }),
        ),
      };
    };
    const stopRuntime = async () => {
      if (stopped) return;
      stopped = true;
      try {
        await runtime.runPromise(owner.close);
      } finally {
        try {
          const owned =
            nativeOwner ??
            (await runtime.runPromise(Ref.get(globals.NATIVE_MPF_OWNER)));
          await owned?.close();
        } finally {
          try {
            await runtime.runPromise(Scope.close(scope, Exit.void));
          } finally {
            await runtime.dispose();
          }
        }
      }
    };
    const close = async () => {
      if (stopped) return;
      try {
        await stopRuntime();
      } finally {
        transport.close();
        recorded.observer.restore();
      }
    };
    try {
      if (synchronizeOnStart) await synchronize();
    } catch (error) {
      try {
        const path =
          process.env.MIDGARD_STREAMING_PRODUCTION_LIFECYCLE_EVIDENCE_PATH;
        if (path !== undefined) {
          mkdirSync(dirname(path), { recursive: true });
          writeFileSync(
            path,
            JSON.stringify(
              {
                stage: "production-owner-startup",
                failure: inspect(error, { depth: 20, colors: false }),
                manifestId: recorded.deployment.manifest.manifestId,
                blueprintSha256:
                  recorded.deployment.manifest.artifacts.blueprintHash,
                deploymentInfoSha256: recorded.deploymentInfoSha256,
                receipts: recorded.receipts,
                production: await evidence().catch((cause: unknown) => ({
                  failure: inspect(cause, { depth: 20, colors: false }),
                  binding,
                  points: transport.points,
                  publications: [...recorded.publications],
                })),
              },
              (_key, value) =>
                typeof value === "bigint" ? value.toString() : value,
              2,
            ) + "\n",
          );
        }
      } finally {
        try {
          await close();
        } finally {
          vi.useRealTimers();
        }
      }
      throw error;
    }
    const handle = {
      ...recorded,
      globals,
      production: { owner, cache, nodeConfig, synchronize, onCommitAttempt },
      commitAttempts,
      synchronize,
      command,
      runWithoutSynchronizing,
      evidence,
      close,
    };
    return { handle, stopRuntime };
  };
  const initial = await startRuntime(recorded.globals, 0);
  let restarting = false;
  return {
    ...initial.handle,
    restartRuntime: async ({
      synchronize = true,
      afterStop,
    }: {
      readonly synchronize?: boolean;
      /** Observe retained storage only after every old service has closed. */
      readonly afterStop?: () => Promise<void>;
    } = {}) => {
      if (restarting)
        throw new Error("Fixture runtime restart was already requested");
      restarting = true;
      const previous = structuredClone(await initial.handle.evidence());
      try {
        await initial.stopRuntime();
        stoppedGenerations.push(previous);
        await afterStop?.();
        const next = await startRuntime(
          await makeGlobalsService(),
          1,
          synchronize,
        );
        return next.handle;
      } catch (error) {
        transport.close();
        recorded.observer.restore();
        throw error;
      }
    },
  };
};
