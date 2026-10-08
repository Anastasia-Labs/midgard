import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Exit, Layer, ManagedRuntime, Ref, Scope } from "effect";
import { expect, vi } from "vitest";

import {
  assertStartupMutationJobsRecoverable,
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "../../src/commands/listen-startup.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
} from "../../src/l1-event-history-source.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../../src/services/event-history-producer.js";
import { makeProductionEventHistoryOwner } from "../../src/services/event-history-runtime.js";
import { Globals } from "../../src/services/globals.js";
import { IntentJournalWithoutFollower } from "../../src/services/intent-journal.js";
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
import { resetApplicationTables } from "../utils.js";
import { readyWithEmulatorFollower } from "./emulator-l1-follower.js";
import {
  awaitHistoryAuthorityReleased,
  GATE_CLOSED_SETTLE_MS,
  type ProductionOwnerFixtureOptions,
  readHistoryAuthority,
} from "./history-production-owner-lifecycle.production-owner-fixture-options.js";
import { historyOutputObservation } from "./history-projection-observations.js";
import {
  makeStreamingHistoryTransport,
  openHistorySourceOwnerLifecycle,
  type RecordedHistoryBatch,
} from "./history-source-owner-emulator.js";
import { followEmulatorStateQueue } from "./landed-state-queue.js";
import { nativeOwnerBinaryPath } from "./native-owner-binary.js";
export const openHistoryProductionOwnerLifecycle = async (
  options: ProductionOwnerFixtureOptions = {},
) => {
  const recorded = await openHistorySourceOwnerLifecycle(
    options.eventHistoryProtectionDurationMs,
  );
  const { fixture, lucidService, binding } = recorded;
  const deployedIdentity = fixture.runtimeOverrides!.deploymentIdentity;
  const identity =
    options.rollbackHorizon === undefined ||
    deployedIdentity.manifest === undefined
      ? deployedIdentity
      : {
          ...deployedIdentity,
          manifest: {
            ...deployedIdentity.manifest,
            l1Finality: {
              ...deployedIdentity.manifest.l1Finality,
              // The manifest type pins 2160; only this in-memory copy differs.
              automaticRecoveryMaxDepth: options.rollbackHorizon as 2160,
            },
          },
        };
  const nodeConfig = await makeNodeConfigForFixture(fixture);
  const binarySha256 = nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256;
  const operatorAddress = await fixture.operatorLucid.wallet().address();
  await recorded.observer.flush();
  const transport = (options.transportFactory ?? makeStreamingHistoryTransport)(
    recorded,
  );
  const stoppedGenerations: unknown[] = [];
  const addresses = [
    ...new Set([
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap(
        ({ address, retentionAddress }) => [address, retentionAddress],
      ),
    ]),
  ];
  // Every source point also records the state queue, as the source owner's
  // own batches do (see openHistorySourceOwnerLifecycle).
  const recordedAddresses = [
    ...addresses,
    fixture.contracts.stateQueue.spendingScriptAddress,
  ];
  const readOutputs = async (at: readonly string[] = addresses) =>
    (
      await Promise.all(
        at.map((address) => fixture.operatorLucid.utxosAt(address)),
      )
    )
      .flat()
      .map(historyOutputObservation);
  /** Seal the emulator's tip as the next authenticated source point and offer
   * it to whichever owner is (or next) connected, without awaiting readiness. */
  const sealTip = async (
    onEmptyInterval: (empty: RecordedHistoryBatch) => void = () => {},
  ) => {
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
      expect(fixture.emulator.blockHeight).toBeGreaterThan(last.observedHeight);
      const empty = {
        observations: [],
        observedSlot: fixture.emulator.slot,
        observedHeight: fixture.emulator.blockHeight,
        outputs: await readOutputs(recordedAddresses),
      };
      recorded.batches.push(empty);
      onEmptyInterval(empty);
    }
    vi.setSystemTime(fixture.emulator.now());
    return transport.appendAccepted();
  };
  const startRuntime = async (
    globals: Globals,
    generation: number,
    synchronizeOnStart = true,
    ogmiosUrl = transport.options.ogmiosUrl,
    stoppedHolder?: string,
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
      Layer.mergeAll(
        mempoolLedgerCacheLayer,
        WriteBehindLive,
        IntentJournalWithoutFollower,
      ).pipe(Layer.provideMerge(services)),
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
          yield* resetApplicationTables;
        }),
      );
    }
    const scope = await runtime.runPromise(Scope.make());
    // The node's follower over the emulator, for this generation's life:
    // P1's facts (the landed state queue) follow the emulator's queue.
    await runtime.runPromise(
      followEmulatorStateQueue(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
      ).pipe(Effect.provideService(Scope.Scope, scope)),
    );
    const cache = await runtime.runPromise(MempoolLedgerCache);
    let nativeOwner: NativeMpfOwnerService | undefined;
    const owner = await runtime
      .runPromise(
        makeProductionEventHistoryOwner({
          transport: { ...transport.options, ogmiosUrl },
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
              if (options.beforeCompletion !== undefined)
                yield* options.beforeCompletion(generation, globals);
              if (nativeOwner === undefined) {
                yield* seedLatestLocalBlockBoundaryOnStartup;
                if (generation > 0) {
                  yield* hydratePendingBlockFinalizationOnStartup;
                  yield* assertStartupMutationJobsRecoverable;
                }
                nativeOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
                if (nativeOwner === undefined)
                  nativeOwner = yield* initializeArchitectureGOwner(
                    globals,
                    nodeConfig,
                    preparation,
                  );
              }
              yield* preparation.assertCurrent;
              if (options.afterNativePreparation !== undefined)
                yield* options.afterNativePreparation(checkpoint, preparation);
              yield* preparation.assertCurrent;
            }),
        }).pipe(Effect.provideService(Scope.Scope, scope)),
      )
      .catch(async (error: unknown) => {
        // Name the row this owner lost to, so the holder can be traced. `live`
        // uses acquire's rule: a suspended (released) row is never live.
        if (
          !inspect(error).includes("History authority still has a live owner")
        )
          throw error;
        const row = await readHistoryAuthority().catch((cause: unknown) =>
          inspect(cause),
        );
        const holder =
          typeof row === "object" && row !== undefined
            ? row.owner_token === stoppedHolder
              ? "the stopped owner"
              : "another owner"
            : "an unread holder";
        throw new Error(
          `History owner generation ${generation} was refused by a live lease held by ${holder} (stopped owner ${stoppedHolder ?? "none"}; test database ${testDatabaseName()}): ${JSON.stringify(row)}`,
          { cause: error },
        );
      });
    await runtime.runPromise(Ref.set(globals.EVENT_HISTORY_OWNER, owner));
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
    const appendTip = async () => {
      requireRunning();
      return sealTip((empty) => emptyIntervals.push(empty));
    };
    /** The owner Ready at `point`, then the follower driver applied there. */
    const readyAt = (point: Parameters<typeof owner.awaitReadyAt>[0]) =>
      runtime.runPromise(
        readyWithEmulatorFollower(owner.awaitReadyAt(point), fixture, globals),
      );
    const synchronize = async (): Promise<void> => {
      const tip = await appendTip();
      const coverage = await readyAt(tip);
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
    /** Append the next source point while pending recovery keeps the gate
     * closed: the owner journals it, then stays not ready and pending for a
     * settle period covering its convergence attempt (`synchronize` would
     * wait for a readiness a refused recovery never grants). */
    const appendTipWhileGateClosed = async () => {
      const tip = await appendTip();
      // Date is faked by the emulator fixture; measure real elapsed time.
      const deadline = performance.now() + 30_000;
      let settledAt: number | undefined;
      for (;;) {
        const frontier = await runtime.runPromise(owner.frontier);
        const pending = await runtime.runPromise(owner.reconciliationStatus);
        if (frontier.headHeight === tip.height) {
          if (frontier.ready)
            throw new Error(
              `History owner became ready at ${tip.height} while recovery was expected to stay refused`,
            );
          if (pending === undefined)
            throw new Error(
              "History owner has no pending reconciliation while its gate is closed",
            );
          settledAt ??= performance.now() + GATE_CLOSED_SETTLE_MS;
          if (performance.now() >= settledAt) return pending;
        } else if (performance.now() >= deadline) {
          // A journaled point takes well under a second; name what the owner
          // saw, so a lost source block, a stalled queue and a failed owner
          // read apart. `ready` is false in all three while the gate is
          // closed, but a failed owner refuses any frontier at once.
          const state = await runtime.runPromise(
            owner.awaitReadyAt(tip).pipe(
              Effect.match({
                onFailure: (error) => `failed: ${inspect(error.cause)}`,
                onSuccess: () => "ready",
              }),
              Effect.timeoutTo({
                duration: "100 millis",
                onSuccess: (state) => state,
                onTimeout: () => "running",
              }),
            ),
          );
          throw new Error(
            `History owner head ${frontier.headHeight} did not reach source tip ${tip.height} (owner tip ${frontier.tipHeight}, owner ${state}, pending ${pending?.reason ?? "none"})`,
          );
        }
        await new Promise((resolve) => setTimeout(resolve, 50));
      }
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
      readyAt,
      appendTipWhileGateClosed,
      command,
      runWithoutSynchronizing,
      evidence,
      close,
    };
    return { handle, stopRuntime };
  };
  let current = await startRuntime(recorded.globals, 0);
  let restarting = false;
  return {
    ...current.handle,
    /** Seal the next source point while no runtime generation is running
     * (from a restart's `afterStop`): the next owner finds it on connect. */
    sealSourcePointWhileStopped: () => sealTip(),
    restartRuntime: async ({
      synchronize = true,
      afterStop,
      ogmiosUrl,
    }: {
      readonly synchronize?: boolean;
      /** Observe retained storage only after every old service has closed. */
      readonly afterStop?: () => Promise<void>;
      /** Reach the same recorded chain through another Ogmios address. */
      readonly ogmiosUrl?: string;
    } = {}) => {
      if (restarting)
        throw new Error("Fixture runtime restart was already requested");
      restarting = true;
      const previous = structuredClone(await current.handle.evidence());
      try {
        const holder = (await readHistoryAuthority())?.owner_token;
        await current.stopRuntime();
        stoppedGenerations.push(previous);
        // `afterStop` runs before the release check: a stopped owner that
        // cannot retire its lease (revoked generation) lapses it here, and
        // the check still refuses any lease live before the next generation.
        await afterStop?.();
        await awaitHistoryAuthorityReleased(holder);
        current = await startRuntime(
          await makeGlobalsService(),
          stoppedGenerations.length,
          synchronize,
          ogmiosUrl,
          holder,
        );
        restarting = false;
        return current.handle;
      } catch (error) {
        transport.close();
        recorded.observer.restore();
        throw error;
      }
    },
  };
};
