/**
 * The node's production pieces over an actual deployment on the emulator
 * (N1): the follower stand-in (`emulator-l1-follower.ts`) and the
 * follower-change driver over it (`makeEmulatorDriver`): its sink, its
 * recompute with the node's startup preparation (`prepareNodeOnStartup`)
 * and native MPF owner, the rebase the own-commit disposition makes due
 * and, with `landedBlocks`, the landed-block hook at each applied view.
 * Producers run at the driver's applied view (`runAtFollowerView`), as the
 * node's fibers do.
 *
 * - `synchronize` runs the driver at the emulator's tip until it applies
 *   there, then checks the validation cache against the durable ledger.
 * - `command` runs a producer, then synchronizes.
 * - `restartRuntime` stops every service of the running generation and
 *   starts the next on the same database and emulator, as a node restart
 *   does: a fresh driver's first view runs the recompute and the startup
 *   preparation again.
 *
 * The deployment, its recorded projection receipts and the emulator come
 * from `openHistorySourceOwnerLifecycle`.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Layer, ManagedRuntime, Ref, Scope } from "effect";
import { expect, vi } from "vitest";

import { prepareNodeOnStartup } from "../../src/commands/listen-startup.prepare-node.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import type { DriverHold } from "../../src/l1-events/driver.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import {
  type FollowerWritePermit,
  readFollowerWriteGate,
  runAtFollowerView,
} from "../../src/services/follower-write-gate.js";
import { Globals } from "../../src/services/globals.js";
import { IntentJournalWithoutFollower } from "../../src/services/intent-journal.js";
import type { DriverRecompute } from "../../src/services/l1-follower.recompute.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  MempoolLedgerCache,
  mempoolLedgerCacheLayer,
} from "../../src/services/mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../../src/services/midgard-contracts.js";
import { WriteBehindLive } from "../../src/services/write-behind.js";
import {
  makeGlobalsService,
  makeNodeConfigForFixture,
  type ProductionHistoryFixtureRuntime,
} from "../deposit-flow-emulator-shared.js";
import { testDatabaseName } from "../test-env.js";
import { resetApplicationTables } from "../utils.js";
import { makeEmulatorDriver } from "./emulator-l1-follower.driver.js";
import { openLandedBlocks } from "./emulator-landed-blocks.js";
import type { emulatorState } from "./emulator-snapshot.js";
import { openHistorySourceOwnerLifecycle } from "./history-source-owner-emulator.js";
import { followEmulatorStateQueue } from "./landed-state-queue.js";
import { nativeOwnerBinaryPath } from "./native-owner-binary.js";

export type ProductionLifecycleOptions = {
  readonly eventHistoryProtectionDurationMs?: bigint;
  /** The deployment's automatic rollback horizon k, overridden in this
   * fixture's in-memory identity only (the manifest pins 2160). */
  readonly rollbackHorizon?: number;
  /** Each driver run also runs the node's landed-block hook at its view;
   * `landedHold` reads its last hold. */
  readonly landedBlocks?: boolean;
  /** Runs first in every startup preparation: observes the state a
   * generation starts from. */
  readonly beforePreparation?: (
    generation: number,
    globals: Globals,
  ) => Effect.Effect<void, unknown, SqlClient.SqlClient>;
};

type CommitAttempt = Parameters<
  NonNullable<ProductionHistoryFixtureRuntime["onCommitAttempt"]>
>[0];

const viewKey = (view: FollowerWritePermit["view"]) =>
  `${view.generation}:${view.slot}:${view.hash}`;

export const openProductionLifecycle = async (
  options: ProductionLifecycleOptions = {},
) => {
  const recorded = await openHistorySourceOwnerLifecycle(
    options.eventHistoryProtectionDurationMs,
  );
  const { fixture, lucidService } = recorded;
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
  const operatorAddress = await fixture.operatorLucid.wallet().address();
  const stoppedGenerations: unknown[] = [];
  const commitAttempts: CommitAttempt[] = [];
  const appliedViews = new Set<string>();
  /** The recorded projection follows the emulator until a rollback, which
   * it cannot follow (its receipts are the chain before it). */
  let observing = true;

  const startRuntime = async (globals: Globals, generation: number) => {
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
      // This worker's database shard: start from empty application tables.
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
    // P1's facts (the landed state queue) follow the emulator's queue.
    await runtime.runPromise(
      followEmulatorStateQueue(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
      ).pipe(Effect.provideService(Scope.Scope, scope)),
    );
    const cache = await runtime.runPromise(MempoolLedgerCache);
    // The landed-block hook's rebase is the driver's recompute, made below.
    const late: { recompute?: DriverRecompute } = {};
    const landed =
      options.landedBlocks === true
        ? await openLandedBlocks({
            contracts: fixture.contracts,
            nodeConfig,
            securityParameter:
              identity.manifest?.l1Finality.automaticRecoveryMaxDepth ?? 2160,
            run: (effect) => runtime.runPromise(effect),
            rebase: (reason) =>
              Effect.suspend(() =>
                late.recompute === undefined
                  ? Effect.succeed(undefined)
                  : late.recompute.rebaseIfDue(reason),
              ),
          })
        : undefined;
    const before = options.beforePreparation;
    const driver = await runtime.runPromise(
      makeEmulatorDriver(fixture, {
        startupPreparation:
          before === undefined
            ? prepareNodeOnStartup
            : before(generation, globals).pipe(
                Effect.zipRight(prepareNodeOnStartup),
              ),
        ...(landed === undefined ? {} : { landed: landed.hook }),
      }),
    );
    late.recompute = driver.recompute;
    let landedHold: DriverHold | undefined;
    let dispositionHold: DriverHold | undefined;
    let stopped = false;
    const requireRunning = () => {
      if (stopped)
        throw new Error("This production fixture runtime generation is closed");
    };
    const onCommitAttempt: NonNullable<
      ProductionHistoryFixtureRuntime["onCommitAttempt"]
    > = (receipt) => {
      // A producer runs at a view this driver applied.
      expect(appliedViews.has(viewKey(receipt.permit.view))).toBe(true);
      commitAttempts.push(structuredClone(receipt));
    };
    const synchronize = async (): Promise<void> => {
      requireRunning();
      if (observing) await recorded.observer.flush();
      if (
        (observing && recorded.observer.pendingCount() !== 0) ||
        Object.keys(fixture.emulator.mempool).length !== 0
      )
        throw new Error(
          "Cannot synchronize the follower with pending emulator submissions",
        );
      vi.setSystemTime(fixture.emulator.now());
      const run = await runtime.runPromise(driver.untilApplied);
      landedHold = run.landedHold;
      dispositionHold = run.dispositionHold;
      const gate = await runtime.runPromise(readFollowerWriteGate);
      if (gate.applied !== undefined) appliedViews.add(viewKey(gate.applied));
      await runtime.runPromise(
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
          }),
        ),
      );
    };
    type Services = ManagedRuntime.ManagedRuntime.Context<typeof runtime>;
    /** `effect` as a producer at the driver's applied view. */
    const runWithoutSynchronizing = async <A, E>(
      effect: Effect.Effect<A, E, Services | FollowerWritePermit>,
    ) => {
      requireRunning();
      return runtime.runPromise(runAtFollowerView(effect));
    };
    const command = async <A, E>(
      effect: Effect.Effect<A, E, Services | FollowerWritePermit>,
    ) => {
      const result = await runWithoutSynchronizing(effect);
      await synchronize();
      return result;
    };
    const nativeOwner = () =>
      runtime.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    const evidence = async () => {
      requireRunning();
      return {
        generation,
        stoppedGenerations: [...stoppedGenerations],
        scope:
          "Actual accepted emulator transactions; the follower stand-in's synthetic block hashes and heights. Production driver sink, recompute, startup preparation, cache and Architecture G owner.",
        binding: recorded.binding,
        genesis: recorded.genesis,
        publications: [...recorded.publications],
        commitAttempts,
        gate: await runtime.runPromise(readFollowerWriteGate),
        binary: {
          path: nativeOwnerBinaryPath,
          sha256: nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
        },
        native: await (await nativeOwner())?.diagnostics(),
      };
    };
    const stopRuntime = async () => {
      if (stopped) return;
      stopped = true;
      try {
        await (await nativeOwner())?.close();
      } finally {
        try {
          await runtime.runPromise(Scope.close(scope, Exit.void));
        } finally {
          try {
            await landed?.close();
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
        recorded.observer.restore();
      }
    };
    try {
      await synchronize();
    } catch (error) {
      try {
        await close();
      } finally {
        vi.useRealTimers();
      }
      throw error;
    }
    const handle = {
      ...recorded,
      globals,
      production: { cache, nodeConfig, synchronize, onCommitAttempt },
      commitAttempts,
      synchronize,
      /** The landed-block hook's hold at the last driver run (with
       * `landedBlocks`; otherwise always undefined). */
      landedHold: () => landedHold,
      /** The hold of the rebase the last run's own-commit disposition made
       * due, if it did not finish. */
      dispositionHold: () => dispositionHold,
      /** One driver run at the emulator's tip, whatever it returns. */
      driveOnce: () => runtime.runPromise(driver.runOnce),
      /** The driver's recompute when the rebase is due, as after S6. */
      rebaseIfDue: (reason: string) =>
        runtime.runPromise(driver.recompute.rebaseIfDue(reason)),
      /** The landed-block hook at the driver's applied view (with
       * `landedBlocks`). */
      processLanded: async () => {
        const view = driver.applied();
        if (landed === undefined || view === null)
          throw new Error("No landed-block hook or no applied view");
        return landed.hook(view);
      },
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
    /**
     * The chain rolls back to `state` (an `emulatorState` capture): the
     * emulator is restored to it, and the next synchronization's follower
     * stand-in rewinds onto it (`rewindToEmulatorChain`), so the driver's
     * sink sees a rewind and runs its recompute. Synchronizes unless told
     * not to.
     */
    rollBackTo: async (
      state: ReturnType<typeof emulatorState>,
      { synchronize = true }: { readonly synchronize?: boolean } = {},
    ) => {
      if (observing) {
        recorded.observer.restore();
        observing = false;
      }
      Object.assign(fixture.emulator, structuredClone(state));
      fixture.operatorLucid.clearUTxOOverride();
      lucidService.api.clearUTxOOverride();
      vi.setSystemTime(fixture.emulator.now());
      if (synchronize) await current.handle.synchronize();
    },
    restartRuntime: async ({
      afterStop,
    }: {
      /** Observe retained storage only after every old service has closed. */
      readonly afterStop?: () => Promise<void>;
    } = {}) => {
      if (restarting)
        throw new Error("Fixture runtime restart was already requested");
      restarting = true;
      const previous = structuredClone(await current.handle.evidence());
      try {
        await current.stopRuntime();
        stoppedGenerations.push(previous);
        await afterStop?.();
        current = await startRuntime(
          await makeGlobalsService(),
          stoppedGenerations.length,
        );
        restarting = false;
        return current.handle;
      } catch (error) {
        recorded.observer.restore();
        throw error;
      }
    },
  };
};

export type ProductionLifecycle = Awaited<
  ReturnType<typeof openProductionLifecycle>
>;
