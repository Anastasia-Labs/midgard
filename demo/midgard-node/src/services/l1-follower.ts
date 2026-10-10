/**
 * The node's L1 follower (plan §7.3, §7.5, N1): the shared follow loop
 * (`followChain`) over the node's own follower store (its tables in the node
 * database), with the event projection current, and the follower-change
 * driver it triggers on every cursor advance and every rewind.
 *
 * - The loop is the follower package's; nothing here follows the chain.
 * - Each status change that moves the cursor while the follower is at the
 *   node's tip runs the driver, coalesced: one run at a time, and one more if
 *   the cursor moved meanwhile. A run left held runs again on a capped
 *   backoff until it clears. A rewind is seen as the generation change
 *   between the view the driver last applied and the next one at the tip.
 * - The driver's sink (`l1-follower.driver-sink.ts`) writes the node's
 *   event rows from the projection with the driver's write capability
 *   (`withDriverView`), which re-checks the follower view (plan §8.1) in
 *   the same transaction, and advances the view the gated writers run at
 *   (`follower-write-gate.ts`). The first view,
 *   a rewind, orphaned admissions, deposits whose spendable ledger row was
 *   restored, or a gate another process took run the driver's recompute
 *   instead (`l1-follower.recompute.ts`): it closes the gate, drains the
 *   gated writers, runs the startup preparation once, rebases the working
 *   ledger and native MPF onto the landed chain with the orphan repair
 *   folded in, and publishes the view. Orphans still held keep
 *   `l1_events_orphan_recovery`; a failure is the held
 *   `l1_driver_recompute_failed` or `startup_preparation_failed`, retried
 *   on the driver's backoff.
 * - The driver's first hook reads the landed state queue (P1, N2) at the
 *   view it applies: it publishes the queue length, bumps the head signal
 *   the planner fibers wake on (`l1-head-trigger.ts`) and holds
 *   `state_queue_unhealthy` while the queue is unhealthy.
 * - The driver's operator-set hook (N6) keeps the operator set from the
 *   facts (only the rows that changed since its last read), publishes it
 *   with the landed queue's tail for the watchdog, and derives this
 *   operator's membership: a removed operator is unready `operator_removed`
 *   with its duties held, and stays up.
 * - The driver's landed-block hook (N3, N5, `landed-blocks/`) processes each
 *   landed block once, in queue order, folding merges into `confirmed_ledger`
 *   (its position joins the handle, P10); what it cannot do is a named hold.
 * - The driver's forced-order hook (N10, plan §12.3) ingests the forced
 *   orders the follower projects, resolving carriage its blocks did not
 *   carry through the local node's ledger and the configured content
 *   sources; carriage no source has yet is the transient
 *   `forced_order_carriage_pending`, retried on the driver's backoff. While
 *   the follower is caught up it also deletes the rows whose order left
 *   the chain.
 * - After each driver run, S6 (`l1-follower.intents.ts`, I1) seeds wallets
 *   and reconciles intents, the own commits it derived dead are disposed of
 *   (I3), and the journal re-reads its refusal holds; all join the driver's.
 *   One trigger's run is `followerTick` (`l1-follower.tick.ts`).
 * - Uncleared states are named `/readyz` reasons. The one exit: a database
 *   transient (the driver's or the follower store's) that outlives its bound
 *   (`transient-exhaustion.ts`) exits the node non-zero.
 */
import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FactStore,
  followChain,
  FOLLOWER_TRANSIENT_EXHAUSTED,
  type FollowStatus,
  httpTxContentSource,
  openPostgresFactStore,
  projectionStoreOptions,
  storeTxContentSource,
  transportLedgerOutputs,
  type WriterLease,
} from "@al-ft/midgard-l1-follower";
import { Effect, Ref, Runtime } from "effect";

import {
  forcedOrderIngestionHook,
  NO_CONTENT_SOURCE,
} from "../forced-orders/index.js";
import { createFollowerDriver } from "../l1-events/driver.js";
import {
  neglectedEventSourcesOf,
  stateQueueTailOf,
} from "../l1-operator-set/index.js";
import { landedStateQueueHook } from "../l1-state-queue/index.js";
import {
  type ConfirmedLedgerPosition,
  landedBlockHook,
  nodeLandedBlockPorts,
} from "../landed-blocks/index.js";
import { NodeConfig } from "./config.js";
import type { Database } from "./database.js";
import { withDriverView } from "./follower-write-gate.driver.js";
import { Globals } from "./globals.globals.js";
import { IntentJournal } from "./intent-journal.js";
import {
  nodeSeededAddresses,
  protocolPaymentCredentials,
} from "./intent-journal.tracked-set.js";
import { coalescedRunner } from "./l1-follower.coalesced-runner.js";
import { driverSink } from "./l1-follower.driver-sink.js";
import { failureHold } from "./l1-follower.failure-hold.js";
import { nodeFamilyPredicate } from "./l1-follower.intent-predicates.js";
import {
  createNodeIntentStage,
  nodeIntentTrackedSet,
} from "./l1-follower.intents.js";
import {
  message,
  recordUnconfigured,
  withNodeNetworkMagic,
} from "./l1-follower.network-magic.js";
import { followerOperatorSet } from "./l1-follower.operator-set.js";
import { type L1FollowerPlan, l1FollowerPlan } from "./l1-follower.plan.js";
import { nodeFollowerProjections } from "./l1-follower.projections.js";
import {
  cursorKey,
  followerCaughtUp,
  type L1FollowerHandle,
  planCurrentView,
  startingStatus,
} from "./l1-follower.readiness.js";
import { makeDriverRecompute } from "./l1-follower.recompute.js";
import { followerTick } from "./l1-follower.tick.js";
import { publishL1HeadChange } from "./l1-head-trigger.js";
import { Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "./midgard-contracts.js";
import { signalTransientExhausted } from "./transient-exhaustion.js";

/** The follower over `plan`, once the node's network magic is known. */
const followL1 = <R>(
  plan: Extract<L1FollowerPlan, { kind: "run" }>,
  networkMagic: number,
  startupPreparation: Effect.Effect<void, unknown, R> | undefined,
  writerLease: (() => Promise<WriterLease | null>) | undefined,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    const globals = yield* Globals;
    const finality =
      identity.manifest?.l1Finality ?? DEPLOYMENT_MANIFEST_L1_FINALITY;
    const unconfigured = (detail: string) =>
      recordUnconfigured(globals, detail);
    if (plan.contentSources.length === 0)
      yield* Effect.logWarning(`L1 follower: ${NO_CONTENT_SOURCE}`);
    const journal = yield* IntentJournal;
    const seededAddresses = nodeSeededAddresses(config);
    const runtime = yield* Effect.runtime<never>();
    const log = (line: string) =>
      Runtime.runFork(runtime)(Effect.logInfo(`L1 follower: ${line}`));
    const dbRuntime = yield* Effect.runtime<
      Database | NodeConfig | Globals | Lucid | ContractDeploymentIdentity
    >();
    const projections = nodeFollowerProjections(
      plan,
      Runtime.runPromise(dbRuntime),
    );
    const opened = yield* Effect.acquireRelease(
      Effect.try(() => {
        const transport = new L1NodeTransport({
          binaryPath: plan.binaryPath,
          socketPath: plan.socketPath,
          networkMagic,
          onDiagnostic: (line) => log(`transport: ${line}`),
        });
        let store: FactStore;
        try {
          store = openPostgresFactStore({
            ...projectionStoreOptions(
              projections.projections,
              {
                securityParameter: plan.securityParameter,
                // The protocol-init tx qualifies through the hub oracle mint;
                // the rest is the intent journal's invariant (§8.2).
                trackedSet: nodeIntentTrackedSet({
                  protocolPaymentCredentials:
                    protocolPaymentCredentials(contracts),
                  hubOraclePolicyId: plan.hubOraclePolicyId,
                }),
              },
              "postgres",
            ),
            // Seeded (§5.3 step 4), never in the tracked-set record.
            wallets: seededAddresses,
            connection: {
              connectionString: plan.connectionString,
              maxConnections: 4,
              onConnectionError: (error) =>
                log(`database connection lost: ${error.message}`),
            },
            ...(writerLease === undefined ? {} : { writerLease }),
          });
        } catch (error) {
          void transport.close();
          throw error;
        }
        return { transport, store, abort: new AbortController() };
      }),
      ({ transport, store, abort }) =>
        Effect.promise(async () => {
          abort.abort();
          await store.close().catch(() => undefined);
          await transport.close().catch(() => undefined);
        }),
    ).pipe(Effect.either);
    if (opened._tag === "Left")
      return yield* unconfigured(
        `the follower store or transport did not open: ${message(opened.left.error)}`,
      );
    const { transport, store, abort } = opened.right;
    const depth = {
      confirmationDepth: finality.confirmationDepth,
      securityParameter: plan.securityParameter,
    };
    const operatorSet = yield* followerOperatorSet({
      store,
      config: plan.operatorSet,
      depth,
      neglectedEvents: neglectedEventSourcesOf(
        plan.projection,
        plan.forcedOrders,
      ),
    });
    projections.bindActivity(operatorSet.activity);
    let confirmedLedger: ConfirmedLedgerPosition | null = null;
    let status: FollowStatus = startingStatus;
    const planCurrent = () => planCurrentView(store, plan.projection);
    const recompute = yield* makeDriverRecompute({
      planCurrent,
      ...(startupPreparation === undefined ? {} : { startupPreparation }),
    });
    const sink = yield* driverSink(recompute);
    const landedPorts = nodeLandedBlockPorts(
      store,
      plan,
      recompute.rebaseIfDue,
    );
    const driver = createFollowerDriver({
      failureHold,
      store,
      config: plan.projection,
      sink,
      hooks: {
        landedStateQueue: landedStateQueueHook({
          store,
          config: plan.stateQueue,
          publish: (change, read) =>
            Runtime.runPromise(runtime)(
              Effect.gen(function* () {
                operatorSet.setStateQueueTail(
                  read.kind === "ok" ? stateQueueTailOf(read.queue) : null,
                );
                if (read.kind === "ok")
                  yield* Ref.set(
                    globals.BLOCKS_IN_QUEUE,
                    read.queue.nodes.length,
                  );
                if (change.kind !== "unchanged")
                  yield* publishL1HeadChange(globals);
              }),
            ),
        }),
        ...(operatorSet.hook === undefined
          ? {}
          : { settlementAndOperatorSet: operatorSet.hook }),
        // The hook writes under the driver's capability at the change's view.
        foreignBlockInclusion: (change) =>
          landedBlockHook({
            store,
            config: plan.stateQueue,
            ports: landedPorts,
            run: (effect) =>
              Runtime.runPromise(dbRuntime)(
                withDriverView(change.view)(effect),
              ),
            publish: { depth, position: (next) => (confirmedLedger = next) },
          })(change),
        forcedOrderIngestion: forcedOrderIngestionHook({
          store,
          config: plan.forcedOrders,
          consensusProfile: identity.consensusProfile,
          ledger: transportLedgerOutputs(transport),
          sources: [
            storeTxContentSource(store),
            ...plan.contentSources.map((urlTemplate) =>
              httpTxContentSource({ urlTemplate }),
            ),
          ],
          contentSourcesConfigured: plan.contentSources.length > 0,
          run: (effect) => Runtime.runPromiseExit(dbRuntime)(effect),
          caughtUp: () => followerCaughtUp(status),
          log: (line) => log(`forced orders: ${line}`),
        }),
      },
      log: (line) => log(`driver: ${line}`),
    });
    const slotClock = (yield* Lucid).api;
    const intents = createNodeIntentStage({
      store,
      transport,
      securityParameter: plan.securityParameter,
      seededAddresses,
      wanted: nodeFamilyPredicate({
        store,
        stateQueue: plan.stateQueue,
        operatorSet:
          operatorSet.ownKey === undefined
            ? null
            : { config: plan.operatorSet, ownKey: operatorSet.ownKey },
        slotToPosixMs: (slot) => slotClock.slotToUnixTime(slot),
        commitEventDepth: config.COMMIT_EVENT_DEPTH,
      }),
      log: (line) => log(`intents: ${line}`),
    });
    // S6 follows the driver in the same coalesced run (§8.3: every head and
    // generation change), then the rebase the own commits it derived dead
    // make due, then the journal's re-read of its refusal holds
    // (`followerTick`).
    const tick = followerTick({
      driver,
      intents,
      nodeBehind: () => status.nodeBehind !== null,
      recompute,
      refreshJournal: () => journal.refresh(),
    });
    // A database transient that outlives its bound exits the node (R2).
    const trigger = coalescedRunner(
      () => Runtime.runPromise(runtime)(tick).then((ran) => ran.holds),
      abort.signal,
      {
        onExhausted: (holds) =>
          signalTransientExhausted(globals.TRANSIENT_EXHAUSTION, {
            source: "driver",
            reason: holds[0]?.reason ?? "l1_driver_transient_failure",
            detail: holds
              .map((hold) => `${hold.reason}: ${hold.detail}`)
              .join("; "),
          }),
      },
    );
    let lastCursor: string | null = null;
    const handle: L1FollowerHandle = {
      kind: "running",
      status: () => status,
      holds: () => [...driver.holds(), ...intents.holds(), ...journal.holds()],
      refused: () => driver.refused(),
      planCurrent,
      confirmedLedger: () => confirmedLedger,
    };
    yield* Ref.set(globals.L1_FOLLOWER, handle);
    const running = followChain({
      store,
      transport,
      origin: plan.origin,
      signal: abort.signal,
      log,
      nodeBehind: {
        slotTime: (slot) => slotClock.slotToUnixTime(slot),
        boundMs: config.L1_NODE_BEHIND_MAX_MS,
      },
      onStatus: (next) => {
        status = next;
        // The driver applies only views at the node's tip: until then a key
        // the follower lacks may be one it has not reached, not an orphan.
        const key = cursorKey(next);
        if (key !== null && key !== lastCursor && followerCaughtUp(next)) {
          lastCursor = key;
          trigger();
        }
      },
    });
    // A follower whose transient store failures outlived their bound stops
    // `exhausted`; the node exits non-zero (R2).
    void running.then(
      (final) => {
        if (final.state === "exhausted" && !abort.signal.aborted)
          signalTransientExhausted(globals.TRANSIENT_EXHAUSTION, {
            source: "l1_follower",
            reason: FOLLOWER_TRANSIENT_EXHAUSTED,
            detail: final.waiting?.detail ?? "transient budget exhausted",
          });
      },
      () => undefined,
    );
    yield* Effect.addFinalizer(() =>
      Effect.promise(async () => {
        abort.abort();
        await running.catch(() => undefined);
        intents.close();
      }),
    );
    yield* Effect.logInfo(
      `L1 follower started from ${plan.origin.origin.slot.toString()}.${plan.origin.origin.hash.toString("hex")}`,
    );
  });

/**
 * Starts the node's follower in the caller's scope and records it in
 * `Globals.L1_FOLLOWER`; closing the scope stops the loop and releases its
 * store and transport. A follower the configuration does not allow is
 * recorded as `unconfigured` (a `/readyz` reason) and the node keeps running.
 * While the node's config files do not yield its network magic, the state is
 * `l1_node_config_unreadable` and the node keeps starting; the follower
 * starts in the background once the magic reads.
 */
export const startL1Follower = <R = never>(
  options: Readonly<{
    /** Runs once, in the driver's first recompute (see `makeDriverRecompute`). */
    startupPreparation?: Effect.Effect<void, unknown, R>;
    /**
     * The follower's writer lease, held by the node's instance lock
     * (`node-instance-lock.ts`); without it the store takes one on a
     * session of its own.
     */
    writerLease?: () => Promise<WriterLease | null>;
  }> = {},
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    const globals = yield* Globals;
    const finality =
      identity.manifest?.l1Finality ?? DEPLOYMENT_MANIFEST_L1_FINALITY;
    const plan = l1FollowerPlan({
      config,
      contracts,
      securityParameter: finality.automaticRecoveryMaxDepth,
    });
    if (plan.kind === "unconfigured")
      return yield* recordUnconfigured(globals, plan.detail);
    return yield* withNodeNetworkMagic(
      { globals, nodeConfigPath: plan.nodeConfigPath, network: config.NETWORK },
      (networkMagic) =>
        followL1(
          plan,
          networkMagic,
          options.startupPreparation,
          options.writerLease,
        ),
    );
  });
