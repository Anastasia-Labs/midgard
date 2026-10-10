/**
 * The node's L1 follower on an emulator chain, as the node's code under test
 * reads it: the process's one follower host (`follower-emulator.host.ts`),
 * the node's Postgres follower store applying the emulator's confirmed
 * blocks with the node's projections. Every reader here first brings the
 * store to the emulator's chain, then reads it through the production read:
 *
 * - `syncEmulatorChain` / `followEmulatorChain` / `withEmulatorChain`: the
 *   store at the emulator's tip once, or in the background while an effect
 *   runs (the node's fibers read P1 and the follower tables meanwhile);
 * - `emulatorStateQueueSnapshot`: P1's read of the landed state queue at
 *   the tip;
 * - `syncEmulatorFollower`: the event projection's plan at the store's
 *   current view (`planCurrentView`), which the follower-change driver
 *   applies (`makeEmulatorDriver` in `emulator-l1-follower.driver.ts`);
 * - `ingestEmulatorEventsUnowned`: that plan through
 *   `reconcileFollowerEvents` under the test-only fixture capability, before
 *   any driver has applied a view.
 *
 * The deployment is bound once per emulator (`bindNodeFollower`), from the
 * fixture's contracts when nothing bound it earlier.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Emulator, type LucidEvolution } from "@lucid-evolution/lucid";
import { Duration, Effect, Ref, Schedule } from "effect";

import { reconcileFollowerEvents } from "../../src/database/follower-events.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../../src/services/follower-write-gate.js";
import {
  Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../../src/services/index.js";
import { planCurrentView } from "../../src/services/l1-follower.readiness.js";
import {
  landedStateQueueSnapshot,
  type StateQueueContract,
  type StateQueueSnapshotReason,
} from "../../src/services/landed-state-queue.js";
import { runningFollower } from "../readiness-l1-follower.fixture.js";
import {
  bindNodeFollower,
  boundNodeFollower,
  readFollowerHost,
  syncFollowerHost,
  withFollowerHost,
} from "./follower-emulator.host.js";

const failed = (message: string, cause?: unknown) =>
  new DatabaseError({ table: "l1_follower_cursor", message, cause });

/** The emulator `lucid` reads. */
export const emulatorOf = (lucid: LucidEvolution): Emulator => {
  const provider: unknown = lucid.config().provider;
  if (!(provider instanceof Emulator))
    throw new Error("The emulator follower follows an emulator only");
  return provider;
};

/** The follower store at the emulator's tip. */
export const syncEmulatorChain = (lucid: LucidEvolution) =>
  Effect.tryPromise({
    try: () => syncFollowerHost(emulatorOf(lucid)),
    catch: (cause) =>
      failed("The follower did not reach the emulator's chain", cause),
  }).pipe(Effect.asVoid);

/**
 * `syncEmulatorChain` now, then every `interval` for as long as the caller's
 * scope is open: the follower following the emulator, as the node's fibers
 * see it.
 */
export const followEmulatorChain = (
  lucid: LucidEvolution,
  interval: Duration.DurationInput = Duration.millis(100),
) =>
  Effect.gen(function* () {
    yield* syncEmulatorChain(lucid);
    yield* Effect.forkScoped(
      Effect.repeat(
        syncEmulatorChain(lucid).pipe(
          Effect.catchAllCause((cause) => Effect.logDebug(cause)),
        ),
        Schedule.spaced(interval),
      ),
    );
  });

/** `effect` with the emulator's chain followed while it runs. */
export const withEmulatorChain =
  (lucid: LucidEvolution) =>
  <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    Effect.scoped(Effect.zipRight(followEmulatorChain(lucid), effect));

/** The landed queue's snapshot (P1) at the emulator's tip. */
export const emulatorStateQueueSnapshot = (
  lucid: LucidEvolution,
  stateQueue: StateQueueContract,
  reason: StateQueueSnapshotReason = "manual_status",
) =>
  Effect.zipRight(
    syncEmulatorChain(lucid),
    landedStateQueueSnapshot(stateQueue, reason),
  );

export type EmulatorFollowerFixture = {
  readonly operatorLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
};

/** The node's follower plan for the fixture's deployment, bound once. */
const nodeFollowerOf = (fixture: EmulatorFollowerFixture) => {
  const emulator = emulatorOf(fixture.operatorLucid);
  const network = fixture.operatorLucid.config().network;
  if (network === undefined) throw new Error("Emulator has no network");
  return {
    emulator,
    plan:
      boundNodeFollower(emulator) ??
      bindNodeFollower(emulator, { contracts: fixture.contracts, network }),
  };
};

/**
 * The follower store at the emulator's tip and the event projection's plan
 * at its current view. With `globals`, the node's follower state becomes a
 * caught-up follower whose current plan is read from the store where it
 * stands.
 */
export const syncEmulatorFollower = (
  fixture: EmulatorFollowerFixture,
  globals?: Globals,
) =>
  Effect.gen(function* () {
    const { emulator, plan } = yield* Effect.try({
      try: () => nodeFollowerOf(fixture),
      catch: (cause) => failed("The node's follower does not bind", cause),
    });
    const read = yield* Effect.tryPromise({
      try: () =>
        withFollowerHost(emulator, (store) =>
          planCurrentView(store, plan.projection),
        ),
      catch: (cause) =>
        failed("The follower did not reach the emulator's chain", cause),
    });
    if (read.kind !== "ok")
      return yield* failed(`The follower has no plan: ${read.detail}`);
    if (globals !== undefined)
      yield* Ref.set(globals.L1_FOLLOWER, {
        ...runningFollower(),
        planCurrent: async () =>
          (await readFollowerHost((store) =>
            planCurrentView(store, plan.projection),
          )) ?? { kind: "none", detail: "the follower host is closed" },
      });
    return read.plan;
  });

/**
 * The driver's ingestion without a driver, under the test-only fixture
 * capability (refused once a driver has applied a view). Deposits due by
 * `projectThroughMs` move into the mempool ledger (hidden) and, with
 * `globals`, the cache delta is published; the default projects nothing, as
 * the old commit barrier did.
 */
export const ingestEmulatorEventsUnowned = (
  fixture: EmulatorFollowerFixture,
  options: Readonly<{ globals?: Globals; projectThroughMs?: number }> = {},
) =>
  Effect.gen(function* () {
    const plan = yield* syncEmulatorFollower(fixture, options.globals);
    const lucid = fixture.operatorLucid;
    const network = lucid.config().network;
    if (network === undefined) return yield* failed("Emulator has no network");
    const outcome = yield* withFollowerWrite(
      reconcileFollowerEvents(plan, {
        network,
        slotToUnixTime: (slot) => lucid.slotToUnixTime(slot),
        cutoffMs: options.projectThroughMs ?? 0,
      }),
    ).pipe(Effect.provideService(FollowerWriteFixture, true));
    if (outcome.kind === "stale")
      return yield* failed("The emulator follower view moved");
    const globals = options.globals;
    const { projected, spendableUpserts } = outcome.ingestion;
    if (globals !== undefined && (projected > 0 || spendableUpserts.length > 0))
      yield* publishMempoolLedgerDelta(
        globals,
        {
          full: false,
          // Newly projected deposits stay hidden until a header is assigned;
          // restored header-assigned rows are spendable at once.
          upserts: spendableUpserts.map((entry) => [
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
            entry[MempoolLedgerDB.Columns.OUTPUT],
          ]),
          deletes: [],
        },
        (yield* NodeConfig).VALIDATION_LEDGER_DELTA_LOG_MAX,
      );
    return outcome.ingestion;
  });
