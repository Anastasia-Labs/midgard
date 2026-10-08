/**
 * The follower-change driver's recompute (`makeDriverRecompute`) over the
 * node database, for tests without a followed chain. Its plan is what
 * `plan` writes and returns (a follower view, e.g. `writeFollowerView`);
 * slots are model time (`modelSlotTime`); the validation cache is the
 * production one, and the caller provides the write-behind; the native MPF
 * owner is the one the test placed in `globals` (none is started). The
 * recompute lives in the run that made it; each `run` or `rebaseIfDue` is
 * one driver run.
 */
import { Effect, Runtime } from "effect";

import type { IngestionPlan } from "../../src/l1-events/driver.js";
import type { Database } from "../../src/services/database.js";
import {
  FollowerDriverWrite,
  followerViewOf,
  readFollowerWriteGate,
  withFollowerWrite,
} from "../../src/services/follower-write-gate.js";
import { makeDriverRecompute } from "../../src/services/l1-follower.recompute.js";
import { Lucid } from "../../src/services/lucid.js";
import { mempoolLedgerCacheLayer } from "../../src/services/mempool-ledger-cache.js";
import { MidgardContracts } from "../../src/services/midgard-contracts.js";
import { modelSlotTime, writeFollowerView } from "./follower-view.js";

/** The slot the default plan's follower view is written at. */
export const DRIVER_TEST_SLOT = 100;

/** Lucid as the recompute reads it: the slot clock only (model time). */
export const modelSlotLucid = {
  api: { slotToUnixTime: modelSlotTime },
} as unknown as Lucid;

export const testDriverRecompute = <R = never>(
  options: {
    /** Writes the follower view the run applies; default: no events at `DRIVER_TEST_SLOT`. */
    readonly plan?: Effect.Effect<IngestionPlan, unknown, Database>;
    readonly startupPreparation?: Effect.Effect<void, unknown, R>;
  } = {},
) =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<Database>();
    const plan = options.plan ?? writeFollowerView(DRIVER_TEST_SLOT, []);
    return yield* makeDriverRecompute({
      planCurrent: () =>
        Runtime.runPromise(runtime)(
          plan.pipe(Effect.map((at) => ({ kind: "ok", plan: at }) as const)),
        ),
      ...(options.startupPreparation === undefined
        ? {}
        : { startupPreparation: options.startupPreparation }),
      initializeNativeOwner: false,
    });
  }).pipe(
    Effect.provideService(Lucid, modelSlotLucid),
    // Reached only to start a native owner, which this recompute never does.
    Effect.provideService(MidgardContracts, {} as MidgardContracts),
    Effect.provide(mempoolLedgerCacheLayer),
  );

/**
 * Runs a test's own write in one gated transaction: under the fixture
 * capability the caller provides while no driver has applied a view, and as
 * the driver that applied the gate's view once one has (a fixture never
 * bypasses an applied view).
 */
export const testWrite = <A, E, R>(work: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const gate = yield* readFollowerWriteGate;
    const write = withFollowerWrite(work);
    if (gate.applied === undefined) return yield* write;
    return yield* write.pipe(
      Effect.provideService(FollowerDriverWrite, {
        view: followerViewOf(gate.applied),
        epoch: gate.epoch,
      }),
    );
  });
