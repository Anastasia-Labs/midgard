/**
 * The node's write gate (plan §8.1): every write that derives from the L1
 * view runs `viewValid(V)` in its own transaction, under `FOR SHARE` on the
 * follower cursor (`followerViewValid`, the node's one view check), then
 * takes the gate row (`node_follower_write_gate`, migration 0021) `FOR
 * UPDATE`, so gated writes are serialized with each other and with the
 * follower-change driver's recompute.
 *
 * - A permit (`FollowerWrite`) is taken at the view the driver last applied,
 *   with the gate's epoch. Its write is refused, as a named hold the caller
 *   retries, while its view is no longer on the follower's chain
 *   (`l1_follower_view_stale`), while a driver recompute is pending
 *   (`l1_driver_recompute_pending`), or once a recompute superseded its
 *   epoch. A refusal is never a defect: `isFollowerWriteHeld` names it.
 * - The driver (`FollowerDriverWrite`) writes at the view it applies, under
 *   the epoch it took, while its own recompute is pending.
 * - `runAtFollowerView` registers the whole lifetime of a producer
 *   (worker threads, network calls, cache deltas published after commit),
 *   so a recompute first drains the producers this process runs.
 * - `FollowerWriteFixture` is the explicit test capability; no runtime layer
 *   provides it. It refuses once a driver has applied a view.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Context, Data, Deferred, Effect, Option, Ref } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import { DatabaseError } from "../database/utils/common.js";
import type { Database } from "./database.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FOLLOWER_VIEW_UNAPPLIED,
  type FollowerWriteGateLocal,
} from "./follower-write-gate.local.js";
import { Globals } from "./globals.globals.js";

export const FOLLOWER_WRITE_GATE_TABLE = "node_follower_write_gate";

export {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FOLLOWER_VIEW_UNAPPLIED,
} from "./follower-write-gate.local.js";

/** A write the gate refused for a named, transient reason; retried. */
export class FollowerWriteHeld extends Data.TaggedError("FollowerWriteHeld")<{
  readonly reason: string;
  readonly detail: string;
}> {}

/** A follower view as the gate stores and permits carry it. */
export type GateView = Readonly<{
  generation: number;
  slot: number;
  /** Block hash, hex. */
  hash: string;
  height: number;
}>;

export const gateViewOf = (view: View): GateView => ({
  generation: view.generation,
  slot: view.point.slot,
  hash: Buffer.from(view.point.hash).toString("hex"),
  height: view.height,
});

export const followerViewOf = (view: GateView): View => ({
  generation: view.generation,
  point: { slot: view.slot, hash: Buffer.from(view.hash, "hex") },
  height: view.height,
});

/** A producer's write permit: the applied view and the gate epoch it was taken at. */
export type FollowerWritePermit = Readonly<{ view: GateView; epoch: string }>;
export const FollowerWrite = Context.GenericTag<FollowerWritePermit>(
  "midgard/FollowerWrite",
);
/**
 * The follower-change driver's capability: its view and its epoch. The
 * driver's writes (its sink, its recompute and its hooks) run one at a time
 * in its run, so they also write while its own recompute is pending.
 */
export type FollowerDriverPermit = Readonly<{
  view: View;
  /** Unset while this process's driver has taken no epoch yet. */
  epoch: string | undefined;
}>;
export const FollowerDriverWrite = Context.GenericTag<FollowerDriverPermit>(
  "midgard/FollowerDriverWrite",
);
/** Explicit model-fixture capability. No runtime layer provides this. */
export const FollowerWriteFixture = Context.GenericTag<true>(
  "midgard/FollowerWriteFixture",
);

type Gated =
  | Readonly<{ kind: "permit"; permit: FollowerWritePermit }>
  | Readonly<{ kind: "driver"; driver: FollowerDriverPermit }>
  | Readonly<{ kind: "fixture" }>;
const GatedTransaction = Context.GenericTag<Gated>(
  "midgard/FollowerGatedTransaction",
);

const FOLLOWER_WRITE_REQUIRED = "A current follower write permit is required";

/** The gate's refusal of a write with no current capability. */
export const followerWriteUnavailable = (cause: unknown) =>
  new DatabaseError({
    table: FOLLOWER_WRITE_GATE_TABLE,
    message: FOLLOWER_WRITE_REQUIRED,
    cause,
  });

const unavailable = followerWriteUnavailable;

/**
 * A refusal by the named, transient `reason` (`isFollowerWriteHeld`); its
 * message names the reason, so a recorded failure (a job's last error) does.
 */
export const followerWriteHeld = (reason: string, detail: string) =>
  new DatabaseError({
    table: FOLLOWER_WRITE_GATE_TABLE,
    message: `${FOLLOWER_WRITE_REQUIRED}; held: ${reason} (${detail})`,
    cause: new FollowerWriteHeld({ reason, detail }),
  });
const held = followerWriteHeld;

/** The named hold of a write the gate refused, if `error` is one. */
export const followerWriteHoldOf = (
  error: unknown,
): FollowerWriteHeld | undefined =>
  error instanceof DatabaseError &&
  error.table === FOLLOWER_WRITE_GATE_TABLE &&
  error.cause instanceof FollowerWriteHeld
    ? error.cause
    : undefined;

/** A write refused for a named, transient reason: the caller retries it. */
export const isFollowerWriteHeld = (error: unknown): boolean =>
  followerWriteHoldOf(error) !== undefined;

type GateRow = {
  epoch: string;
  applied_generation: string | null;
  applied_slot: string | null;
  applied_hash: Buffer | null;
  applied_height: string | null;
  pending_reason: string | null;
  pending_detail: string | null;
};

export type GateState = Readonly<{
  epoch: string;
  applied: GateView | undefined;
  pending: Readonly<{ reason: string; detail: string }> | undefined;
}>;

const stateOf = (row: GateRow): GateState => ({
  epoch: row.epoch,
  applied:
    row.applied_generation === null ||
    row.applied_slot === null ||
    row.applied_hash === null ||
    row.applied_height === null
      ? undefined
      : {
          generation: Number(row.applied_generation),
          slot: Number(row.applied_slot),
          hash: row.applied_hash.toString("hex"),
          height: Number(row.applied_height),
        },
  pending:
    row.pending_reason === null
      ? undefined
      : { reason: row.pending_reason, detail: row.pending_detail ?? "" },
});

const readGate = (lock: boolean) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<GateRow>`SELECT epoch::text AS epoch,
        applied_generation::text AS applied_generation,
        applied_slot::text AS applied_slot, applied_hash,
        applied_height::text AS applied_height, pending_reason, pending_detail
      FROM node_follower_write_gate WHERE singleton
      ${lock ? sql`FOR UPDATE` : sql``}`;
    const row = rows[0];
    if (row === undefined)
      return yield* Effect.fail(
        unavailable("The follower write gate row is missing"),
      );
    return stateOf(row);
  });

/** The gate as it stands (no lock): its epoch, applied view and pending recompute. */
export const readFollowerWriteGate = readGate(false).pipe(
  Effect.mapError((error) =>
    error instanceof DatabaseError ? error : unavailable(error),
  ),
);

const checkView = (view: View) =>
  followerViewValid(view).pipe(
    Effect.flatMap((valid) =>
      valid
        ? Effect.void
        : Effect.fail(
            held(
              FOLLOWER_VIEW_STALE,
              `the follower left the view at ${view.point.slot.toString()}.${Buffer.from(view.point.hash).toString("hex")} (generation ${view.generation.toString()})`,
            ),
          ),
    ),
  );

const checkPermit = (permit: FollowerWritePermit) =>
  Effect.gen(function* () {
    yield* checkView(followerViewOf(permit.view));
    const gate = yield* readGate(true);
    if (gate.pending !== undefined)
      return yield* Effect.fail(
        held(DRIVER_RECOMPUTE_PENDING, gate.pending.detail),
      );
    if (gate.epoch !== permit.epoch)
      return yield* Effect.fail(
        held(
          FOLLOWER_VIEW_STALE,
          `a driver recompute superseded the permit's epoch ${permit.epoch} (now ${gate.epoch})`,
        ),
      );
  });

const checkDriver = (driver: FollowerDriverPermit) =>
  Effect.gen(function* () {
    if (driver.epoch === undefined)
      return yield* Effect.fail(
        held(
          FOLLOWER_VIEW_UNAPPLIED,
          "this node's follower driver has taken no epoch yet",
        ),
      );
    yield* checkView(driver.view);
    const gate = yield* readGate(true);
    if (gate.epoch !== driver.epoch)
      return yield* Effect.fail(
        held(
          FOLLOWER_VIEW_STALE,
          `another driver took the gate (epoch ${gate.epoch}, this driver's ${driver.epoch})`,
        ),
      );
  });

/** A fixture writes only while no driver has applied a view; a database
 * without the gate's row (a test's stand-in client) has none. */
const checkFixture = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const applied = yield* sql<{ singleton: boolean }>`SELECT singleton
    FROM node_follower_write_gate
    WHERE singleton AND applied_generation IS NOT NULL FOR UPDATE`;
  if (applied.length > 0)
    return yield* Effect.fail(
      unavailable("A fixture cannot bypass a follower driver's applied view"),
    );
});

/**
 * Runs `work` in one gated transaction that it owns (the outermost one).
 * Inside a gated transaction, `work` runs in it directly.
 */
export const withFollowerWrite = <A, E, R>(
  work: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  Effect.gen(function* () {
    const transaction = yield* Effect.serviceOption(
      SqlClient.TransactionConnection,
    );
    const existing = yield* Effect.serviceOption(GatedTransaction);
    if (Option.isSome(existing)) {
      if (Option.isNone(transaction))
        return yield* Effect.fail(
          unavailable("A follower write escaped its SQL transaction"),
        );
      return yield* work;
    }
    if (Option.isSome(transaction))
      return yield* Effect.fail(
        unavailable(
          "The follower write gate must own the outermost transaction",
        ),
      );
    const driver = yield* Effect.serviceOption(FollowerDriverWrite);
    const permit = yield* Effect.serviceOption(FollowerWrite);
    const fixture = yield* Effect.serviceOption(FollowerWriteFixture);
    const gated: Gated | undefined = Option.isSome(driver)
      ? { kind: "driver", driver: driver.value }
      : Option.isSome(permit)
        ? { kind: "permit", permit: permit.value }
        : Option.isSome(fixture)
          ? { kind: "fixture" }
          : undefined;
    if (gated === undefined)
      return yield* Effect.fail(unavailable("Missing follower write permit"));
    const check =
      gated.kind === "driver"
        ? checkDriver(gated.driver)
        : gated.kind === "permit"
          ? checkPermit(gated.permit)
          : checkFixture;
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      check.pipe(
        Effect.mapError((error) =>
          error instanceof DatabaseError ? error : unavailable(error),
        ),
        Effect.zipRight(Effect.provideService(work, GatedTransaction, gated)),
      ),
    ) as Effect.Effect<A, E | DatabaseError, R | Database>;
  });

/**
 * Candidate creation (a new pending block journal, a signed intent) needs a
 * producer permit's gated transaction: the driver's recompute can repair
 * journals, but cannot make a newly eligible candidate. A fixture
 * transaction has no permit (`None`).
 */
export const requireCandidateView = Effect.gen(function* () {
  const gated = yield* Effect.serviceOption(GatedTransaction);
  if (Option.isNone(gated))
    return yield* Effect.fail(
      unavailable("Candidate has no gated follower write transaction"),
    );
  if (gated.value.kind === "permit") return Option.some(gated.value.permit);
  if (gated.value.kind === "fixture") return Option.none<FollowerWritePermit>();
  return yield* Effect.fail(
    unavailable("A driver recompute cannot create a newly eligible candidate"),
  );
});

/**
 * Whether the running SQL work is in a runtime gated transaction (a
 * producer permit's or the driver's): the strict checks that only the
 * explicit fixture capability, or work outside any gated transaction,
 * relaxes for old model members.
 */
export const inRuntimeFollowerWrite = Effect.map(
  Effect.serviceOption(GatedTransaction),
  (gated) => Option.isSome(gated) && gated.value.kind !== "fixture",
);

/** Checks `permit` (or the context's capability) in a gated transaction of its own. */
export const assertFollowerWrite = (permit: FollowerWritePermit | undefined) =>
  withFollowerWrite(Effect.void).pipe(
    permit === undefined
      ? (effect) => effect
      : Effect.provideService(FollowerWrite, permit),
  );

/**
 * Runs a producer's whole lifetime under a permit at the view the driver
 * last applied. Refused (a named hold, `isFollowerWriteHeld`) while this
 * process's driver has applied no view, while a recompute is pending, or
 * when another process's driver holds the gate. Every write inside still
 * goes through `withFollowerWrite`; the work's own failures keep their type.
 */
export const runAtFollowerView = <A, E, R>(
  work: Effect.Effect<A, E, R>,
): Effect.Effect<
  A,
  E | DatabaseError,
  Exclude<R, FollowerWritePermit> | Globals | Database
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const done = yield* Deferred.make<void>();
    const registered = yield* Ref.modify(
      globals.FOLLOWER_WRITE_GATE,
      (local): readonly [string | FollowerWriteHeld, FollowerWriteGateLocal] =>
        local.recomputing
          ? [
              new FollowerWriteHeld({
                reason: DRIVER_RECOMPUTE_PENDING,
                detail: "this node's follower driver is recomputing",
              }),
              local,
            ]
          : local.epoch === undefined
            ? [
                new FollowerWriteHeld({
                  reason: FOLLOWER_VIEW_UNAPPLIED,
                  detail: "this node's follower driver has applied no view yet",
                }),
                local,
              ]
            : [local.epoch, { ...local, producers: local.producers.add(done) }],
    );
    if (registered instanceof FollowerWriteHeld)
      return yield* Effect.fail(held(registered.reason, registered.detail));
    return yield* Effect.gen(function* () {
      const gate = yield* readFollowerWriteGate;
      if (gate.pending !== undefined)
        return yield* Effect.fail(
          held(DRIVER_RECOMPUTE_PENDING, gate.pending.detail),
        );
      if (gate.epoch !== registered)
        return yield* Effect.fail(
          held(
            FOLLOWER_VIEW_STALE,
            `the gate's epoch ${gate.epoch} is not this driver's ${registered}`,
          ),
        );
      if (gate.applied === undefined)
        return yield* Effect.fail(
          held(FOLLOWER_VIEW_UNAPPLIED, "the gate holds no applied view"),
        );
      return yield* Effect.provideService(work, FollowerWrite, {
        view: gate.applied,
        epoch: gate.epoch,
      });
    }).pipe(
      Effect.ensuring(
        Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => {
          local.producers.delete(done);
          return local;
        }).pipe(Effect.zipRight(Deferred.succeed(done, undefined))),
      ),
    );
  });

/**
 * Moves the applied view forward under the driver's capability, in its
 * gated transaction: the view permits are taken at from then on.
 */
export const advanceDriverView = Effect.gen(function* () {
  const gated = yield* Effect.serviceOption(GatedTransaction);
  if (Option.isNone(gated) || gated.value.kind !== "driver")
    return yield* Effect.fail(
      unavailable("Only the driver's gated transaction moves the applied view"),
    );
  const { view, epoch } = gated.value.driver;
  if (epoch === undefined)
    return yield* Effect.fail(unavailable("The driver holds no epoch"));
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE node_follower_write_gate
    SET applied_generation = ${view.generation}, applied_slot = ${view.point.slot},
      applied_hash = ${Buffer.from(view.point.hash)}, applied_height = ${view.height},
      updated_at = NOW()
    WHERE singleton AND epoch = ${epoch}::bigint`;
}).pipe(
  Effect.mapError((error) =>
    error instanceof DatabaseError ? error : unavailable(error),
  ),
);
