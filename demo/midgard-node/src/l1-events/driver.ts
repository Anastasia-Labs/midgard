/**
 * The node's follower-change driver (plan §7.3, N1). It runs on every
 * follower cursor advance and every rewind (a generation change), and is the
 * one place the node's L1-derived state catches up with the follower:
 *
 * - it reads the event projection at the follower's current view (P2) and
 *   hands the whole set to a sink, which ingests the admissions it lacks,
 *   moves locations and finds orphaned admissions (ruling 3);
 * - it then runs one typed hook per ticket that recomputes from the change
 *   (N2, N3, N4, I1, I3, N6, N10), in a fixed order.
 *
 * It holds no loop of its own: the shared follow loop (`followChain`) calls
 * it through the node follower service. Every condition it cannot clear by
 * itself is returned as a named hold for `/readyz`; it never throws, never
 * exits and never needs a CLI.
 */
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import {
  eventsAt,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";

/** The follower view the driver last applied, and the one it is applying. */
export type FollowerChange =
  | Readonly<{ kind: "initial"; view: View }>
  | Readonly<{ kind: "advance"; before: View; view: View }>
  /** The generation changed: the follower rewound since `before`. */
  | Readonly<{ kind: "rewind"; before: View; view: View }>
  | Readonly<{ kind: "unchanged"; view: View }>;

const sameView = (a: View, b: View): boolean =>
  a.generation === b.generation &&
  a.point.slot === b.point.slot &&
  a.point.hash.equals(b.point.hash) &&
  a.height === b.height;

/** How the follower moved from `before` (the last applied view) to `view`. */
export const classifyChange = (
  before: View | null,
  view: View,
): FollowerChange => {
  if (before === null) return { kind: "initial", view };
  if (sameView(before, view)) return { kind: "unchanged", view };
  if (before.generation !== view.generation)
    return { kind: "rewind", before, view };
  return { kind: "advance", before, view };
};

/** A named reason the node is not ready, with its detail, for `/readyz`. */
export type DriverHold = Readonly<{ reason: string; detail: string }>;

/** Orphaned admissions wait for the recovery that rejects their dependents. */
export const EVENTS_ORPHAN_RECOVERY = "l1_events_orphan_recovery";
/** The sink refused or failed to ingest; the detail names why. */
export const EVENTS_INGESTION_FAILED = "l1_events_ingestion_failed";
/** The sink is waiting for its write gate (a recovery is running). */
export const EVENTS_INGESTION_WAITING = "l1_events_ingestion_waiting";
/** A ticket hook failed; the detail names the hook. */
export const EVENTS_HOOK_FAILED = "l1_events_hook_failed";

/**
 * One hook per ticket that recomputes from a follower change, named by what
 * it recomputes. Each runs after the sink applied the change, in
 * `DRIVER_HOOK_ORDER`; a hook returns a hold to keep the node unready.
 */
export type DriverHooks = Readonly<{
  /** N2: the landed state queue (P1), its health and the head signal. */
  landedStateQueue?: DriverHook;
  /** N3: inclusion of events in foreign blocks (P3 beyond own blocks). */
  foreignBlockInclusion?: DriverHook;
  /** N4: the correction recompute after a rewind. */
  correctionRecompute?: DriverHook;
  /** I1: the status of the node's signed commit intents. */
  intentStatus?: DriverHook;
  /** N6: settlement and the operator set. */
  settlementAndOperatorSet?: DriverHook;
  /** N10: forced-order carriage resolution and ingestion. */
  forcedOrderIngestion?: DriverHook;
}>;

export type DriverHook = (
  change: FollowerChange,
) => Promise<DriverHold | undefined>;

export const DRIVER_HOOK_ORDER = [
  "landedStateQueue",
  "foreignBlockInclusion",
  "correctionRecompute",
  "intentStatus",
  "settlementAndOperatorSet",
  "forcedOrderIngestion",
] as const satisfies readonly (keyof DriverHooks)[];

/** Every event of the projection at the view, both lists (P2). */
export type IngestionPlan = Readonly<{
  view: View;
  events: readonly ProjectedEvent[];
}>;

/**
 * What a sink did with a plan. `stale`: the follower moved between the read
 * and the write, so nothing was written; the next run catches up.
 * `applied` carries the orphaned admissions it found; a sink that cannot
 * reject their dependents in this write returns `held`.
 */
export type SinkResult =
  | Readonly<{ kind: "applied"; inserted: number; orphans: number }>
  | Readonly<{ kind: "stale"; detail: string }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

export type FollowerEventSink = Readonly<{
  apply: (change: FollowerChange, plan: IngestionPlan) => Promise<SinkResult>;
}>;

/**
 * The projection at the view, or why it cannot be read there (the follower
 * moved on, or the projection is unhealthy).
 */
export const planIngestion = async (
  store: FactStore,
  config: EventProjectionConfig,
  view: View,
): Promise<
  | Readonly<{ kind: "ok"; plan: IngestionPlan }>
  | Readonly<{ kind: "unreadable"; detail: string }>
> => {
  const events: ProjectedEvent[] = [];
  for (const list of config.lists) {
    const read = await eventsAt(store, list, view.point);
    if (read.kind !== "ok")
      return {
        kind: "unreadable",
        detail: `${list.kind} events at ${view.point.slot.toString()}: ${read.kind}${"detail" in read ? ` (${String(read.detail)})` : ""}`,
      };
    events.push(...read.value);
  }
  return { kind: "ok", plan: { view, events } };
};

/** One driver run: the change it applied, the sink's result and the holds. */
export type DriverRun =
  | Readonly<{ kind: "no_view" }>
  | Readonly<{
      kind: "ran";
      change: FollowerChange;
      result: SinkResult | Readonly<{ kind: "unreadable"; detail: string }>;
      holds: readonly DriverHold[];
    }>;

export type FollowerDriver = Readonly<{
  /** Applies the follower's current view; serialized, never throws. */
  run: () => Promise<DriverRun>;
  /** The holds of the last run (empty when the node may be ready). */
  holds: () => readonly DriverHold[];
  /** The view the sink last applied. */
  applied: () => View | null;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * The driver over `store`'s view. A run whose sink did not apply (stale,
 * held, failed) leaves the last applied view unchanged, so the next run
 * replays the same change; a full pass is idempotent.
 */
export const createFollowerDriver = (options: {
  readonly store: FactStore;
  readonly config: EventProjectionConfig;
  readonly sink: FollowerEventSink;
  readonly hooks?: DriverHooks;
  readonly log?: (line: string) => void;
}): FollowerDriver => {
  const log = options.log ?? (() => undefined);
  const hooks = options.hooks ?? {};
  let applied: View | null = null;
  let holds: readonly DriverHold[] = [];
  let tail: Promise<unknown> = Promise.resolve();

  const once = async (): Promise<DriverRun> => {
    let view: View | null;
    try {
      view = await options.store.currentView();
    } catch (error) {
      holds = [{ reason: EVENTS_INGESTION_FAILED, detail: message(error) }];
      return { kind: "no_view" };
    }
    if (view === null) {
      holds = [];
      return { kind: "no_view" };
    }
    const change = classifyChange(applied, view);
    // Nothing moved and nothing was left held: the last run still stands.
    if (change.kind === "unchanged" && holds.length === 0)
      return {
        kind: "ran",
        change,
        result: { kind: "applied", inserted: 0, orphans: 0 },
        holds,
      };
    const next: DriverHold[] = [];
    let result: SinkResult | Readonly<{ kind: "unreadable"; detail: string }>;
    try {
      const planned = await planIngestion(options.store, options.config, view);
      result =
        planned.kind === "ok"
          ? await options.sink.apply(change, planned.plan)
          : planned;
    } catch (error) {
      result = {
        kind: "held",
        hold: { reason: EVENTS_INGESTION_FAILED, detail: message(error) },
      };
    }
    if (result.kind === "applied") applied = view;
    else if (result.kind === "held") next.push(result.hold);
    else if (result.kind === "unreadable")
      // The follower moved on (or broke) between its view and the read: the
      // next run reads the new view; a broken projection stays named.
      log(`event ingestion deferred: ${result.detail}`);
    for (const name of DRIVER_HOOK_ORDER) {
      const hook = hooks[name];
      if (hook === undefined) continue;
      try {
        const hold = await hook(change);
        if (hold !== undefined) next.push(hold);
      } catch (error) {
        next.push({
          reason: EVENTS_HOOK_FAILED,
          detail: `${name}: ${message(error)}`,
        });
      }
    }
    holds = next;
    return { kind: "ran", change, result, holds: next };
  };

  return {
    run: () => {
      const run = tail.then(once, once);
      tail = run.catch(() => undefined);
      return run;
    },
    holds: () => holds,
    applied: () => applied,
  };
};
