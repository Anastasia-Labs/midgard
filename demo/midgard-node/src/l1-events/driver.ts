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
 * exits and never needs a CLI. An event the sink refuses is left out and
 * the rest of the change applies: an identity conflict is a hold, an
 * undecodable event a refusal `/readyz` names while the node stays ready.
 */
import {
  SidecarExitedError,
  StreamInterruptedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import {
  classifyFailure,
  type FactStore,
  type View,
} from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import {
  eventsAt,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";

import {
  isConnectionClassError,
  isRetryableProviderError,
} from "../provider-retry.js";

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

/** Holds the coalesced runner does not retry on its timer (`notRetried`). */
const NOT_RETRIED = new WeakSet<DriverHold>();

/**
 * Marks `hold` as one a timer retry cannot clear: a failure that is not
 * transient, or a refusal only a chain change can lift. The next follower
 * change runs the driver again; the hold stays named until then.
 */
export const notRetried = (hold: DriverHold): DriverHold => {
  NOT_RETRIED.add(hold);
  return hold;
};

/** Whether the coalesced runner retries `hold` on its backoff. */
export const isRetriedHold = (hold: DriverHold): boolean =>
  !NOT_RETRIED.has(hold);

/** Retried holds that stand for a transient failure (`transientFailure`). */
const TRANSIENT_FAILURE = new WeakSet<DriverHold>();

/**
 * Marks `hold` as a transient failure: retried on the coalesced runner's
 * backoff for a bounded time (`NODE_TRANSIENT_BUDGET_MS` in a row), unlike a
 * wait on another actor, which has no bound.
 */
export const transientFailure = (hold: DriverHold): DriverHold => {
  TRANSIENT_FAILURE.add(hold);
  return hold;
};

/** Whether `hold` stands for a transient failure (`transientFailure`). */
export const isTransientFailureHold = (hold: DriverHold): boolean =>
  TRANSIENT_FAILURE.has(hold);

/**
 * Whether a failure says only that a dependency did not answer: a
 * connection-class or retryable provider failure (`isConnectionClassError`,
 * `isRetryableProviderError`) or a store failure the follower classifies as
 * transient (`classifyFailure`). An unrecognised failure is not transient.
 */
export const isTransientDriverFailure = (error: unknown): boolean => {
  if (isConnectionClassError(error) || isRetryableProviderError(error))
    return true;
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (classifyFailure(current) === "transient") return true;
    current = current.cause;
  }
  return false;
};

/**
 * Whether a failure comes from the L1 node: its transport or sidecar, or
 * the follower provider's node or follower path (not its store). Waiting on
 * the node has no bound (plan §7.5).
 */
export const isL1NodeOutage = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (
      current instanceof TransportUnavailableError ||
      current instanceof SidecarExitedError ||
      current instanceof TransportTimeoutError ||
      current instanceof StreamInterruptedError ||
      current instanceof TransportRequestError ||
      (current instanceof L1ProviderTransientError &&
        current.source !== "store")
    )
      return true;
    current = current.cause;
  }
  return false;
};

/**
 * A failure hold. A transient failure is retried: one from the L1 node
 * (`isL1NodeOutage`) without bound, any other (the database) as a
 * `transientFailure`, for a bounded time. A failure that is not transient
 * is `notRetried`.
 */
export const failureHold = (
  reason: string,
  detail: string,
  error: unknown,
): DriverHold => {
  const hold = { reason, detail };
  if (!isTransientDriverFailure(error)) return notRetried(hold);
  return isL1NodeOutage(error) ? hold : transientFailure(hold);
};

/**
 * Orphaned admissions wait for the recovery that rejects their dependents:
 * an orphan an unfinished block journal holds (forced ones included) until
 * that journal's disposition, one a foreign landed block holds until the
 * header leaves the landed queue. An own landed block's orphan does not
 * hold here (`l1_own_block_event_orphaned`).
 */
export const EVENTS_ORPHAN_RECOVERY = "l1_events_orphan_recovery";
/** The sink refused or failed to ingest; the detail names why. */
export const EVENTS_INGESTION_FAILED = "l1_events_ingestion_failed";
/** The sink is waiting for its write gate (a recovery is running). */
export const EVENTS_INGESTION_WAITING = "l1_events_ingestion_waiting";
/** A ticket hook failed; the detail names the hook. */
export const EVENTS_HOOK_FAILED = "l1_events_hook_failed";
/**
 * A projected event that does not decode into the node's row: left out of
 * ingestion and named on `/readyz` as a degradation; the node stays ready.
 */
export const EVENT_UNDECODABLE = "l1_event_undecodable";
/**
 * A projected event whose public id has a local row under another live
 * admission or none: left out of ingestion, and a hold until it clears.
 */
export const EVENT_IDENTITY_CONFLICT = "l1_event_identity_conflict";

/** A projected event the sink refused and left out; the rest applied. */
export type EventRefusal = Readonly<{
  kind: ProjectedEvent["kind"];
  key: string;
  idCbor: string;
  reason: typeof EVENT_UNDECODABLE | typeof EVENT_IDENTITY_CONFLICT;
  detail: string;
}>;

const refusalDetail = (refusal: EventRefusal): string =>
  `${refusal.kind} ${refusal.key} (id ${refusal.idCbor}): ${refusal.detail}`;

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
  | Readonly<{
      kind: "applied";
      inserted: number;
      orphans: number;
      /** Events left out by name. */
      refused: readonly EventRefusal[];
    }>
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
  /** The undecodable events the last applied view left out. */
  refused: () => readonly EventRefusal[];
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
  let refused: readonly EventRefusal[] = [];
  let conflicts: readonly DriverHold[] = [];
  /** The refusals already logged, so each is logged once while it stands. */
  let logged = new Set<string>();
  let tail: Promise<unknown> = Promise.resolve();

  const once = async (): Promise<DriverRun> => {
    let view: View | null;
    try {
      view = await options.store.currentView();
    } catch (error) {
      holds = [failureHold(EVENTS_INGESTION_FAILED, message(error), error)];
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
        result: { kind: "applied", inserted: 0, orphans: 0, refused },
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
        hold: failureHold(EVENTS_INGESTION_FAILED, message(error), error),
      };
    }
    if (result.kind === "applied") {
      applied = view;
      const all = result.refused;
      const current = new Set<string>();
      for (const refusal of all) {
        const id = `${refusal.reason}:${refusal.kind}:${refusal.key}`;
        current.add(id);
        if (!logged.has(id))
          log(`event refused (${refusal.reason}): ${refusalDetail(refusal)}`);
      }
      logged = current;
      refused = all.filter((refusal) => refusal.reason === EVENT_UNDECODABLE);
      conflicts = all
        .filter((refusal) => refusal.reason === EVENT_IDENTITY_CONFLICT)
        // Only a chain change lifts an identity conflict: no timer re-runs it.
        .map((refusal) =>
          notRetried({
            reason: refusal.reason,
            detail: refusalDetail(refusal),
          }),
        );
    } else if (result.kind === "held") next.push(result.hold);
    else if (result.kind === "unreadable")
      // The follower moved on (or broke) between its view and the read: the
      // next run reads the new view; a broken projection stays named.
      log(`event ingestion deferred: ${result.detail}`);
    // The last applied view's conflicts stand until a run applies without them.
    next.push(...conflicts);
    for (const name of DRIVER_HOOK_ORDER) {
      const hook = hooks[name];
      if (hook === undefined) continue;
      try {
        const hold = await hook(change);
        if (hold !== undefined) next.push(hold);
      } catch (error) {
        next.push(
          failureHold(EVENTS_HOOK_FAILED, `${name}: ${message(error)}`, error),
        );
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
    refused: () => refused,
    applied: () => applied,
  };
};
