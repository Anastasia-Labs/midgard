import type { PinResult, SqlTx } from "@al-ft/midgard-l1-follower";

import { recordsUnitHistory, stateQueueNodeUnitPattern } from "./projection.js";
import {
  followerBlockDepth,
  type WatcherBlockDepth,
  type WatcherProofRetentionStore,
} from "./proof-retention.block-depth.js";
import { eventRowStateIn } from "./proof-retention.events.js";
import {
  type HistoryState,
  historyState,
  unpruned,
} from "./proof-retention.history-state.js";
import {
  createUnownedHoldRelease,
  lockCursorIn,
  releasePinIn,
  type WatcherUnownedHoldsInput,
} from "./proof-retention.unowned.js";
import type { WatcherProofRosterRetention } from "./proof-roster-retention.js";
import { headerPrunedIn, unitPrunedIn } from "./pruned-keys.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROOF_PIN_EVENTS_TABLE,
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";
import type { WatcherL1Degradation } from "./tx-inputs.js";

/**
 * Proof retention (E1 ruling, option (a)): while a proof objective over a
 * header is open, the header's L1 history is pinned past the follower's
 * pruning (at its retained depth, never less than k); once its completion
 * marker is verified past k the pin releases and normal pruning applies.
 * Never a time window.
 *
 * A pin row (header, category) holds the header's queue unit history and
 * its departed-header row (registered row pins), and through the history
 * its txs and checkpoints, so the header's own state-queue node unit needs
 * no hold of its own. The followed units the objective's proof reads (its
 * computation thread and proof token: units whose history
 * `WATCHER_UNIT_HISTORY_TABLE` records) are held by unit rows, written when
 * a capture names them and only while the header holds a pin; a header's
 * unit rows go with its last pin. A unit with no history rows is held all
 * the same (it may not be minted yet), and the raw read decides what it
 * answers, unless a prune step deleted rows of it (`unitPrunedIn`): then the
 * rows left, if any, are not its whole history, and nothing holds it. The
 * same goes for a header pin over a header whose queue history rows a prune
 * step deleted (`headerPrunedIn`): a header committed again has rows again,
 * and they are not its whole history.
 *
 * Both writes go through the follower's `pinRetained`, under its cursor
 * lock: a prune step either committed first, and the pin reports
 * `already_pruned`, or runs after the pin and keeps the rows.
 *
 * No result is dropped. An objective whose history pruning removed first
 * is the named degradation `l1_proof_history_pruned` (status and metrics)
 * until it is released (a unit's, until a later hold of that unit finds its
 * history whole); its captures refuse to read the history rather than read
 * a partial one.
 *
 * The deposit and withdrawal events a capture reads (the shared event
 * projection's `node_l1_events`, whose retired rows prune once retired as
 * deep as the store's retained depth) are held the same way by event rows,
 * written when a capture names them and only while the header holds a pin. An
 * event whose row is gone while its key stays in the follower's never-reuse
 * key set was pruned first: nothing holds it, and the objective is the named
 * readiness reason `l1_proof_event_pruned` until it is released (or a later
 * hold finds the row again). A capture whose header holds no pin reads for no
 * open objective (header classification reads every finalized header before
 * any objective exists; an open objective's header holds a pin or is named
 * pruned), so it writes no hold and reads as before: within the retained
 * depth the rows are stored, and past it the raw reads refuse it
 * `beyond_retention`.
 */

/** An open objective whose history pruning removed before a pin held it. */
export const L1_PROOF_HISTORY_PRUNED = "l1_proof_history_pruned";
/** An open objective whose user event pruning removed before a hold held it. */
export const L1_PROOF_EVENT_PRUNED = "l1_proof_event_pruned";
/** Holds no objective owns that the last pass at tip kept (informational). */
export const L1_PROOF_HOLDS_UNOWNED = "l1_proof_holds_unowned";
/** Of those, the holds a live held unit keeps (informational). */
export const L1_PROOF_HOLDS_UNOWNED_LIVE = "l1_proof_holds_unowned_live";

export type WatcherProofRetentionTarget = Readonly<{
  category: string;
  headerHash: string;
}>;

/**
 * `pinned`: the header's history is held from now on (or already was).
 * `already_pruned`: pruning removed the header's history first (or the
 * store holds none of it and has pruned since its origin, so it cannot
 * tell); nothing is held, and a proof over the header cannot read its L1
 * history from this store. A history the store holds none of before any
 * pruning is empty, not gone, and is pinned.
 */
export type WatcherProofPinResult = PinResult;

/**
 * `held`: every named unit's history is held from now on (or already was),
 * by a unit row or, for the header's own state-queue node unit, by the
 * header's pin.
 * `not_pinned`: the header holds no pin (a read for no open objective), so
 * no unit hold was written; or a named unit is neither followed nor the
 * header's own node unit (only that unit's own header pin could hold it),
 * and the followed units are held.
 * `already_pruned`: a prune step had deleted part of the history of `units`
 * when the hold came; the other followed units are held. A header whose own
 * pin pruning beat reports every unit.
 */
export type WatcherProofUnitHoldResult =
  | Readonly<{ kind: "held" }>
  | Readonly<{ kind: "not_pinned" }>
  | Readonly<{ kind: "already_pruned"; units: readonly string[] }>;

/** A deposit or withdrawal event, by its list kind and key (hex). */
export type WatcherProofEventRef = Readonly<{
  kind: "deposit" | "withdrawal";
  key: string;
}>;

/**
 * `held`: every named event's row is held from now on (or already was), or
 * it has none and was never admitted (the read decides).
 * `not_pinned`: the header holds no pin (a read for no open objective), so
 * no event hold was written.
 * `already_pruned`: pruning had removed the rows of `events` (as
 * `kind:key`) when the hold came; the other events are held.
 */
export type WatcherProofEventHoldResult =
  | Readonly<{ kind: "held" }>
  | Readonly<{ kind: "not_pinned" }>
  | Readonly<{ kind: "already_pruned"; events: readonly string[] }>;

export type WatcherProofRetention = Readonly<{
  /**
   * The k the follower store rewinds with, in blocks (it prunes at least
   * this deep): a proof completion is final, and its hold may be released,
   * only beyond k, since the store refuses any deeper rollback.
   */
  securityParameter: number;
  /** Holds the target's history under the follower's cursor lock; idempotent. */
  pin(target: WatcherProofRetentionTarget): Promise<WatcherProofPinResult>;
  /** Releases the target's hold, and the header's unit holds with its last pin. */
  release(target: WatcherProofRetentionTarget): Promise<void>;
  /**
   * Holds the units a capture for `headerHash` reads, while the header is
   * pinned: a followed unit by a unit row, the header's own state-queue
   * node unit through the header pin.
   */
  holdUnits(
    headerHash: string,
    units: readonly string[],
  ): Promise<WatcherProofUnitHoldResult>;
  /** Holds the user events a capture for `headerHash` reads, while the header is pinned. */
  holdEvents(
    headerHash: string,
    events: readonly WatcherProofEventRef[],
  ): Promise<WatcherProofEventHoldResult>;
  /**
   * Whether an admission over the observation at `point`, at its final
   * return, completed a pass at tip (`WatcherProofRosterRetention`).
   */
  passAtTip(
    point: Readonly<{ slot: string; blockHash: string }>,
  ): Promise<boolean>;
  /** Whether a pass at tip has lifted the roster's floor in this process
   * (`WatcherProofRosterRetention.lifted`). */
  lifted(): boolean;
  /**
   * Releases the holds no objective owns (`createUnownedHoldRelease`); a
   * no-op until a pass at tip has lifted the roster's floor.
   */
  releaseUnowned(
    input: WatcherUnownedHoldsInput,
  ): Promise<readonly WatcherProofRetentionTarget[]>;
  /** Every held target, header order. */
  pinned(): Promise<readonly WatcherProofRetentionTarget[]>;
  /** A block's depth on the follower's chain (`followerBlockDepth`). */
  blockDepth: WatcherBlockDepth["blockDepth"];
  /** The pins and holds pruning beat, as one status-only degradation. */
  degradations(): readonly WatcherL1Degradation[];
  /** The event holds pruning beat, as one readiness reason (`L1_PROOF_EVENT_PRUNED`). */
  readiness(): readonly Readonly<{ reason: string; detail: string }>[];
}>;

const HEADER = /^[0-9a-f]{56}$/u;

const headerBytes = (headerHash: string): Buffer => {
  if (!HEADER.test(headerHash))
    throw new Error(`proof retention header ${headerHash} is malformed`);
  return Buffer.from(headerHash, "hex");
};

/**
 * `unitHistoryPolicies` are the policies whose units' histories the
 * projection records (`watcherUnitHistoryPolicies`): the units a unit row
 * holds. `stateQueuePolicyId` names the node units a header pin holds (each
 * header's own). `roster` is the store's proof roster floor, and
 * `fraudProofSpend` the deployment's fraud-proof spend script hash, whose
 * live units keep no hold (`createUnownedHoldRelease`).
 */
export const createWatcherProofRetention = (
  store: WatcherProofRetentionStore,
  options: Readonly<{
    unitHistoryPolicies: ReadonlySet<string>;
    stateQueuePolicyId: string;
    roster: WatcherProofRosterRetention;
    fraudProofSpend: string;
  }>,
): WatcherProofRetention => {
  const nodeUnit = stateQueueNodeUnitPattern(options.stateQueuePolicyId);
  /** `category:header` (or `header#unit`) to what pruning removed first. */
  const pruned = new Map<string, string>();
  const prunedHeader = (headerHash: string): boolean =>
    [...pruned.keys()].some((key) => key.endsWith(`:${headerHash}`));
  const unitKey = (headerHash: string, unit: string): string =>
    `${headerHash}#${unit}`;
  /** `header@kind:key` to the event pruning removed first. */
  const prunedEvents = new Map<string, string>();
  const headerPinnedIn = async (
    tx: SqlTx,
    header: Buffer,
    lock: "update" | null,
  ): Promise<boolean> =>
    (
      await tx.query(
        `SELECT 1 AS one FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? LIMIT 1${lock === null ? "" : store.dialect.lockClause(lock)}`,
        [header],
      )
    ).length > 0;
  const forget = (
    { category, headerHash }: WatcherProofRetentionTarget,
    last: boolean,
  ): void => {
    pruned.delete(`${category}:${headerHash}`);
    if (!last) return;
    for (const key of [...pruned.keys()])
      if (key.startsWith(`${headerHash}#`)) pruned.delete(key);
    for (const key of [...prunedEvents.keys()])
      if (key.startsWith(`${headerHash}@`)) prunedEvents.delete(key);
  };
  const unowned = createUnownedHoldRelease(store, {
    securityParameter: store.securityParameter,
    fraudProofSpend: options.fraudProofSpend,
  });
  return Object.freeze({
    ...followerBlockDepth(store),
    securityParameter: store.securityParameter,
    pin: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      const result = await store.pinRetained({
        retained: async (tx, cursor) => {
          if (cursor === null) return false;
          const state = await historyState(
            tx,
            cursor,
            header,
            { table: WATCHER_PROOF_PINS_TABLE, column: "header_hash" },
            [
              {
                table: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
                column: "header_hash",
              },
              { table: WATCHER_DEPARTED_HEADERS_TABLE, column: "header_hash" },
            ],
            headerPrunedIn,
          );
          // None of it stored: empty before any pruning, else cannot tell.
          return state === "none" ? unpruned(cursor) : state !== "pruned";
        },
        insert: async (tx) => {
          await tx.query(
            `INSERT INTO ${WATCHER_PROOF_PINS_TABLE} (header_hash, category) VALUES (?, ?) ON CONFLICT DO NOTHING`,
            [header, category],
          );
        },
      });
      const key = `${category}:${headerHash}`;
      if (result.kind === "already_pruned")
        pruned.set(key, `the history of header ${headerHash} (${category})`);
      else pruned.delete(key);
      return result;
    },
    release: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      const last = await store.transaction("write", async (tx) => {
        await lockCursorIn(tx, store);
        return releasePinIn(tx, header, category);
      });
      forget({ category, headerHash }, last);
    },
    passAtTip: async ({ slot, blockHash }) => {
      const observed = await store.pointStatus({
        slot: Number(slot),
        hash: Buffer.from(blockHash, "hex"),
      });
      return options.roster.passAtTip(
        observed.kind === "canonical" ? observed.height : null,
      );
    },
    lifted: () => options.roster.lifted(),
    releaseUnowned: (input) =>
      options.roster.lifted()
        ? unowned.release(input, forget)
        : Promise.resolve([]),
    holdUnits: async (headerHash, units) => {
      const header = headerBytes(headerHash);
      if (units.length === 0) return { kind: "held" };
      const unpinned = (): WatcherProofUnitHoldResult =>
        // A header whose own pin pruning beat holds none of its units.
        prunedHeader(headerHash)
          ? { kind: "already_pruned", units: [...units] }
          : { kind: "not_pinned" };
      // The header's own state-queue node unit is held by the header's pin,
      // through its queue unit history rows. Any other unit the projection
      // does not record in the unit table (another header's node unit) only
      // its own header's pin could hold: it is not held here.
      const distinct = [...new Set(units)];
      const followed = distinct.filter((unit) =>
        recordsUnitHistory(options.unitHistoryPolicies, unit),
      );
      const foreign = distinct.some(
        (unit) =>
          !followed.includes(unit) && nodeUnit.exec(unit)?.[1] !== headerHash,
      );
      if (followed.length === 0)
        return (await store.transaction("read", (tx) =>
          headerPinnedIn(tx, header, null),
        ))
          ? foreign
            ? { kind: "not_pinned" }
            : { kind: "held" }
          : unpinned();
      const gone: string[] = [];
      for (const unit of followed) {
        const unitBytes = Buffer.from(unit, "hex");
        let headerPinned = true;
        let state = "held" as HistoryState;
        let wasPruned = true;
        const result = await store.pinRetained({
          retained: async (tx, cursor) => {
            // Locked so a concurrent release cannot delete the pin between
            // this read and the unit insert (Postgres READ COMMITTED; SQLite
            // serializes every write transaction and its clause is empty).
            headerPinned = await headerPinnedIn(tx, header, "update");
            if (!headerPinned || cursor === null) return false;
            wasPruned = !unpruned(cursor);
            state = await historyState(
              tx,
              cursor,
              unitBytes,
              { table: WATCHER_PROOF_PIN_UNITS_TABLE, column: "unit" },
              [{ table: WATCHER_UNIT_HISTORY_TABLE, column: "unit" }],
              unitPrunedIn,
            );
            // No rows and no record: held all the same; the raw read decides.
            return state !== "pruned";
          },
          insert: async (tx) => {
            await tx.query(
              `INSERT INTO ${WATCHER_PROOF_PIN_UNITS_TABLE} (header_hash, unit) VALUES (?, ?) ON CONFLICT DO NOTHING`,
              [header, unitBytes],
            );
          },
        });
        if (!headerPinned) return unpinned();
        if (result.kind === "already_pruned") {
          gone.push(unit);
          pruned.set(
            unitKey(headerHash, unit),
            `the history of unit ${unit} for header ${headerHash}`,
          );
        } else if (state === "whole" || (state === "none" && !wasPruned))
          // Only a hold that finds the history whole clears the unit's
          // degradation: a hold over no rows after pruning, or over an
          // existing hold, cannot tell whether pruning deleted it.
          pruned.delete(unitKey(headerHash, unit));
      }
      return gone.length > 0
        ? { kind: "already_pruned", units: gone }
        : foreign
          ? { kind: "not_pinned" }
          : { kind: "held" };
    },
    holdEvents: async (headerHash, events) => {
      const header = headerBytes(headerHash);
      const named = events.map(({ kind, key }) => `${kind}:${key}`);
      const gone: string[] = [];
      for (const [index, { kind, key }] of events.entries()) {
        const keyBytes = Buffer.from(key, "hex");
        let headerPinned = true;
        let present = false;
        const result = await store.pinRetained({
          retained: async (tx, cursor) => {
            // Locked against a concurrent release, as for a unit hold.
            headerPinned = await headerPinnedIn(tx, header, "update");
            if (!headerPinned || cursor === null) return false;
            const state = await eventRowStateIn(tx, kind, keyBytes);
            present = state === "present";
            // No row and never admitted: held all the same; the read decides.
            return state !== "pruned";
          },
          insert: async (tx) => {
            await tx.query(
              `INSERT INTO ${WATCHER_PROOF_PIN_EVENTS_TABLE} (header_hash, kind, event_key) VALUES (?, ?, ?) ON CONFLICT DO NOTHING`,
              [header, kind, keyBytes],
            );
          },
        });
        if (!headerPinned)
          // A header whose own pin pruning beat holds none of its events.
          return prunedHeader(headerHash)
            ? { kind: "already_pruned", events: named }
            : { kind: "not_pinned" };
        const name = `${headerHash}@${named[index]!}`;
        if (result.kind === "already_pruned") {
          gone.push(named[index]!);
          prunedEvents.set(
            name,
            `the ${kind} event ${key} for header ${headerHash}`,
          );
        } else if (present) prunedEvents.delete(name);
      }
      return gone.length > 0
        ? { kind: "already_pruned", events: gone }
        : { kind: "held" };
    },
    pinned: async () =>
      (
        await store.transaction("read", (tx) =>
          tx.query(
            `SELECT header_hash, category FROM ${WATCHER_PROOF_PINS_TABLE} ORDER BY header_hash, category`,
          ),
        )
      ).map((row) => ({
        category: String(row.category),
        headerHash: Buffer.from(row.header_hash as Uint8Array).toString("hex"),
      })),
    degradations: () => {
      const removed = [...pruned.values()];
      const { unowned: kept, live } = unowned.counts();
      return [
        ...(removed.length === 0
          ? []
          : [
              {
                reason: L1_PROOF_HISTORY_PRUNED,
                count: removed.length,
                detail: `${removed.length.toString()} open proof(s) lost history to pruning before a pin held it; first: ${removed[0]!} (its captures refuse; clears when the objective is released)`,
              },
            ]),
        ...(kept === 0
          ? []
          : [
              {
                reason: L1_PROOF_HOLDS_UNOWNED,
                count: kept,
                detail: `${kept.toString()} history hold(s) no proof objective owns are kept: their header or a held unit has not settled past completion depth (informational)`,
              },
            ]),
        ...(live === 0
          ? []
          : [
              {
                reason: L1_PROOF_HOLDS_UNOWNED_LIVE,
                count: live,
                detail: `${live.toString()} of the holds no proof objective owns are kept by a live held unit (informational)`,
              },
            ]),
      ];
    },
    readiness: () => {
      const removed = [...prunedEvents.values()];
      return removed.length === 0
        ? []
        : [
            {
              reason: L1_PROOF_EVENT_PRUNED,
              detail: `${removed.length.toString()} user event(s) an open proof reads were pruned before a hold held them; first: ${removed[0]!} (its captures refuse; clears when the objective is released)`,
            },
          ];
    },
  });
};
