/**
 * S6's view of the derived intent statuses (§8.3), kept between passes so a
 * pass reads what changed since the last one instead of the whole journal
 * (bounded per-block work). Each pass re-derives, from the facts at the
 * cursor and in one read transaction:
 *
 * - every live intent (its mempool, input and predicate reads follow);
 * - every intent recorded since the last pass (`intentsRecordedSinceIn`);
 * - every intent with an input, reference input or collateral spent in the
 *   slots the cursor moved over (landed, failed or conflicted there);
 * - every intent whose `invalid_hereafter` the cursor moved over;
 * - after a rewind, every intent whose status rests on a fact above the
 *   rewind target (a rewind only removes facts: landings, spends, passed
 *   validity), and every intent dead through a dependency;
 * - an earlier conflicted intent whose spender was just recorded (it is now
 *   superseded, not foreign-conflicted);
 * - the journaled dependants (transitively) of all of these, and their
 *   journaled dependencies, as `deriveIntentStatusIn` reads them.
 *
 * Every other status is unchanged: a landed intent's depth is recomputed
 * from the cursor height. The view is rebuilt from the whole journal on the
 * first pass, and when a generation change cannot be explained by the
 * retained rollback log (a reset, or a log pruned past the last pass). An
 * intent leaves the view once it is terminal for k (the prune boundary is at
 * or past its terminal slot), as the prune hook deletes it.
 */
import { depth, isFinal } from "../heads.js";
import { asNumber, type Dialect, type SqlTx } from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import type { Cursor } from "../types.js";
import {
  deriveIntentClosureIn,
  deriveIntentStatusesIn,
  type IntentState,
} from "./status.js";
import {
  intentsExpiringIn,
  intentsRecordedSinceIn,
  intentsTouchedBySpendsIn,
  type RecordingPosition,
  recordingPositionIn,
} from "./windows.js";

const hex = (bytes: Buffer): string => bytes.toString("hex");

type Keyed = Readonly<{ at: number; key: string }>;

/** A binary min-heap on `at`; entries are checked against the view when popped. */
class MinHeap {
  private readonly items: Keyed[] = [];

  push(item: Keyed): void {
    const items = this.items;
    items.push(item);
    let index = items.length - 1;
    while (index > 0) {
      const parent = (index - 1) >> 1;
      if (items[parent]!.at <= items[index]!.at) break;
      [items[parent], items[index]] = [items[index]!, items[parent]!];
      index = parent;
    }
  }

  peek(): Keyed | undefined {
    return this.items[0];
  }

  pop(): Keyed | undefined {
    const items = this.items;
    const top = items[0];
    const last = items.pop();
    if (top === undefined || last === undefined || items.length === 0)
      return top;
    items[0] = last;
    let index = 0;
    for (;;) {
      const left = 2 * index + 1;
      const right = left + 1;
      let least = index;
      if (left < items.length && items[left]!.at < items[least]!.at)
        least = left;
      if (right < items.length && items[right]!.at < items[least]!.at)
        least = right;
      if (least === index) break;
      [items[least], items[index]] = [items[index]!, items[least]!];
      index = least;
    }
    return top;
  }
}

/**
 * The slot of the fact a dead or landed status rests on; a rewind to below
 * it may change the status. Abandoned rests on no fact a rewind removes (a
 * rewind only removes facts, so no higher-precedence reason appears); a
 * live or dependency-dead status is always re-derived.
 */
const restsOnSlot = (state: IntentState): number => {
  const { status } = state;
  switch (status.kind) {
    case "landed":
    case "failed_landed":
    case "conflicted":
      return status.slot;
    case "expired":
      return status.validToSlot;
    case "abandoned":
      return Number.NEGATIVE_INFINITY;
    case "dependency_dead":
    case "live":
      return Number.POSITIVE_INFINITY;
  }
};

export type IntentStatesPass = Readonly<{
  cursor: Cursor | null;
  /** The states this pass derived (all of them on a rebuild), plus the landed ones that became final. */
  changed: readonly IntentState[];
}>;

export type IntentStatesView = Readonly<{
  /** Brings the view to the store's cursor, in the caller's read transaction. */
  advance(tx: SqlTx): Promise<IntentStatesPass>;
  /** One intent's state at the last pass, or undefined when it is not in the view. */
  state(txHash: Buffer): IntentState | undefined;
  /** Every state in the view at the last pass. */
  states(): IntentState[];
  /** The live intents' keys at the last pass. */
  liveKeys(): ReadonlySet<string>;
}>;

export const createIntentStatesView = (
  dialect: Dialect,
  securityParameter: number,
): IntentStatesView => {
  let cursor: Cursor | null = null;
  let position: RecordingPosition | null = null;
  const states = new Map<string, IntentState>();
  const live = new Set<string>();
  const children = new Map<string, Set<string>>();
  const conflictedBy = new Map<string, Set<string>>();
  let terminal = new MinHeap();
  let following = new MinHeap();
  /** The keys `following` holds (each once). */
  const followingKeys = new Set<string>();

  const isFollowing = (state: IntentState, at: Cursor | null): boolean =>
    state.status.kind === "landed" &&
    !isFinal(at === null ? 0 : depth(at.height, state.status.height), {
      securityParameter,
    });

  const remove = (key: string): void => {
    states.delete(key);
    live.delete(key);
    children.delete(key);
  };

  const put = (state: IntentState, at: Cursor | null): void => {
    const key = hex(state.intent.txHash);
    states.set(key, state);
    if (state.status.kind === "live") live.add(key);
    else live.delete(key);
    for (const dependency of state.intent.dependsOn) {
      const parent = hex(dependency);
      const set = children.get(parent) ?? new Set<string>();
      set.add(key);
      children.set(parent, set);
    }
    if (state.status.kind === "conflicted") {
      const spender = hex(state.status.spender);
      const set = conflictedBy.get(spender) ?? new Set<string>();
      set.add(key);
      conflictedBy.set(spender, set);
    }
    if (state.terminalSlot !== null)
      terminal.push({ at: state.terminalSlot, key });
    if (
      state.status.kind === "landed" &&
      isFollowing(state, at) &&
      !followingKeys.has(key)
    ) {
      followingKeys.add(key);
      following.push({ at: state.status.height, key });
    }
  };

  /** The state with its depth at `at` (a landed one's depth moves with the tip). */
  const at = (state: IntentState, tip: Cursor | null): IntentState =>
    (state.status.kind === "landed" || state.status.kind === "failed_landed") &&
    tip !== null
      ? {
          ...state,
          status: {
            ...state.status,
            depth: depth(tip.height, state.status.height),
          },
        }
      : state;

  const rebuild = async (tx: SqlTx): Promise<IntentStatesPass> => {
    const all = await deriveIntentStatusesIn(tx, dialect);
    states.clear();
    live.clear();
    children.clear();
    conflictedBy.clear();
    terminal = new MinHeap();
    following = new MinHeap();
    followingKeys.clear();
    for (const state of all.states) put(state, all.cursor);
    return { cursor: all.cursor, changed: all.states };
  };

  /** The rewind target below the last pass, or undefined when the log cannot explain the generation change. */
  const rewoundTo = async (
    tx: SqlTx,
    from: Cursor,
    to: Cursor,
  ): Promise<number | undefined> => {
    const rows = await tx.query(
      "SELECT to_slot FROM l1_rollbacks WHERE generation > ? AND generation <= ?",
      [from.generation, to.generation],
    );
    if (rows.length !== to.generation - from.generation) return undefined;
    return Math.min(...rows.map((row) => asNumber(row.to_slot)));
  };

  const advance = async (tx: SqlTx): Promise<IntentStatesPass> => {
    const now = await readCursor(tx, dialect);
    const nextPosition = await recordingPositionIn(tx, dialect);
    const last = cursor;
    const since = position;
    cursor = now;
    position = nextPosition;
    if (now === null || last === null || since === null) return rebuild(tx);
    let lower = last.point.slot;
    let rewind: number | null = null;
    if (now.generation !== last.generation) {
      const target = await rewoundTo(tx, last, now);
      if (target === undefined) return rebuild(tx);
      rewind = target;
      lower = Math.min(lower, target);
    } else if (now.point.slot < last.point.slot) return rebuild(tx);
    const range = { after: lower, through: now.point.slot };

    const seeds = new Set<string>(live);
    const hashes = new Map<string, Buffer>();
    const add = (key: string, hash?: Buffer): void => {
      seeds.add(key);
      if (hash !== undefined) hashes.set(key, hash);
    };
    for (const hash of await intentsRecordedSinceIn(tx, dialect, since)) {
      const key = hex(hash);
      add(key, hash);
      for (const conflicted of conflictedBy.get(key) ?? []) add(conflicted);
      conflictedBy.delete(key);
    }
    for (const hash of await intentsTouchedBySpendsIn(tx, range))
      add(hex(hash), hash);
    for (const hash of await intentsExpiringIn(tx, range)) add(hex(hash), hash);
    if (rewind !== null)
      for (const [key, state] of states)
        if (restsOnSlot(state) > rewind) add(key);
    const frontier = [...seeds];
    while (frontier.length > 0)
      for (const child of children.get(frontier.pop()!) ?? [])
        if (!seeds.has(child) && states.has(child)) {
          seeds.add(child);
          frontier.push(child);
        }
    const named = [...seeds].map(
      (key) =>
        hashes.get(key) ??
        states.get(key)?.intent.txHash ??
        Buffer.from(key, "hex"),
    );
    const derived = await deriveIntentClosureIn(tx, dialect, now, named);
    for (const key of seeds) if (!derived.has(key)) remove(key);
    const changed = new Map<string, IntentState>();
    for (const [key, state] of derived) {
      put(state, now);
      changed.set(key, state);
    }
    if (rewind !== null) {
      // The tip is lower: a landed intent may be within k again.
      following = new MinHeap();
      followingKeys.clear();
      for (const [key, state] of states)
        if (state.status.kind === "landed" && isFollowing(state, now)) {
          followingKeys.add(key);
          following.push({ at: state.status.height, key });
          if (!changed.has(key)) changed.set(key, at(state, now));
        }
    }
    for (;;) {
      const top = following.peek();
      if (top === undefined) break;
      const state = states.get(top.key);
      const current =
        state?.status.kind === "landed" && state.status.height === top.at;
      if (current && isFollowing(state, now)) break;
      following.pop();
      followingKeys.delete(top.key);
      if (current && !changed.has(top.key))
        changed.set(top.key, at(state, now));
    }
    for (;;) {
      const top = terminal.peek();
      if (top === undefined || top.at > now.prunedThroughSlot) break;
      terminal.pop();
      if (states.get(top.key)?.terminalSlot === top.at) remove(top.key);
    }
    return { cursor: now, changed: [...changed.values()] };
  };

  return {
    advance,
    state: (txHash) => {
      const state = states.get(hex(txHash));
      return state === undefined ? undefined : at(state, cursor);
    },
    states: () => [...states.values()].map((state) => at(state, cursor)),
    liveKeys: () => live,
  };
};
