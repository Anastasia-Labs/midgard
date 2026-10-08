import {
  asNumber,
  asString,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type { TrackedSet } from "../types.js";
import { resetIn } from "./reset.js";
import type { Rewound } from "./rewind.js";
import { readCursor } from "./rows.js";

/**
 * The protocol tracked set the stored facts were built under (§5.2, §5.3).
 * The facts are complete from the origin only for the set they were applied
 * under, so the store records it (`l1_follower_tracked_set`, bookkeeping
 * that reset keeps) and compares it with the configured set at each start
 * that finds a cursor:
 *
 * - equal: nothing to do;
 * - only removals: the record is rewritten; rows for the removed items stay
 *   until their own retention rule removes them;
 * - any addition, or no record (a store built before the record existed):
 *   the stored facts are incomplete for the added items, so the start resets
 *   the store (classes A, D-t and D-x; B and C are kept) and the follow loop
 *   replays from the origin.
 *
 * Every reset of a store with a record marks the replay in its own
 * transaction (`resetIn`): this one and a manual `reset --to-origin` alike.
 * The mark is the record's `replaying` flag and `replay_height`, the
 * cursor height before the reset. While it is set, prune runs no projection
 * prune hook (`pruneIn`: the facts below the cursor are incomplete until the
 * replay passes them) and the role is unready with `tracked_set_changed`. The
 * loop clears it at the first report at the node tip, with the node
 * available, once the cursor height is at least `replay_height`.
 *
 * Own wallets are never part of the record: they are seeded at the cursor
 * (§5.3 step 4) and never reset the store. The comparison is by item, as
 * configured: an address whose payment credential is also tracked still
 * counts as an item of its own (conservative: an item change resets).
 */

/** A tracked set as sorted lowercase hex lists. */
export type TrackedSetItems = Readonly<{
  addresses: readonly string[];
  paymentCredentials: readonly string[];
  policies: readonly string[];
}>;

export type TrackedSetRecord = Readonly<{
  trackedSet: TrackedSetItems;
  /** Set by a reset; cleared at the node tip once the cursor reached `replayHeight`. */
  replaying: boolean;
  /** The cursor height before the reset that set `replaying`; null when none was known. */
  replayHeight: number | null;
}>;

/** What `endTrackedSetReplayIn` did. */
export type TrackedSetReplayEnd =
  /** The flag was set and is now cleared. */
  | "ended"
  /** The flag was not set. */
  | "not_replaying"
  /** The cursor is below the height it held before the reset: still replaying. */
  | "below_replay_height";

/** What the start found when it compared the configured set with the record. */
export type TrackedSetCheck =
  /** No cursor: `initialize` records the set. */
  | Readonly<{ kind: "unchecked" }>
  | Readonly<{ kind: "equal" }>
  | Readonly<{ kind: "removed"; removed: TrackedSetItems }>
  | Readonly<{
      kind: "reset";
      /** `unrecorded`: the store had a cursor and no record. */
      cause: "added" | "unrecorded";
      added: TrackedSetItems;
      removed: TrackedSetItems;
      /** The catalog tables whose rows were deleted, sorted. */
      tables: readonly string[];
      /**
       * The rewind to the origin the reset amounts to, for the generation
       * listeners. A reset marker (`reset: true`); its `deleted` is empty.
       */
      rewound: Rewound;
    }>;

/** The set plus `addresses` (own wallets, tracked by address). */
export const withTrackedAddresses = (
  set: TrackedSet,
  addresses: readonly Buffer[],
): TrackedSet => ({
  addresses: new Set([
    ...set.addresses,
    ...addresses.map((address) => address.toString("hex")),
  ]),
  paymentCredentials: set.paymentCredentials,
  policies: set.policies,
});

const lowercase = (values: Iterable<string>): Set<string> =>
  new Set([...values].map((value) => value.toLowerCase()));

/**
 * The set with every item as lowercase hex, the form qualification and the
 * record compare: `createFactStore` normalises the configured set once, so
 * an item configured in uppercase qualifies the same outputs.
 */
export const normalizeTrackedSet = (set: TrackedSet): TrackedSet => ({
  addresses: lowercase(set.addresses),
  paymentCredentials: lowercase(set.paymentCredentials),
  policies: lowercase(set.policies),
});

const sorted = (values: Iterable<string>): string[] =>
  [...lowercase(values)].sort();

export const trackedSetItems = (set: TrackedSet): TrackedSetItems => ({
  addresses: sorted(set.addresses),
  paymentCredentials: sorted(set.paymentCredentials),
  policies: sorted(set.policies),
});

const minus = (
  left: TrackedSetItems,
  right: TrackedSetItems,
): TrackedSetItems => {
  const without = (from: readonly string[], drop: readonly string[]) => {
    const dropped = new Set(drop);
    return from.filter((value) => !dropped.has(value));
  };
  return {
    addresses: without(left.addresses, right.addresses),
    paymentCredentials: without(
      left.paymentCredentials,
      right.paymentCredentials,
    ),
    policies: without(left.policies, right.policies),
  };
};

const isEmpty = (items: TrackedSetItems): boolean =>
  items.addresses.length === 0 &&
  items.paymentCredentials.length === 0 &&
  items.policies.length === 0;

const hexList = (value: unknown): string[] => {
  const parsed: unknown = JSON.parse(asString(value));
  if (!Array.isArray(parsed) || parsed.some((v) => typeof v !== "string"))
    throw new Error("the tracked-set record holds a malformed list");
  return parsed as string[];
};

export const readTrackedSetRecordIn = async (
  tx: SqlTx,
): Promise<TrackedSetRecord | null> => {
  const row = (
    await tx.query(
      "SELECT addresses, payment_credentials, policies, replaying, replay_height FROM l1_follower_tracked_set WHERE id = 1",
    )
  )[0];
  if (row === undefined) return null;
  return {
    trackedSet: {
      addresses: hexList(row.addresses),
      paymentCredentials: hexList(row.payment_credentials),
      policies: hexList(row.policies),
    },
    replaying: asNumber(row.replaying) !== 0,
    replayHeight:
      row.replay_height === null || row.replay_height === undefined
        ? null
        : asNumber(row.replay_height),
  };
};

/**
 * Writes the record's items. A new record is not replaying; an existing
 * one keeps its replay mark (only `resetIn` sets it, only
 * `endTrackedSetReplayIn` clears it).
 */
export const writeTrackedSetRecordIn = async (
  tx: SqlTx,
  items: TrackedSetItems,
): Promise<void> => {
  await tx.query(
    `INSERT INTO l1_follower_tracked_set (id, addresses, payment_credentials, policies, replaying)
     VALUES (1, ?, ?, ?, 0)
     ON CONFLICT (id) DO UPDATE SET addresses = excluded.addresses,
       payment_credentials = excluded.payment_credentials,
       policies = excluded.policies`,
    [
      JSON.stringify(items.addresses),
      JSON.stringify(items.paymentCredentials),
      JSON.stringify(items.policies),
    ],
  );
};

/**
 * Clears the replay mark unless the cursor is below the height it held
 * before the reset: a node tip below it is no proof the replay passed every
 * fact the reset deleted.
 */
export const endTrackedSetReplayIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<TrackedSetReplayEnd> => {
  const cursor = await readCursor(tx, dialect, "update");
  const record = await readTrackedSetRecordIn(tx);
  if (record === null || !record.replaying) return "not_replaying";
  if (
    record.replayHeight !== null &&
    (cursor === null || cursor.height < record.replayHeight)
  )
    return "below_replay_height";
  await tx.query(
    "UPDATE l1_follower_tracked_set SET replaying = 0, replay_height = NULL WHERE id = 1",
  );
  return "ended";
};

/**
 * The start's comparison, in one write transaction: on an addition or a
 * missing record, the new record, the reset with its replay mark and its
 * `l1_rollbacks` row, and the rewind to the origin the reset amounts to all
 * commit together.
 */
export const checkTrackedSetIn = async (
  tx: SqlTx,
  dialect: Dialect,
  configured: TrackedSet,
): Promise<TrackedSetCheck> => {
  const cursor = await readCursor(tx, dialect, "update");
  if (cursor === null) return { kind: "unchecked" };
  const items = trackedSetItems(configured);
  const record = await readTrackedSetRecordIn(tx);
  const empty = trackedSetItems({
    addresses: new Set(),
    paymentCredentials: new Set(),
    policies: new Set(),
  });
  const added = record === null ? items : minus(items, record.trackedSet);
  const removed = record === null ? empty : minus(record.trackedSet, items);
  if (record !== null && isEmpty(added)) {
    if (isEmpty(removed)) return { kind: "equal" };
    await writeTrackedSetRecordIn(tx, items);
    return { kind: "removed", removed };
  }
  // The record first: the reset marks the replay on it.
  await writeTrackedSetRecordIn(tx, items);
  const reset = await resetIn(tx, dialect);
  if (reset.rewound === null)
    throw new Error("the reset found no cursor under the cursor lock");
  return {
    kind: "reset",
    cause: record === null ? "unrecorded" : "added",
    added,
    removed,
    tables: reset.tables,
    rewound: reset.rewound,
  };
};

/** One log line naming the items of a check that changed the record. */
export const describeTrackedSetCheck = (
  check: TrackedSetCheck,
): string | null => {
  const list = (items: TrackedSetItems): string =>
    `addresses [${items.addresses.join(", ")}], payment credentials [${items.paymentCredentials.join(", ")}], policies [${items.policies.join(", ")}]`;
  if (check.kind === "removed")
    return `the tracked set lost ${list(check.removed)}; record rewritten, no reset`;
  if (check.kind !== "reset") return null;
  return check.cause === "unrecorded"
    ? `the store has no tracked-set record; reset ${check.tables.length.toString()} tables and replaying from the origin under ${list(check.added)}`
    : `the tracked set gained ${list(check.added)}; reset ${check.tables.length.toString()} tables and replaying from the origin`;
};
