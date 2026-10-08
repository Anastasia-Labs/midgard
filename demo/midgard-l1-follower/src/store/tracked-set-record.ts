import {
  asBuffer,
  asNumber,
  asString,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type { OutRef, TrackedSet } from "../types.js";
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
 *   replays from the origin, in the same transaction setting the record's
 *   `replaying` flag. The loop clears the flag the first time it reports the
 *   cursor at the node tip; until then the role is unready with
 *   `tracked_set_changed`.
 *
 * Own wallets are never part of the record: they are seeded at the cursor
 * (§5.3 step 4) and never reset the store.
 */

/** A tracked set as sorted lowercase hex lists. */
export type TrackedSetItems = Readonly<{
  addresses: readonly string[];
  paymentCredentials: readonly string[];
  policies: readonly string[];
}>;

export type TrackedSetRecord = Readonly<{
  trackedSet: TrackedSetItems;
  /** Set by a tracked-set reset; cleared at the first report at the node tip. */
  replaying: boolean;
}>;

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
      /** The rewind to the origin the reset amounts to, for the generation listeners. */
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

const sorted = (values: Iterable<string>): string[] =>
  [...new Set([...values].map((value) => value.toLowerCase()))].sort();

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
      "SELECT addresses, payment_credentials, policies, replaying FROM l1_follower_tracked_set WHERE id = 1",
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
  };
};

/** Writes the record; `keep` leaves an existing `replaying` flag as it is. */
export const writeTrackedSetRecordIn = async (
  tx: SqlTx,
  items: TrackedSetItems,
  replaying: boolean | "keep",
): Promise<void> => {
  const values = [
    JSON.stringify(items.addresses),
    JSON.stringify(items.paymentCredentials),
    JSON.stringify(items.policies),
    replaying === true ? 1 : 0,
  ];
  await tx.query(
    `INSERT INTO l1_follower_tracked_set (id, addresses, payment_credentials, policies, replaying)
     VALUES (1, ?, ?, ?, ?)
     ON CONFLICT (id) DO UPDATE SET addresses = excluded.addresses,
       payment_credentials = excluded.payment_credentials,
       policies = excluded.policies${replaying === "keep" ? "" : ", replaying = excluded.replaying"}`,
    values,
  );
};

/** Clears the `replaying` flag; true when it was set. */
export const endTrackedSetReplayIn = async (tx: SqlTx): Promise<boolean> =>
  (
    await tx.query(
      "UPDATE l1_follower_tracked_set SET replaying = 0 WHERE id = 1 AND replaying <> 0 RETURNING id",
    )
  ).length > 0;

/**
 * The start's comparison, in one write transaction: on an addition or a
 * missing record, the reset, the new record with `replaying` set, and the
 * rewind to the origin the reset amounts to (from the old cursor, deleting
 * every live outref) all commit together.
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
    await writeTrackedSetRecordIn(tx, items, "keep");
    return { kind: "removed", removed };
  }
  const origin = (
    await tx.query("SELECT height FROM l1_blocks WHERE slot = ? AND hash = ?", [
      cursor.origin.slot,
      cursor.origin.hash,
    ])
  )[0];
  if (origin === undefined)
    throw new Error("the origin block row is missing; the store is broken");
  const originHeight = asNumber(origin.height);
  const deleted: OutRef[] = (
    await tx.query(
      "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL",
    )
  ).map((row) => ({
    txHash: asBuffer(row.tx_hash),
    index: asNumber(row.output_index),
  }));
  const reset = await resetIn(tx, dialect);
  await writeTrackedSetRecordIn(tx, items, true);
  return {
    kind: "reset",
    cause: record === null ? "unrecorded" : "added",
    added,
    removed,
    tables: reset.tables,
    rewound: {
      kind: "rewound",
      generation: reset.nextGeneration,
      from: cursor.point,
      to: cursor.origin,
      depth: cursor.height - originHeight,
      cursor: {
        point: cursor.origin,
        height: originHeight,
        generation: reset.nextGeneration,
        origin: cursor.origin,
        prunedThroughSlot: cursor.origin.slot,
      },
      unspent: [],
      deleted,
    },
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
