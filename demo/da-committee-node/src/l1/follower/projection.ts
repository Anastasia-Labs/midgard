import {
  depth,
  type DepthParameters,
  type FactStore,
  type FollowerProjection,
  type SqlRow,
  type SqlTx,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";

import {
  type AwaitingHeader,
  headersAwaitingAttestation,
  type LandedQueue,
  readLiveQueueRows,
  readQueueHeaderHashesAt,
  walkLandedQueue,
} from "./landed-queue.js";
import {
  type Obligation,
  obligations,
  readObligationFacts,
  type SignedHeader,
  type SlotTime,
  slotTimeMs,
} from "./obligations.js";
import {
  committeeQueueDerivation,
  type CommitteeQueueParameters,
} from "./queue-derivation.js";
import {
  COMMITTEE_QUEUE_TABLE,
  COMMITTEE_QUEUE_TABLE_SPEC,
  COMMITTEE_RETENTION_PINS,
  committeeMigrations,
} from "./queue-table.js";

/**
 * What the committee tracks on L1 beside the state queue (plan §4.4,
 * committee column): raw address bytes, script payment credentials (hex)
 * and policy ids (hex).
 */
export type CommitteeTracked = Readonly<{
  addresses: readonly Buffer[];
  paymentCredentials: readonly string[];
  policies: readonly string[];
}>;

/**
 * The committee's tracked set (plan §4.4, committee column): the state-queue
 * address and policy, plus every other address and policy the committee
 * reads (`tracked`). The own wallets join through the wallet seeder.
 */
export const committeeTrackedSet = (
  parameters: CommitteeQueueParameters,
  tracked: CommitteeTracked = {
    addresses: [],
    paymentCredentials: [],
    policies: [],
  },
): TrackedSet => ({
  addresses: new Set([
    parameters.stateQueueAddress.toString("hex"),
    ...tracked.addresses.map((address) => address.toString("hex")),
  ]),
  paymentCredentials: new Set(
    tracked.paymentCredentials.map((credential) => credential.toLowerCase()),
  ),
  policies: new Set([
    parameters.stateQueuePolicyId,
    ...tracked.policies.map((policy) => policy.toLowerCase()),
  ]),
});

/** The committee's projections, as the follower store plugs them in. */
export const committeeProjection = (
  parameters: CommitteeQueueParameters,
  tracked?: CommitteeTracked,
): FollowerProjection => ({
  name: "committee",
  trackedSet: committeeTrackedSet(parameters, tracked),
  temporalTables: [COMMITTEE_QUEUE_TABLE_SPEC],
  migrations: committeeMigrations,
  derivations: [committeeQueueDerivation(parameters)],
  retentionPins: COMMITTEE_RETENTION_PINS,
});

/** Where a header left the landed queue: the block that spent its last node. */
export type QueueExit = Readonly<{
  headerHash: string;
  /** `merged`: a root carrying the header exists; otherwise `removed`. */
  status: "merged" | "removed";
  slot: number;
  blockHash: string;
  blockHeight: number;
  depth: number;
}>;

/** Everything one committee tick reads from the follower, at one cursor. */
export type CommitteeView = Readonly<{
  at: Readonly<{ slot: number; height: number; generation: number }>;
  queue: LandedQueue;
  awaiting: readonly AwaitingHeader[];
  obligations: readonly Obligation[];
  /**
   * The header hashes of the queue live at the latest final block (the
   * root's confirmed header and every node), sorted. Empty while the store
   * holds no final block.
   */
  finalQueueHeaderHashes: readonly string[];
  /** The slot of the latest final block the store holds, or null. */
  finalSlot: number | null;
  /** The exits of the asked headers (`exitsOf`) whose node outputs are all spent. */
  exits: readonly QueueExit[];
  /** Block hash (hex) per created slot of each live node output. */
  nodeBlocks: ReadonlyMap<number, string>;
  /** The slot at or below which the store may have pruned facts. */
  prunedThroughSlot: number;
}>;

export type CommitteeViewOptions = Readonly<{
  parameters: DepthParameters;
  slotTime: SlotTime;
  /** The member's signed headers (class B), read by the caller. */
  signed?: readonly SignedHeader[];
  /** Header hashes whose exit from the queue the caller asks for. */
  exitsOf?: readonly string[];
}>;

const asNumber = (value: unknown): number => Number(value as string | number);

const asHex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

const byText = (a: string, b: string): number => (a < b ? -1 : a > b ? 1 : 0);

/** Hashes per `IN` list: well under every backend's parameter cap. */
const BATCH = 500;

const inBatches = async <T extends string | number>(
  values: readonly T[],
  read: (batch: readonly T[], marks: string) => Promise<SqlRow[]>,
): Promise<SqlRow[]> => {
  const out: SqlRow[] = [];
  for (let at = 0; at < values.length; at += BATCH) {
    const batch = values.slice(at, at + BATCH);
    out.push(...(await read(batch, batch.map(() => "?").join(", "))));
  }
  return out;
};

/** The hash of each stored block at the given slots, keyed by slot. */
const readBlockHashes = async (
  tx: SqlTx,
  slots: readonly number[],
): Promise<Map<number, string>> =>
  new Map(
    (
      await inBatches([...new Set(slots)], (batch, marks) =>
        tx.query(
          `SELECT slot, hash FROM l1_blocks WHERE slot IN (${marks})`,
          batch,
        ),
      )
    ).map((block) => [asNumber(block.slot), asHex(block.hash)]),
  );

/**
 * The exit of each asked header whose node outputs are all spent: the latest
 * spend, its block, and whether a root carrying the header exists (a merge
 * creates one in the spending tx). A header with a live node, with no row
 * left, or whose spending block the store no longer holds, has none.
 */
const readExits = async (
  tx: SqlTx,
  headerHashes: readonly string[],
  tipHeight: number,
): Promise<QueueExit[]> => {
  const spent = await inBatches([...new Set(headerHashes)], (batch, marks) =>
    tx.query(
      `SELECT header_hash, MAX(spent_slot) AS spent_slot FROM ${COMMITTEE_QUEUE_TABLE} WHERE kind = 'node' AND header_hash IN (${marks}) GROUP BY header_hash HAVING COUNT(*) = COUNT(spent_slot)`,
      batch,
    ),
  );
  if (spent.length === 0) return [];
  const merged = new Set(
    (
      await inBatches(
        spent.map((row) => String(row.header_hash)),
        (batch, marks) =>
          tx.query(
            `SELECT DISTINCT header_hash FROM ${COMMITTEE_QUEUE_TABLE} WHERE kind = 'root' AND header_hash IN (${marks})`,
            batch,
          ),
      )
    ).map((row) => String(row.header_hash)),
  );
  const blocks = new Map(
    (
      await inBatches(
        [...new Set(spent.map((row) => asNumber(row.spent_slot)))],
        (batch, marks) =>
          tx.query(
            `SELECT slot, hash, height FROM l1_blocks WHERE slot IN (${marks})`,
            batch,
          ),
      )
    ).map((block) => [asNumber(block.slot), block]),
  );
  const exits: QueueExit[] = [];
  for (const row of spent) {
    const slot = asNumber(row.spent_slot);
    const block = blocks.get(slot);
    if (block === undefined) continue;
    const blockHeight = asNumber(block.height);
    const headerHash = String(row.header_hash);
    exits.push({
      headerHash,
      status: merged.has(headerHash) ? "merged" : "removed",
      slot,
      blockHash: asHex(block.hash),
      blockHeight,
      depth: depth(tipHeight, blockHeight),
    });
  }
  return exits.sort((a, b) => byText(a.headerHash, b.headerHash));
};

/**
 * One committee tick's read: the landed queue, the headers awaiting
 * attestation, the obligations, the queue at the latest final block and the
 * asked exits, all in one read transaction (a snapshot) at the store's
 * cursor. Null before the store is initialized.
 */
export const readCommitteeView = async (
  store: FactStore,
  options: CommitteeViewOptions,
): Promise<CommitteeView | null> => {
  const signed = options.signed ?? [];
  const read = await store.transaction("read", async (tx) => {
    const cursor = (
      await tx.query(
        "SELECT slot, height, generation, origin_slot, pruned_through_slot FROM l1_follower_cursor",
      )
    )[0];
    if (cursor === undefined) return null;
    const at = {
      slot: asNumber(cursor.slot),
      height: asNumber(cursor.height),
      generation: asNumber(cursor.generation),
    };
    const prunedThroughSlot = asNumber(cursor.pruned_through_slot);
    const rows = await readLiveQueueRows(tx);
    const facts = await readObligationFacts(
      tx,
      signed,
      at.height,
      options.parameters,
    );
    const finalHeaderHashes =
      facts.finalBlockSlot === null
        ? []
        : await readQueueHeaderHashesAt(tx, facts.finalBlockSlot);
    const exits = await readExits(tx, options.exitsOf ?? [], at.height);
    const nodeBlocks = await readBlockHashes(
      tx,
      rows.flatMap((row) => (row.kind === "node" ? [row.createdSlot] : [])),
    );
    return {
      at,
      rows,
      facts,
      finalHeaderHashes,
      exits,
      nodeBlocks,
      prunedThroughSlot,
      pruned: prunedThroughSlot > asNumber(cursor.origin_slot),
    };
  });
  if (read === null) return null;
  const tipHeight = read.at.height;
  const queue = walkLandedQueue(read.rows);
  return {
    at: read.at,
    queue,
    awaiting: headersAwaitingAttestation(queue, tipHeight, options.parameters),
    obligations: obligations({
      signed,
      presence: read.facts.presence,
      tipHeight,
      finalBlockTimeMs:
        read.facts.finalBlockSlot === null
          ? null
          : slotTimeMs(read.facts.finalBlockSlot, options.slotTime),
      pruned: read.pruned,
      parameters: options.parameters,
    }),
    finalQueueHeaderHashes: read.finalHeaderHashes,
    finalSlot: read.facts.finalBlockSlot,
    exits: read.exits,
    nodeBlocks: read.nodeBlocks,
    prunedThroughSlot: read.prunedThroughSlot,
  };
};
