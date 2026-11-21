import type {
  FactStore,
  OutRef,
  SqlRow,
  SqlTx,
} from "@al-ft/midgard-l1-follower";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
} from "@lucid-evolution/lucid";

import { outRefLabel, parseOutRefLabel, resolveRawUtxoIn } from "./reads.js";
import {
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_UNIT_CARRIERS_TABLE,
} from "./tables.js";

/**
 * Our computation threads as the follower holds them: the live outputs
 * under the computation-thread policy (`watcher_unit_carriers`), each
 * resolved to its exact bytes, kept when it holds exactly one thread token
 * and its step datum names our key as `fraud_prover`. The read is paged by
 * out-ref under a fixed cap, so one pass costs at most that many outputs
 * however many threads the chain holds; a provider scan of addresses is
 * never made.
 *
 * Each thread is classified against the follower's tip and the block
 * `finalDepth` deep (the tip is depth 1), in the same read transaction:
 *
 * - `workflow`: its header is a live queue node at the tip, so the
 *   workflow that opened it owns it;
 * - `reclaim`: no live queue node of its header at the tip nor at the deep
 *   block, and the thread was created at or before the deep block, so the
 *   header's removal is at least `finalDepth` deep and nothing will step
 *   the thread again;
 * - `settling`: otherwise (the header left within the last `finalDepth`
 *   blocks, or the thread is newer than that); a later read decides.
 *
 * A rollback that returns the header reopens its node row, and the next
 * read says `workflow`. Nothing here reads a stored workflow record.
 */

/** One thread token: `policy ++ categoryId (4 bytes) ++ headerHash (28 bytes)`. */
const THREAD_ASSET_NAME_BYTES = 32;
const CATEGORY_ID_BYTES = 4;

export type WatcherOwnThreadDisposition = "workflow" | "settling" | "reclaim";

export type WatcherOwnThread = Readonly<{
  outRef: string;
  /** The four-byte catalogue id the token names (hex). */
  categoryId: string;
  /** The header the thread disputes (hex). */
  headerHash: string;
  lovelace: bigint;
  /** The slot of the block that created the thread's output. */
  createdAtSlot: number;
  /** The payment script hash of the thread's address. */
  paymentScriptHash: string;
  disposition: WatcherOwnThreadDisposition;
}>;

export type WatcherOwnThreadScope = Readonly<{
  /** The computation-thread minting policy (hex). */
  policyId: string;
  /** Our payment key hash: a thread is ours when its `fraud_prover` is this. */
  proverKeyHash: string;
  /** The depth a removal must reach before its threads are reclaimed. */
  finalDepth: number;
}>;

export type WatcherOwnThreadPage =
  | Readonly<{ kind: "unready"; reason: string }>
  | Readonly<{
      kind: "page";
      threads: readonly WatcherOwnThread[];
      /** Live outputs under the policy that this page read. */
      scanned: number;
      /** The out-ref range the page covered: after `after`, through `through` (null: the end). */
      after: string | null;
      through: string | null;
    }>;

type Points = Readonly<{ tipSlot: number; deepSlot: number }>;

const outRefOf = (row: SqlRow): OutRef => ({
  txHash: Buffer.from(row.tx_hash as Uint8Array),
  index: Number(row.output_index as number | string),
});

/** The tip's and the deep block's slots, or why the chain is too short. */
const pointsIn = async (
  tx: SqlTx,
  finalDepth: number,
): Promise<Points | string> => {
  const cursor = (
    await tx.query("SELECT height, origin_slot FROM l1_follower_cursor", [])
  )[0];
  if (cursor === undefined) return "the follower has not applied a block";
  const height = Number(cursor.height as number | string);
  const slotAt = async (at: number): Promise<number | null> => {
    const row = (
      await tx.query("SELECT slot FROM l1_blocks WHERE height = ?", [at])
    )[0];
    return row === undefined ? null : Number(row.slot as number | string);
  };
  const [tipSlot, deepSlot] = [
    await slotAt(height),
    await slotAt(height - finalDepth + 1),
  ];
  if (
    tipSlot === null ||
    deepSlot === null ||
    deepSlot <= Number(cursor.origin_slot as number | string)
  )
    return `the follower holds fewer than ${finalDepth.toString()} blocks after its origin`;
  return { tipSlot, deepSlot };
};

const nodeLiveAt = async (
  tx: SqlTx,
  headerHash: Buffer,
  slot: number | null,
): Promise<boolean> =>
  (
    await tx.query(
      slot === null
        ? `SELECT 1 FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash = ? AND to_slot IS NULL LIMIT 1`
        : `SELECT 1 FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash = ? AND from_slot <= ? AND (to_slot IS NULL OR to_slot > ?) LIMIT 1`,
      slot === null ? [headerHash] : [headerHash, slot, slot],
    )
  ).length > 0;

/**
 * The thread at `outRef` created at `createdAtSlot`, when its exact output
 * holds exactly one token of `policyId` (and nothing else but ada) at a
 * script address with our step datum; else null.
 */
const ownThreadIn = async (
  tx: SqlTx,
  outRef: OutRef,
  createdAtSlot: number,
  scope: WatcherOwnThreadScope,
  points: Points,
): Promise<WatcherOwnThread | null> => {
  const raw = await resolveRawUtxoIn(tx, outRef);
  if (raw === null || raw.datumCbor === null) return null;
  const core = CML.TransactionOutput.from_cbor_hex(raw.outputCbor);
  let output: ReturnType<typeof coreToTxOutput>;
  try {
    output = coreToTxOutput(core);
  } finally {
    core.free();
  }
  const tokens = Object.entries(output.assets).filter(
    ([unit]) => unit !== "lovelace",
  );
  const [token] = tokens;
  if (tokens.length !== 1 || token === undefined) return null;
  const [unit, quantity] = token;
  const name = unit.slice(scope.policyId.length);
  if (
    !unit.startsWith(scope.policyId) ||
    quantity !== 1n ||
    name.length !== THREAD_ASSET_NAME_BYTES * 2
  )
    return null;
  const payment = getAddressDetails(output.address).paymentCredential;
  if (payment?.type !== "Script") return null;
  let prover: string;
  try {
    prover = Data.from(
      raw.datumCbor,
      FraudProofComputationThreadStepDatum,
    ).fraud_prover;
  } catch {
    return null;
  }
  if (prover !== scope.proverKeyHash) return null;
  const headerHash = name.slice(CATEGORY_ID_BYTES * 2);
  const headerBytes = Buffer.from(headerHash, "hex");
  const disposition: WatcherOwnThreadDisposition = (await nodeLiveAt(
    tx,
    headerBytes,
    null,
  ))
    ? "workflow"
    : createdAtSlot > points.deepSlot ||
        (await nodeLiveAt(tx, headerBytes, points.deepSlot))
      ? "settling"
      : "reclaim";
  return Object.freeze({
    outRef: outRefLabel(outRef),
    categoryId: name.slice(0, CATEGORY_ID_BYTES * 2),
    headerHash,
    lovelace: output.assets.lovelace ?? 0n,
    createdAtSlot,
    paymentScriptHash: payment.hash,
    disposition,
  });
};

/** The byte range every unit of `policyId` falls in. */
const unitRange = (policyId: string): readonly [Buffer, Buffer] => [
  Buffer.from(policyId, "hex"),
  Buffer.from(policyId + "ff".repeat(THREAD_ASSET_NAME_BYTES), "hex"),
];

/**
 * One page of our threads: the live outputs under the policy after `after`
 * (out-ref order; null starts at the first), at most `limit` of them, each
 * kept when it is ours and classified. A page shorter than `limit` reaches
 * the end (`through` null).
 */
export const readWatcherOwnThreadPage = async (
  store: Pick<FactStore, "transaction">,
  input: WatcherOwnThreadScope &
    Readonly<{ after: string | null; limit: number }>,
): Promise<WatcherOwnThreadPage> => {
  if (!Number.isSafeInteger(input.limit) || input.limit < 1)
    throw new Error("own-thread page limit must be a positive integer");
  if (!Number.isSafeInteger(input.finalDepth) || input.finalDepth < 1)
    throw new Error("own-thread final depth must be a positive integer");
  return await store.transaction("read", async (tx) => {
    const points = await pointsIn(tx, input.finalDepth);
    if (typeof points === "string")
      return Object.freeze({ kind: "unready", reason: points });
    const [low, high] = unitRange(input.policyId);
    const after = input.after === null ? null : parseOutRefLabel(input.after);
    const rows = await tx.query(
      `SELECT tx_hash, output_index, MIN(from_slot) AS from_slot FROM ${WATCHER_UNIT_CARRIERS_TABLE} WHERE to_slot IS NULL AND unit >= ? AND unit <= ?${after === null ? "" : " AND (tx_hash > ? OR (tx_hash = ? AND output_index > ?))"} GROUP BY tx_hash, output_index ORDER BY tx_hash, output_index LIMIT ?`,
      after === null
        ? [low, high, input.limit]
        : [low, high, after.txHash, after.txHash, after.index, input.limit],
    );
    const threads: WatcherOwnThread[] = [];
    for (const row of rows) {
      const thread = await ownThreadIn(
        tx,
        outRefOf(row),
        Number(row.from_slot as number | string),
        input,
        points,
      );
      if (thread !== null) threads.push(thread);
    }
    const last = rows.at(-1);
    return Object.freeze({
      kind: "page",
      threads: Object.freeze(threads),
      scanned: rows.length,
      after: input.after,
      through:
        rows.length < input.limit || last === undefined
          ? null
          : outRefLabel(outRefOf(last)),
    });
  });
};

/**
 * The thread at `outRef` as the follower holds it now, classified; null
 * when it is spent or not ours. The check a cancel makes just before it is
 * sent.
 */
export const readWatcherOwnThread = async (
  store: Pick<FactStore, "transaction">,
  input: WatcherOwnThreadScope & Readonly<{ outRef: string }>,
): Promise<WatcherOwnThread | null> =>
  await store.transaction("read", async (tx) => {
    const points = await pointsIn(tx, input.finalDepth);
    if (typeof points === "string") return null;
    const outRef = parseOutRefLabel(input.outRef);
    const [low, high] = unitRange(input.policyId);
    const row = (
      await tx.query(
        `SELECT MIN(from_slot) AS from_slot FROM ${WATCHER_UNIT_CARRIERS_TABLE} WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL AND unit >= ? AND unit <= ?`,
        [outRef.txHash, outRef.index, low, high],
      )
    )[0];
    if (row?.from_slot === null || row?.from_slot === undefined) return null;
    return await ownThreadIn(
      tx,
      outRef,
      Number(row.from_slot as number | string),
      input,
      points,
    );
  });

/** Out-ref order as the page reads it: transaction hash bytes, then index. */
export const compareOwnThreadOutRefs = (
  left: string,
  right: string,
): number => {
  const a = parseOutRefLabel(left);
  const b = parseOutRefLabel(right);
  return Buffer.compare(a.txHash, b.txHash) || a.index - b.index;
};
