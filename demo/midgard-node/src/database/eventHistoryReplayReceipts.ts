import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Schema } from "effect";
import JSONBig from "json-bigint";

import type { EventHistoryListReplay } from "../l1-event-history-list-replay.js";
import {
  type BoundHistoryChainBlock,
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_replay_receipts";
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
type Point = BoundHistoryChainBlock["point"];
type Row = {
  binding_digest: Buffer;
  manifest_id: Buffer;
  block_hash: Buffer;
  block_slot: string;
  block_height: string;
  parent_hash: Buffer;
  predecessor_hash: Buffer | null;
  activation_hash: Buffer;
  blocks_replayed: string;
  receipt: string;
  receipt_digest: Buffer;
  range_digest: Buffer;
};
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const bytes = (value: string) => {
  if (!/^[0-9a-f]{64}$/u.test(value)) throw new Error("Invalid replay digest");
  return Buffer.from(value, "hex");
};
const natural = Schema.Number.pipe(Schema.int(), Schema.nonNegative());
const pointSchema = Schema.Struct({
  id: Schema.String,
  slot: natural,
  height: natural,
});
const receiptSchema = Schema.Struct({
  domain: Schema.Literal("midgard-node-authenticated-history-block-v1"),
  bindingDigest: Schema.String,
  block: Schema.Struct({
    point: pointSchema,
    parent: Schema.String,
    transactions: Schema.Array(Schema.Unknown),
  }),
  creatingBodies: Schema.Array(
    Schema.Struct({ txHash: Schema.String, bodyCbor: Schema.String }),
  ),
});
const originSchema = Schema.Struct({
  domain: Schema.Literal("midgard-node-authenticated-history-origin-v1"),
  bindingDigest: Schema.String,
  manifestId: Schema.String,
  activation: Schema.Struct({
    point: pointSchema,
    parent: Schema.String,
    transactionIndex: natural,
    transactionHash: Schema.String,
    receipt: Schema.String,
  }),
  replay: Schema.Struct({
    head: pointSchema,
    blocks: natural,
    digest: Schema.String,
  }),
  anchorSnapshotDigest: Schema.String,
});
const checked = <A>(work: () => A) =>
  Effect.try({
    try: work,
    catch: (cause) =>
      new DatabaseError({
        table,
        message: "Invalid retained history replay receipt",
        cause,
      }),
  });
const samePoint = (a: Point, b: Point) =>
  a.id === b.id && a.slot === b.slot && a.height === b.height;
const decodeRow = (row: Row, binding: EventHistorySourceBinding) => {
  const raw: unknown = lossless.parse(row.receipt);
  const receipt = Schema.decodeUnknownSync(receiptSchema)(raw);
  const point = receipt.block.point;
  if (
    eventHistoryCanonicalJson(raw) !== row.receipt ||
    sha(row.receipt) !== row.receipt_digest.toString("hex") ||
    row.binding_digest.toString("hex") !== binding.digest ||
    row.manifest_id.toString("hex") !== binding.manifestId ||
    receipt.bindingDigest !== binding.digest ||
    point.id !== row.block_hash.toString("hex") ||
    String(point.slot) !== row.block_slot ||
    String(point.height) !== row.block_height ||
    receipt.block.parent !== row.parent_hash.toString("hex")
  )
    throw new Error("Replay receipt bytes, binding or indexes disagree");
  return receipt;
};

/** Retain one source-verified replay step inside recovery.persist/withRecovery.
 * This is immutable node-owned recovery material, not authority for imported
 * archives. The caller must have admitted the block through its bound follower
 * and obtained replay/receipt from begin/advanceEventHistoryListReplay.
 * Each call writes one bounded block; parents are retained by a restrictive FK.
 */
export const put = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly block: BoundHistoryChainBlock;
  readonly receipt: string;
  readonly replay: EventHistoryListReplay;
  readonly maximumReceiptBytes: number;
}) =>
  Effect.gen(function* () {
    const owner = yield* requireRecoveryTransaction;
    const sql = yield* SqlClient.SqlClient;
    const prepared = yield* checked(() => {
      const { binding, replay, block, receipt } = input;
      if (
        owner.deploymentIdentity !== binding.manifestId ||
        replay.bindingDigest !== binding.digest ||
        replay.manifestId !== binding.manifestId ||
        !Number.isSafeInteger(input.maximumReceiptBytes) ||
        input.maximumReceiptBytes <= 0 ||
        Buffer.byteLength(receipt) > input.maximumReceiptBytes ||
        !Number.isSafeInteger(replay.blocksReplayed) ||
        replay.blocksReplayed < 1 ||
        !samePoint(replay.point, block.point) ||
        replay.blocksReplayed !==
          block.point.height - replay.activation.point.height + 1
      )
        throw new Error(
          "Replay step does not match its owner, point or bounds",
        );
      const raw: unknown = lossless.parse(receipt);
      const decoded = Schema.decodeUnknownSync(receiptSchema)(raw);
      if (
        eventHistoryCanonicalJson(raw) !== receipt ||
        decoded.bindingDigest !== binding.digest ||
        eventHistoryCanonicalJson(decoded.block) !==
          eventHistoryCanonicalJson(block)
      )
        throw new Error(
          "Replay receipt does not contain the exact admitted block",
        );
      for (const value of [
        binding.digest,
        binding.manifestId,
        block.point.id,
        block.parent,
        replay.activation.point.id,
        replay.replayDigest,
      ])
        bytes(value);
      return Object.freeze({
        binding,
        point: Object.freeze({ ...block.point }),
        parent: block.parent,
        receipt,
        activation: Object.freeze({
          ...replay.activation,
          point: Object.freeze({ ...replay.activation.point }),
        }),
        activationTransactionHash:
          block.transactions[replay.activation.transactionIndex]?.txHash,
        count: replay.blocksReplayed,
        rangeDigest: replay.replayDigest,
      });
    });
    const key = bytes(prepared.binding.digest);
    const existing =
      yield* sql<Row>`SELECT * FROM event_history_replay_receipts WHERE binding_digest = ${key} AND block_hash = ${bytes(prepared.point.id)}`;
    if (existing[0] !== undefined) {
      yield* checked(() => {
        decodeRow(existing[0]!, prepared.binding);
        if (
          existing[0]!.receipt !== prepared.receipt ||
          existing[0]!.range_digest.toString("hex") !== prepared.rangeDigest ||
          existing[0]!.activation_hash.toString("hex") !==
            prepared.activation.point.id ||
          Number(existing[0]!.blocks_replayed) !== prepared.count
        )
          throw new Error("Conflicting immutable replay receipt");
      });
      return;
    }
    const previous =
      prepared.count === 1
        ? undefined
        : (yield* sql<Row>`SELECT * FROM event_history_replay_receipts WHERE binding_digest = ${key} AND block_hash = ${bytes(prepared.parent)}`)[0];
    yield* checked(() => {
      if (prepared.count === 1) {
        const activation = prepared.activation;
        if (
          !samePoint(prepared.point, activation.point) ||
          prepared.parent !== activation.parent ||
          prepared.receipt !== activation.receipt ||
          sha(prepared.receipt) !== prepared.rangeDigest ||
          prepared.activationTransactionHash !== activation.transactionHash
        )
          throw new Error("First replay receipt is not its exact activation");
      } else {
        if (previous === undefined)
          throw new Error("Previous replay receipt is missing");
        const parent = decodeRow(previous, prepared.binding).block.point;
        if (
          parent.height + 1 !== prepared.point.height ||
          parent.slot >= prepared.point.slot ||
          previous.activation_hash.toString("hex") !==
            prepared.activation.point.id ||
          Number(previous.blocks_replayed) + 1 !== prepared.count ||
          sha(
            eventHistoryCanonicalJson({
              previous: previous.range_digest.toString("hex"),
              receipt: prepared.receipt,
            }),
          ) !== prepared.rangeDigest
        )
          throw new Error("Replay receipt breaks its persisted range");
      }
    });
    yield* sql`INSERT INTO event_history_replay_receipts
    (binding_digest, manifest_id, block_hash, block_slot, block_height, parent_hash, predecessor_hash, activation_hash, blocks_replayed, receipt, receipt_digest, range_digest)
    VALUES (${key}, ${bytes(prepared.binding.manifestId)}, ${bytes(prepared.point.id)}, ${prepared.point.slot}, ${prepared.point.height}, ${bytes(prepared.parent)}, ${previous === undefined ? null : previous.block_hash}, ${bytes(prepared.activation.point.id)}, ${prepared.count}, ${prepared.receipt}, ${bytes(sha(prepared.receipt))}, ${bytes(prepared.rangeDigest)})`;
  }).pipe(sqlErrorToDatabaseError(table, "Failed to retain replay receipt"));

/** Bounded endpoint check of the append-only frontier maintained by put. SQL
 * parent FKs retain all intermediate chunks. This relies on the same local
 * storage-integrity trust contract as the journal; a privileged imported DB is
 * not admitted L1 history. Replaying a saved chunk must recheck its own bytes.
 */
export const requireOriginCoverage = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly originReceipt: string;
  readonly anchor: Point;
  readonly anchorSnapshotDigest: string;
}) =>
  Effect.gen(function* () {
    // The earlier full-capture initialization receipt is self-contained. Only
    // authenticated-list origins require this separately retained replay range.
    let raw: unknown;
    try {
      raw = lossless.parse(input.originReceipt);
    } catch {
      return;
    }
    if (
      typeof raw !== "object" ||
      raw === null ||
      !("domain" in raw) ||
      raw.domain !== "midgard-node-authenticated-history-origin-v1"
    )
      return;
    const origin = yield* checked(() => {
      const value = Schema.decodeUnknownSync(originSchema)(raw);
      if (
        value.bindingDigest !== input.binding.digest ||
        value.manifestId !== input.binding.manifestId ||
        !samePoint(value.replay.head, input.anchor) ||
        value.anchorSnapshotDigest !== input.anchorSnapshotDigest ||
        value.replay.blocks < 1 ||
        value.replay.blocks !==
          input.anchor.height - value.activation.point.height + 1
      )
        throw new Error("Origin replay endpoint or binding disagrees");
      bytes(value.activation.point.id);
      bytes(value.replay.head.id);
      return value;
    });
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<Row>`SELECT * FROM event_history_replay_receipts WHERE binding_digest = ${bytes(input.binding.digest)} AND block_hash IN (${bytes(origin.activation.point.id)}, ${bytes(origin.replay.head.id)})`;
    yield* checked(() => {
      const first = rows.find(
        (row) => row.block_hash.toString("hex") === origin.activation.point.id,
      );
      const last = rows.find(
        (row) => row.block_hash.toString("hex") === origin.replay.head.id,
      );
      if (first === undefined || last === undefined)
        throw new Error("Origin replay receipts are missing");
      const activation = decodeRow(first, input.binding);
      const endpoint = decodeRow(last, input.binding);
      const transaction =
        activation.block.transactions[origin.activation.transactionIndex];
      if (
        first.receipt !== origin.activation.receipt ||
        first.blocks_replayed !== "1" ||
        first.predecessor_hash !== null ||
        !samePoint(activation.block.point, origin.activation.point) ||
        activation.block.parent !== origin.activation.parent ||
        typeof transaction !== "object" ||
        transaction === null ||
        !("txHash" in transaction) ||
        transaction.txHash !== origin.activation.transactionHash ||
        !samePoint(endpoint.block.point, input.anchor) ||
        last.activation_hash.toString("hex") !== origin.activation.point.id ||
        Number(last.blocks_replayed) !== origin.replay.blocks ||
        last.range_digest.toString("hex") !== origin.replay.digest
      )
        throw new Error(
          "Origin replay frontier disagrees with its retained receipts",
        );
    });
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to verify retained replay origin"),
  );
