import { createHash } from "node:crypto";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Schema } from "effect";

import type { BoundHistoryCapture } from "../l1-event-history-projection.js";
import {
  type HistoryIncarnation,
  historyIncarnationDigest,
  historyIncarnationId,
  type HistoryProvenanceChange,
  reverseHistoryProvenance,
  stageHistoryProvenance,
} from "../l1-event-history-provenance.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import type { HistoryTransition } from "../l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";
import {
  requireRecoveryTransaction,
  requireSourceTransaction,
} from "./eventHistoryAuthority.js";
import {
  decodeJournalIncarnation,
  decodeJournalOutput,
  encodeJournalIncarnation,
  encodeJournalOutput,
} from "./eventHistoryJournalCodec.js";
import {
  originReplayHead,
  prune as pruneReplayReceipts,
  requireOriginCoverage,
} from "./eventHistoryReplayReceipts.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_cursor";
type Point = BoundHistoryChainBlock["point"];
export type Checkpoint = Readonly<{
  bindingDigest: string;
  manifestId: string;
  originReceipt: string;
  originReceiptDigest: string;
  anchor: Point;
  anchorSnapshotDigest: string;
  head: Point;
  headApplicationRevision: string | null;
  revision: string;
  capture: BoundHistoryCapture;
  incarnations: readonly HistoryIncarnation[];
}>;
type CursorRow = {
  binding_digest: Buffer;
  manifest_id: Buffer;
  origin_receipt: string;
  origin_receipt_digest: Buffer;
  anchor_hash: Buffer;
  anchor_slot: string;
  anchor_height: string;
  anchor_snapshot_digest: Buffer;
  head_hash: Buffer;
  head_slot: string;
  head_height: string;
  head_application_revision: string | null;
  snapshot_digest: Buffer;
  revision: string;
  addresses: unknown;
};
type ApplicationRow = {
  block_hash: Buffer;
  application_revision: string;
  parent_hash: Buffer;
  parent_application_revision: string | null;
  block_slot: string;
  block_height: string;
  before_snapshot_digest: Buffer;
  after_snapshot_digest: Buffer;
  ledger_receipt: string;
  ledger_receipt_digest: Buffer;
  undo_record: string;
  undo_digest: Buffer;
  canonical: boolean;
};
const undoSchema = Schema.Struct({
  outputs: Schema.Array(
    Schema.Struct({
      before: Schema.NullOr(Schema.String),
      after: Schema.NullOr(Schema.String),
    }),
  ),
  incarnations: Schema.Array(
    Schema.Struct({
      before: Schema.NullOr(Schema.String),
      after: Schema.String,
    }),
  ),
});
type Undo = typeof undoSchema.Type;
const digest = (text: string) =>
  createHash("sha256").update(text).digest("hex");
function fail(message: string): never {
  throw new Error(message);
}
const checked = <A>(f: () => A) =>
  Effect.try({
    try: f,
    catch: (cause) =>
      new DatabaseError({
        table,
        message: `Invalid history journal: ${cause instanceof Error ? cause.message : String(cause)}`,
        cause,
      }),
  });
const bytes = (value: string) =>
  /^[0-9a-f]{64}$/u.test(value)
    ? Buffer.from(value, "hex")
    : fail("Expected a complete lowercase digest");
const natural = (value: string | number) => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0)
    fail("Unsafe history coordinate");
  return number;
};
const key = (value: Pick<LedgerSnapshotOutput, "txHash" | "outputIndex">) =>
  `${value.txHash}#${value.outputIndex}`;
const stable = (value: unknown): unknown => {
  if (typeof value === "bigint") return { integer: value.toString() };
  if (Array.isArray(value)) return value.map(stable);
  if (typeof value === "object" && value !== null)
    return Object.fromEntries(
      Object.entries(value)
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([k, v]) => [k, stable(v)]),
    );
  return value;
};
const serialise = (value: unknown) => JSON.stringify(stable(value));
const freeze = <T>(value: T): T => {
  if (typeof value === "object" && value !== null) {
    for (const child of Object.values(value)) freeze(child);
    Object.freeze(value);
  }
  return value;
};
const samePoint = (a: Point, b: Point) =>
  a.id === b.id && a.slot === b.slot && a.height === b.height;
const point = (id: Buffer, slot: string, height: string): Point =>
  Object.freeze({
    id: id.toString("hex"),
    slot: natural(slot),
    height: natural(height),
  });

const validateIncarnations = (
  bindingDigest: string,
  values: readonly HistoryIncarnation[],
) => {
  const ids = new Set<string>();
  const canonicalKeys = new Set<string>();
  const canonicalIds = new Set<string>();
  for (const value of values) {
    if (
      value.bindingDigest !== bindingDigest ||
      value.id !==
        historyIncarnationId(bindingDigest, value.kind, value.event) ||
      ids.has(value.id)
    )
      fail("Incarnation identity or binding disagrees");
    ids.add(value.id);
    if (value.placement !== null) {
      const k = `${value.kind}:${value.event.key}`;
      const id = `${value.kind}:${value.event.idCbor}`;
      if (canonicalKeys.has(k) || canonicalIds.has(id))
        fail("Repeated canonical event origin");
      canonicalKeys.add(k);
      canonicalIds.add(id);
    }
  }
};

/** Completeness of historical admissions still comes from authenticated replay.
 * This check prevents a current live Order from being persisted without an
 * origin, or a retired/orphan record from being substituted for that origin. */
const validateLiveCoverage = (
  capture: BoundHistoryCapture,
  values: readonly HistoryIncarnation[],
  head: Point,
) => {
  const scope = new Set(capture.history.ledger.addresses);
  const refs = new Set<string>();
  if (scope.size !== capture.history.ledger.addresses.length)
    fail("Repeated captured address");
  for (const output of capture.history.ledger.outputs) {
    if (!scope.has(output.address) || refs.has(key(output)))
      fail("Capture output scope or identity disagrees");
    refs.add(key(output));
  }
  validateIncarnations(capture.bindingDigest, values);
  const live = new Map<string, HistoryIncarnation>();
  for (const value of values) {
    const p = value.placement;
    if (p === null) continue;
    for (const at of [p.admission, p.current?.at, p.retirement?.at]) {
      if (
        at !== undefined &&
        (at.height > head.height ||
          at.slot > head.slot ||
          (at.height === head.height &&
            (at.blockHash !== head.id || at.slot !== head.slot)))
      )
        fail("Incarnation placement is beyond or conflicts with journal head");
    }
    if (p.current !== null) {
      const label = `${value.kind}:${key(p.current.outRef)}`;
      if (live.has(label)) fail("Repeated live incarnation location");
      live.set(label, value);
    }
  }
  for (const [kind, events] of [
    ["deposit", capture.history.deposits],
    ["withdrawal", capture.history.withdrawals],
  ] as const) {
    for (const event of events) {
      const label = `${kind}:${key(event.utxo)}`;
      const origin = live.get(label);
      if (
        origin === undefined ||
        origin.event.idCbor !== event.idCbor.toString("hex") ||
        origin.event.key !== event.assetName
      )
        fail("Live Order is missing its matching canonical origin");
      if (
        origin.event.inclusionTime !== event.facts.inclusion_time ||
        origin.event.factsCbor !==
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(event.utxo.datum!, [3, 0]),
          ) ||
        origin.event.payloadCbor !== event.history.payloadCbor ||
        origin.event.originalAssetsCbor !==
          Data.to(SDK.assetsToValue(event.originalAssets), SDK.Value)
      )
        fail("Live origin immutable facts disagree with its Order");
      live.delete(label);
    }
  }
  if (live.size !== 0)
    fail("Live incarnation is absent from the complete history capture");
};

const decodeUndo = (row: ApplicationRow, bindingDigest: string): Undo => {
  if (
    digest(row.undo_record) !== row.undo_digest.toString("hex") ||
    digest(row.ledger_receipt) !== row.ledger_receipt_digest.toString("hex")
  )
    fail("Application receipt or undo digest disagrees");
  const receipt = Schema.decodeUnknownSync(
    Schema.Struct({
      domain: Schema.Literal("midgard-history-ledger-application-v1"),
      bindingDigest: Schema.String,
      block: Schema.Struct({
        parent: Schema.String,
        point: Schema.Struct({
          id: Schema.String,
          slot: Schema.Number,
          height: Schema.Number,
        }),
      }),
      beforeSnapshotDigest: Schema.String,
      afterSnapshotDigest: Schema.String,
    }),
  )(JSON.parse(row.ledger_receipt));
  if (
    receipt.bindingDigest !== bindingDigest ||
    receipt.block.parent !== row.parent_hash.toString("hex") ||
    receipt.block.point.id !== row.block_hash.toString("hex") ||
    receipt.block.point.slot !== natural(row.block_slot) ||
    receipt.block.point.height !== natural(row.block_height) ||
    receipt.beforeSnapshotDigest !==
      row.before_snapshot_digest.toString("hex") ||
    receipt.afterSnapshotDigest !== row.after_snapshot_digest.toString("hex")
  )
    fail("Ledger receipt and application columns disagree");
  return Schema.decodeUnknownSync(undoSchema)(JSON.parse(row.undo_record), {
    onExcessProperty: "error",
  });
};

/** "chain" walks and re-verifies every retained canonical application from
 * the anchor (startup load, coverage). "head" verifies only the cursor's exact
 * head application and, when it is the first retained block, its link to the
 * anchor: under the cursor lock, the cursor's revision and digests bind the
 * chain a previous full load already verified, and every writer extends or
 * reverses it by exactly one verified link. Its cost does not grow with the
 * retained range. */
type Verification = "chain" | "head";

const loadLocked = (
  binding: EventHistorySourceBinding,
  row: CursorRow,
  verification: Verification,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bindingBytes = yield* checked(() => bytes(binding.digest));
    const outputs = yield* sql<{
      tx_hash: Buffer;
      output_index: number;
      output_record: string;
      output_digest: Buffer;
    }>`SELECT * FROM event_history_live_outputs WHERE binding_digest = ${bindingBytes} ORDER BY tx_hash, output_index`;
    const origins = yield* sql<{
      incarnation_id: Buffer;
      kind: string;
      event_id: Buffer;
      event_key: Buffer;
      origin_canonical: boolean;
      incarnation_record: string;
      incarnation_digest: Buffer;
    }>`SELECT * FROM event_history_incarnations WHERE binding_digest = ${bindingBytes} ORDER BY incarnation_id`;
    const applications =
      verification === "chain"
        ? yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bindingBytes} AND canonical ORDER BY block_height`
        : row.head_application_revision === null
          ? []
          : yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bindingBytes} AND block_hash = ${row.head_hash} AND application_revision = ${row.head_application_revision}::bigint AND canonical`;
    const decoded = yield* checked(() => {
      if (
        row.binding_digest.toString("hex") !== binding.digest ||
        row.manifest_id.toString("hex") !== binding.manifestId
      )
        fail("Stored cursor belongs to another source or manifest");
      if (
        row.origin_receipt.length === 0 ||
        digest(row.origin_receipt) !== row.origin_receipt_digest.toString("hex")
      )
        fail("Stored origin receipt digest disagrees");
      const anchor = point(row.anchor_hash, row.anchor_slot, row.anchor_height);
      const head = point(row.head_hash, row.head_slot, row.head_height);
      let previous = anchor;
      let previousRevision: string | null = null;
      let previousDigest = row.anchor_snapshot_digest.toString("hex");
      if (verification === "head" && applications.length === 1) {
        const application = applications[0]!;
        decodeUndo(application, binding.digest);
        const next = point(
          application.block_hash,
          application.block_slot,
          application.block_height,
        );
        const parentRevision = application.parent_application_revision;
        if (
          (parentRevision === null
            ? application.parent_hash.toString("hex") !== anchor.id ||
              application.before_snapshot_digest.toString("hex") !==
                previousDigest ||
              next.height !== anchor.height + 1 ||
              next.slot <= anchor.slot
            : next.height < anchor.height + 2 ||
              BigInt(application.application_revision) <=
                BigInt(parentRevision)) ||
          BigInt(application.application_revision) > BigInt(row.revision)
        )
          fail("Canonical application ancestry disagrees");
        previous = next;
        previousRevision = application.application_revision;
        previousDigest = application.after_snapshot_digest.toString("hex");
      }
      if (verification === "chain")
        for (const application of applications) {
          decodeUndo(application, binding.digest);
          const next = point(
            application.block_hash,
            application.block_slot,
            application.block_height,
          );
          if (
            application.parent_hash.toString("hex") !== previous.id ||
            application.parent_application_revision !== previousRevision ||
            application.before_snapshot_digest.toString("hex") !==
              previousDigest ||
            next.height !== previous.height + 1 ||
            next.slot <= previous.slot ||
            BigInt(application.application_revision) <=
              BigInt(previousRevision ?? "0") ||
            BigInt(application.application_revision) > BigInt(row.revision)
          )
            fail("Canonical application ancestry disagrees");
          previous = next;
          previousRevision = application.application_revision;
          previousDigest = application.after_snapshot_digest.toString("hex");
        }
      if (
        !samePoint(previous, head) ||
        previousRevision !== row.head_application_revision ||
        previousDigest !== row.snapshot_digest.toString("hex")
      )
        fail("Cursor does not identify its exact canonical head application");
      const addresses = Schema.decodeUnknownSync(
        Schema.Array(Schema.NonEmptyString),
      )(row.addresses);
      if (new Set(addresses).size !== addresses.length)
        fail("Repeated captured address");
      const ledgerOutputs = outputs.map((stored) => {
        const output = decodeJournalOutput(stored.output_record);
        if (
          digest(stored.output_record) !==
            stored.output_digest.toString("hex") ||
          output.txHash !== stored.tx_hash.toString("hex") ||
          output.outputIndex !== stored.output_index ||
          !addresses.includes(output.address)
        )
          fail("Stored output record, index or digest disagrees");
        return output;
      });
      const incarnations = Object.freeze(
        origins.map((stored) => {
          const value = decodeJournalIncarnation(stored.incarnation_record);
          if (
            historyIncarnationDigest(value) !==
              stored.incarnation_digest.toString("hex") ||
            value.id !== stored.incarnation_id.toString("hex") ||
            value.kind !== stored.kind ||
            value.event.idCbor !== stored.event_id.toString("hex") ||
            value.event.key !== stored.event_key.toString("hex") ||
            (value.placement !== null) !== stored.origin_canonical
          )
            fail("Stored incarnation record, index or digest disagrees");
          return value;
        }),
      );
      return {
        anchor,
        head,
        incarnations,
        ledger: Object.freeze({
          point: Object.freeze({ id: head.id, slot: head.slot }),
          addresses: Object.freeze([...addresses]),
          outputs: Object.freeze(ledgerOutputs),
        }),
      };
    });
    yield* requireOriginCoverage({
      binding,
      originReceipt: row.origin_receipt,
      anchor: decoded.anchor,
      anchorSnapshotDigest: row.anchor_snapshot_digest.toString("hex"),
    });
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      decoded.ledger,
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Stored history capture is invalid",
            cause,
          }),
      ),
    );
    return yield* checked(() => {
      if (capture.snapshotDigest !== row.snapshot_digest.toString("hex"))
        fail("Stored snapshot digest disagrees");
      validateLiveCoverage(capture, decoded.incarnations, decoded.head);
      return Object.freeze({
        bindingDigest: binding.digest,
        manifestId: binding.manifestId,
        originReceipt: row.origin_receipt,
        originReceiptDigest: row.origin_receipt_digest.toString("hex"),
        anchor: decoded.anchor,
        anchorSnapshotDigest: row.anchor_snapshot_digest.toString("hex"),
        head: decoded.head,
        headApplicationRevision: row.head_application_revision,
        revision: row.revision,
        capture,
        incarnations: decoded.incarnations,
      });
    });
  });

const read = (binding: EventHistorySourceBinding, verification: Verification) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bindingBytes = yield* checked(() => bytes(binding.digest));
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows =
          yield* sql<CursorRow>`SELECT * FROM event_history_cursor WHERE binding_digest = ${bindingBytes} FOR SHARE`;
        return rows[0] === undefined
          ? null
          : yield* loadLocked(binding, rows[0], verification);
      }),
    );
  }).pipe(sqlErrorToDatabaseError(table, "Failed to load history journal"));

/** Coherent local recovery material only, with the whole retained chain
 * re-verified from the anchor. The source owner must re-admit the origin
 * receipt, branch and coverage before publishing readiness. All journal
 * writers lock this same cursor after the authority lock. */
export const load = (binding: EventHistorySourceBinding) =>
  read(binding, "chain");

/** The current checkpoint for a caller that already holds a fully verified
 * one and has since advanced it only through append/undoHead: verifies the
 * cursor, its head application and the live image, not the retained chain. */
export const loadCurrent = (binding: EventHistorySourceBinding) =>
  read(binding, "head");

/** Restart intersection candidates, newest first: the head, then retained
 * canonical blocks exponentially further back (head-1, head-2, head-4, ...),
 * then the anchor. A short fork while offline rewinds about as far as the
 * fork is deep, never the whole retained range. */
export const intersections = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
) =>
  Effect.gen(function* () {
    const heights: number[] = [];
    for (
      let depth = 1;
      checkpoint.head.height - depth > checkpoint.anchor.height;
      depth *= 2
    )
      heights.push(checkpoint.head.height - depth);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      heights.length === 0
        ? []
        : yield* sql<
            Pick<ApplicationRow, "block_hash" | "block_slot" | "block_height">
          >`SELECT block_hash, block_slot, block_height FROM event_history_block_applications
          WHERE binding_digest = ${bytes(binding.digest)} AND canonical AND block_height IN ${sql.in(heights)}
          ORDER BY block_height DESC`;
    return yield* checked(() => {
      const points = rows.map((row) =>
        point(row.block_hash, row.block_slot, row.block_height),
      );
      if (
        points.length !== heights.length ||
        points.some((at, index) => at.height !== heights[index])
      )
        fail("Retained canonical intersections have a gap");
      return Object.freeze(
        samePoint(checkpoint.head, checkpoint.anchor)
          ? [checkpoint.head]
          : [checkpoint.head, ...points, checkpoint.anchor],
      );
    });
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read retained intersections"),
  );

/** Whether a rollback target is this checkpoint's anchor or one of its
 * retained canonical blocks: the only points undoHead can rewind to. */
export const retains = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  target: Readonly<{ id: string; slot: number }>,
) =>
  Effect.gen(function* () {
    if (
      target.id === checkpoint.anchor.id &&
      target.slot === checkpoint.anchor.slot
    )
      return true;
    if (
      target.slot <= checkpoint.anchor.slot ||
      target.slot > checkpoint.head.slot
    )
      return false;
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT 1 FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)}
        AND canonical AND block_hash = ${bytes(target.id)} AND block_slot = ${target.slot}`;
    return rows.length === 1;
  }).pipe(sqlErrorToDatabaseError(table, "Failed to read retained ancestry"));

const putOutput = (bindingDigest: string, output: LedgerSnapshotOutput) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const record = encodeJournalOutput(output);
    yield* sql`INSERT INTO event_history_live_outputs (binding_digest, tx_hash, output_index, output_record, output_digest)
    VALUES (${bytes(bindingDigest)}, ${bytes(output.txHash)}, ${output.outputIndex}, ${record}, ${bytes(digest(record))})
    ON CONFLICT (binding_digest, tx_hash, output_index) DO UPDATE SET output_record = EXCLUDED.output_record, output_digest = EXCLUDED.output_digest`;
  });
const putIncarnation = (value: HistoryIncarnation) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO event_history_incarnations (binding_digest, incarnation_id, kind, event_id, event_key, origin_canonical, incarnation_record, incarnation_digest)
    VALUES (${bytes(value.bindingDigest)}, ${bytes(value.id)}, ${value.kind}, ${Buffer.from(value.event.idCbor, "hex")}, ${bytes(value.event.key)}, ${value.placement !== null}, ${encodeJournalIncarnation(value)}, ${bytes(historyIncarnationDigest(value))})
    ON CONFLICT (binding_digest, incarnation_id) DO UPDATE SET origin_canonical = EXCLUDED.origin_canonical, incarnation_record = EXCLUDED.incarnation_record, incarnation_digest = EXCLUDED.incarnation_digest`;
  });
// Seed and undo belong to recovery. An append only extends the journaled
// prefix producers hold, so the owner may also run it at the head of its Ready
// generation (withReadyAppend).
const owned = (
  binding: EventHistorySourceBinding,
  transaction: "recovery" | "source",
) =>
  Effect.gen(function* () {
    const token =
      transaction === "recovery"
        ? yield* requireRecoveryTransaction
        : yield* requireSourceTransaction;
    yield* checked(() => {
      if (token.deploymentIdentity !== binding.manifestId)
        fail("Recovery owner belongs to another manifest");
    });
  });

/** Seed only from an authenticated continuous initialization replay. An empty
 * current list is not evidence of missing historical origins. originReceipt
 * retains the exact replay evidence atomically with its digest and projection.
 * Receipt integrity does not establish its source authority on restart.
 * Compose inside Authority.withRecovery with dependent SQL materialization. */
export const seed = (input: {
  binding: EventHistorySourceBinding;
  capture: BoundHistoryCapture;
  height: number;
  originReceipt: string;
  originReceiptDigest: string;
  incarnations: readonly HistoryIncarnation[];
}) =>
  Effect.gen(function* () {
    yield* owned(input.binding, "recovery");
    const sql = yield* SqlClient.SqlClient;
    const prepared = yield* checked(() => {
      const originReceipt = input.originReceipt;
      const originReceiptDigest = input.originReceiptDigest;
      bytes(originReceiptDigest);
      if (
        originReceipt.length === 0 ||
        digest(originReceipt) !== originReceiptDigest
      )
        fail("Seed origin receipt digest disagrees");
      natural(input.height);
      if (input.capture.bindingDigest !== input.binding.digest)
        fail("Seed capture binding disagrees");
      const incarnations = Object.freeze(
        input.incarnations.map((value) =>
          decodeJournalIncarnation(encodeJournalIncarnation(value)),
        ),
      );
      const head = {
        ...input.capture.history.ledger.point,
        height: input.height,
      };
      return {
        originReceipt,
        originReceiptDigest,
        incarnations,
        head,
        ledger: freeze(structuredClone(input.capture.history.ledger)),
        snapshotDigest: input.capture.snapshotDigest,
      };
    });
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      prepared.ledger,
      input.binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({ table, message: "Invalid seed capture", cause }),
      ),
    );
    yield* checked(() => {
      if (capture.snapshotDigest !== prepared.snapshotDigest)
        fail("Seed snapshot digest disagrees");
      validateLiveCoverage(capture, prepared.incarnations, prepared.head);
    });
    yield* requireOriginCoverage({
      binding: input.binding,
      originReceipt: prepared.originReceipt,
      anchor: prepared.head,
      anchorSnapshotDigest: capture.snapshotDigest,
    });
    yield* sql`INSERT INTO event_history_cursor (binding_digest, manifest_id, origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot, anchor_height, anchor_snapshot_digest, head_hash, head_slot, head_height, head_application_revision, snapshot_digest, revision, addresses)
    VALUES (${bytes(input.binding.digest)}, ${bytes(input.binding.manifestId)}, ${prepared.originReceipt}, ${bytes(prepared.originReceiptDigest)}, ${bytes(prepared.head.id)}, ${prepared.head.slot}, ${prepared.head.height}, ${bytes(capture.snapshotDigest)}, ${bytes(prepared.head.id)}, ${prepared.head.slot}, ${prepared.head.height}, NULL, ${bytes(capture.snapshotDigest)}, 0, CAST(${JSON.stringify(capture.history.ledger.addresses)} AS TEXT)::jsonb)`;
    for (const output of capture.history.ledger.outputs)
      yield* putOutput(input.binding.digest, output);
    for (const value of prepared.incarnations) yield* putIncarnation(value);
  }).pipe(sqlErrorToDatabaseError(table, "Failed to seed history journal"));

type Projection = Readonly<{
  capture: BoundHistoryCapture;
  transitions: readonly Readonly<{
    transactionIndex: number;
    transition: HistoryTransition;
  }>[];
}>;
type Prepared = Readonly<{
  expected: Checkpoint;
  block: BoundHistoryChainBlock;
  projection: Projection;
  receipt: string;
  undo: Undo;
  changes: readonly HistoryProvenanceChange[];
}>;

/** Network/reference resolution and whole-block projection precede this pure
 * staging step. A projection must come from projectEventHistoryBlock for this
 * exact checkpoint and admitted block; this storage layer is not a validator. */
export const prepareAppend = (
  expected: Checkpoint,
  block: BoundHistoryChainBlock,
  projection: Projection,
): Prepared => {
  if (
    block.parent !== expected.head.id ||
    block.point.height !== expected.head.height + 1 ||
    block.point.slot <= expected.head.slot ||
    block.point.id === expected.head.id ||
    projection.capture.bindingDigest !== expected.bindingDigest ||
    projection.capture.history.ledger.point.id !== block.point.id ||
    projection.capture.history.ledger.point.slot !== block.point.slot ||
    serialise([...expected.capture.history.ledger.addresses].sort()) !==
      serialise([...projection.capture.history.ledger.addresses].sort())
  )
    fail("Block projection does not extend its exact checkpoint");
  for (const item of projection.transitions) {
    if (
      block.transactions[item.transactionIndex]?.txHash !==
      item.transition.transactionHash
    )
      fail("Transition is not from its indexed block transaction");
  }
  const changes = stageHistoryProvenance({
    bindingDigest: expected.bindingDigest,
    block,
    transitions: projection.transitions,
    incarnations: expected.incarnations,
  });
  const after = new Map(
    expected.incarnations.map((value) => [value.id, value]),
  );
  for (const change of changes) after.set(change.after.id, change.after);
  validateLiveCoverage(projection.capture, [...after.values()], block.point);
  const beforeOutputs = new Map(
    expected.capture.history.ledger.outputs.map((value) => [
      key(value),
      encodeJournalOutput(value),
    ]),
  );
  const afterOutputs = new Map(
    projection.capture.history.ledger.outputs.map((value) => [
      key(value),
      encodeJournalOutput(value),
    ]),
  );
  const outputs = [
    ...new Set([...beforeOutputs.keys(), ...afterOutputs.keys()]),
  ]
    .sort()
    .flatMap((ref) => {
      const before = beforeOutputs.get(ref) ?? null;
      const next = afterOutputs.get(ref) ?? null;
      return before === next ? [] : [{ before, after: next }];
    });
  // The immutable receipt excludes application revision and incarnation
  // before-images: reapplying a reverted admission starts from a retained orphan.
  const receipt = serialise({
    domain: "midgard-history-ledger-application-v1",
    bindingDigest: expected.bindingDigest,
    block,
    beforeSnapshotDigest: expected.capture.snapshotDigest,
    afterSnapshotDigest: projection.capture.snapshotDigest,
    transitions: projection.transitions,
  });
  return Object.freeze({
    expected,
    block: freeze(structuredClone(block)),
    projection: Object.freeze({
      capture: Object.freeze({
        ...projection.capture,
        history: Object.freeze({
          ...projection.capture.history,
          ledger: freeze(structuredClone(projection.capture.history.ledger)),
        }),
      }),
      transitions: freeze(structuredClone(projection.transitions)),
    }),
    receipt,
    changes,
    undo: freeze({
      outputs,
      incarnations: changes.map((change) => ({
        before:
          change.before === null
            ? null
            : encodeJournalIncarnation(change.before),
        after: encodeJournalIncarnation(change.after),
      })),
    }),
  });
};

const requireExpected = (actual: Checkpoint, expected: Checkpoint) => {
  if (
    actual.bindingDigest !== expected.bindingDigest ||
    actual.manifestId !== expected.manifestId ||
    actual.originReceipt !== expected.originReceipt ||
    actual.originReceiptDigest !== expected.originReceiptDigest ||
    actual.revision !== expected.revision ||
    actual.headApplicationRevision !== expected.headApplicationRevision ||
    !samePoint(actual.head, expected.head) ||
    actual.capture.snapshotDigest !== expected.capture.snapshotDigest ||
    !samePoint(actual.anchor, expected.anchor) ||
    actual.anchorSnapshotDigest !== expected.anchorSnapshotDigest
  )
    fail("History cursor revision or head changed");
};
const lockCheckpoint = (
  binding: EventHistorySourceBinding,
  transaction: "recovery" | "source",
) =>
  Effect.gen(function* () {
    yield* owned(binding, transaction);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<CursorRow>`SELECT * FROM event_history_cursor WHERE binding_digest = ${bytes(binding.digest)} FOR UPDATE`;
    if (rows[0] === undefined)
      return yield* checked(() => fail("History journal has no replay anchor"));
    return yield* loadLocked(binding, rows[0], "head");
  });

/** Rows each retention step may delete per table. Forward progress adds one
 * block per step, so any backlog (a first move off an old seed, a restart far
 * behind the tip) drains at this rate without an unbounded transaction. */
export const RETENTION_BATCH = 128;

export type Retention = Readonly<{
  /** Height of the follower's current source tip (at least the new block's). */
  tipHeight: number;
  /** k: the manifest's deepest automatic rollback. Blocks deeper than this
   * behind the tip are never rolled back automatically. */
  horizon: number;
  /** Earliest signed validity-start slot that signed-header recovery may still
   * need to evaluate; the first retained block must not be later than it. */
  holdSlot: number | undefined;
}>;

/** Set when holdSlot, not the horizon or batch size, stopped the anchor: the
 * anchor this step left, and the one the horizon alone would allow. */
export type RetentionHold = Readonly<{
  holdSlot: number;
  anchorHeight: number;
  unheldAnchorHeight: number;
}>;

/** Advance the anchor to the deepest journaled block that is past the rollback
 * horizon (height <= tipHeight - horizon), strictly behind head (so the retained
 * canonical range is never empty) and never so far that the first retained
 * block starts after holdSlot, by at most RETENTION_BATCH blocks. The anchor
 * moves with its snapshot digest in this transaction, and the applications at
 * or behind it (plus orphan branches rooted there) are deleted. Once the anchor
 * has left the seed point, replay receipts are deleted in bounded batches too.
 * The origin receipt is never changed. */
const retain = (
  binding: EventHistorySourceBinding,
  current: Readonly<{ anchor: Point; head: Point; originReceipt: string }>,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const key = bytes(binding.digest);
    const limit = yield* checked(() => {
      natural(retention.tipHeight);
      natural(retention.horizon);
      if (retention.holdSlot !== undefined) natural(retention.holdSlot);
      return Math.min(
        retention.tipHeight - retention.horizon,
        current.head.height - 1,
        current.anchor.height + RETENTION_BATCH,
      );
    });
    let target = limit;
    let holding = false;
    if (retention.holdSlot !== undefined && target > current.anchor.height) {
      // The deepest block whose successor still starts at or before holdSlot.
      const [held] = yield* sql<{
        height: string | null;
      }>`SELECT max(block_height)::text AS height FROM event_history_block_applications
        WHERE binding_digest = ${key} AND canonical AND block_slot <= ${retention.holdSlot}`;
      const cap =
        held?.height === null || held?.height === undefined
          ? current.anchor.height
          : natural(held.height) - 1;
      if (cap < target) {
        target = Math.max(cap, current.anchor.height);
        holding = true;
      }
    }
    let anchor = current.anchor;
    if (target > current.anchor.height) {
      const rows =
        yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications
        WHERE binding_digest = ${key} AND canonical AND block_height IN (${target}, ${target + 1})
        ORDER BY block_height`;
      const [next, child] = yield* checked(() => {
        const next = rows[0];
        if (next === undefined || natural(next.block_height) !== target)
          fail("Retention target is not a canonical journal application");
        const child = rows[1];
        if (child === undefined)
          fail("Retention target has no canonical successor");
        return [next, child] as const;
      });
      // The first retained application now descends from the anchor itself.
      yield* sql`UPDATE event_history_block_applications SET parent_application_revision = NULL
          WHERE binding_digest = ${key} AND block_hash = ${child.block_hash}
            AND application_revision = ${child.application_revision}::bigint`;
      // One statement per batch: every deleted row's descendants are deleted
      // with it, so no RESTRICT parent link is left dangling.
      const deleted = yield* sql<{
        canonical: boolean;
      }>`WITH RECURSIVE doomed AS (
          SELECT block_hash, application_revision FROM event_history_block_applications
            WHERE binding_digest = ${key} AND block_height <= ${target}
          UNION
          SELECT c.block_hash, c.application_revision FROM event_history_block_applications c
            JOIN doomed d ON c.parent_hash = d.block_hash AND c.parent_application_revision = d.application_revision
            WHERE c.binding_digest = ${key}
        )
        DELETE FROM event_history_block_applications a USING doomed d
        WHERE a.binding_digest = ${key} AND a.block_hash = d.block_hash
          AND a.application_revision = d.application_revision
        RETURNING a.canonical`;
      yield* checked(() => {
        if (
          deleted.filter((row) => row.canonical).length !==
          target - current.anchor.height
        )
          fail("Retention deleted a different canonical range");
      });
      anchor = point(next.block_hash, next.block_slot, next.block_height);
      yield* sql`UPDATE event_history_cursor SET anchor_hash = ${next.block_hash},
        anchor_slot = ${anchor.slot}, anchor_height = ${anchor.height},
        anchor_snapshot_digest = ${next.after_snapshot_digest}
        WHERE binding_digest = ${key}`;
    }
    const seed = yield* checked(() => originReplayHead(current.originReceipt));
    if (seed === undefined || !samePoint(seed, anchor))
      yield* pruneReplayReceipts(binding.digest, RETENTION_BATCH);
    return holding
      ? Object.freeze({
          holdSlot: retention.holdSlot!,
          anchorHeight: anchor.height,
          unheldAnchorHeight: Math.min(
            retention.tipHeight - retention.horizon,
            current.head.height - 1,
          ),
        })
      : undefined;
  });

/** Recovery or Ready-head append. All changes and the callback share the already-owned
 * outer transaction. Duplicate current-head delivery skips materialization;
 * a new application after rollback receives a fresh monotone revision.
 * Bounded retention (see retain) runs in the same transaction, before the
 * callback observes the new checkpoint. */
export const append = <A, E, R>(
  binding: EventHistorySourceBinding,
  prepared: Prepared,
  materialize: Effect.Effect<A, E, R>,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const actual = yield* lockCheckpoint(binding, "source");
    const sql = yield* SqlClient.SqlClient;
    const historical =
      yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(prepared.block.point.id)} ORDER BY application_revision`;
    yield* checked(() => {
      for (const application of historical) {
        decodeUndo(application, binding.digest);
        if (application.ledger_receipt !== prepared.receipt)
          fail("Same block has a conflicting immutable ledger receipt");
      }
    });
    if (
      samePoint(actual.head, prepared.block.point) &&
      historical.some(
        (application) =>
          application.canonical &&
          application.application_revision === actual.headApplicationRevision,
      )
    )
      return { applied: false as const, revision: actual.revision };
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      prepared.projection.capture.history.ledger,
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Invalid appended capture",
            cause,
          }),
      ),
    );
    const staged = yield* checked(() => {
      requireExpected(actual, prepared.expected);
      const fresh = prepareAppend(actual, prepared.block, {
        capture,
        transitions: prepared.projection.transitions,
      });
      if (
        fresh.receipt !== prepared.receipt ||
        serialise(fresh.undo) !== serialise(prepared.undo)
      )
        fail("Prepared block images changed");
      return {
        revision: (BigInt(actual.revision) + 1n).toString(),
        undoRecord: JSON.stringify(fresh.undo),
        fresh,
      };
    });
    yield* checked(() => {
      if (capture.snapshotDigest !== prepared.projection.capture.snapshotDigest)
        fail("Appended capture digest disagrees");
      const after = new Map(
        actual.incarnations.map((value) => [value.id, value]),
      );
      for (const change of staged.fresh.changes)
        after.set(change.after.id, change.after);
      validateLiveCoverage(capture, [...after.values()], prepared.block.point);
    });
    yield* sql`INSERT INTO event_history_block_applications (binding_digest, block_hash, application_revision, parent_hash, parent_application_revision, block_slot, block_height, before_snapshot_digest, after_snapshot_digest, ledger_receipt, ledger_receipt_digest, undo_record, undo_digest, canonical)
    VALUES (${bytes(binding.digest)}, ${bytes(prepared.block.point.id)}, ${staged.revision}::bigint, ${bytes(actual.head.id)}, ${actual.headApplicationRevision}::bigint, ${prepared.block.point.slot}, ${prepared.block.point.height}, ${bytes(actual.capture.snapshotDigest)}, ${bytes(capture.snapshotDigest)}, ${prepared.receipt}, ${bytes(digest(prepared.receipt))}, ${staged.undoRecord}, ${bytes(digest(staged.undoRecord))}, true)`;
    for (const change of staged.fresh.undo.outputs) {
      if (change.after !== null)
        yield* putOutput(binding.digest, decodeJournalOutput(change.after));
      else if (change.before !== null) {
        const previous = decodeJournalOutput(change.before);
        yield* sql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${bytes(binding.digest)} AND tx_hash = ${bytes(previous.txHash)} AND output_index = ${previous.outputIndex}`;
      }
    }
    for (const change of staged.fresh.changes)
      yield* putIncarnation(change.after);
    yield* sql`UPDATE event_history_cursor SET head_hash = ${bytes(prepared.block.point.id)}, head_slot = ${prepared.block.point.slot}, head_height = ${prepared.block.point.height}, head_application_revision = ${staged.revision}::bigint, snapshot_digest = ${bytes(capture.snapshotDigest)}, revision = ${staged.revision}::bigint WHERE binding_digest = ${bytes(binding.digest)}`;
    const hold = yield* retain(
      binding,
      {
        anchor: actual.anchor,
        head: prepared.block.point,
        originReceipt: actual.originReceipt,
      },
      retention,
    );
    const result = yield* materialize;
    return { applied: true as const, revision: staged.revision, result, hold };
  }).pipe(sqlErrorToDatabaseError(table, "Failed to append history journal"));

/** Reverse exactly one current head while producers remain fenced. Retains
 * orphan admissions, never restores D/W L2 classifications from L1 images.
 * The callback must repair affected L2 descendants in this same transaction. */
export const undoHead = <A, E, R>(
  binding: EventHistorySourceBinding,
  expected: Checkpoint,
  repair: Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const actual = yield* lockCheckpoint(binding, "recovery");
    yield* checked(() => {
      requireExpected(actual, expected);
      if (actual.headApplicationRevision === null)
        fail("Rollback exceeds retained replay anchor");
    });
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(actual.head.id)} AND application_revision = ${actual.headApplicationRevision}::bigint AND canonical`;
    const restored = yield* checked(() => {
      const application = rows[0];
      if (application === undefined)
        return fail("Missing canonical head application");
      const undo = decodeUndo(application, binding.digest);
      const current = new Map(
        actual.capture.history.ledger.outputs.map((value) => [
          key(value),
          value,
        ]),
      );
      const seen = new Set<string>();
      for (const change of undo.outputs) {
        const before =
          change.before === null ? null : decodeJournalOutput(change.before);
        const after =
          change.after === null ? null : decodeJournalOutput(change.after);
        const ref = before ?? after;
        if (
          ref === null ||
          (before !== null && after !== null && key(before) !== key(after)) ||
          seen.has(key(ref))
        )
          fail("Malformed output undo image");
        const label = key(ref!);
        seen.add(label);
        const output = current.get(label);
        if (
          (output === undefined ? null : encodeJournalOutput(output)) !==
          change.after
        )
          fail("Rollback output poststate disagrees");
        if (before === null) current.delete(label);
        else current.set(label, before);
      }
      const changes = undo.incarnations.map((change) => ({
        before:
          change.before === null
            ? null
            : decodeJournalIncarnation(change.before),
        after: decodeJournalIncarnation(change.after),
      }));
      const replacements = reverseHistoryProvenance(
        changes,
        actual.incarnations,
      );
      return {
        application,
        undo,
        replacements,
        outputs: Object.freeze([...current.values()]),
      };
    });
    let parent = actual.anchor;
    if (restored.application.parent_application_revision !== null) {
      const parents =
        yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${restored.application.parent_hash} AND application_revision = ${restored.application.parent_application_revision}::bigint AND canonical`;
      parent = yield* checked(() => {
        const row = parents[0];
        if (row === undefined)
          return fail("Missing canonical parent application");
        const at = point(row.block_hash, row.block_slot, row.block_height);
        // The one link this step exposes: the new head must be the exact
        // image the undone head was applied to.
        if (
          at.height !== actual.head.height - 1 ||
          at.slot >= actual.head.slot ||
          row.after_snapshot_digest.toString("hex") !==
            restored.application.before_snapshot_digest.toString("hex")
        )
          fail("Canonical application ancestry disagrees");
        return at;
      });
    }
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      Object.freeze({
        point: Object.freeze({ id: parent.id, slot: parent.slot }),
        addresses: actual.capture.history.ledger.addresses,
        outputs: restored.outputs,
      }),
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Invalid restored history capture",
            cause,
          }),
      ),
    );
    yield* checked(() => {
      if (
        capture.snapshotDigest !==
        restored.application.before_snapshot_digest.toString("hex")
      )
        fail("Restored snapshot digest disagrees");
      const incarnations = new Map(
        actual.incarnations.map((value) => [value.id, value]),
      );
      for (const value of restored.replacements)
        incarnations.set(value.id, value);
      validateLiveCoverage(capture, [...incarnations.values()], parent);
    });
    for (const change of restored.undo.outputs) {
      if (change.before !== null)
        yield* putOutput(binding.digest, decodeJournalOutput(change.before));
      else if (change.after !== null) {
        const output = decodeJournalOutput(change.after);
        yield* sql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${bytes(binding.digest)} AND tx_hash = ${bytes(output.txHash)} AND output_index = ${output.outputIndex}`;
      }
    }
    for (const value of restored.replacements) yield* putIncarnation(value);
    yield* sql`UPDATE event_history_block_applications SET canonical = false WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(actual.head.id)} AND application_revision = ${actual.headApplicationRevision}::bigint`;
    const revision = (BigInt(actual.revision) + 1n).toString();
    yield* sql`UPDATE event_history_cursor SET head_hash = ${bytes(parent.id)}, head_slot = ${parent.slot}, head_height = ${parent.height}, head_application_revision = ${restored.application.parent_application_revision}::bigint, snapshot_digest = ${bytes(capture.snapshotDigest)}, revision = ${revision}::bigint WHERE binding_digest = ${bytes(binding.digest)}`;
    const result = yield* repair;
    return { revision, result };
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to undo history journal head"),
  );
