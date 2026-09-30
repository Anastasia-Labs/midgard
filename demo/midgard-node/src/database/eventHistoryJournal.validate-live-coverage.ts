import { createHash } from "node:crypto";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Schema } from "effect";

import type { BoundHistoryCapture } from "../l1-event-history-projection.js";
import {
  type HistoryIncarnation,
  historyIncarnationId,
} from "../l1-event-history-provenance.js";
import { type BoundHistoryChainBlock } from "../l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";
import { DatabaseError } from "./utils/common.js";

export const table = "event_history_cursor";

export type Point = BoundHistoryChainBlock["point"];

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

export type CursorRow = {
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

export type ApplicationRow = {
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

export type Undo = typeof undoSchema.Type;

export const digest = (text: string) =>
  createHash("sha256").update(text).digest("hex");

export function fail(message: string): never {
  throw new Error(message);
}

export const checked = <A>(f: () => A) =>
  Effect.try({
    try: f,
    catch: (cause) =>
      new DatabaseError({
        table,
        message: `Invalid history journal: ${cause instanceof Error ? cause.message : String(cause)}`,
        cause,
      }),
  });

export const bytes = (value: string) =>
  /^[0-9a-f]{64}$/u.test(value)
    ? Buffer.from(value, "hex")
    : fail("Expected a complete lowercase digest");

export const natural = (value: string | number) => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0)
    fail("Unsafe history coordinate");
  return number;
};

export const key = (
  value: Pick<LedgerSnapshotOutput, "txHash" | "outputIndex">,
) => `${value.txHash}#${value.outputIndex}`;

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

export const serialise = (value: unknown) => JSON.stringify(stable(value));

export const freeze = <T>(value: T): T => {
  if (typeof value === "object" && value !== null) {
    for (const child of Object.values(value)) freeze(child);
    Object.freeze(value);
  }
  return value;
};

// Checkpoints this module loaded or built under the cursor lock, and the
// preparations staged from them. A Ready append trusts no other image.
export const issued = new WeakSet<Checkpoint>();

export const preparations = new WeakSet<object>();

export const issue = (value: Checkpoint): Checkpoint => {
  const frozen = Object.freeze(value);
  issued.add(frozen);
  return frozen;
};

export const samePoint = (a: Point, b: Point) =>
  a.id === b.id && a.slot === b.slot && a.height === b.height;

export const point = (id: Buffer, slot: string, height: string): Point =>
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
export const validateLiveCoverage = (
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

export const decodeUndo = (
  row: ApplicationRow,
  bindingDigest: string,
): Undo => {
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
export type Verification = "chain" | "head";
