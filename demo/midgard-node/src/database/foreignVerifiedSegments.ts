import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Schema } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { VerifiedForeignCommitBase } from "../workers/commit-block-header.verify-foreign-base.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import { DatabaseError } from "./utils/common.js";

export type RetainedForeignSegment = Readonly<{
  kind: "local" | "foreign";
  headerCbor: string;
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  root: string;
  events: readonly (readonly { key: string; output: string | null }[])[];
  eventRoots: readonly string[];
  ledgerKeys: readonly string[];
  ledgerBefore: readonly { outref: string; output: string }[];
}>;
export type SegmentRow = Readonly<{
  header_hash: Buffer;
  parent_header_hash: Buffer;
  parent_root: string;
  utxos_root: string;
  manifest_id: Buffer;
  segment_record: string;
  segment_digest: Buffer;
  canonical: boolean;
  confirmed: boolean;
  source_sealed: boolean;
}>;
const retainedSegmentSchema = Schema.Struct({
  kind: Schema.Literal("local", "foreign"),
  headerCbor: Schema.String,
  headerHash: Schema.String,
  parentHeaderHash: Schema.String,
  parentUtxosRoot: Schema.String,
  root: Schema.String,
  events: Schema.Array(
    Schema.Array(
      Schema.Struct({
        key: Schema.String,
        output: Schema.NullOr(Schema.String),
      }),
    ),
  ),
  eventRoots: Schema.Array(Schema.String),
  ledgerKeys: Schema.Array(Schema.String),
  ledgerBefore: Schema.Array(
    Schema.Struct({ outref: Schema.String, output: Schema.String }),
  ),
});
const fail = (message: string) =>
  Effect.fail(
    new DatabaseError({
      table: "foreign_verified_segments",
      message,
      cause: undefined,
    }),
  );
const sha = (value: string) => createHash("sha256").update(value).digest();

export const retainVerifiedForeignSegments = (
  base: VerifiedForeignCommitBase,
) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (
      base.authority !== "recovery" ||
      token.deploymentIdentity !== base.history.token.deploymentIdentity ||
      token.ownerToken !== base.history.token.ownerToken ||
      token.generation !== base.history.token.generation
    )
      return yield* fail(
        "Verified ancestry requires the current recovery owner",
      );
    const sql = yield* SqlClient.SqlClient;
    const coverage = base.history.coverage;
    for (const segment of base.importedBlocks) {
      const header = yield* Effect.try(() =>
        Data.from(segment.headerCbor, SDK.Header),
      );
      if (
        (yield* SDK.hashBlockHeader(header)) !== segment.headerHash ||
        header.prevHeaderHash !== segment.parentHeaderHash ||
        header.prevUtxosRoot !== segment.parentUtxosRoot ||
        header.utxosRoot !== segment.root
      )
        return yield* fail(
          "Verified ancestry header identity differs from its exact segment",
        );
      const record = eventHistoryCanonicalJson({
        kind: segment.kind,
        headerCbor: segment.headerCbor,
        headerHash: segment.headerHash,
        parentHeaderHash: segment.parentHeaderHash,
        parentUtxosRoot: segment.parentUtxosRoot,
        root: segment.root,
        events: segment.events.map((event) =>
          event.map((mutation) => ({
            key: mutation.key,
            output: mutation.output?.toString("hex") ?? null,
          })),
        ),
        eventRoots: segment.eventRoots,
        ledgerKeys: segment.ledgerKeys.map((key) => key.toString("hex")),
        ledgerBefore: segment.ledgerBefore.map((entry) => ({
          outref: entry.outref.toString("hex"),
          output: entry.output.toString("hex"),
        })),
      });
      const rows = yield* sql`INSERT INTO foreign_verified_segments
      (binding_digest,manifest_id,header_hash,parent_header_hash,parent_root,utxos_root,
        source_hash,source_slot,source_snapshot,segment_record,segment_digest)
      VALUES (${Buffer.from(coverage.bindingDigest, "hex")},${Buffer.from(token.deploymentIdentity, "hex")},
        ${Buffer.from(segment.headerHash, "hex")},${Buffer.from(segment.parentHeaderHash, "hex")},${segment.parentUtxosRoot},${segment.root},
        ${Buffer.from(coverage.point.id, "hex")},${coverage.point.slot},${Buffer.from(coverage.snapshotDigest, "hex")},${record},${sha(record)})
      ON CONFLICT (binding_digest,header_hash) DO UPDATE SET source_hash=EXCLUDED.source_hash,
        source_slot=EXCLUDED.source_slot,source_snapshot=EXCLUDED.source_snapshot
      WHERE foreign_verified_segments.manifest_id = EXCLUDED.manifest_id
        AND foreign_verified_segments.segment_digest = EXCLUDED.segment_digest RETURNING header_hash`;
      if (rows.length !== 1)
        return yield* fail(
          "Retained verified ancestry changed its immutable segment",
        );
    }
    const first = base.importedBlocks[0];
    if (first !== undefined)
      yield* sql`INSERT INTO foreign_confirmed_frontier(binding_digest,manifest_id,header_hash,utxos_root)
    VALUES (${Buffer.from(coverage.bindingDigest, "hex")},${Buffer.from(token.deploymentIdentity, "hex")},
      ${Buffer.from(first.parentHeaderHash, "hex")},${first.parentUtxosRoot}) ON CONFLICT (binding_digest) DO NOTHING`;
  });

export const retrieveVerifiedForeignSegments = (bindingDigest: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<SegmentRow>`SELECT s.*,
    (EXISTS (SELECT 1 FROM event_history_cursor c WHERE c.binding_digest=s.binding_digest
      AND ((c.head_hash=s.source_hash AND c.snapshot_digest=s.source_snapshot)
        OR (c.anchor_hash=s.source_hash AND c.anchor_snapshot_digest=s.source_snapshot)))
    OR EXISTS (SELECT 1 FROM event_history_block_applications a WHERE a.binding_digest=s.binding_digest
      AND a.block_hash=s.source_hash AND a.after_snapshot_digest=s.source_snapshot AND a.canonical)) AS canonical
    FROM foreign_verified_segments s WHERE s.binding_digest=${Buffer.from(bindingDigest, "hex")}`;
  });

export const decodeRetainedForeignSegment = (
  row: SegmentRow,
): RetainedForeignSegment => {
  if (!sha(row.segment_record).equals(row.segment_digest))
    throw new Error("Retained verified segment digest differs");
  // The immutable document is produced only by the checked current source
  // verifier above. Validate again against header hashes and computed roots
  // when using it; JSON parsing alone grants no authority.
  return Schema.decodeUnknownSync(retainedSegmentSchema)(
    JSON.parse(row.segment_record),
  );
};
