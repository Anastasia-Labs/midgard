import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Schema } from "effect";
import JSONBig from "json-bigint";

import type { EventHistorySourceBinding } from "../l1-event-history-source.js";
import type { SignedIntentCoverageBlock } from "../services/signed-intent-canonical-coverage.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import * as Journal from "./eventHistoryJournal.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_cursor";
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const hash = Schema.String.pipe(Schema.pattern(/^[0-9a-f]{64}$/u));
const natural = Schema.Number.pipe(
  Schema.int(),
  Schema.nonNegative(),
  Schema.lessThanOrEqualTo(Number.MAX_SAFE_INTEGER),
);
const point = Schema.Struct({ id: hash, slot: natural, height: natural });
const blockSchema = Schema.Struct({
  point,
  parent: hash,
  transactions: Schema.Array(
    Schema.Struct({
      txHash: hash,
      spends: Schema.Literal("inputs", "collaterals"),
    }),
  ),
});
const receiptSchema = Schema.Struct({
  domain: Schema.Literal(
    "midgard-node-authenticated-history-block-v1",
    "midgard-history-ledger-application-v1",
  ),
  bindingDigest: hash,
  block: blockSchema,
});
const originSchema = Schema.Struct({
  domain: Schema.Literal("midgard-node-authenticated-history-origin-v1"),
  bindingDigest: hash,
  manifestId: hash,
  activation: Schema.Struct({
    point,
    parent: hash,
    transactionIndex: natural,
    transactionHash: hash,
    receipt: Schema.String,
  }),
  replay: Schema.Struct({ head: point, blocks: natural, digest: hash }),
});
type ReceiptRow = {
  block_hash: Buffer;
  block_slot: string;
  block_height: string;
  parent_hash: Buffer;
  receipt: string;
  receipt_digest: Buffer;
};
const sha = (text: string) => createHash("sha256").update(text).digest("hex");
const samePoint = (
  a: SignedIntentCoverageBlock["point"],
  b: SignedIntentCoverageBlock["point"],
) => a.id === b.id && a.slot === b.slot && a.height === b.height;
const checked = <A>(work: () => A) =>
  Effect.try({
    try: work,
    catch: (cause) =>
      new DatabaseError({
        table,
        message: "Canonical transaction coverage is incomplete or changed",
        cause,
      }),
  });

/** Read retained complete rosters only within a freshly source-admitted recovery
 * generation. The caller must check its preparation's assertCurrent before and
 * after this transaction and bind any resulting disposition to this exact tuple.
 * Retained bytes are recovery evidence, never independent L1 authority.
 * This does not authorize abandonment or claim the activation precedes every
 * transaction's possible inclusion; the journal transition must establish that
 * dependency for the particular signed commitment it intends to dispose of.
 */
export const loadCanonicalHistoryCoverage = (
  binding: EventHistorySourceBinding,
  expected: Journal.Checkpoint,
) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    const current = yield* Journal.load(binding);
    const checkpoint = yield* checked(() => {
      if (
        current === null ||
        token.deploymentIdentity !== binding.manifestId ||
        expected.bindingDigest !== binding.digest ||
        expected.manifestId !== binding.manifestId ||
        current.revision !== expected.revision ||
        !samePoint(current.head, expected.head) ||
        current.capture.snapshotDigest !== expected.capture.snapshotDigest
      )
        throw new Error("Recovery generation or canonical checkpoint changed");
      return current;
    });
    const origin = yield* checked(() => {
      if (sha(checkpoint.originReceipt) !== checkpoint.originReceiptDigest)
        throw new Error("Origin receipt digest changed");
      const decoded = Schema.decodeUnknownSync(originSchema)(
        lossless.parse(checkpoint.originReceipt),
      );
      if (
        decoded.bindingDigest !== binding.digest ||
        decoded.manifestId !== binding.manifestId ||
        !samePoint(decoded.replay.head, checkpoint.anchor) ||
        decoded.replay.blocks !==
          checkpoint.anchor.height - decoded.activation.point.height + 1
      )
        throw new Error("Origin does not bind the complete activation range");
      return decoded;
    });
    const sql = yield* SqlClient.SqlClient;
    const bindingBytes = Buffer.from(binding.digest, "hex");
    // Follow the exact anchor's retained predecessors, not every archived fork
    // sharing an activation. Counts must decrease on each recursive step.
    const replay = yield* sql<ReceiptRow>`WITH RECURSIVE selected AS (
      SELECT * FROM event_history_replay_receipts
        WHERE binding_digest = ${bindingBytes}
          AND block_hash = ${Buffer.from(checkpoint.anchor.id, "hex")}
      UNION ALL
      SELECT parent.* FROM event_history_replay_receipts parent JOIN selected child
        ON parent.binding_digest = child.binding_digest
          AND parent.block_hash = child.predecessor_hash
          AND parent.blocks_replayed = child.blocks_replayed - 1
    ) SELECT block_hash, block_slot::text, block_height::text, parent_hash,
        receipt, receipt_digest FROM selected
      WHERE manifest_id = ${Buffer.from(binding.manifestId, "hex")}
        AND activation_hash = ${Buffer.from(origin.activation.point.id, "hex")}
      ORDER BY selected.block_height`;
    const applications =
      yield* sql<ReceiptRow>`SELECT block_hash, block_slot::text,
      block_height::text, parent_hash, ledger_receipt AS receipt,
      ledger_receipt_digest AS receipt_digest FROM event_history_block_applications
      WHERE binding_digest = ${bindingBytes} AND canonical ORDER BY event_history_block_applications.block_height`;
    const blocks = yield* checked(() => {
      if (
        replay.length !== origin.replay.blocks ||
        applications.length !==
          checkpoint.head.height - checkpoint.anchor.height
      )
        throw new Error("Retained canonical block range has a gap");
      const rows = [...replay, ...applications];
      const decoded = rows.map((row, index) => {
        if (sha(row.receipt) !== row.receipt_digest.toString("hex"))
          throw new Error("Retained complete block receipt digest changed");
        const receipt = Schema.decodeUnknownSync(receiptSchema)(
          lossless.parse(row.receipt),
        );
        const block = receipt.block;
        if (
          receipt.bindingDigest !== binding.digest ||
          receipt.domain !==
            (index < replay.length
              ? "midgard-node-authenticated-history-block-v1"
              : "midgard-history-ledger-application-v1") ||
          block.point.id !== row.block_hash.toString("hex") ||
          String(block.point.slot) !== row.block_slot ||
          String(block.point.height) !== row.block_height ||
          block.parent !== row.parent_hash.toString("hex") ||
          new Set(block.transactions.map((tx) => tx.txHash)).size !==
            block.transactions.length
        )
          throw new Error("Receipt roster, domain or indexed point changed");
        return block;
      });
      const first = decoded[0];
      const last = decoded.at(-1);
      if (
        first === undefined ||
        last === undefined ||
        !samePoint(first.point, origin.activation.point) ||
        first.parent !== origin.activation.parent ||
        first.transactions[origin.activation.transactionIndex]?.txHash !==
          origin.activation.transactionHash ||
        replay[0]!.receipt !== origin.activation.receipt ||
        !samePoint(last.point, checkpoint.head)
      )
        throw new Error("Canonical coverage endpoints or activation changed");
      for (let index = 1; index < decoded.length; index += 1) {
        const before = decoded[index - 1]!;
        const after = decoded[index]!;
        if (
          after.parent !== before.point.id ||
          after.point.height !== before.point.height + 1 ||
          after.point.slot <= before.point.slot
        )
          throw new Error(
            "Canonical transaction coverage mixes branches or gaps",
          );
      }
      return decoded;
    });
    return {
      bindingDigest: binding.digest,
      manifestId: binding.manifestId,
      ownerGeneration: token.generation,
      checkpointRevision: checkpoint.revision,
      snapshotDigest: checkpoint.capture.snapshotDigest,
      start: blocks[0]!.point,
      head: checkpoint.head,
      activationTransactionHash: origin.activation.transactionHash,
      blocks,
    };
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to load canonical block coverage"),
  );
