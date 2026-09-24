import { encodeMidgardAddressText } from "@al-ft/midgard-core/codec";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  ledgerOutputToCbor,
  midgardOutRefToCbor,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import { requireCandidateHistory } from "../services/event-history-producer.js";
import type { ProcessedTx } from "../utils.js";
import { compactLedgerEffects } from "./mempool.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "event_history_l2_ledger_receipts";
const fail = (message: string) =>
  Effect.fail(
    new DatabaseError({ table: tableName, message, cause: undefined }),
  );
const byteaArray = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

type LedgerRow = {
  tx_id: Buffer;
  outref: Buffer;
  output: Buffer;
  address: string;
  source_event_id: Buffer | null;
};

export type AcceptedLedgerReceipt = Readonly<{
  sequence: string;
  produced: ReturnType<typeof compactLedgerEffects>["produced"];
  spent: readonly Buffer[];
}>;

/** Called before either acceptance SQL path mutates the ledger. The outer
 * Ready authority lock serializes successful persistence, so sequence order is
 * persistence order, including concurrent validation batches. These logged
 * receipts are inverse evidence, never authority to undo a published header.
 */
export const beginAcceptedLedgerReceipt = (
  processedTxs: readonly ProcessedTx[],
) =>
  Effect.gen(function* () {
    const permit = yield* requireCandidateHistory;
    // Only the explicit isolated unowned fixture capability can reach None.
    // Such fixtures do not manufacture production recovery evidence.
    if (Option.isNone(permit) || processedTxs.length === 0) return undefined;
    const { token, coverage } = permit.value;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const { produced, spent } = compactLedgerEffects(processedTxs);
    const allProduced = new Set(
      processedTxs.flatMap((tx) =>
        tx.produced.map((row) => row.outref.toString("hex")),
      ),
    );
    const references = yield* Effect.try({
      try: () => {
        const unique = new Map<string, Buffer>();
        for (const tx of processedTxs) {
          const decoded = decodeMidgardSubmittedTxFromCanonicalCbor(tx.txCbor);
          if (!Buffer.from(decoded.ledgerTx.txId).equals(tx.txId))
            throw new Error("Accepted receipt transaction identity differs");
          const canonicalSpent =
            decoded.ledgerTx.spendInputs.map(midgardOutRefToCbor);
          if (
            canonicalSpent.length !== tx.spent.length ||
            canonicalSpent.some(
              (ref, index) => !Buffer.from(ref).equals(tx.spent[index]!),
            )
          )
            throw new Error(
              "Accepted receipt spend inputs differ from canonical body",
            );
          if (
            decoded.ledgerTx.outputs.length !== tx.produced.length ||
            decoded.ledgerTx.outputs.some((output, index) => {
              const row = tx.produced[index]!;
              return (
                !row.tx_id.equals(tx.txId) ||
                !row.outref.equals(
                  Buffer.from(
                    midgardOutRefToCbor({
                      txId: tx.txId,
                      index: BigInt(index),
                    }),
                  ),
                ) ||
                !row.output.equals(Buffer.from(ledgerOutputToCbor(output))) ||
                row.address !== encodeMidgardAddressText(output.address)
              );
            })
          )
            throw new Error(
              "Accepted receipt outputs differ from canonical body",
            );
          for (const ref of decoded.ledgerTx.referenceInputs) {
            const bytes = Buffer.from(midgardOutRefToCbor(ref));
            unique.set(bytes.toString("hex"), bytes);
          }
        }
        return [...unique.values()];
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Invalid accepted receipt body",
          cause,
        }),
    });
    const externalReferences = references.filter(
      (ref) => !allProduced.has(ref.toString("hex")),
    );
    const needed = [
      ...new Map(
        [...spent, ...externalReferences].map((ref) => [
          ref.toString("hex"),
          ref,
        ]),
      ).values(),
    ];
    const before = yield* sql<LedgerRow>`SELECT * FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(needed))}::bytea[]) ORDER BY outref FOR UPDATE`;
    if (before.length !== needed.length)
      return yield* fail(
        "Cannot retain complete consumed/reference ledger before-images",
      );
    const collisions = yield* sql<{
      outref: Buffer;
    }>`SELECT outref FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(produced.map((row) => row.outref)))}::bytea[]) LIMIT 1`;
    if (collisions.length !== 0)
      return yield* fail(
        "Accepted output already exists before ledger mutation",
      );
    const sourceIds = before.flatMap((row) =>
      row.source_event_id === null ? [] : [row.source_event_id],
    );
    const invalidOrigins = yield* sql<{
      event_id: Buffer;
    }>`SELECT d.event_id FROM deposits_utxos d
      LEFT JOIN event_history_incarnations i ON i.binding_digest = d.history_binding_digest
        AND i.incarnation_id = d.history_incarnation_id
      WHERE d.event_id = ANY(${pg.array(byteaArray(sourceIds))}::bytea[])
        AND (d.history_binding_digest IS DISTINCT FROM ${Buffer.from(coverage.bindingDigest, "hex")}
          OR i.origin_canonical IS DISTINCT FROM true OR i.kind IS DISTINCT FROM 'deposit'
          OR i.event_id IS DISTINCT FROM d.event_id) LIMIT 1`;
    if (invalidOrigins.length !== 0)
      return yield* fail(
        "Accepted ledger receipt depends on a noncanonical deposit incarnation",
      );
    const txIds = processedTxs.map((tx) => tx.txId);
    const payloads = yield* sql<{
      tx_id: Buffer;
      tx_canonical_cbor: Buffer;
    }>`SELECT tx_id, tx_canonical_cbor
      FROM tx_admission_payloads WHERE tx_id = ANY(${pg.array(byteaArray(txIds))}::bytea[]) FOR UPDATE`;
    const payloadById = new Map(
      payloads.map((row) => [row.tx_id.toString("hex"), row]),
    );
    if (
      payloads.length !== processedTxs.length ||
      processedTxs.some(
        (tx) =>
          !payloadById
            .get(tx.txId.toString("hex"))
            ?.tx_canonical_cbor.equals(tx.txCbor),
      )
    )
      return yield* fail(
        "Accepted receipt requires every original canonical admission payload",
      );
    const inserted = yield* sql<{
      sequence: string;
    }>`INSERT INTO event_history_l2_ledger_receipts
      (binding_digest, owner_generation, checkpoint_revision, head_hash, snapshot_digest,
       tx_ids, reference_outrefs, ledger_before, reference_before, deposits_before, payloads_before)
      VALUES (${Buffer.from(coverage.bindingDigest, "hex")}, ${token.generation}, ${coverage.checkpointRevision},
        ${Buffer.from(coverage.point.id, "hex")}, ${Buffer.from(coverage.snapshotDigest, "hex")},
        ${pg.array(byteaArray(txIds))}::bytea[], ${pg.array(byteaArray(references))}::bytea[],
        (SELECT COALESCE(jsonb_agg(to_jsonb(l) ORDER BY outref), '[]'::jsonb) FROM mempool_ledger l
          WHERE outref = ANY(${pg.array(byteaArray(spent))}::bytea[])),
        (SELECT COALESCE(jsonb_agg(to_jsonb(l) ORDER BY outref), '[]'::jsonb) FROM mempool_ledger l
          WHERE outref = ANY(${pg.array(byteaArray(externalReferences))}::bytea[])),
        (SELECT COALESCE(jsonb_agg(to_jsonb(d) ORDER BY event_id), '[]'::jsonb) FROM deposits_utxos d
          WHERE event_id = ANY(${pg.array(byteaArray(sourceIds))}::bytea[])),
        (SELECT COALESCE(jsonb_agg(to_jsonb(p) ORDER BY tx_id), '[]'::jsonb) FROM tx_admission_payloads p
          WHERE tx_id = ANY(${pg.array(byteaArray(txIds))}::bytea[])))
      RETURNING sequence::text`;
    if (inserted.length !== 1)
      return yield* fail("Failed to record accepted ledger batch");
    return {
      sequence: inserted[0]!.sequence,
      produced,
      spent,
    } satisfies AcceptedLedgerReceipt;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed accepted ledger before-image capture",
    ),
  );

/** Finish in the same SQL transaction, after acceptance and before sidecar
 * scrubbing. A receipt is complete only when every actual net output agrees.
 */
export const finishAcceptedLedgerReceipt = (
  receipt: AcceptedLedgerReceipt | undefined,
) =>
  Effect.gen(function* () {
    if (receipt === undefined) return;
    const permit = yield* requireCandidateHistory;
    if (Option.isNone(permit))
      return yield* fail("Accepted receipt lost its owner");
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const unconsumed = yield* sql<{
      outref: Buffer;
    }>`SELECT outref FROM mempool_ledger
      WHERE outref = ANY(${pg.array(byteaArray(receipt.spent))}::bytea[]) LIMIT 1`;
    if (unconsumed.length !== 0)
      return yield* fail(
        "Accepted ledger still contains a consumed before-image",
      );
    const refs = byteaArray(receipt.produced.map((row) => row.outref));
    const after = yield* sql<LedgerRow>`SELECT * FROM mempool_ledger
      WHERE outref = ANY(${pg.array(refs)}::bytea[]) ORDER BY outref FOR UPDATE`;
    const byRef = new Map(
      after.map((row) => [row.outref.toString("hex"), row]),
    );
    if (
      after.length !== receipt.produced.length ||
      receipt.produced.some((expected) => {
        const actual = byRef.get(expected.outref.toString("hex"));
        return (
          actual === undefined ||
          !actual.tx_id.equals(expected.tx_id) ||
          !actual.output.equals(expected.output) ||
          actual.address !== expected.address ||
          actual.source_event_id !== null
        );
      })
    )
      return yield* fail(
        "Accepted ledger after-image differs from validated outputs",
      );
    const updated = yield* sql<{
      sequence: string;
    }>`UPDATE event_history_l2_ledger_receipts
      SET ledger_after = (SELECT COALESCE(jsonb_agg(to_jsonb(l) ORDER BY outref), '[]'::jsonb)
        FROM mempool_ledger l WHERE outref = ANY(${pg.array(refs)}::bytea[]))
      WHERE sequence = ${receipt.sequence} AND ledger_after IS NULL
        AND binding_digest = ${Buffer.from(permit.value.coverage.bindingDigest, "hex")}
        AND owner_generation = ${permit.value.token.generation}
      RETURNING sequence::text`;
    if (updated.length !== 1)
      return yield* fail(
        "Accepted ledger receipt was changed or already completed",
      );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed accepted ledger after-image capture",
    ),
  );
