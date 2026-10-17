import {
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardCekProgramMaterialSidecar,
  hashMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  MidgardCekProgramMaterialMissingRootError,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { NodeConfig } from "../services/config.js";
import { Database } from "../services/database.js";
import {
  admissionOwnerTableName,
  canonicalEntries,
  entryTableName,
  type MaterialRow,
  membershipTableName,
  postgresByteaArray,
  STORE_ADVISORY_LOCK_KEY,
  STORE_ADVISORY_LOCK_NAMESPACE,
} from "./cekProgramMaterial.canonical-entries.js";
import { collectUnownedMaterial } from "./cekProgramMaterial.collect-unowned.js";
import { persistVerifiedBundles } from "./cekProgramMaterial.persist-verified-bundles.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * Promotes one validation-accepted admission's sidecar into the global
 * availability index. Decoding from the durable transaction and sidecar bytes
 * keeps the terminal transition independent of ephemeral HTTP state.
 */
export const persistVerifiedAdmissionBundle = ({
  txId,
  txCanonicalCbor,
  sidecarCbor,
  programEnvelopes,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly sidecarCbor: Buffer;
  /**
   * The transaction's program set as Phase B resolved it
   * (`collectMidgardEventProgramEnvelopes`). Required when the transaction
   * has reference inputs; otherwise its attached programs are the whole set.
   */
  readonly programEnvelopes?: readonly MidgardCekProgramEnvelope[];
}): Effect.Effect<void, DatabaseError, Database | NodeConfig> =>
  Effect.try({
    try: () => decodeMidgardCekProgramMaterialSidecar(sidecarCbor),
    catch: (cause) =>
      new DatabaseError({
        table: entryTableName,
        message:
          "Accepted admission contains malformed CEK material sidecar bytes",
        cause,
      }),
  }).pipe(
    Effect.flatMap((material) =>
      Effect.try({
        try: () => {
          const tx =
            decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor);
          const hasReferenceInputs =
            decodeMidgardNativeByteListPreimage(
              tx.body.referenceInputsPreimageCbor,
              "reference_inputs_preimage",
            ).length > 0;
          if (hasReferenceInputs && programEnvelopes === undefined) {
            throw new Error(
              "accepted transaction reference inputs lack Phase B resolution",
            );
          }
          return programEnvelopes ?? collectMidgardAttachedProgramEnvelopes(tx);
        },
        catch: (cause) =>
          new DatabaseError({
            table: entryTableName,
            message: "Accepted CEK material has malformed transaction bytes",
            cause,
          }),
      }).pipe(
        Effect.flatMap((envelopes) =>
          envelopes.length === 0 && material.length === 0
            ? Effect.void
            : persistVerifiedBundles(envelopes, material, {
                kind: "admission",
                txId,
              }).pipe(
                Effect.mapError((cause) =>
                  cause instanceof MidgardCekProgramMaterialMissingRootError
                    ? new DatabaseError({
                        table: entryTableName,
                        message:
                          "Accepted admission contains incomplete CEK program material",
                        cause,
                      })
                    : cause,
                ),
              ),
        ),
      ),
    ),
  );

/**
 * Releases accepted-admission ownership once commit processing reaches a
 * terminal lifecycle outcome. Durable-pinned L1 material is never collected;
 * shared unpinned material survives until its final admission owner releases.
 */
export const releaseAdmissionOwnership = (
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> => {
  const uniqueTxIds = [
    ...new Map(txIds.map((txId) => [txId.toString("hex"), txId])).values(),
  ];
  if (uniqueTxIds.length === 0) return Effect.void;
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SELECT pg_advisory_xact_lock(
            ${STORE_ADVISORY_LOCK_NAMESPACE},
            ${STORE_ADVISORY_LOCK_KEY}
          )`;
        yield* sql`DELETE FROM ${sql(admissionOwnerTableName)}
          WHERE tx_id =
            ANY(${pg.array(postgresByteaArray(uniqueTxIds))}::bytea[])`;
        yield* collectUnownedMaterial;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      admissionOwnerTableName,
      "Failed to release CEK admission ownership",
    ),
  );
};

/**
 * Loads and re-verifies the exact durable bundle for the supplied envelopes.
 * A missing membership, missing node, collision, or stale index fails closed.
 */
export const retrieveVerifiedBundles = (
  envelopes: readonly MidgardCekProgramEnvelope[],
): Effect.Effect<
  readonly MidgardCekProgramMaterialEntry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (envelopes.length === 0) return Object.freeze([]);
    const envelopeHashes = [
      ...new Map(
        envelopes.map((envelope) => {
          const hash = Buffer.from(hashMidgardCekProgramEnvelope(envelope));
          return [hash.toString("hex"), hash] as const;
        }),
      ).values(),
    ];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<MaterialRow>`SELECT
        membership.material_root,
        material.da_value_cbor
      FROM ${sql(membershipTableName)} membership
      INNER JOIN ${sql(entryTableName)} material
        ON material.material_root = membership.material_root
      WHERE ${sql.in("membership.program_envelope_hash", envelopeHashes)}
      ORDER BY membership.material_root`;
    return yield* Effect.try({
      try: () => {
        const entries = canonicalEntries(
          rows.map((row) =>
            decodeMidgardCekProgramMaterialDaEntry(
              row.material_root,
              row.da_value_cbor,
            ),
          ),
        );
        verifyMidgardCekProgramMaterialBundle(envelopes, entries);
        return Object.freeze(entries);
      },
      catch: (cause) =>
        new DatabaseError({
          table: membershipTableName,
          message:
            "Durable CEK program material bundle is incomplete or malformed",
          cause,
        }),
    });
  }).pipe(
    sqlErrorToDatabaseError(
      membershipTableName,
      "Failed to retrieve verified CEK program material bundles",
    ),
  );
