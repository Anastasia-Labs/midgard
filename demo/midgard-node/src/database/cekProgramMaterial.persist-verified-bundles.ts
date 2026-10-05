import {
  encodeMidgardCekProgramMaterialDaValue,
  hashMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  MidgardCekProgramMaterialMissingRootError,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
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
  retainedStateOwnerTableName,
  rootHex,
  STORE_ADVISORY_LOCK_KEY,
  STORE_ADVISORY_LOCK_NAMESPACE,
  type StoreUsageRow,
} from "./cekProgramMaterial.canonical-entries.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * Persists canonical content nodes and the exact reachable-root set for each
 * attached program envelope. The root remains the security identity; this
 * store is only a durable availability/index surface.
 */
type MaterialOwnership =
  | { readonly kind: "durable" }
  | { readonly kind: "admission"; readonly txId: Buffer }
  | { readonly kind: "retained-state"; readonly headerHash: Buffer };

export function persistVerifiedBundles(
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: readonly MidgardCekProgramMaterialEntry[],
  ownership: Extract<MaterialOwnership, { readonly kind: "retained-state" }>,
): Effect.Effect<
  void,
  DatabaseError | MidgardCekProgramMaterialMissingRootError,
  Database
>;
export function persistVerifiedBundles(
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: readonly MidgardCekProgramMaterialEntry[],
  ownership?: Exclude<MaterialOwnership, { readonly kind: "retained-state" }>,
): Effect.Effect<
  void,
  DatabaseError | MidgardCekProgramMaterialMissingRootError,
  Database | NodeConfig
>;
export function persistVerifiedBundles(
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: readonly MidgardCekProgramMaterialEntry[],
  ownership: MaterialOwnership = { kind: "durable" },
): Effect.Effect<
  void,
  DatabaseError | MidgardCekProgramMaterialMissingRootError,
  Database | NodeConfig
> {
  return Effect.try({
    try: () => {
      if (ownership.kind === "admission" && ownership.txId.length !== 32) {
        throw new Error("CEK admission owner transaction id must be 32 bytes");
      }
      if (
        ownership.kind === "retained-state" &&
        ownership.headerHash.length !== 28
      ) {
        throw new Error(
          "CEK retained-state owner header hash must be 28 bytes",
        );
      }
      const material = canonicalEntries(entries);
      const verifications = verifyMidgardCekProgramMaterialBundle(
        envelopes,
        material,
        { allowUnreachable: true },
      );
      const reachableRoots = new Set(
        verifications.flatMap((verification) => [
          ...verification.reachableRoots,
        ]),
      );
      const verifiedMaterial = material.filter((entry) =>
        reachableRoots.has(rootHex(entry.root)),
      );
      // The availability/index store never persists caller-supplied extras.
      // The permissive pass above identifies each envelope's reachable union;
      // this strict pass proves that the filtered material is exact.
      verifyMidgardCekProgramMaterialBundle(envelopes, verifiedMaterial);
      const materialByRoot = new Map(
        verifiedMaterial.map((entry) => [rootHex(entry.root), entry]),
      );
      const memberships = new Map<
        string,
        {
          readonly envelopeHash: Buffer;
          readonly materialRoot: Buffer;
        }
      >();
      for (let index = 0; index < envelopes.length; index += 1) {
        const envelopeHash = Buffer.from(
          hashMidgardCekProgramEnvelope(envelopes[index]!),
        );
        for (const reachableRoot of verifications[index]!.reachableRoots) {
          const materialEntry = materialByRoot.get(reachableRoot);
          if (materialEntry === undefined) {
            throw new Error(
              `verified CEK root ${reachableRoot} is absent from canonical material`,
            );
          }
          memberships.set(`${envelopeHash.toString("hex")}:${reachableRoot}`, {
            envelopeHash,
            materialRoot: Buffer.from(materialEntry.root),
          });
        }
      }
      return {
        material: verifiedMaterial,
        memberships: [...memberships.values()],
      };
    },
    catch: (cause) =>
      cause instanceof MidgardCekProgramMaterialMissingRootError
        ? cause
        : new DatabaseError({
            table: entryTableName,
            message: "Failed to canonicalize CEK program material bundles",
            cause,
          }),
  }).pipe(
    Effect.flatMap(({ material, memberships }) =>
      Effect.gen(function* () {
        if (material.length === 0 && memberships.length === 0) return;
        const config =
          ownership.kind === "retained-state" ? undefined : yield* NodeConfig;
        const sql = yield* SqlClient.SqlClient;
        const pg = sql as PgClient;
        yield* sql.withTransaction(
          Effect.gen(function* () {
            // Serialize every insert, ownership release, and GC decision across
            // node processes sharing this database. This makes the configured
            // aggregate byte bound authoritative rather than advisory.
            yield* sql`SELECT pg_advisory_xact_lock(
                ${STORE_ADVISORY_LOCK_NAMESPACE},
                ${STORE_ADVISORY_LOCK_KEY}
              )`;
            if (material.length > 0) {
              const roots = material.map((entry) => Buffer.from(entry.root));
              const values = material.map((entry) =>
                encodeMidgardCekProgramMaterialDaValue(entry),
              );
              yield* sql`WITH input AS (
                  SELECT *
                  FROM unnest(
                    ${pg.array(postgresByteaArray(roots))}::bytea[],
                    ${pg.array(postgresByteaArray(values))}::bytea[]
                  ) AS material_input(material_root, da_value_cbor)
                )
                INSERT INTO ${sql(entryTableName)} (
                  material_root,
                  da_value_cbor
                )
                SELECT material_root, da_value_cbor
                FROM input
                ON CONFLICT (material_root) DO NOTHING`;
              const persisted = yield* sql<MaterialRow>`SELECT
                  stored.material_root,
                  stored.da_value_cbor
                FROM ${sql(entryTableName)} stored
                WHERE ${sql.in("stored.material_root", roots)}`;
              const persistedByRoot = new Map(
                persisted.map((row) => [
                  row.material_root.toString("hex"),
                  row.da_value_cbor,
                ]),
              );
              for (let index = 0; index < roots.length; index += 1) {
                const key = roots[index]!.toString("hex");
                const persistedValue = persistedByRoot.get(key);
                if (
                  persistedValue === undefined ||
                  !persistedValue.equals(values[index]!)
                ) {
                  return yield* Effect.fail(
                    new DatabaseError({
                      table: entryTableName,
                      message:
                        "CEK program material content-root collision or incomplete persistence",
                      cause: key,
                    }),
                  );
                }
              }
            }
            if (memberships.length > 0) {
              yield* sql`WITH input AS (
                  SELECT *
                  FROM unnest(
                    ${pg.array(
                      postgresByteaArray(
                        memberships.map((value) => value.envelopeHash),
                      ),
                    )}::bytea[],
                    ${pg.array(
                      postgresByteaArray(
                        memberships.map((value) => value.materialRoot),
                      ),
                    )}::bytea[]
                  ) AS membership_input(program_envelope_hash, material_root)
                )
                INSERT INTO ${sql(membershipTableName)} (
                  program_envelope_hash,
                  material_root,
                  durable_pin
                )
                SELECT
                  program_envelope_hash,
                  material_root,
                  ${ownership.kind === "durable"}
                FROM input
                ON CONFLICT (program_envelope_hash, material_root)
                DO UPDATE SET durable_pin =
                  ${sql(membershipTableName)}.durable_pin
                    OR EXCLUDED.durable_pin`;
              if (ownership.kind === "admission") {
                yield* sql`WITH input AS (
                    SELECT *
                    FROM unnest(
                      ${pg.array(
                        postgresByteaArray(
                          memberships.map((value) => value.envelopeHash),
                        ),
                      )}::bytea[],
                      ${pg.array(
                        postgresByteaArray(
                          memberships.map((value) => value.materialRoot),
                        ),
                      )}::bytea[]
                    ) AS owner_input(program_envelope_hash, material_root)
                  )
                  INSERT INTO ${sql(admissionOwnerTableName)} (
                    tx_id,
                    program_envelope_hash,
                    material_root
                  )
                  SELECT
                    ${ownership.txId},
                    program_envelope_hash,
                    material_root
                  FROM input
                  ON CONFLICT (
                    tx_id,
                    program_envelope_hash,
                    material_root
                  ) DO NOTHING`;
              }
            }
            if (ownership.kind === "retained-state" && memberships.length > 0) {
              yield* sql`INSERT INTO ${sql(retainedStateOwnerTableName)} (
                header_hash, program_envelope_hash, material_root
              ) SELECT ${ownership.headerHash}, program_envelope_hash, material_root
                FROM ${sql(membershipTableName)}
                WHERE ${sql.in(
                  "program_envelope_hash",
                  memberships.map((value) => value.envelopeHash),
                )}
                ON CONFLICT DO NOTHING`;
            }
            // Authenticated live state must stay available even when the admission
            // cache is full. Only retained-only bytes are exempt: durable and
            // admission owners reserve their bytes for their entire lifetime,
            // so pruning a retained owner cannot overfill this bounded store.
            if (config === undefined) return;
            const usage = yield* sql<StoreUsageRow>`SELECT (
                COALESCE((
                  SELECT SUM(
                    octet_length(material_root)
                      + octet_length(da_value_cbor)
                  )
                  FROM ${sql(entryTableName)} entry
                  WHERE NOT EXISTS (
                    SELECT 1 FROM ${sql(retainedStateOwnerTableName)} retained
                    WHERE retained.material_root = entry.material_root
                  ) OR EXISTS (
                    SELECT 1 FROM ${sql(membershipTableName)} membership
                    WHERE membership.material_root = entry.material_root
                      AND membership.durable_pin = true
                  ) OR EXISTS (
                    SELECT 1 FROM ${sql(admissionOwnerTableName)} owner
                    WHERE owner.material_root = entry.material_root
                  )
                ), 0)
                + COALESCE((
                  SELECT SUM(
                    octet_length(program_envelope_hash)
                      + octet_length(material_root)
                      + 1
                  )
                  FROM ${sql(membershipTableName)} membership
                  WHERE membership.durable_pin = true OR EXISTS (
                    SELECT 1 FROM ${sql(admissionOwnerTableName)} owner
                    WHERE owner.program_envelope_hash = membership.program_envelope_hash
                      AND owner.material_root = membership.material_root
                  ) OR NOT EXISTS (
                    SELECT 1 FROM ${sql(retainedStateOwnerTableName)} retained
                    WHERE retained.program_envelope_hash = membership.program_envelope_hash
                      AND retained.material_root = membership.material_root
                  )
                ), 0)
                + COALESCE((
                  SELECT SUM(
                    octet_length(tx_id)
                      + octet_length(program_envelope_hash)
                      + octet_length(material_root)
                  )
                  FROM ${sql(admissionOwnerTableName)}
                ), 0)
              )::text AS total_bytes`;
            const totalBytes = BigInt(usage[0]?.total_bytes ?? "0");
            if (
              totalBytes > BigInt(config.CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES)
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: entryTableName,
                  message:
                    "CEK program material store exceeds its durable aggregate byte cap",
                  cause: `stored_bytes=${totalBytes.toString()},max_bytes=${config.CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES.toString()}`,
                }),
              );
            }
          }),
        );
      }).pipe(
        sqlErrorToDatabaseError(
          entryTableName,
          "Failed to persist CEK program material bundles",
        ),
      ),
    ),
  );
}
