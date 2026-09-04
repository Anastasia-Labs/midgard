import {
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialDaValue,
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
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const entryTableName = "cek_program_material_entries";
export const membershipTableName = "cek_program_material_memberships";
export const admissionOwnerTableName = "cek_program_material_admission_owners";

const STORE_ADVISORY_LOCK_NAMESPACE = 1_129_606_987;
const STORE_ADVISORY_LOCK_KEY = 1_296_651_247;

type MaterialRow = {
  readonly material_root: Buffer;
  readonly da_value_cbor: Buffer;
};

type StoreUsageRow = {
  readonly total_bytes: string;
};

const postgresByteaArray = (values: readonly Buffer[]): readonly string[] =>
  values.map((value) => `\\x${value.toString("hex")}`);

const rootHex = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const canonicalEntries = (
  entries: readonly MidgardCekProgramMaterialEntry[],
): readonly MidgardCekProgramMaterialEntry[] => {
  const byRoot = new Map<string, MidgardCekProgramMaterialEntry>();
  for (const entry of entries) {
    const daValue = encodeMidgardCekProgramMaterialDaValue(entry);
    const canonical = decodeMidgardCekProgramMaterialDaEntry(
      entry.root,
      daValue,
    );
    const key = rootHex(canonical.root);
    const existing = byRoot.get(key);
    if (
      existing !== undefined &&
      !encodeMidgardCekProgramMaterialDaValue(existing).equals(daValue)
    ) {
      throw new Error(`conflicting CEK material for root ${key}`);
    }
    byRoot.set(key, canonical);
  }
  return [...byRoot.values()].sort((left, right) =>
    Buffer.compare(Buffer.from(left.root), Buffer.from(right.root)),
  );
};

/**
 * Persists canonical content nodes and the exact reachable-root set for each
 * attached program envelope. The root remains the security identity; this
 * store is only a durable availability/index surface.
 */
export const persistVerifiedBundles = (
  envelopes: readonly MidgardCekProgramEnvelope[],
  entries: readonly MidgardCekProgramMaterialEntry[],
  ownership:
    | { readonly kind: "durable" }
    | { readonly kind: "admission"; readonly txId: Buffer } = {
    kind: "durable",
  },
): Effect.Effect<
  void,
  DatabaseError | MidgardCekProgramMaterialMissingRootError,
  Database | NodeConfig
> =>
  Effect.try({
    try: () => {
      if (ownership.kind === "admission" && ownership.txId.length !== 32) {
        throw new Error("CEK admission owner transaction id must be 32 bytes");
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
        const config = yield* NodeConfig;
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
            const usage = yield* sql<StoreUsageRow>`SELECT (
                COALESCE((
                  SELECT SUM(
                    octet_length(material_root)
                      + octet_length(da_value_cbor)
                  )
                  FROM ${sql(entryTableName)}
                ), 0)
                + COALESCE((
                  SELECT SUM(
                    octet_length(program_envelope_hash)
                      + octet_length(material_root)
                      + 1
                  )
                  FROM ${sql(membershipTableName)}
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

/**
 * Promotes one validation-accepted admission's sidecar into the global
 * availability index. Decoding from the durable transaction and sidecar bytes
 * keeps the terminal transition independent of ephemeral HTTP state.
 */
export const persistVerifiedAdmissionBundle = ({
  txId,
  txCanonicalCbor,
  sidecarCbor,
  referenceProgramEnvelopes,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly sidecarCbor: Buffer;
  readonly referenceProgramEnvelopes?: readonly MidgardCekProgramEnvelope[];
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
          if (hasReferenceInputs && referenceProgramEnvelopes === undefined) {
            throw new Error(
              "accepted transaction reference inputs lack Phase B resolution",
            );
          }
          return [
            ...collectMidgardAttachedProgramEnvelopes(tx),
            ...(referenceProgramEnvelopes ?? []),
          ] as const;
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
        yield* sql`DELETE FROM ${sql(membershipTableName)} AS membership
          WHERE membership.durable_pin = false
            AND NOT EXISTS (
              SELECT 1
              FROM ${sql(admissionOwnerTableName)} AS owner
              WHERE owner.program_envelope_hash =
                  membership.program_envelope_hash
                AND owner.material_root = membership.material_root
            )`;
        yield* sql`DELETE FROM ${sql(entryTableName)} AS material
          WHERE NOT EXISTS (
            SELECT 1
            FROM ${sql(membershipTableName)} AS membership
            WHERE membership.material_root = material.material_root
          )`;
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
