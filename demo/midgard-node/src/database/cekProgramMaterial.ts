import {
  decodeMidgardCekProgramMaterialDaEntryV1,
  encodeMidgardCekProgramMaterialDaValueV1,
  hashMidgardCekProgramEnvelopeV1,
  type MidgardCekProgramEnvelopeV1,
  type MidgardCekProgramMaterialEntryV1,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const entryTableName = "cek_program_material_entries";
export const membershipTableName = "cek_program_material_memberships";

type MaterialRow = {
  readonly material_root: Buffer;
  readonly da_value_cbor: Buffer;
};

const postgresByteaArray = (values: readonly Buffer[]): readonly string[] =>
  values.map((value) => `\\x${value.toString("hex")}`);

const rootHex = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const canonicalEntries = (
  entries: readonly MidgardCekProgramMaterialEntryV1[],
): readonly MidgardCekProgramMaterialEntryV1[] => {
  const byRoot = new Map<string, MidgardCekProgramMaterialEntryV1>();
  for (const entry of entries) {
    const daValue = encodeMidgardCekProgramMaterialDaValueV1(entry);
    const canonical = decodeMidgardCekProgramMaterialDaEntryV1(
      entry.root,
      daValue,
    );
    const key = rootHex(canonical.root);
    const existing = byRoot.get(key);
    if (
      existing !== undefined &&
      !encodeMidgardCekProgramMaterialDaValueV1(existing).equals(daValue)
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
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
  entries: readonly MidgardCekProgramMaterialEntryV1[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.try({
    try: () => {
      const material = canonicalEntries(entries);
      const verifications = verifyMidgardCekProgramMaterialBundleV1(
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
      verifyMidgardCekProgramMaterialBundleV1(envelopes, verifiedMaterial);
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
          hashMidgardCekProgramEnvelopeV1(envelopes[index]!),
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
      new DatabaseError({
        table: entryTableName,
        message: "Failed to canonicalize CEK program material bundles",
        cause,
      }),
  }).pipe(
    Effect.flatMap(({ material, memberships }) =>
      Effect.gen(function* () {
        if (material.length === 0 && memberships.length === 0) return;
        const sql = yield* SqlClient.SqlClient;
        const pg = sql as PgClient;
        yield* sql.withTransaction(
          Effect.gen(function* () {
            if (material.length > 0) {
              const roots = material.map((entry) => Buffer.from(entry.root));
              const values = material.map((entry) =>
                encodeMidgardCekProgramMaterialDaValueV1(entry),
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
                  material_root
                )
                SELECT program_envelope_hash, material_root
                FROM input
                ON CONFLICT (program_envelope_hash, material_root) DO NOTHING`;
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
 * Loads and re-verifies the exact durable bundle for the supplied envelopes.
 * A missing membership, missing node, collision, or stale index fails closed.
 */
export const retrieveVerifiedBundles = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
): Effect.Effect<
  readonly MidgardCekProgramMaterialEntryV1[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (envelopes.length === 0) return Object.freeze([]);
    const envelopeHashes = [
      ...new Map(
        envelopes.map((envelope) => {
          const hash = Buffer.from(hashMidgardCekProgramEnvelopeV1(envelope));
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
            decodeMidgardCekProgramMaterialDaEntryV1(
              row.material_root,
              row.da_value_cbor,
            ),
          ),
        );
        verifyMidgardCekProgramMaterialBundleV1(envelopes, entries);
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
