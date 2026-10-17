import {
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardCekProgramMaterialSidecar,
  hashMidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  MidgardCekProgramMaterialMissingRootError,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { decodeMidgardScriptProgramEnvelope } from "@al-ft/midgard-core/script-proof";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  canonicalEntries,
  entryTableName,
  type MaterialRow,
  membershipTableName,
  STORE_ADVISORY_LOCK_KEY,
  STORE_ADVISORY_LOCK_NAMESPACE,
} from "./cekProgramMaterial.canonical-entries.js";
import { persistVerifiedBundles } from "./cekProgramMaterial.persist-verified-bundles.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Pin the script_refs of a durable DA post-state before its admissions release.
 * Inherited refs use the store; newly introduced refs also have journal material.
 * The DA head and live queue are retained indefinitely; older state owners
 * release only when that snapshot is safely pruned by authenticated retention.
 */
export const pinRetainedStateScriptRefs = ({
  headerHash,
  outputs,
  material,
}: {
  readonly headerHash: Buffer;
  readonly outputs: readonly Uint8Array[];
  readonly material: readonly MidgardCekProgramMaterialEntry[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const envelopes = yield* Effect.try({
      try: () =>
        outputs.flatMap((output) => {
          const script = decodeMidgardTxOutput(output).script_ref;
          if (script === undefined) return [];
          const envelope = decodeMidgardScriptProgramEnvelope(script);
          return envelope === null ? [] : [envelope];
        }),
      catch: (cause) =>
        new DatabaseError({
          table: entryTableName,
          message: "Retained L2 state contains a malformed script_ref",
          cause,
        }),
    });
    if (envelopes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        // Serialize loading inherited material with every release and prune.
        yield* sql`SELECT pg_advisory_xact_lock(
        ${STORE_ADVISORY_LOCK_NAMESPACE}, ${STORE_ADVISORY_LOCK_KEY}
      )`;
        const uniqueEnvelopes = new Map(
          envelopes.map((envelope) => [
            Buffer.from(hashMidgardCekProgramEnvelope(envelope)).toString(
              "hex",
            ),
            envelope,
          ]),
        );
        for (const [identity, envelope] of uniqueEnvelopes) {
          const hashes = [Buffer.from(identity, "hex")];
          const rows =
            yield* sql<MaterialRow>`SELECT membership.material_root, material.da_value_cbor
        FROM ${sql(membershipTableName)} membership
        JOIN ${sql(entryTableName)} material USING (material_root)
        WHERE ${sql.in("membership.program_envelope_hash", hashes)}`;
          const entries = yield* Effect.try({
            try: () =>
              canonicalEntries([
                ...material,
                ...rows.map((row) =>
                  decodeMidgardCekProgramMaterialDaEntry(
                    row.material_root,
                    row.da_value_cbor,
                  ),
                ),
              ]),
            catch: (cause) =>
              new DatabaseError({
                table: entryTableName,
                message: "Malformed retained script material",
                cause,
              }),
          });
          const persist = (
            materialEntries: readonly MidgardCekProgramMaterialEntry[],
          ) =>
            persistVerifiedBundles([envelope], materialEntries, {
              kind: "retained-state",
              headerHash,
            });
          yield* persist(entries).pipe(
            Effect.catchIf(
              (cause): cause is MidgardCekProgramMaterialMissingRootError =>
                cause instanceof MidgardCekProgramMaterialMissingRootError,
              () =>
                Effect.gen(function* () {
                  // Upgrade/recovery: admission cleanup may predate retained-state
                  // ownership. The creator's durable journal still contains its exact
                  // sidecar even if its original DA payload is beyond the horizon.
                  const sources: MidgardCekProgramMaterialEntry[] = [
                    ...entries,
                  ];
                  const journals = yield* sql<{
                    readonly cek_program_material_sidecar_cbor: Buffer;
                  }>`
                SELECT cek_program_material_sidecar_cbor FROM pending_block_finalization_txs
                WHERE position(${Buffer.from(envelope.termRoot)} IN cek_program_material_sidecar_cbor) > 0`;
                  const recovered = yield* Effect.try({
                    try: () =>
                      journals.flatMap((row) =>
                        decodeMidgardCekProgramMaterialSidecar(
                          row.cek_program_material_sidecar_cbor,
                        ),
                      ),
                    catch: (cause) =>
                      new DatabaseError({
                        table: entryTableName,
                        message: "Malformed script material recovery journal",
                        cause,
                      }),
                  });
                  sources.push(...recovered);
                  yield* persist(sources);
                }),
            ),
            Effect.mapError((cause) =>
              cause instanceof DatabaseError
                ? cause
                : new DatabaseError({
                    table: entryTableName,
                    message:
                      "Live L2 script_ref has no complete retained material",
                    cause,
                  }),
            ),
          );
        }
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      entryTableName,
      "Failed to pin retained L2 script material",
    ),
  );
