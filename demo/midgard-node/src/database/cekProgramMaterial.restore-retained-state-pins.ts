import { decodeMidgardCekProgramMaterialDaEntry } from "@al-ft/midgard-core/cek-proof";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { pinRetainedStateScriptRefs } from "./cekProgramMaterial.pin-retained-state.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Restore retained DA state owners before workers can release admissions or
 * prune DA. Keyset paging bounds the DB fetch; each payload is decoded alone.
 */
export const restoreRetainedStatePins: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  let after = Buffer.alloc(0);
  while (true) {
    const rows = yield* sql<{
      readonly header_hash: Buffer;
      readonly payload_cbor: Buffer;
    }>`
        SELECT header_hash, payload_cbor FROM da_payloads
        WHERE header_hash > ${after} ORDER BY header_hash LIMIT 16`;
    if (rows.length === 0) return;
    for (const row of rows) {
      const body = yield* Effect.tryPromise({
        try: async () => {
          const { innerBytes } = await unwrapDaPayload(row.payload_cbor, {
            maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
          });
          const payload = SDK.decodeDaPayload(innerBytes);
          if (
            payload.block_body.header_hash !== row.header_hash.toString("hex")
          )
            throw new Error(
              "Retained DA payload header differs from its owner",
            );
          return payload.block_body;
        },
        catch: (cause) =>
          new DatabaseError({
            table: "da_payloads",
            message:
              "Failed to restore script material owners from retained DA",
            cause,
          }),
      });
      const material = yield* Effect.try({
        try: () =>
          body.cek_program_material.map((entry) =>
            decodeMidgardCekProgramMaterialDaEntry(
              Buffer.from(entry[0], "hex"),
              Buffer.from(entry[1], "hex"),
            ),
          ),
        catch: (cause) =>
          new DatabaseError({
            table: "da_payloads",
            message: "Malformed retained DA script material",
            cause,
          }),
      });
      yield* pinRetainedStateScriptRefs({
        headerHash: row.header_hash,
        outputs: body.utxos.map((entry) => Buffer.from(entry[1], "hex")),
        material,
      });
      after = row.header_hash;
    }
  }
}).pipe(
  sqlErrorToDatabaseError(
    "da_payloads",
    "Failed to restore retained-state script pins",
  ),
);
