import { decodeMidgardCekProgramMaterialDaEntry } from "@al-ft/midgard-core/cek-proof";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { pinRetainedStateScriptRefs } from "../../database/cekProgramMaterial.pin-retained-state.js";
import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import * as Ledger from "../../database/utils/ledger.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../mpf/index.js";
import { Database } from "../../services/database.js";
import { materializeConfirmedLedgerDeltaChain } from "../../transactions/state-queue/confirmed-ledger-snapshot.js";

/**
 * The post-state outputs of a retained block, from the node's own ledger:
 * the confirmed ledger when the block is its head, or the confirmed ledger
 * plus the journaled deltas up to the block. A block behind the confirmed
 * head has no materializable post-state; its pin was written durably when it
 * was finalized, so there is nothing to restore.
 */
const retainedPostStateOutputs = ({
  headerHash,
  confirmedEntries,
  confirmedRoot,
}: {
  readonly headerHash: Buffer;
  readonly confirmedEntries: readonly Ledger.Entry[];
  readonly confirmedRoot: string;
}): Effect.Effect<readonly Buffer[] | undefined, DatabaseError, Database> =>
  Effect.gen(function* () {
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    if (Option.isNone(journal)) return undefined;
    if (
      journal.value[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT] ===
      confirmedRoot
    )
      return confirmedEntries.map((entry) => entry[Ledger.Columns.OUTPUT]);
    return yield* materializeConfirmedLedgerDeltaChain({
      record: journal.value,
      confirmedEntries,
      confirmedRoot,
      retrieveParent: PendingBlockFinalizationsDB.retrieveByHeaderHash,
    }).pipe(
      Effect.map((snapshot) =>
        snapshot.entries.map((entry) => entry[Ledger.Columns.OUTPUT]),
      ),
      Effect.catchAll((error) =>
        Effect.logDebug(
          `Retained block ${headerHash.toString("hex")} is not ahead of the confirmed ledger: ${formatUnknownError(error)}`,
        ).pipe(Effect.as(undefined)),
      ),
    );
  });

/** Restore retained DA state owners before workers can release admissions or
 * prune DA. Keyset paging bounds the DB fetch; each payload is decoded alone.
 * The payload carries no UTxO set, so the outputs come from the node's ledger.
 */
export const restoreRetainedStatePins: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
  const confirmedRoot = yield* computeLedgerMpfRootFromLedgerEntries(
    confirmedEntries,
  ).pipe(
    Effect.mapError(
      (cause) =>
        new DatabaseError({
          table: ConfirmedLedgerDB.tableName,
          message: "Failed to compute the confirmed ledger root",
          cause: formatUnknownError(cause),
        }),
    ),
  );
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
      after = row.header_hash;
      const outputs = yield* retainedPostStateOutputs({
        headerHash: row.header_hash,
        confirmedEntries,
        confirmedRoot,
      });
      if (outputs === undefined) continue;
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
        outputs,
        material,
      });
    }
  }
}).pipe(
  sqlErrorToDatabaseError(
    "da_payloads",
    "Failed to restore retained-state script pins",
  ),
);
