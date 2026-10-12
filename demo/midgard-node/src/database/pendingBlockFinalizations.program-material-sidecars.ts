import { decodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { Effect } from "effect";

import { txsTableName } from "./pendingBlockFinalizations.columns.js";
import type { PrepareInput } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { DatabaseError } from "./utils/common.js";
import * as TxTable from "./utils/tx.js";

/**
 * The pending journal's program-material sidecars by transaction id: each
 * decodes as canonical V1 program material, and there is exactly one per
 * normal transaction.
 */
export const programMaterialSidecarsByTxId = (
  input: Pick<PrepareInput, "mempoolTxs" | "mempoolTxProgramMaterialSidecars">,
): Effect.Effect<Map<string, Buffer>, DatabaseError> =>
  Effect.gen(function* () {
    const programMaterialByTxId = new Map<string, Buffer>();
    for (const material of input.mempoolTxProgramMaterialSidecars ?? []) {
      const txIdHex = material.txId.toString("hex");
      if (programMaterialByTxId.has(txIdHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to prepare duplicate V1 transaction program material",
            cause: `tx_id=${txIdHex}`,
          }),
        );
      }
      yield* Effect.try({
        try: () => decodeMidgardCekProgramMaterialSidecar(material.sidecarCbor),
        catch: (cause) =>
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to journal malformed V1 transaction program material",
            cause,
          }),
      });
      programMaterialByTxId.set(txIdHex, Buffer.from(material.sidecarCbor));
    }
    if (
      programMaterialByTxId.size !== input.mempoolTxs.length ||
      input.mempoolTxs.some(
        (entry) =>
          !programMaterialByTxId.has(
            entry[TxTable.Columns.TX_ID].toString("hex"),
          ),
      )
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: txsTableName,
          message:
            "V1 pending journal requires one canonical program-material sidecar per normal transaction",
          cause: `transactions=${input.mempoolTxs.length.toString()},sidecars=${programMaterialByTxId.size.toString()}`,
        }),
      );
    }
    return programMaterialByTxId;
  });
