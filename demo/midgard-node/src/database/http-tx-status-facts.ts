import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { Status } from "./txAdmissions.js";

/** Presence/status metadata only; never fetch transaction or proof-material blobs. */
export const readHttpTxStatusFacts = (
  txId: Buffer,
  includeDiagnostic: boolean,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{
      reject_code: string | null;
      reject_detail: string | null;
      created_at: Date | null;
      admission_status: Status | null;
      in_immutable: boolean;
      in_mempool: boolean;
      in_processed_mempool: boolean;
    }>`SELECT rejected.reject_code, CASE WHEN ${includeDiagnostic} THEN rejected.reject_detail END AS reject_detail,
      rejected.created_at, admitted.status AS admission_status,
      EXISTS (SELECT 1 FROM immutable WHERE tx_id = ${txId}) AS in_immutable,
      EXISTS (SELECT 1 FROM mempool WHERE tx_id = ${txId} AND included_by IS NULL) AS in_mempool,
      EXISTS (SELECT 1 FROM processed_mempool WHERE tx_id = ${txId} AND included_by IS NULL) AS in_processed_mempool
    FROM (VALUES (${txId}::bytea)) AS requested(tx_id)
    LEFT JOIN LATERAL (SELECT reject_code, reject_detail, created_at FROM tx_rejections
      WHERE tx_id = requested.tx_id ORDER BY created_at DESC LIMIT 1) rejected ON TRUE
    LEFT JOIN tx_admissions admitted ON admitted.tx_id = requested.tx_id`;
    return row!;
  });
