import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Duration, Effect } from "effect";

import { emitPhase1AcceptCommitCheckpoint } from "../e2e/phase1-accept-crash-checkpoint.js";
import { NodeConfig } from "../services/config.js";
import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { WriteBehind } from "../services/write-behind.js";
import { ProcessedTx } from "../utils.js";
import * as CekProgramMaterialDB from "./cekProgramMaterial.js";
import * as DepositsDB from "./deposits.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import {
  type AcceptedPersistenceCounts,
  Columns,
  type Entry,
  payloadTableName,
  postgresByteaArray,
  postgresLedgerTimestampArray,
  type RawEntry,
  tableName,
  txAdmissionAcceptedMempoolDurationTimer,
  txAdmissionAcceptedTerminalDurationTimer,
  txAdmissionAcceptedTotalDurationTimer,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const markAccepted = ({
  rows,
  leaseOwner,
  processedTxs,
  programEnvelopesByTxId,
}: {
  readonly rows: readonly Pick<Entry, Columns.TX_ID>[];
  readonly leaseOwner: string;
  readonly processedTxs: readonly ProcessedTx[];
  readonly programEnvelopesByTxId?: ReadonlyMap<
    string,
    readonly MidgardCekProgramEnvelope[]
  >;
}): Effect.Effect<void, DatabaseError, Database | NodeConfig | WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const totalStartedAt = Date.now();
    const acceptedTxIds = processedTxs.map((tx) => tx.txId);
    const acceptedTxIdArray = postgresByteaArray(acceptedTxIds);
    const terminalSidecar = encodeMidgardCekProgramMaterialSidecar([]);
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* withFollowerWrite(
      sql.withTransaction(
        Effect.gen(function* () {
          const acceptedPayloads = yield* sql<{
            readonly tx_id: Buffer;
            readonly tx_canonical_cbor: Buffer;
            readonly cek_program_material_sidecar_cbor: Buffer;
          }>`SELECT
            admission.${sql(Columns.TX_ID)},
            payload.${sql(Columns.TX_CANONICAL_CBOR)},
            payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}
          FROM ${sql(tableName)} admission
          INNER JOIN ${sql(payloadTableName)} payload
            ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
          WHERE admission.${sql(Columns.STATUS)} = 'validating'
            AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
            AND admission.${sql(Columns.TX_ID)} =
              ANY(${pg.array(acceptedTxIdArray)}::bytea[])
          ORDER BY admission.${sql(Columns.TX_ID)} ASC`;
          if (acceptedPayloads.length !== processedTxs.length) {
            return yield* Effect.fail(
              new DatabaseError({
                table: payloadTableName,
                message:
                  "Failed to load every accepted CEK sidecar under the active validation lease",
                cause: `expected=${processedTxs.length},loaded=${acceptedPayloads.length}`,
              }),
            );
          }
          yield* Effect.forEach(
            acceptedPayloads,
            (payload) => {
              const txIdHex = payload.tx_id.toString("hex");
              const programEnvelopes = programEnvelopesByTxId?.get(txIdHex);
              if (
                programEnvelopesByTxId !== undefined &&
                programEnvelopes === undefined
              ) {
                return Effect.fail(
                  new DatabaseError({
                    table: payloadTableName,
                    message:
                      "Accepted admission is missing its Phase B program resolution",
                    cause: txIdHex,
                  }),
                );
              }
              return CekProgramMaterialDB.persistVerifiedAdmissionBundle({
                txId: payload.tx_id,
                txCanonicalCbor: payload.tx_canonical_cbor,
                sidecarCbor: payload.cek_program_material_sidecar_cbor,
                ...(programEnvelopes === undefined ? {} : { programEnvelopes }),
              });
            },
            { discard: true },
          );

          const mempoolStartedAt = Date.now();
          const { produced, spent } =
            MempoolDB.compactLedgerEffects(processedTxs);
          const compactArrays =
            produced.length > 0 && spent.length > 0
              ? {
                  producedTxIds: postgresByteaArray(
                    produced.map(
                      (entry) => entry[MempoolLedgerDB.Columns.TX_ID],
                    ),
                  ),
                  producedOutrefs: postgresByteaArray(
                    produced.map(
                      (entry) => entry[MempoolLedgerDB.Columns.OUTREF],
                    ),
                  ),
                  producedOutputs: postgresByteaArray(
                    produced.map(
                      (entry) => entry[MempoolLedgerDB.Columns.OUTPUT],
                    ),
                  ),
                  producedAddresses: produced.map(
                    (entry) => entry[MempoolLedgerDB.Columns.ADDRESS],
                  ),
                  producedTimestamps: postgresLedgerTimestampArray(produced),
                  spentOutrefs: postgresByteaArray(spent),
                }
              : null;
          let fallbackMempoolCount = 0;
          if (compactArrays === null) {
            const insertedMemberships = yield* sql<
              Pick<RawEntry, Columns.TX_ID>
            >`INSERT INTO ${sql(MempoolDB.tableName)} (tx_id, tx)
            SELECT
              admission.${sql(Columns.TX_ID)},
              payload.${sql(Columns.TX_CANONICAL_CBOR)}
            FROM ${sql(tableName)} AS admission
            INNER JOIN ${sql(payloadTableName)} AS payload
              ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
            WHERE admission.${sql(Columns.STATUS)} = 'validating'
              AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
              AND admission.${sql(Columns.TX_ID)} =
                ANY(${pg.array(acceptedTxIdArray)}::bytea[])
            ON CONFLICT (tx_id) DO NOTHING
            RETURNING ${sql(Columns.TX_ID)}`;
            fallbackMempoolCount = insertedMemberships.length;
            if (fallbackMempoolCount !== processedTxs.length) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: MempoolDB.tableName,
                  message:
                    "Failed to persist accepted mempool memberships exactly once with durable admission payloads",
                  cause: `expected=${processedTxs.length},inserted=${fallbackMempoolCount}`,
                }),
              );
            }
            yield* MempoolDB.applyLedgerEffectsCore(processedTxs);
          }
          yield* txAdmissionAcceptedMempoolDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - mempoolStartedAt)),
          );
          const terminalStartedAt = Date.now();
          const counts =
            compactArrays !== null
              ? yield* sql<AcceptedPersistenceCounts>`
                WITH accepted_admissions AS (
                  UPDATE ${sql(tableName)} AS admission
                  SET
                    ${sql(Columns.STATUS)} = 'accepted',
                    ${sql(Columns.LEASE_OWNER)} = NULL,
                    ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
                    ${sql(Columns.TERMINAL_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    ),
                    ${sql(Columns.UPDATED_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    )
                  FROM ${sql(payloadTableName)} AS payload
                  WHERE admission.${sql(Columns.STATUS)} = 'validating'
                    AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
                    AND admission.${sql(Columns.TX_ID)} =
                      ANY(${pg.array(acceptedTxIdArray)}::bytea[])
                    AND payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
                  RETURNING
                    admission.${sql(Columns.TX_ID)},
                    payload.${sql(Columns.TX_CANONICAL_CBOR)}
                ),
                mempool_insert AS (
                  INSERT INTO ${sql(MempoolDB.tableName)} (tx_id, tx)
                  SELECT
                    ${sql(Columns.TX_ID)},
                    ${sql(Columns.TX_CANONICAL_CBOR)}
                  FROM accepted_admissions
                ),
                produced_insert AS (
                  INSERT INTO ${sql(MempoolLedgerDB.tableName)} (
                    ${sql(MempoolLedgerDB.Columns.TX_ID)},
                    ${sql(MempoolLedgerDB.Columns.OUTREF)},
                    ${sql(MempoolLedgerDB.Columns.OUTPUT)},
                    ${sql(MempoolLedgerDB.Columns.ADDRESS)},
                    ${sql(MempoolLedgerDB.Columns.TIMESTAMPTZ)}
                  )
                  SELECT
                    produced_input.tx_id,
                    produced_input.outref,
                    produced_input.output,
                    produced_input.address,
                    COALESCE(produced_input.time_stamp_tz, NOW())
                  FROM unnest(
                    ${pg.array(compactArrays.producedTxIds)}::bytea[],
                    ${pg.array(compactArrays.producedOutrefs)}::bytea[],
                    ${pg.array(compactArrays.producedOutputs)}::bytea[],
                    ${pg.array(compactArrays.producedAddresses)}::text[],
                    ${pg.array(compactArrays.producedTimestamps)}::timestamptz[]
                  ) AS produced_input(
                    tx_id,
                    outref,
                    output,
                    address,
                    time_stamp_tz
                  )
                ),
                spent_delete AS (
                  DELETE FROM ${sql(MempoolLedgerDB.tableName)}
                  WHERE ${sql(MempoolLedgerDB.Columns.OUTREF)} =
                    ANY(${pg.array(compactArrays.spentOutrefs)}::bytea[])
                  RETURNING ${sql(MempoolLedgerDB.Columns.SOURCE_EVENT_ID)}
                ),
                deposit_update AS (
                  UPDATE ${sql(DepositsDB.tableName)} deposits
                  SET ${sql(DepositsDB.Columns.STATUS)} = ${DepositsDB.Status.Consumed}
                  FROM spent_delete
                  WHERE spent_delete.${sql(
                    MempoolLedgerDB.Columns.SOURCE_EVENT_ID,
                  )} IS NOT NULL
                    AND deposits.${sql(DepositsDB.Columns.ID)} =
                      spent_delete.${sql(
                        MempoolLedgerDB.Columns.SOURCE_EVENT_ID,
                      )}
                    AND deposits.${sql(DepositsDB.Columns.STATUS)}
                      IN (${DepositsDB.Status.Projected}, ${DepositsDB.Status.Consumed})
                  RETURNING 1
                )
                SELECT
                  (SELECT COUNT(*)::bigint FROM accepted_admissions)
                    AS accepted_count,
                  (SELECT COUNT(*)::bigint FROM spent_delete)
                    AS spent_count,
                  (SELECT COUNT(*)::bigint FROM spent_delete
                    WHERE ${sql(MempoolLedgerDB.Columns.SOURCE_EVENT_ID)}
                      IS NOT NULL)
                    AS consumed_deposit_count,
                  (SELECT COUNT(*)::bigint FROM deposit_update)
                    AS updated_deposit_count
              `
              : yield* sql<AcceptedPersistenceCounts>`
                WITH accepted_admissions AS (
                  UPDATE ${sql(tableName)} AS admission
                  SET
                    ${sql(Columns.STATUS)} = 'accepted',
                    ${sql(Columns.LEASE_OWNER)} = NULL,
                    ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
                    ${sql(Columns.TERMINAL_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    ),
                    ${sql(Columns.UPDATED_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    )
                  FROM ${sql(payloadTableName)} AS payload
                  WHERE admission.${sql(Columns.STATUS)} = 'validating'
                    AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
                    AND admission.${sql(Columns.TX_ID)} =
                      ANY(${pg.array(acceptedTxIdArray)}::bytea[])
                    AND payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
                  RETURNING 1
                )
                SELECT
                  (SELECT COUNT(*)::bigint FROM accepted_admissions)
                    AS accepted_count,
                  ${BigInt(spent.length)}::bigint AS spent_count,
                  0::bigint AS consumed_deposit_count,
                  0::bigint AS updated_deposit_count
              `;
          const result = counts[0];
          const expected = {
            accepted_count: processedTxs.length,
            ...(compactArrays === null ? {} : { spent_count: spent.length }),
          } as const;
          const mismatch = Object.entries(expected).find(
            ([key, value]) =>
              Number(result?.[key as keyof AcceptedPersistenceCounts] ?? -1) !==
              value,
          );
          const consumedDepositCount = Number(
            result?.consumed_deposit_count ?? -1,
          );
          const updatedDepositCount = Number(
            result?.updated_deposit_count ?? -1,
          );
          if (
            mismatch !== undefined ||
            consumedDepositCount !== updatedDepositCount
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: tableName,
                message:
                  "Failed to mark accepted admissions exactly once under the active validation lease",
                cause: `expected=${JSON.stringify(expected)},actual=${JSON.stringify(result)},claimed=${rows.length}`,
              }),
            );
          }
          // Accepted rows retain the original sidecar digest for exact duplicate
          // identity but no longer retain attacker-sized sidecar bytes. Global
          // material promotion above and this tombstone update share the terminal
          // acceptance transaction.
          const scrubbedPayloads = yield* sql<{ readonly tx_id: Buffer }>`UPDATE
            ${sql(payloadTableName)}
          SET ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} =
            ${terminalSidecar}
          WHERE ${sql(Columns.TX_ID)} =
            ANY(${pg.array(acceptedTxIdArray)}::bytea[])
          RETURNING ${sql(Columns.TX_ID)}`;
          if (scrubbedPayloads.length !== acceptedTxIds.length) {
            return yield* Effect.fail(
              new DatabaseError({
                table: payloadTableName,
                message:
                  "Failed to scrub every accepted CEK sidecar in the terminal transaction",
                cause: `expected=${acceptedTxIds.length},scrubbed=${scrubbedPayloads.length}`,
              }),
            );
          }
          yield* txAdmissionAcceptedTerminalDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - terminalStartedAt)),
          );
        }),
      ),
    );
    yield* emitPhase1AcceptCommitCheckpoint(acceptedTxIds);
    yield* MempoolDB.enqueueAcceptedWriteBehind(processedTxs);
    yield* txAdmissionAcceptedTotalDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - totalStartedAt)),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions accepted"),
  );
