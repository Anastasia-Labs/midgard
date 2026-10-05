import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  STORE_ADVISORY_LOCK_KEY,
  STORE_ADVISORY_LOCK_NAMESPACE,
} from "./cekProgramMaterial.canonical-entries.js";
import { collectUnownedMaterial } from "./cekProgramMaterial.collect-unowned.js";
import {
  observerCursorHeader,
  recoveryRelevantJournal,
} from "./retention-holds.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const tableName = "da_payloads";

export enum Columns {
  HEADER_HASH = "header_hash",
  CONSENSUS_PROFILE_ID = "consensus_profile_id",
  VERSION = "version",
  PAYLOAD_CBOR = "payload_cbor",
  PAYLOAD_SHA256 = "payload_sha256",
  UTXOS_ROOT = "utxos_root",
  FORCED_TRANSACTIONS_ROOT = "forced_transactions_root",
  TRANSACTIONS_ROOT = "transactions_root",
  DEPOSITS_ROOT = "deposits_root",
  WITHDRAWALS_ROOT = "withdrawals_root",
  TRANSITION_TRACE_ROOT = "transition_trace_root",
  EVENT_TO_STEP_ROOT = "event_to_step_root",
  VALIDATION_TRACES_ROOT = "validation_traces_root",
  WITHDRAWAL_COUNT = "withdrawal_count",
  FORCED_TRANSACTION_COUNT = "forced_transaction_count",
  L2_TRANSACTION_COUNT = "l2_transaction_count",
  DEPOSIT_COUNT = "deposit_count",
  TOTAL_EVENT_COUNT = "total_event_count",
  TRANSITION_STEP_COUNT = "transition_step_count",
  VALIDATION_TRACE_COUNT = "validation_trace_count",
  BLOCK_START_TIME = "block_start_time",
  BLOCK_END_TIME = "block_end_time",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
}

export type Row = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  [Columns.VERSION]: 1;
  [Columns.PAYLOAD_CBOR]: Buffer;
  [Columns.PAYLOAD_SHA256]: Buffer;
  [Columns.UTXOS_ROOT]: string;
  [Columns.FORCED_TRANSACTIONS_ROOT]: string;
  [Columns.TRANSACTIONS_ROOT]: string;
  [Columns.DEPOSITS_ROOT]: string;
  [Columns.WITHDRAWALS_ROOT]: string;
  [Columns.TRANSITION_TRACE_ROOT]: string;
  [Columns.EVENT_TO_STEP_ROOT]: string;
  [Columns.VALIDATION_TRACES_ROOT]: string;
  [Columns.WITHDRAWAL_COUNT]: bigint;
  [Columns.FORCED_TRANSACTION_COUNT]: bigint;
  [Columns.L2_TRANSACTION_COUNT]: bigint;
  [Columns.DEPOSIT_COUNT]: bigint;
  [Columns.TOTAL_EVENT_COUNT]: bigint;
  [Columns.TRANSITION_STEP_COUNT]: bigint;
  [Columns.VALIDATION_TRACE_COUNT]: bigint;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

type PgBigInt = bigint | number | string;

type RawRow = Omit<
  Row,
  | Columns.WITHDRAWAL_COUNT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.L2_TRANSACTION_COUNT
  | Columns.DEPOSIT_COUNT
  | Columns.TOTAL_EVENT_COUNT
  | Columns.TRANSITION_STEP_COUNT
  | Columns.VALIDATION_TRACE_COUNT
> & {
  [Columns.WITHDRAWAL_COUNT]: PgBigInt;
  [Columns.FORCED_TRANSACTION_COUNT]: PgBigInt;
  [Columns.L2_TRANSACTION_COUNT]: PgBigInt;
  [Columns.DEPOSIT_COUNT]: PgBigInt;
  [Columns.TOTAL_EVENT_COUNT]: PgBigInt;
  [Columns.TRANSITION_STEP_COUNT]: PgBigInt;
  [Columns.VALIDATION_TRACE_COUNT]: PgBigInt;
};

const toBigInt = (value: PgBigInt): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

const normalizeRow = (row: RawRow): Row => ({
  ...row,
  [Columns.WITHDRAWAL_COUNT]: toBigInt(row[Columns.WITHDRAWAL_COUNT]),
  [Columns.FORCED_TRANSACTION_COUNT]: toBigInt(
    row[Columns.FORCED_TRANSACTION_COUNT],
  ),
  [Columns.L2_TRANSACTION_COUNT]: toBigInt(row[Columns.L2_TRANSACTION_COUNT]),
  [Columns.DEPOSIT_COUNT]: toBigInt(row[Columns.DEPOSIT_COUNT]),
  [Columns.TOTAL_EVENT_COUNT]: toBigInt(row[Columns.TOTAL_EVENT_COUNT]),
  [Columns.TRANSITION_STEP_COUNT]: toBigInt(row[Columns.TRANSITION_STEP_COUNT]),
  [Columns.VALIDATION_TRACE_COUNT]: toBigInt(
    row[Columns.VALIDATION_TRACE_COUNT],
  ),
});

export type InsertInput = Pick<
  Row,
  | Columns.HEADER_HASH
  | Columns.CONSENSUS_PROFILE_ID
  | Columns.VERSION
  | Columns.PAYLOAD_CBOR
  | Columns.PAYLOAD_SHA256
  | Columns.UTXOS_ROOT
  | Columns.FORCED_TRANSACTIONS_ROOT
  | Columns.TRANSACTIONS_ROOT
  | Columns.DEPOSITS_ROOT
  | Columns.WITHDRAWALS_ROOT
  | Columns.TRANSITION_TRACE_ROOT
  | Columns.EVENT_TO_STEP_ROOT
  | Columns.VALIDATION_TRACES_ROOT
  | Columns.WITHDRAWAL_COUNT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.L2_TRANSACTION_COUNT
  | Columns.DEPOSIT_COUNT
  | Columns.TOTAL_EVENT_COUNT
  | Columns.TRANSITION_STEP_COUNT
  | Columns.VALIDATION_TRACE_COUNT
  | Columns.BLOCK_START_TIME
  | Columns.BLOCK_END_TIME
>;

export const upsertAvailable = (
  input: InsertInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        // Take the material-store lock before the DA row lock, matching prune.
        yield* sql`SELECT pg_advisory_xact_lock(${STORE_ADVISORY_LOCK_NAMESPACE}, ${STORE_ADVISORY_LOCK_KEY})`;
        const rows = yield* sql<Pick<Row, Columns.HEADER_HASH>>`
      INSERT INTO ${sql(tableName)} ${sql.insert(input)}
      ON CONFLICT (${sql(Columns.HEADER_HASH)}) DO UPDATE SET
        ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(tableName)}.${sql(Columns.VERSION)} = EXCLUDED.${sql(
        Columns.VERSION,
      )}
        AND ${sql(tableName)}.${sql(Columns.PAYLOAD_CBOR)} = EXCLUDED.${sql(
          Columns.PAYLOAD_CBOR,
        )}
        AND ${sql(tableName)}.${sql(Columns.PAYLOAD_SHA256)} = EXCLUDED.${sql(
          Columns.PAYLOAD_SHA256,
        )}
        AND ${sql(tableName)}.${sql(Columns.UTXOS_ROOT)} = EXCLUDED.${sql(
          Columns.UTXOS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.FORCED_TRANSACTIONS_ROOT,
        )} = EXCLUDED.${sql(Columns.FORCED_TRANSACTIONS_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.TRANSACTIONS_ROOT)} = EXCLUDED.${sql(
          Columns.TRANSACTIONS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.DEPOSITS_ROOT)} = EXCLUDED.${sql(
          Columns.DEPOSITS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWALS_ROOT)} = EXCLUDED.${sql(
          Columns.WITHDRAWALS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.TRANSITION_TRACE_ROOT,
        )} = EXCLUDED.${sql(Columns.TRANSITION_TRACE_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.EVENT_TO_STEP_ROOT)} = EXCLUDED.${sql(
          Columns.EVENT_TO_STEP_ROOT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.VALIDATION_TRACES_ROOT,
        )} = EXCLUDED.${sql(Columns.VALIDATION_TRACES_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWAL_COUNT)} = EXCLUDED.${sql(
          Columns.WITHDRAWAL_COUNT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.FORCED_TRANSACTION_COUNT,
        )} = EXCLUDED.${sql(Columns.FORCED_TRANSACTION_COUNT)}
        AND ${sql(tableName)}.${sql(
          Columns.L2_TRANSACTION_COUNT,
        )} = EXCLUDED.${sql(Columns.L2_TRANSACTION_COUNT)}
        AND ${sql(tableName)}.${sql(Columns.DEPOSIT_COUNT)} = EXCLUDED.${sql(
          Columns.DEPOSIT_COUNT,
        )}
        AND ${sql(tableName)}.${sql(Columns.TOTAL_EVENT_COUNT)} = EXCLUDED.${sql(
          Columns.TOTAL_EVENT_COUNT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.TRANSITION_STEP_COUNT,
        )} = EXCLUDED.${sql(Columns.TRANSITION_STEP_COUNT)}
        AND ${sql(tableName)}.${sql(
          Columns.VALIDATION_TRACE_COUNT,
        )} = EXCLUDED.${sql(Columns.VALIDATION_TRACE_COUNT)}
        AND ${sql(tableName)}.${sql(
          Columns.CONSENSUS_PROFILE_ID,
        )} = EXCLUDED.${sql(Columns.CONSENSUS_PROFILE_ID)}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_START_TIME)} = EXCLUDED.${sql(
          Columns.BLOCK_START_TIME,
        )}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_END_TIME)} = EXCLUDED.${sql(
          Columns.BLOCK_END_TIME,
        )}
      RETURNING ${sql(Columns.HEADER_HASH)}
    `;
        if (rows.length !== 1) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Refusing to overwrite DA payload because an existing payload for the header differs",
              cause: `header_hash=${input[Columns.HEADER_HASH].toString("hex")}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    Effect.withLogSpan(`upsertAvailable ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to store DA payload"),
  );

export const retrieveByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Row>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(normalizeRow(rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve DA payload"),
  );

/**
 * The node's authenticated L1 view of the state queue: the header hash in the
 * `ConfirmedState` datum and the hash of every header node currently in the
 * queue. The only payloads exempt from retention pruning.
 */
export type DaPayloadRetirementProof = Readonly<{
  headerHash: string;
  payloadSha256: string;
  transactionHash: string;
  blockHash: string;
  transitionDigest: string;
}>;

export type RetentionL1View = {
  readonly confirmedHeadHash: Buffer;
  readonly liveQueueHeaderHashes: readonly Buffer[];
  /** Fresh canonical proofs for these exact bytes and terminal identities. */
  readonly retirementProofs?: readonly DaPayloadRetirementProof[];
  readonly retirementProofUnavailable?: boolean;
};

/**
 * Holds the latest merge boundary, pending removals, and every terminal
 * outcome without a fresh canonical retirement proof for its exact point,
 * transition and original payload hash. Confirmation admission cannot retire
 * bytes; the signed automatic recovery horizon counts blocks AFTER inclusion.
 * Callers without fresh proof retain terminal bytes and retry later.
 */
const retirementProofKey = (proof: DaPayloadRetirementProof): string =>
  [
    proof.headerHash,
    proof.payloadSha256,
    proof.transactionHash,
    proof.blockHash,
    proof.transitionDigest,
  ].join(":");

/** SQL binding of a current proof to the original bytes and terminal identity. */
export const terminalRecoveryUnproven = (
  sql: SqlClient.SqlClient,
  deploymentIdentityDigest: Buffer,
  headerColumn: string,
  proofs: readonly DaPayloadRetirementProof[] = [],
) => sql`EXISTS (
  SELECT 1 FROM da_payload_terminal_outcomes terminal
  JOIN da_payloads bytes ON bytes.header_hash = terminal.header_hash
  WHERE terminal.header_hash = ${sql(headerColumn)}
    AND terminal.deployment_identity_digest = ${deploymentIdentityDigest}
    AND ${
      proofs.length === 0
        ? sql`TRUE`
        : sql`concat_ws(':', encode(terminal.header_hash, 'hex'),
      encode(bytes.payload_sha256, 'hex'), encode(terminal.transaction_hash, 'hex'),
      encode(terminal.block_hash, 'hex'), encode(terminal.transition_digest, 'hex')) NOT IN ${sql.in(proofs.map(retirementProofKey))}`
    })`;

export const finalityHeldPayload = (
  sql: SqlClient.SqlClient,
  deploymentIdentityDigest: Buffer | undefined,
  headerColumn: string = `${tableName}.${Columns.HEADER_HASH}`,
  proofs: readonly DaPayloadRetirementProof[] = [],
) =>
  deploymentIdentityDigest === undefined
    ? sql`FALSE`
    : sql`(${terminalRecoveryUnproven(sql, deploymentIdentityDigest, headerColumn, proofs)}
      OR ${observerCursorHeader(sql, headerColumn, deploymentIdentityDigest)}
      OR ${sql(headerColumn)} IN (
        SELECT latest.header_hash FROM (
          SELECT terminal.header_hash FROM da_payload_terminal_outcomes AS terminal
          WHERE terminal.terminal_outcome = 'merged'
            AND terminal.deployment_identity_digest = ${deploymentIdentityDigest}
          ORDER BY terminal.block_no DESC, terminal.transaction_index DESC
          LIMIT 1) AS latest
        UNION ALL
        SELECT decode(removed.header_hex, 'hex')
        FROM state_queue_terminal_observer_states AS observer,
          jsonb_array_elements(
            CASE jsonb_typeof(observer.state_record)
              WHEN 'string' THEN (observer.state_record #>> '{}')::jsonb
              ELSE observer.state_record
            END -> 'pending') AS pending(transition),
          jsonb_array_elements_text(
            pending.transition -> 'removedHeaderHashes') AS removed(header_hex)
        WHERE observer.deployment_identity_digest = ${deploymentIdentityDigest}
          AND removed.header_hex ~ '^[0-9a-f]{56}$'
        UNION ALL
        SELECT decode(removed.header_hex, 'hex')
        FROM state_queue_terminal_observer_states AS observer,
          jsonb_array_elements(
            CASE jsonb_typeof(observer.state_record)
              WHEN 'string' THEN (observer.state_record #>> '{}')::jsonb
              ELSE observer.state_record
            END -> 'admitted') AS admitted(transition),
          jsonb_array_elements_text(
            admitted.transition -> 'removedHeaderHashes') AS removed(header_hex)
        WHERE observer.deployment_identity_digest = ${deploymentIdentityDigest}
          AND removed.header_hex ~ '^[0-9a-f]{56}$'
          -- A revoke happens before the observer saves its restored cursor.
          -- A different terminal cannot release this admitted transition.
          -- Matching terminals are held above unless their exact bytes and
          -- identity have a fresh authenticated retirement proof.
          AND NOT EXISTS (
            SELECT 1 FROM da_payload_terminal_outcomes AS terminal
            WHERE terminal.deployment_identity_digest = ${deploymentIdentityDigest}
              AND terminal.header_hash = decode(removed.header_hex, 'hex')
              AND encode(terminal.transaction_hash, 'hex') = admitted.transition ->> 'transactionHash'
              AND encode(terminal.block_hash, 'hex') = admitted.transition ->> 'blockHash'
              AND encode(terminal.transition_digest, 'hex') = admitted.transition ->> 'transitionDigest')))`;

/**
 * Retention prune (GOAL_SPEC 9.4 / Q54), the SQL form of the core
 * `daRetentionPruneDecision`.
 *
 * Challengeability age or a removed outcome permits deletion only after
 * terminal recovery finality, and never for the confirmed head, live queue,
 * durable live cursor, pending/revoked admitted transition or a journal still
 * read by recovery. Fresh proofs are
 * compared again against the current terminal identity and byte hash in the
 * DELETE statement. Missing proof retains bytes; bounded sweeps retry it.
 */
export const pruneBeyondRetention = (args: {
  readonly challengeableCutoff: Date;
  readonly view: RetentionL1View;
  readonly deploymentIdentityDigest: Buffer | undefined;
}): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const exempt = [
      args.view.confirmedHeadHash,
      ...args.view.liveQueueHeaderHashes,
    ];
    const removed =
      args.deploymentIdentityDigest === undefined
        ? sql`FALSE`
        : sql`${sql(Columns.HEADER_HASH)} IN (
            SELECT terminal.header_hash FROM da_payload_terminal_outcomes AS terminal
            WHERE terminal.terminal_outcome = 'removed'
              AND terminal.deployment_identity_digest = ${args.deploymentIdentityDigest})`;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SELECT pg_advisory_xact_lock(${STORE_ADVISORY_LOCK_NAMESPACE}, ${STORE_ADVISORY_LOCK_KEY})`;
        const rows = yield* sql<{ readonly header_hash: Buffer }>`
      DELETE FROM ${sql(tableName)}
      WHERE (${sql(Columns.BLOCK_END_TIME)} < ${args.challengeableCutoff} OR ${removed})
        AND NOT ${sql.in(Columns.HEADER_HASH, exempt)}
        AND NOT ${finalityHeldPayload(sql, args.deploymentIdentityDigest, `${tableName}.${Columns.HEADER_HASH}`, args.view.retirementProofs)}
        AND NOT ${args.deploymentIdentityDigest === undefined ? sql`FALSE` : recoveryRelevantJournal(sql, Columns.HEADER_HASH, args.deploymentIdentityDigest)}
      RETURNING ${sql(Columns.HEADER_HASH)}`;
        yield* collectUnownedMaterial;
        return rows.length;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`pruneBeyondRetention ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune DA payloads"),
  );

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
