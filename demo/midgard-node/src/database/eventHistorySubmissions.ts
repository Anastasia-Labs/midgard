import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "event_history_submissions";

/** Explicit encodings preserve large quantities and Plutus maps on restart. */
export type StoredRequest = {
  readonly payloadCbor: string;
  readonly reclaimAuthCbor: string;
  readonly assets: Readonly<Record<string, string>>;
  readonly structuralLovelace: string;
  readonly structuralRefundKey: string;
  readonly nonce: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly address: string;
    readonly assets: Readonly<Record<string, string>>;
  };
};

export type Identity = {
  readonly submission_id: string;
  readonly kind: "Deposit" | "Withdrawal";
  readonly policy_id: string;
  readonly wallet_address: string;
  readonly intent_hash: string;
};

export type Row = Identity & {
  readonly nonce_out_ref: string;
  readonly request: StoredRequest;
  readonly checkpoint: SDK.EventHistorySubmissionCheckpoint;
  readonly revision: number;
};

export const matchesIdentity = (row: Identity, identity: Identity): boolean =>
  row.submission_id === identity.submission_id &&
  row.kind === identity.kind &&
  row.policy_id === identity.policy_id &&
  row.wallet_address === identity.wallet_address &&
  row.intent_hash === identity.intent_hash;

export const retrieve = (
  submissionId: string,
): Effect.Effect<Option.Option<Row>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<Row>`SELECT * FROM event_history_submissions WHERE submission_id = ${submissionId}`;
    return Option.fromNullable(rows[0]);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to retrieve history submission"),
  );

const reserveInputs = (submissionId: string, inputs: readonly string[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const outRef of [...new Set(inputs)].sort()) {
      yield* sql`INSERT INTO event_history_submission_inputs (out_ref, submission_id)
      VALUES (${outRef}, ${submissionId}) ON CONFLICT (out_ref) DO NOTHING`;
      const owners = yield* sql<{
        submission_id: string;
      }>`SELECT submission_id FROM event_history_submission_inputs WHERE out_ref = ${outRef}`;
      if (owners[0]?.submission_id !== submissionId)
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "History transaction input is reserved by another submission",
            cause: outRef,
          }),
        );
    }
  });

const pendingInputReferences = (
  checkpoint: SDK.EventHistorySubmissionCheckpoint,
) => {
  if (checkpoint.pending === undefined) return [];
  const body = CML.Transaction.from_cbor_hex(
    checkpoint.pending.transactionCbor,
  ).body();
  if (CML.hash_transaction(body).to_hex() !== checkpoint.pending.txHash)
    throw new Error("Pending history hash does not match its completed body");
  const refs: string[] = [];
  for (const inputs of [body.inputs(), body.collateral_inputs()]) {
    if (inputs === undefined) continue;
    for (let index = 0; index < inputs.len(); index++) {
      const input = inputs.get(index);
      refs.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
  }
  return refs;
};

/** ON CONFLICT never overwrites intent. Concurrent creators reload the winner;
 * a competing submission ID cannot reserve an already assigned nonce. */
export const reserve = (
  input: Omit<Row, "revision">,
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`
      INSERT INTO event_history_submissions
        (submission_id, kind, policy_id, wallet_address, intent_hash, nonce_out_ref, request, checkpoint)
      VALUES (${input.submission_id}, ${input.kind}, ${input.policy_id}, ${input.wallet_address},
        ${input.intent_hash}, ${input.nonce_out_ref},
        CAST(${JSON.stringify(input.request)} AS TEXT)::JSONB,
        CAST(${JSON.stringify(input.checkpoint)} AS TEXT)::JSONB)
      ON CONFLICT (submission_id) DO NOTHING`;
        const result = yield* retrieve(input.submission_id);
        if (Option.isNone(result) || !matchesIdentity(result.value, input))
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "History submission ID belongs to a different intent, wallet or deployment",
              cause: input.submission_id,
            }),
          );
        yield* reserveInputs(result.value.submission_id, [
          result.value.nonce_out_ref,
        ]);
        return result.value;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reserve history submission nonce",
    ),
  );

/** A stale process must reload/reconcile instead of replacing another body's
 * pending checkpoint. The caller resolves this write before any signature. */
export const saveCheckpoint = (
  row: Row,
  checkpoint: SDK.EventHistorySubmissionCheckpoint,
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (checkpoint.requestHash !== row.checkpoint.requestHash)
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Cannot replace history request identity",
          cause: row.submission_id,
        }),
      );
    const sql = yield* SqlClient.SqlClient;
    const inputs = yield* Effect.try({
      try: () => pendingInputReferences(checkpoint),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Invalid completed history transaction",
          cause,
        }),
    });
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows = yield* sql<Row>`UPDATE event_history_submissions
      SET checkpoint = CAST(${JSON.stringify(checkpoint)} AS TEXT)::JSONB,
          revision = revision + 1, updated_at = now()
      WHERE submission_id = ${row.submission_id} AND revision = ${row.revision}
      RETURNING *`;
        if (rows.length !== 1)
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Concurrent history submission changed; reload and reconcile before continuing",
              cause: row.submission_id,
            }),
          );
        yield* reserveInputs(row.submission_id, inputs);
        return rows[0]!;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to persist history submission checkpoint",
    ),
  );

export const reservedInputs = (
  walletAddress: string,
): Effect.Effect<ReadonlySet<string>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      out_ref: string;
    }>`SELECT inputs.out_ref FROM event_history_submission_inputs inputs
      JOIN event_history_submissions submissions USING (submission_id)
      WHERE submissions.wallet_address = ${walletAddress}`;
    return new Set(rows.map((row) => row.out_ref));
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read reserved history inputs",
    ),
  );
