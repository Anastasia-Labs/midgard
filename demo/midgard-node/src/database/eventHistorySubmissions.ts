import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Clock, Data, Duration, Effect, Option } from "effect";

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

/** Another submission already holds an input of the pending attempt. The
 * surrounding transaction rolled back, so nothing was recorded. */
export class HistoryInputReservedError extends Data.TaggedError(
  "HistoryInputReservedError",
)<{
  readonly message: string;
  readonly outRef: string;
  readonly holder: string | undefined;
}> {}

/** Inputs, collateral included, and validity upper bound (TTL slot) of a
 * completed attempt. */
export const attemptSpend = (attempt: SDK.EventHistorySubmissionAttempt) => {
  const body = CML.Transaction.from_cbor_hex(attempt.transactionCbor).body();
  if (CML.hash_transaction(body).to_hex() !== attempt.txHash)
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
  return { inputs: refs, ttl: body.ttl() };
};

/** Inputs and validity upper bound (TTL slot) of the pending attempt. */
const pendingSpend = (checkpoint: SDK.EventHistorySubmissionCheckpoint) =>
  checkpoint.pending === undefined
    ? { inputs: [], ttl: undefined }
    : attemptSpend(checkpoint.pending);

/** A holder can no longer spend a reserved input other than its nonce once
 * its current checkpoint has no pending attempt spending it (the attempt
 * settled or was rebuilt), or that attempt's validity ended below the caller's
 * observed L1 tip slot. An unreadable checkpoint counts as still spending. */
const holderReleased = (
  holder: Pick<Row, "nonce_out_ref" | "checkpoint">,
  outRef: string,
  tipSlot: number | undefined,
) => {
  if (outRef === holder.nonce_out_ref) return false;
  try {
    const { inputs, ttl } = pendingSpend(holder.checkpoint);
    return (
      !inputs.includes(outRef) ||
      (ttl !== undefined && tipSlot !== undefined && ttl < BigInt(tipSlot))
    );
  } catch {
    return false;
  }
};

/** Locks each input row in sorted order, inserted or already held. With
 * `takeover`, an input its holder can no longer spend moves to this
 * submission: the input row lock and the holder row share lock keep that
 * decision atomic with the holder's own saves. A holder saving right now is
 * live; skipping its locked row also keeps lock waits acyclic. */
const reserveInputs = (
  submissionId: string,
  inputs: readonly string[],
  takeover?: { readonly tipSlot: number | undefined },
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const outRef of [...new Set(inputs)].sort()) {
      const [owner] = yield* sql<{
        submission_id: string;
      }>`INSERT INTO event_history_submission_inputs (out_ref, submission_id)
      VALUES (${outRef}, ${submissionId})
      ON CONFLICT (out_ref) DO UPDATE SET submission_id = event_history_submission_inputs.submission_id
      RETURNING submission_id`;
      if (owner?.submission_id === submissionId) continue;
      const [holder] =
        takeover === undefined || owner === undefined
          ? []
          : yield* sql<Row>`SELECT * FROM event_history_submissions
            WHERE submission_id = ${owner.submission_id} FOR SHARE SKIP LOCKED`;
      if (
        holder !== undefined &&
        holderReleased(holder, outRef, takeover?.tipSlot)
      ) {
        yield* sql`UPDATE event_history_submission_inputs SET submission_id = ${submissionId}
          WHERE out_ref = ${outRef} AND submission_id = ${holder.submission_id}`;
        continue;
      }
      return yield* Effect.fail(
        new HistoryInputReservedError({
          message:
            "History transaction input is reserved by another submission",
          outRef,
          holder: owner?.submission_id,
        }),
      );
    }
  });

/** Every reservation of a wallet's outputs holds this transaction-scoped lock:
 * each checkpoint's inputs, and a fresh nonce from reading the reservations it
 * chooses among through reserving it. Postgres releases the lock when its
 * transaction ends or its connection dies, so a crashed holder never wedges
 * the wallet. */
const WALLET_RESERVATIONS_LOCK_NAMESPACE = 0x48_53_54_52;
const lockWalletReservations = (walletAddress: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`SELECT pg_advisory_xact_lock(${WALLET_RESERVATIONS_LOCK_NAMESPACE}, hashtext(${walletAddress}))`,
  );

/** Runs `choose` with the wallet's reservations locked, so no checkpoint of
 * another submission can reserve the nonce `choose` reads as unreserved
 * before `choose` reserves it. Such a save waits for the lock and then meets
 * the nonce's reservation. A `choose` that outlasts `holdTimeoutMs`, such as
 * on a hung provider call, is interrupted and rolled back: that frees the
 * wallet for every other submission, and this one fails having reserved
 * nothing, so a rerun chooses again. The bound runs on the wall clock, not on
 * the caller's clock, such as an emulator clock whose sleeps jump ahead. A
 * chooser that vanishes without closing its connection cannot run that
 * bound, so Postgres itself ends a transaction left idle past
 * `CHOOSER_IDLE_MARGIN_MS` beyond it, rather than at TCP keepalive. */
const wallClock = Clock.make();
const CHOOSER_IDLE_MARGIN_MS = 30_000;
export const choosingNonce = <A, E, R>(
  walletAddress: string,
  choose: Effect.Effect<A, E, R>,
  holdTimeoutMs = 60_000,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.zipRight(
        Effect.zipRight(
          sql`SELECT set_config('idle_in_transaction_session_timeout', ${(
            Math.ceil(holdTimeoutMs) + CHOOSER_IDLE_MARGIN_MS
          ).toString()}, true)`,
          lockWalletReservations(walletAddress),
        ),
        Effect.raceFirst(
          choose,
          Effect.zipRight(
            Effect.withClock(
              Effect.sleep(Duration.millis(holdTimeoutMs)),
              wallClock,
            ),
            Effect.fail(
              new DatabaseError({
                table: tableName,
                message: `Choosing a history nonce took over ${holdTimeoutMs.toString()} ms; nothing was reserved`,
                cause: walletAddress,
              }),
            ),
          ),
        ),
      ),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to choose a history nonce"),
  );

/** ON CONFLICT never overwrites intent. Concurrent creators reload the winner;
 * a competing submission ID cannot reserve an already assigned nonce. A wallet
 * output that another submission reserved only as an input it can no longer
 * spend (see `reservedInputs`) is taken over as this nonce. */
export const reserve = (
  input: Omit<Row, "revision">,
  l1TipSlot?: number,
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* lockWalletReservations(input.wallet_address);
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
        // A nonce held by another ID is a conflicting intent, not a wait.
        yield* reserveInputs(
          result.value.submission_id,
          [result.value.nonce_out_ref],
          { tipSlot: l1TipSlot },
        ).pipe(
          Effect.catchTag("HistoryInputReservedError", ({ message, outRef }) =>
            Effect.fail(
              new DatabaseError({ table: tableName, message, cause: outRef }),
            ),
          ),
        );
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
 * pending checkpoint. The caller resolves this write before any signature.
 * A pending input another submission can still spend fails with
 * HistoryInputReservedError and records nothing; `l1TipSlot`, when observed,
 * lets it take over inputs of a holder's expired attempt. The submission then
 * holds exactly its nonce and the new pending attempt's inputs: the workflow
 * drops or replaces a pending attempt only once it settled, so every other
 * reservation is released. */
export const saveCheckpoint = (
  row: Row,
  checkpoint: SDK.EventHistorySubmissionCheckpoint,
  l1TipSlot?: number,
): Effect.Effect<Row, DatabaseError | HistoryInputReservedError, Database> =>
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
    const { inputs } = yield* Effect.try({
      try: () => pendingSpend(checkpoint),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Invalid completed history transaction",
          cause,
        }),
    });
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* lockWalletReservations(row.wallet_address);
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
        yield* reserveInputs(row.submission_id, inputs, {
          tipSlot: l1TipSlot,
        });
        yield* sql`DELETE FROM event_history_submission_inputs
          WHERE submission_id = ${row.submission_id} AND out_ref <> ${rows[0]!.nonce_out_ref}
          AND NOT ${sql.in("out_ref", inputs)}`;
        return rows[0]!;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to persist history submission checkpoint",
    ),
  );

/** The wallet's outputs other submissions may still spend, as nonces or
 * inputs of their pending attempts. An input its holder can no longer spend at
 * `l1TipSlot` is left out, so a new nonce can take it over; otherwise a holder
 * that died with every wallet output in its attempt would starve every later
 * submission of that wallet. */
export const reservedInputs = (
  walletAddress: string,
  l1TipSlot?: number,
): Effect.Effect<ReadonlySet<string>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<
      Pick<Row, "nonce_out_ref" | "checkpoint"> & { out_ref: string }
    >`SELECT inputs.out_ref, submissions.nonce_out_ref, submissions.checkpoint
      FROM event_history_submission_inputs inputs
      JOIN event_history_submissions submissions USING (submission_id)
      WHERE submissions.wallet_address = ${walletAddress}`;
    return new Set(
      rows
        .filter((row) => !holderReleased(row, row.out_ref, l1TipSlot))
        .map((row) => row.out_ref),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read reserved history inputs",
    ),
  );

/** Every nonce the wallet's submissions hold. A nonce stays reserved after its
 * submission settles, so no other submission may spend it. */
export const reservedNonces = (
  walletAddress: string,
): Effect.Effect<ReadonlySet<string>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Pick<Row, "nonce_out_ref">>`SELECT nonce_out_ref
      FROM event_history_submissions WHERE wallet_address = ${walletAddress}`;
    return new Set(rows.map((row) => row.nonce_out_ref));
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read reserved history nonces",
    ),
  );
