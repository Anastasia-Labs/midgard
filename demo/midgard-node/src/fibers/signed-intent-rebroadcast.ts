/**
 * Rebroadcast of a signed commit intent that was persisted but never
 * accepted: the provider refused it as not yet valid (OutsideValidityInterval,
 * which in no-inline mode ends in a provider-slot or early-validity defer
 * rather than an inline wait), or the node stopped between persisting the
 * intent and submitting it. A pre-submit defer persists nothing. The journal
 * row stays `pending_submission` with its signed bytes and no submitted hash,
 * and no new block is built while it is active, so without a resubmission the
 * intent only resolves at its TTL.
 *
 * Owner: this small fiber. The commit worker's slot-aware due work does not
 * suffice: it is in-memory (a restart between persist and submit loses it),
 * and it schedules a build, which is refused while the journal is active and
 * would sign different bytes if it were not. The confirmation worker runs a
 * read-and-confirm pass in a worker thread and defers signed intents to the
 * history owner, which decides only once the intent cannot land. The fiber
 * holds the L1 control plane only when it is free (never while a commit is
 * being built or submitted).
 *
 * It resubmits exactly the journaled bytes (whichever commit on the base
 * lands wins, as for any signed commit) while the ledger tip has reached
 * their validity lower bound, is still before their TTL, and their base
 * output is unspent, backing off between attempts. Once the provider has
 * accepted them it stops: it resubmits again only if they leave the mempool
 * without landing, which it takes from their base output (which they spend)
 * still unspent a bounded wait after the acceptance. A refusal is warned once
 * per intent. It never writes the journal: `submitted_tx_hash` stays unset
 * until block confirmation records the landing of the intended transaction
 * (as for an intent whose submit response was lost), and an intent that can
 * no longer land (TTL reached, base spent) is reconciled by the history
 * owner.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Cause, Effect, Option, Ref, type Schedule } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { withL1ControlPlaneIfAvailable } from "../services/globals.with-l1-control-plane-held.js";
import { type Database, Globals, Lucid } from "../services/index.js";

/** First attempt after the intent is first seen (a commit worker that just
 * persisted it submits it itself), then doubling to the cap. */
export const REBROADCAST_INITIAL_DELAY_MS = 2_000;
export const REBROADCAST_MAX_DELAY_MS = 30_000;

/** How long accepted bytes may wait in the mempool, their base output still
 * unspent, before they are taken to have left it without landing. */
export const REBROADCAST_ACCEPTED_WAIT_MS = 60_000;

export const rebroadcastDelayMs = (attempts: number) =>
  Math.min(
    REBROADCAST_MAX_DELAY_MS,
    REBROADCAST_INITIAL_DELAY_MS * 2 ** Math.max(0, attempts - 1),
  );

/** The persisted, never-accepted signed intent, decoded. */
export type UnacceptedSignedIntent = Readonly<{
  headerHash: string;
  txHash: string;
  signedTxCbor: string;
  baseOutRef: string;
  invalidBefore: bigint | undefined;
  ttl: bigint | undefined;
}>;

/** Per-intent backoff, keyed by header and transaction: `acceptedAtMs` while
 * the provider's acceptance of the bytes stands, `warned` once a refusal was
 * warned. */
export type RebroadcastState = Map<
  string,
  {
    attempts: number;
    nextAtMs: number;
    acceptedAtMs?: number | undefined;
    warned?: boolean;
  }
>;

export type RebroadcastOutcome =
  | "none"
  | "first_seen"
  | "backoff"
  | "accepted"
  | "no_ttl"
  | "ttl_reached"
  | "not_yet_valid"
  | "base_spent"
  | "submitted"
  | "submit_failed";

export type RebroadcastDeps<R> = Readonly<{
  intent: Effect.Effect<UnacceptedSignedIntent | undefined, unknown, R>;
  tipSlot: Effect.Effect<number, unknown, R>;
  baseUnspent: (outRef: string) => Effect.Effect<boolean, unknown, R>;
  submit: (signedTxCbor: string) => Effect.Effect<string, unknown, R>;
  nowMs: () => number;
}>;

const intentKey = (intent: UnacceptedSignedIntent) =>
  `${intent.headerHash}:${intent.txHash}`;

/** One pass: resubmits the intent's exact bytes when it is due and eligible.
 * A check that finds it not eligible is not an attempt. */
export const rebroadcastOnce = <R>(
  deps: RebroadcastDeps<R>,
  state: RebroadcastState,
): Effect.Effect<RebroadcastOutcome, unknown, R> =>
  Effect.gen(function* () {
    const intent = yield* deps.intent;
    const key = intent === undefined ? undefined : intentKey(intent);
    for (const known of [...state.keys()])
      if (known !== key) state.delete(known);
    if (intent === undefined || key === undefined) return "none";
    const now = deps.nowMs();
    const entry = state.get(key);
    if (entry === undefined) {
      state.set(key, {
        attempts: 0,
        nextAtMs: now + REBROADCAST_INITIAL_DELAY_MS,
      });
      return "first_seen";
    }
    if (now < entry.nextAtMs)
      return entry.acceptedAtMs === undefined ? "backoff" : "accepted";
    if (intent.ttl === undefined) return "no_ttl";
    const tip = BigInt(yield* deps.tipSlot);
    if (tip >= intent.ttl) return "ttl_reached";
    if (intent.invalidBefore !== undefined && tip < intent.invalidBefore)
      return "not_yet_valid";
    if (!(yield* deps.baseUnspent(intent.baseOutRef))) return "base_spent";
    if (entry.acceptedAtMs !== undefined) {
      yield* Effect.logWarning(
        `Signed commit ${intent.txHash} of block ${intent.headerHash}, accepted ${(now - entry.acceptedAtMs).toString()}ms ago, has not landed and its base output ${intent.baseOutRef} is unspent at tip slot ${tip.toString()}: it left the mempool, so its exact bytes are resubmitted.`,
      );
      entry.acceptedAtMs = undefined;
    }
    entry.attempts += 1;
    entry.nextAtMs = now + rebroadcastDelayMs(entry.attempts);
    const submitted = yield* Effect.either(deps.submit(intent.signedTxCbor));
    if (submitted._tag === "Left") {
      const message = `Rebroadcast ${entry.attempts.toString()} of signed commit ${intent.txHash} of block ${intent.headerHash} failed at tip slot ${tip.toString()}; retrying in ${rebroadcastDelayMs(entry.attempts).toString()}ms while it can land: ${formatUnknownError(submitted.left, { includeCause: true })}`;
      yield* entry.warned === true
        ? Effect.logDebug(message)
        : Effect.logWarning(message);
      entry.warned = true;
      return "submit_failed";
    }
    if (submitted.right !== intent.txHash)
      return yield* Effect.fail(
        new Error(
          `Rebroadcast of signed commit ${intent.txHash} returned a different transaction hash ${submitted.right}`,
        ),
      );
    entry.acceptedAtMs = now;
    entry.nextAtMs = now + REBROADCAST_ACCEPTED_WAIT_MS;
    yield* Effect.logInfo(
      `Rebroadcast signed commit ${intent.txHash} of block ${intent.headerHash} (attempt ${entry.attempts.toString()}, tip slot ${tip.toString()}, validity [${intent.invalidBefore?.toString() ?? "-"}, ${intent.ttl.toString()})); block confirmation records its landing, and it is not resubmitted unless it leaves the mempool without landing.`,
    );
    return "submitted";
  });

/** Decodes a journaled signed commit; bytes that do not hash to the intended
 * transaction are never resubmitted. */
export const decodeUnacceptedSignedIntent = (row: {
  readonly header_hash: Buffer;
  readonly intended_tx_hash: Buffer;
  readonly signed_tx_cbor: Buffer;
  readonly base_tail_out_ref: string;
}): UnacceptedSignedIntent => {
  const tx = CML.Transaction.from_cbor_bytes(row.signed_tx_cbor);
  try {
    const body = tx.body();
    const hash = CML.hash_transaction(body);
    const txHash = hash.to_hex();
    const invalidBefore = body.validity_interval_start();
    const ttl = body.ttl();
    hash.free();
    body.free();
    if (txHash !== row.intended_tx_hash.toString("hex"))
      throw new Error(
        `signed commit bytes of block ${row.header_hash.toString("hex")} hash to ${txHash}, not its intended transaction`,
      );
    return {
      headerHash: row.header_hash.toString("hex"),
      txHash,
      signedTxCbor: row.signed_tx_cbor.toString("hex"),
      baseOutRef: row.base_tail_out_ref,
      invalidBefore,
      ttl,
    };
  } finally {
    tx.free();
  }
};

/** The active journal's signed intent when it was persisted and never
 * accepted. */
export const unacceptedSignedIntent = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    intended_tx_hash: Buffer;
    signed_tx_cbor: Buffer;
    base_tail_out_ref: string;
  }>`SELECT header_hash, intended_tx_hash, signed_tx_cbor, base_tail_out_ref
    FROM pending_block_finalizations
    WHERE status = ${Pending.Status.PendingSubmission}
      AND intended_tx_hash IS NOT NULL
      AND signed_tx_cbor IS NOT NULL
      AND submitted_tx_hash IS NULL`;
  if (rows.length !== 1) return undefined;
  return yield* Effect.try(() => decodeUnacceptedSignedIntent(rows[0]!));
});

/** The node's dependencies: the journal, the ledger tip slot, the
 * provider's view of the base output, and exact-bytes submission. */
export const liveRebroadcastDeps = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const sql = yield* SqlClient.SqlClient;
  return {
    intent: unacceptedSignedIntent.pipe(
      Effect.provideService(SqlClient.SqlClient, sql),
    ),
    // The mempool checks the validity interval against the ledger tip, which
    // the wall-clock submit slot runs ahead of between blocks.
    tipSlot: lucid
      .submitSlotSnapshot()
      .pipe(
        Effect.map(
          (snapshot) => snapshot.ledgerTipSlot ?? snapshot.currentSlot,
        ),
      ),
    baseUnspent: (outRef: string) => {
      const [txHash, outputIndex] = outRef.split("#");
      return Effect.tryPromise(() =>
        lucid.api.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(outputIndex) },
        ]),
      ).pipe(Effect.map((utxos) => utxos.length > 0));
    },
    submit: (signedTxCbor: string) =>
      Effect.gen(function* () {
        const provider = lucid.api.config().provider;
        if (provider === undefined)
          return yield* Effect.fail(new Error("L1 provider unavailable"));
        return yield* Effect.tryPromise(() => provider.submitTx(signedTxCbor));
      }),
    nowMs: () => Date.now(),
  } satisfies RebroadcastDeps<never>;
});

export type RebroadcastTickOutcome =
  | RebroadcastOutcome
  | "reset_in_progress"
  | "commit_worker_active"
  | "control_plane_busy"
  | "failed";

/** The failure last warned about: a failure that persists (journaled bytes
 * that do not decode or hash to the intended transaction, an unavailable
 * provider) is warned once, then logged at debug until it changes. */
export type RebroadcastFailures = { last: string | undefined };

/** One scheduled tick: never while a reset or the commit worker runs, and
 * only when the L1 control plane is free. */
export const rebroadcastTick = <R>(
  globals: Pick<
    Globals,
    "RESET_IN_PROGRESS" | "COMMIT_WORKER_ACTIVE" | "L1_CONTROL_PLANE"
  >,
  deps: RebroadcastDeps<R>,
  state: RebroadcastState,
  failures: RebroadcastFailures,
): Effect.Effect<RebroadcastTickOutcome, never, R> =>
  Effect.gen(function* () {
    if (yield* Ref.get(globals.RESET_IN_PROGRESS)) return "reset_in_progress";
    if (yield* Ref.get(globals.COMMIT_WORKER_ACTIVE))
      return "commit_worker_active";
    const held = yield* withL1ControlPlaneIfAvailable(
      globals as Globals,
      { scope: "signed_intent_rebroadcast", maxHoldMs: 60_000 },
      rebroadcastOnce(deps, state),
    );
    // A busy control plane ran no pass, so it says nothing about the failure.
    if (Option.isSome(held)) failures.last = undefined;
    return Option.getOrElse(held, () => "control_plane_busy" as const);
  }).pipe(
    Effect.catchAllCause((cause) => {
      const message = `Signed-intent rebroadcast failed: ${formatUnknownError(Cause.squash(cause), { includeCause: true })}`;
      const repeated = failures.last === message;
      failures.last = message;
      return (
        repeated ? Effect.logDebug(message) : Effect.logWarning(message)
      ).pipe(Effect.as("failed" as const));
    }),
  );

/** The scheduled loop over `deps`, with its own state. */
export const runRebroadcastFiber = <R>(
  globals: Parameters<typeof rebroadcastTick>[0],
  deps: RebroadcastDeps<R>,
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, R> =>
  Effect.gen(function* () {
    const state: RebroadcastState = new Map();
    const failures: RebroadcastFailures = { last: undefined };
    yield* Effect.logInfo("Signed-intent rebroadcast fiber started.");
    const tick = rebroadcastTick(globals, deps, state, failures).pipe(
      Effect.withSpan("signed-intent-rebroadcast-fiber"),
    );
    yield* Effect.repeat(tick, schedule);
  });

export const signedIntentRebroadcastFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Globals | Lucid | Database> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const deps = yield* liveRebroadcastDeps;
    yield* runRebroadcastFiber(globals, deps, schedule);
  });
