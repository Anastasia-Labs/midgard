import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { type EventHistorySourceBinding } from "../l1-event-history-source.js";
import type { HistoryOwnerChange } from "./event-history-owner.js";
import {
  loadStateQueueCorrectionObserverState,
  type StateQueueCorrectionRewindAuthority,
} from "./state-queue-correction-rewind.js";

/**
 * Reconciliation of a signed commit that missed its validity window, by the
 * owner ruling "whichever lands wins".
 *
 * Every commit of block E spends the state-queue tail node D it was built on,
 * and the queue validator appends only onto a node whose `next` is empty. So
 * E and any replacement built on D are mutually exclusive on L1: at most one
 * of them ever holds D's slot. The only hazard is local: the node must never
 * build on local state that is not the block that actually landed.
 *
 * This is the single place that decides. It runs in the history owner's
 * pending reconciliation because only there does the node hold an
 * authenticated, exact-point view of the queue bound to the canonical
 * checkpoint it has journaled, with native recovery plans and generation
 * fencing; it also runs at startup before hydration and the local job gate.
 * The confirmation worker only defers to it. Once the observed head slot has
 * reached the signed commit's TTL (its exclusive validity upper bound, so E
 * can never be included in any later block of this chain), authenticated
 * evidence decides. Evidence bound to the checkpoint decides first (the
 * exact-point queue and the canonical history the owner journaled); the
 * correction observer's persisted view of every state-queue removal
 * (corrections and merges, pending or admitted), which is not bound to the
 * checkpoint, only explains a correction of E or why neither E nor D is on
 * the checkpoint's queue.
 *  - E's own node is on the queue, or the confirmed state is E's header: E
 *    landed; its observation is recorded.
 *  - A correction removed E: E landed and was removed; the correction path
 *    owns its journal, so this defers (below). Never a landed block.
 *  - E's signed commit is in the journaled canonical history, or (neither E
 *    nor D on the queue) an observed merge folded E: E landed; its
 *    observation is recorded against the node its signed commit created, and
 *    local finalization replays it.
 *  - A correction removed D (neither E nor D on the queue): when this node
 *    journals D, the correction path owns E's journal (its rewind proves and
 *    abandons an unlanded descendant of a removed block), so this defers to
 *    it without holding the gate closed. An admitted correction's deferral
 *    is remembered for this journal until a rollback (the only way D
 *    returns) or a runtime restart; a pending one only until the observer's
 *    view changes, since it may be retracted. Without a journal of D nothing
 *    else ever resolves E, and the correction consumed the node E spends: E
 *    is replaced.
 *  - D's `next` is still empty, or holds a block that is not this node's
 *    replaced sibling of E (a foreign block): E is replaced. Its journal is
 *    abandoned under its replacement digest, its local-finalization job row
 *    and lease are retired, every member is reopened, the native root and SQL
 *    marker return to its base; the commit worker then builds anew.
 *  - D's `next` is an earlier journal of this node that was replaced on the
 *    same base (any generation): that block won after all (a rollback brought
 *    it back). The active journal is abandoned as above and the winner is
 *    revived with its members taken back; local finalization then replays it.
 *  - Neither E nor D on the queue, and the confirmed state links to D (a
 *    merge sets the confirmed predecessor to the header it folded over): the
 *    block it confirms took D's slot after D was merged, and decides as D's
 *    `next` does. This is checkpoint-bound and covers a D no removal ever
 *    names, such as the root E was built on, but only until the next merge:
 *    no queue output or transition links a confirmed header to anything
 *    after that.
 *  - A replaced sibling of E (this node's block built on the same base
 *    output, so its commit spends what E's spends) shows it landed, by the
 *    evidence the reviver reads: it holds the slot and is revived as D's
 *    `next` would be. This needs no link from the slot to the base.
 *  - E's base output is a root that an observed transition left with an
 *    empty queue (E was built on the root): the next observed transition
 *    names the block appended first after it, which spent that output, and it
 *    decides as D's `next` does (or, removed by a correction, E is replaced;
 *    this node's block only once that correction is admitted). The merge of
 *    D, made before E was built on the root, says nothing about E's slot.
 *  - D was merged into the confirmed state (an observed merge removed it):
 *    its successor at that merge decides as D's `next` does, and a D merged
 *    while it was still the tail means E can never land: E is replaced.
 *  - D absent with none of that recorded: nothing is decided and the gate
 *    stays open (E stays active, so nothing is built) until the observer's
 *    view changes. Nothing ever resolves it when E was built on a root that
 *    no observed transition produced (the genesis root, or one produced
 *    before the observer first ran), a foreign block took that slot, and two
 *    or more merges passed it before this decision.
 *  - Anything else (another own block of a different kind in D's slot, D's
 *    successor node absent) keeps the gate closed and says why.
 * A signed commit is never replaced before its TTL, and never on wall-clock
 * time or queue absence alone. With E's replacement plan already retained, a
 * landed E discards it (after replaying E natively when the plan was prepared
 * from E's candidate root, so its rewind may have run), and a deferral to the
 * correction path resumes it instead, since the correction path waits for
 * every retained plan.
 *
 * With no journal active, a replaced block of this node can still win its
 * base's slot (it landed late, or a rollback brought it back);
 * `prepareReplacedBlockRevival` revives it on the same checkpoint-bound
 * evidence, or an admitted merge that saw it on the queue.
 */

const table = "event_history_recovery_plans";

export const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });

export const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");

export const C = Pending.Columns;

export const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);

export const REPLACEMENT_EVIDENCE_DOMAIN =
  "midgard-signed-intent-replacement-evidence-v1";

/** Active journal statuses that record no L1 observation of the commit. */
export const UNLANDED_STATUSES: readonly Pending.Status[] = [
  Pending.Status.PendingSubmission,
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
];

const ACTIVE_STATUSES: readonly Pending.Status[] = [
  ...UNLANDED_STATUSES,
  Pending.Status.ObservedWaitingStability,
];

/** The signed validity upper bound (TTL, an exclusive slot bound), or
 * undefined when the bytes do not decode or carry none: such an intent can
 * never be shown unable to land, so it is never replaced. */
export const signedTtl = (cbor: Buffer): bigint | undefined => {
  let tx: CML.Transaction | undefined;
  try {
    tx = CML.Transaction.from_cbor_bytes(cbor);
    const body = tx.body();
    const ttl = body.ttl();
    body.free();
    return ttl;
  } catch {
    return undefined;
  } finally {
    tx?.free();
  }
};

type ActiveSignedIntent = Readonly<{
  headerHash: Buffer;
  status: Pending.Status;
  signedTxCbor: Buffer;
  intendedTxHash: Buffer;
}>;

/** The node's single active journal when it holds a signed intent and records
 * no L1 observation. At most one journal is active at a time. */
export const activeSignedIntent = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    status: Pending.Status;
    signed_tx_cbor: Buffer | null;
    intended_tx_hash: Buffer | null;
  }>`SELECT header_hash, status, signed_tx_cbor, intended_tx_hash
    FROM pending_block_finalizations WHERE status IN ${sql.in(ACTIVE_STATUSES)}`;
  if (rows.length !== 1) return undefined;
  const row = rows[0]!;
  if (
    !UNLANDED_STATUSES.includes(row.status) ||
    row.signed_tx_cbor === null ||
    row.intended_tx_hash === null
  )
    return undefined;
  return {
    headerHash: row.header_hash,
    status: row.status,
    signedTxCbor: row.signed_tx_cbor,
    intendedTxHash: row.intended_tx_hash,
  } satisfies ActiveSignedIntent;
});

/** Whether any journal is active, landed or not. */
export const anyActiveJournal = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
    FROM pending_block_finalizations WHERE status IN ${sql.in(ACTIVE_STATUSES)}
    LIMIT 1`;
  return rows.length > 0;
});

/** Last reported reason per binding and topic: WARN when it first appears or
 * changes, debug while it persists. */
const reported = new Map<string, string>();

export const reportOnce = (key: string, message: string | undefined) =>
  Effect.suspend(() => {
    if (message === undefined) {
      reported.delete(key);
      return Effect.void;
    }
    if (reported.get(key) === message) return Effect.logDebug(message);
    reported.set(key, message);
    return Effect.logWarning(message);
  });

/** The one signed intent (header and intended transaction) whose
 * reconciliation defers: to the correction path (`current`, until a rollback;
 * only on an admitted correction), or until the correction observer's
 * persisted view changes (`untilObserved`: it has not yet recorded why its
 * base left the queue, or has recorded only a correction not yet admitted;
 * without a change of that view or a rollback, the same evidence decides the
 * same way, so nothing is captured again). `revival`: the replaced blocks and
 * source point at which no authenticated evidence showed one of them holding
 * its base's slot. Owned by one history runtime: a restart starts empty. */
export type SignedIntentDeferral = {
  current: string | undefined;
  untilObserved: string | undefined;
  revival: string | undefined;
};

export const makeSignedIntentDeferral = (): SignedIntentDeferral => ({
  current: undefined,
  untilObserved: undefined,
  revival: undefined,
});

/** The correction observer's persisted view, named by its state digest (or
 * why it has none). */
export const observerFingerprint = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  loadStateQueueCorrectionObserverState(authority).pipe(
    Effect.map((observer) =>
      observer.kind === "observed"
        ? `observed:${observer.state.stateDigest}`
        : `blocked:${observer.reason}`,
    ),
  );

export const deferredUntilObserved = (key: string, fingerprint: string) =>
  `${key}#${fingerprint}`;

export const deferralKey = (intent: {
  readonly headerHash: Buffer;
  readonly intendedTxHash: Buffer;
}) =>
  `${intent.headerHash.toString("hex")}:${intent.intendedTxHash.toString("hex")}`;

/** Forward-append and resume disposition: pending exactly when the active
 * signed intent's TTL has been reached at this checkpoint, so the gate closes
 * and recovery reconciles its base's state-queue slot. Before the TTL the
 * normal confirmation path stays in charge, and after a deferral to the
 * correction path (its base was removed) it stays open until a rollback, and
 * after a deferral to the correction observer until its view changes. SQL
 * only; the reason is stable while the journal is unchanged, so the owner's
 * retry backoff applies. */
export const expiredIntentReleaseDisposition = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly change: HistoryOwnerChange;
  readonly deferral: SignedIntentDeferral;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    // Only a rollback (or a fresh seed) can bring a removed base back, or
    // change the evidence a deferral was decided on without the observer.
    if (input.change.kind === "rollback" || input.change.kind === "seed") {
      input.deferral.current = undefined;
      input.deferral.untilObserved = undefined;
      input.deferral.revival = undefined;
    }
    const intent = yield* activeSignedIntent;
    if (intent === undefined) return undefined;
    const key = deferralKey(intent);
    if (input.deferral.current === key) return undefined;
    if (
      input.deferral.untilObserved?.startsWith(`${key}#`) === true &&
      input.deferral.untilObserved ===
        deferredUntilObserved(
          key,
          yield* observerFingerprint(input.rewindAuthority),
        )
    )
      return undefined;
    const header = intent.headerHash.toString("hex");
    const ttl = signedTtl(intent.signedTxCbor);
    yield* reportOnce(
      `${input.binding.digest}:ttl`,
      ttl === undefined
        ? `Signed commit intent of block ${header} does not decode to a transaction with a finite validity upper bound; it is never replaced and stays fail-closed.`
        : undefined,
    );
    if (ttl === undefined || BigInt(input.change.after.head.slot) < ttl)
      return undefined;
    return {
      status: "pending" as const,
      reason: `Signed commit ${intent.intendedTxHash.toString("hex")} of block ${header} reached its validity upper bound (TTL slot ${ttl.toString()}) unobserved; whichever block holds its base's state-queue slot must be reconciled`,
    };
  });
