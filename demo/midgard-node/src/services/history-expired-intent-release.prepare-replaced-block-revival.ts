import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import { retainedPreparedRecoveryPlan } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { invalidateSpeculativeCommitCandidate } from "../fibers/speculative-commit-builder.js";
import {
  type EventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { LEDGER_SCAN_TIMEOUT_MS } from "../l1-ledger-snapshot.js";
import { serializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import {
  journalAbandonment,
  reviveReplacedCanonicalJournal,
} from "./canonical-journal-recovery.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryOwnerChange } from "./event-history-owner.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import {
  includedInCanonicalHistory,
  replacedBlockLanding,
} from "./history-expired-intent-release.replaced-block-landing.js";
import {
  authenticateQueue,
  type QueueNode,
  signedCommitNode,
} from "./history-expired-intent-release.signed-commit-node.js";
import {
  anyActiveJournal,
  C,
  failure,
  reportOnce,
  type SignedIntentDeferral,
} from "./history-expired-intent-release.table.js";
import {
  loadStateQueueCorrectionObserverState,
  type StateQueueCorrectionRewindAuthority,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.js";

/** This node's replacement-abandoned journals whose blocks the correction
 * observer's persisted view shows on a state queue: its cursor queue, a queue
 * a recorded transition saw, or a block a recorded merge folded. A block a
 * recorded correction removed is excluded: its members stay reopened, which
 * is what that correction's path does anyway. A hint only: it is not bound to
 * the checkpoint, so it only selects which blocks the checkpoint-bound
 * evidence is read for. Sorted by header. */
const revivalCandidates = (
  authority: StateQueueCorrectionRewindAuthority,
  lock = false,
) =>
  Effect.gen(function* () {
    const observer = yield* loadStateQueueCorrectionObserverState(
      authority,
      lock,
    );
    if (observer.kind !== "observed") return [];
    const { cursorQueue, pending, admitted } = observer.state;
    const transitions = [...pending, ...admitted];
    const corrected = new Set(
      transitions
        .filter((transition) => transition.transitionKind !== "merge")
        .flatMap((transition) => transition.removedHeaderHashes),
    );
    const seen = new Set<string>();
    for (const node of [
      ...cursorQueue,
      ...transitions.flatMap((transition) => [
        ...transition.previousQueue,
        ...transition.nextQueue,
      ]),
    ])
      if (node.headerHash !== null) seen.add(node.headerHash);
    for (const transition of transitions)
      if (transition.transitionKind === "merge")
        for (const hash of transition.removedHeaderHashes) seen.add(hash);
    const hashes = [...seen].filter((hash) => !corrected.has(hash)).sort();
    if (hashes.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE status = ${Pending.Status.Abandoned}
        AND header_hash IN ${sql.in(hashes.map((hash) => Buffer.from(hash, "hex")))}
      ORDER BY header_hash`;
    const records: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        records.push(found.value);
    }
    return records;
  });

const revivalKey = (
  candidates: readonly Pending.Record[],
  point: { readonly id: string },
) =>
  `${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(",")}@${point.id}`;

/** Forward-append and resume disposition: pending when no journal is active
 * and the correction observer's view shows one of this node's replaced blocks
 * on a state queue (whichever lands wins: a replaced block that won its base's
 * slot, by landing late or through a rollback, is revived, otherwise the
 * commit worker refuses to build on it for good). Once no checkpoint-bound
 * evidence showed a candidate at a source point, it stays open until the next
 * point. SQL only. */
export const replacedBlockRevivalDisposition = (input: {
  readonly change: HistoryOwnerChange;
  readonly deferral: SignedIntentDeferral;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
}) =>
  Effect.gen(function* () {
    if (input.change.kind === "rollback" || input.change.kind === "seed")
      input.deferral.revival = undefined;
    if (yield* anyActiveJournal) return undefined;
    const candidates = yield* revivalCandidates(input.rewindAuthority);
    if (candidates.length === 0) return undefined;
    if (
      input.deferral.revival === revivalKey(candidates, input.change.after.head)
    )
      return undefined;
    return {
      status: "pending" as const,
      reason: `The state-queue correction observer shows replaced block ${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node on the state queue while no journal is active; a replaced block that holds its base's slot must be revived`,
    };
  });

/**
 * Recovery preparation: revives this node's replaced block when authenticated
 * evidence bound to the checkpoint shows it landed, while no journal is active
 * (with one active, `prepareExpiredIntentRelease` reconciles its base's slot,
 * reviving there). Evidence: its node on the exact-point queue, the confirmed
 * state equal to its header, its signed commit in the journaled canonical
 * history, or an admitted (final) merge that folded it or saw it on the
 * queue. The revival abandons nothing: with no journal active every sibling on
 * its base is already abandoned, which the revival itself enforces (a sibling
 * that landed or was locally finalized is an integrity failure). Its members
 * are taken back, its SQL marker moves to its candidate root, and local
 * finalization replays it natively. Defers while a correction rewind is owed
 * or any plan is retained.
 */
export const prepareReplacedBlockRevival = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly contracts: Pick<SDK.MidgardValidators, "stateQueue">;
  readonly deferral: SignedIntentDeferral;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation, config } = input;
    const reportKey = `${input.binding.digest}:revival`;
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    // Every candidate that is not excluded, or undefined when another recovery
    // goes first or a journal is active.
    const derive = Effect.gen(function* () {
      if (
        (yield* stateQueueCorrectionRewindDisposition(
          input.rewindAuthority,
        )) !== undefined ||
        (yield* retainedPreparedRecoveryPlan(input.binding.digest)) !==
          undefined ||
        (yield* anyActiveJournal)
      )
        return undefined;
      return yield* revivalCandidates(input.rewindAuthority, true);
    });
    yield* preparation.assertCurrent;
    const candidates = yield* owned(derive);
    if (candidates === undefined || candidates.length === 0) return;
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
          binding: input.binding,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure(
          `Exact-point state-queue capture failed: ${formatUnknownError(cause, { includeCause: true })}`,
          cause,
        ),
    });
    yield* preparation.assertCurrent;
    const queue = yield* authenticateQueue(
      capture.ledger.outputs,
      input.contracts,
    );
    const canonicalHistory = yield* owned(
      includedInCanonicalHistory(
        input.binding,
        checkpoint,
        candidates.flatMap((record) => {
          const hash = record[C.INTENDED_TX_HASH];
          return hash == null ? [] : [hash.toString("hex")];
        }),
      ),
    );
    // Which candidates the evidence shows landed, with their nodes. Read in
    // the recovery transaction, re-derived before anything is written.
    const landed = Effect.gen(function* () {
      const current = yield* derive;
      if (current === undefined) return [];
      const observer = yield* loadStateQueueCorrectionObserverState(
        input.rewindAuthority,
        true,
      );
      const found: {
        record: Pending.Record;
        node: QueueNode;
        evidence: string;
      }[] = [];
      for (const record of current) {
        const landing = replacedBlockLanding(
          record,
          queue,
          observer,
          canonicalHistory,
        );
        if (landing !== undefined)
          found.push({
            record,
            node:
              landing.onQueue ??
              (yield* signedCommitNode(record, input.contracts)),
            evidence: landing.evidence,
          });
      }
      if (found.length > 1)
        return yield* Effect.fail(
          failure(
            `Replaced blocks ${found.map(({ record }) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node all landed`,
          ),
        );
      return found;
    });
    const winners = yield* owned(landed);
    const header = winners[0]?.record[C.HEADER_HASH].toString("hex");
    if (header === undefined) {
      input.deferral.revival = revivalKey(candidates, checkpoint.head);
      yield* reportOnce(
        reportKey,
        `The correction observer shows replaced block ${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node on the state queue, but no evidence bound to checkpoint ${checkpoint.head.id} shows it landed; the next source point decides again.`,
      );
      return;
    }
    const winner = winners[0]!;
    const serialized = yield* serializeStateQueueUTxO(winner.node.node);
    const globals = yield* Globals;
    if (config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
    yield* owned(
      landed.pipe(
        Effect.flatMap((current) =>
          current.length === 1 &&
          current[0]!.record[C.HEADER_HASH].toString("hex") === header &&
          current[0]!.node.node.utxo.txHash === winner.node.node.utxo.txHash &&
          current[0]!.node.node.utxo.outputIndex ===
            winner.node.node.utxo.outputIndex
            ? reviveReplacedCanonicalJournal(winner.record[C.HEADER_HASH])
            : Effect.fail(
                failure(`The revival evidence for block ${header} changed`),
              ),
        ),
      ),
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
    yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
    yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, serialized);
    yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
    yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
    input.deferral.revival = undefined;
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logWarning(
      `Revived replaced block ${header}: ${winner.evidence}, and no journal was active; local finalization replays it.`,
    );
  });
