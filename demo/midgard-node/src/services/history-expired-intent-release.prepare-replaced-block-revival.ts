import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import { retainedPreparedRecoveryPlan } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import type { DatabaseError } from "../database/utils/common.js";
import {
  type EventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { LEDGER_SCAN_TIMEOUT_MS } from "../l1-ledger-snapshot.js";
import { serializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import {
  REVIVAL_BLOCKING_SIBLING_STATUSES,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryOwnerChange } from "./event-history-owner.js";
import {
  type HistoryRecoveryPreparation,
  HistoryRecoverySuperseded,
} from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import {
  journalBase,
  sameBaseJournals,
} from "./history-expired-intent-release.base-spend.js";
import { compensateDisplacement } from "./history-expired-intent-release.compensate-displacement.js";
import {
  displacement,
  observedRemoval,
} from "./history-expired-intent-release.displacement.js";
import { heldOnIntegrityFailure } from "./history-expired-intent-release.integrity-hold.js";
import {
  displacementIdentity,
  recoverDisplacement,
} from "./history-expired-intent-release.recover-displacement.js";
import {
  canonicalEvidence,
  replacedBlockLanding,
} from "./history-expired-intent-release.replaced-block-landing.js";
import { resumeRetainedDisplacement } from "./history-expired-intent-release.resume-displacement.js";
import {
  type LandedReplacement,
  returnedReplacementChain,
} from "./history-expired-intent-release.returned-chain.js";
import {
  revivalCandidates,
  revivalKey,
} from "./history-expired-intent-release.revival-candidates.js";
import { reviveOver } from "./history-expired-intent-release.revive-over.js";
import {
  authenticateQueue,
  requireHeaderBoundBase,
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
  clearLivenessIncident,
  clearLivenessReasonIf,
  HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import {
  SIGNED_INTENT_UNDECIDED,
  SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
} from "./signed-intent-undecided.js";
import {
  loadStateQueueCorrectionObserverState,
  type StateQueueCorrectionRewindAuthority,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.js";

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
    // With a journal active its release reconciles the base slot instead,
    // and with no candidate no revival is in question: either way, whatever
    // the revival raised no longer applies.
    const settled = Effect.flatMap(Globals, (globals) =>
      clearLivenessIncident(globals, HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE),
    ).pipe(Effect.as(undefined));
    if (yield* anyActiveJournal) return yield* settled;
    const candidates = yield* revivalCandidates(input.rewindAuthority);
    if (candidates.length === 0) return yield* settled;
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
 * queue. With no journal active every unlanded sibling on its base is already
 * abandoned. A locally finalized sibling is abandoned first, in the same
 * transaction, once L1 evidence shows a rollback displaced it (see
 * `displacement`); until then the revival is undecided, and the gate stays
 * closed with `signed_intent_undecided` raised. Two landed blocks of one
 * base are an integrity failure, held (see `heldOnIntegrityFailure`). Its members
 * are taken back, its SQL marker moves to its candidate root, and local
 * finalization replays it natively. Defers while a correction rewind is owed
 * or any plan is retained.
 */
export type ReplacedBlockRevivalInput = {
  readonly binding: EventHistorySourceBinding;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly contracts: Pick<SDK.MidgardValidators, "stateQueue">;
  readonly deferral: SignedIntentDeferral;
};
export const prepareReplacedBlockRevival = (input: ReplacedBlockRevivalInput) =>
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
    const original = yield* owned(
      retainedPreparedRecoveryPlan(input.binding.digest),
    );
    if (original?.kind === "displacement_compensation") {
      yield* compensateDisplacement(input, {
        recoveryId: original.recoveryId,
        originalRecoveryId: original.intent.originalRecoveryId,
        originalIntent: original.intent.originalIntent,
      });
      return;
    }
    if (original?.kind === "displaced_block_revival") {
      const resumed = yield* resumeRetainedDisplacement(input, original);
      if (resumed.kind !== "resume") return;
    }
    // Every candidate that is not excluded, or undefined when another recovery
    // goes first or a journal is active.
    const derive = Effect.gen(function* () {
      const retained = yield* retainedPreparedRecoveryPlan(
        input.binding.digest,
      );
      if (
        (yield* stateQueueCorrectionRewindDisposition(
          input.rewindAuthority,
        )) !== undefined ||
        (retained !== undefined &&
          retained.kind !== "displaced_block_revival") ||
        (yield* anyActiveJournal)
      )
        return undefined;
      const candidates = yield* revivalCandidates(input.rewindAuthority, true);
      if (
        retained?.kind === "displaced_block_revival" &&
        !candidates.some(
          (record) =>
            record[C.HEADER_HASH].toString("hex") === retained.headerHash,
        )
      ) {
        const found = yield* Pending.retrieveByHeaderHash(
          Buffer.from(retained.headerHash, "hex"),
          true,
        );
        if (Option.isSome(found)) candidates.push(found.value);
      }
      return candidates;
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
    // One read of the canonical history: which candidates' signed commits it
    // includes, and how deep each transaction in it is (which decides whether
    // a winner that holds its base's slot displaced a locally finalized
    // sibling).
    const { canonicalHistory, canonicalDepth: depth } = yield* owned(
      canonicalEvidence(
        input.binding,
        checkpoint,
        candidates.flatMap((record) => {
          const hash = record[C.INTENDED_TX_HASH];
          return hash == null ? [] : [hash.toString("hex")];
        }),
      ),
    );
    // Which candidate the evidence shows landed, with its node, and what its
    // revival must first abandon: the locally finalized blocks on its base
    // that L1 evidence shows an L1 rollback displaced (see `displacement`),
    // or why that is not shown yet. Read in the recovery transaction, and
    // re-derived there before anything is written.
    const assess = Effect.gen(function* () {
      const current = yield* derive;
      if (current === undefined) return undefined;
      const observer = yield* loadStateQueueCorrectionObserverState(
        input.rewindAuthority,
        true,
      );
      const found: LandedReplacement[] = [];
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
            node: landing.onQueue,
            queued: landing.onQueue !== undefined,
            evidence: landing.evidence,
          });
      }
      // Coexisting ancestor+descendants are recovered parent-first. Competing
      // siblings or historical-only multiple hints remain an integrity hold.
      const chain =
        found.length > 1
          ? returnedReplacementChain(found, queue, checkpoint.manifestId)
          : found;
      if (chain === undefined)
        return yield* Effect.fail(
          new SignedIntentReplacementIntegrityError(
            found[0]!.record[C.HEADER_HASH].toString("hex"),
            `replaced blocks ${found.map(({ record }) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node all landed`,
          ),
        );
      const landed = chain[0];
      if (landed === undefined) return undefined;
      const winner = {
        ...landed,
        node:
          landed.node ??
          (yield* signedCommitNode(landed.record, input.contracts)),
      };
      const baseHeader = winner.record[C.BASE_TAIL_HEADER_HASH];
      const parent = yield* Pending.retrieveByHeaderHash(baseHeader);
      if (
        observedRemoval(observer, baseHeader.toString("hex"), false) !==
          undefined ||
        (Option.isSome(parent) &&
          parent.value[C.STATUS] === Pending.Status.Abandoned)
      )
        return {
          kind: "undecided" as const,
          winner,
          reason: `${SIGNED_INTENT_UNDECIDED}: replaced block ${winner.record[C.HEADER_HASH].toString("hex")} extends corrected or abandoned base ${baseHeader.toString("hex")}; its removal or authenticated canonical parent recovery must resolve before replay`,
        };
      // The winner's base root is the revival's target root: its retained
      // parent journal's root, or, with none, named by its own header.
      if (Option.isNone(parent))
        yield* requireHeaderBoundBase(winner.record, "replaced block");
      else if (
        parent.value[C.EXPECTED_UTXOS_ROOT] !== winner.record[C.BASE_UTXOS_ROOT]
      )
        return yield* Effect.fail(
          new SignedIntentReplacementIntegrityError(
            winner.record[C.HEADER_HASH].toString("hex"),
            `the replay base of replaced block ${winner.record[C.HEADER_HASH].toString("hex")} is not its retained parent's root`,
          ),
        );
      const blocking = (yield* sameBaseJournals(journalBase(winner.record), [
        winner.record[C.HEADER_HASH],
      ])).filter(({ status }) =>
        REVIVAL_BLOCKING_SIBLING_STATUSES.includes(status),
      );
      if (blocking.length === 0)
        return { kind: "revive" as const, winner, displaced: [] };
      const finalized = blocking
        .map(
          ({ header_hash, status }) =>
            `block ${header_hash.toString("hex")} built on the same base is already ${status}`,
        )
        .join("; ");
      const displaced =
        depth === undefined
          ? "the canonical history coverage is unavailable"
          : yield* displacement({
              blocking: blocking.map(({ header_hash }) =>
                header_hash.toString("hex"),
              ),
              node: winner.node,
              queued: winner.queued,
              winner: winner.record,
              base: winner.record[C.BASE_TAIL_HEADER_HASH].toString("hex"),
              baseRoot: winner.record[C.BASE_UTXOS_ROOT],
              queue,
              observer,
              depth,
              required: input.rewindAuthority.requiredFinalityDepth,
            });
      return typeof displaced === "string"
        ? {
            kind: "undecided" as const,
            winner,
            reason: `${SIGNED_INTENT_UNDECIDED}: this node's replaced block ${winner.record[C.HEADER_HASH].toString("hex")} holds its base's slot, but ${finalized}, and ${displaced}`,
          }
        : { kind: "revive" as const, winner, displaced };
    });
    const globals = yield* Globals;
    const assessed = yield* owned(assess);
    if (assessed === undefined) {
      input.deferral.revival = revivalKey(candidates, checkpoint.head);
      yield* clearLivenessReasonIf(
        globals,
        HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
        SIGNED_INTENT_UNDECIDED,
      );
      yield* reportOnce(
        reportKey,
        `The correction observer shows replaced block ${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(", ")} of this node on the state queue, but no evidence bound to checkpoint ${checkpoint.head.id} shows it landed; the next source point decides again.`,
      );
      return;
    }
    const { winner } = assessed;
    const header = winner.record[C.HEADER_HASH].toString("hex");
    if (assessed.kind === "undecided") {
      // Neither the winner nor the locally finalized sibling can be taken
      // without writing over the other yet: the gate stays closed (no
      // deferral), block production with it, and every evaluation decides
      // again until the winner's commit is deep enough or a rollback settles
      // it.
      yield* raiseLivenessIncident(
        globals,
        HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
        SIGNED_INTENT_UNDECIDED,
        assessed.reason,
        { escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS },
      );
      yield* reportOnce(
        reportKey,
        `Cannot revive replaced block ${header} yet: ${assessed.reason}. The history gate stays closed.`,
      );
      return;
    }
    const serialized = yield* serializeStateQueueUTxO(winner.node.node);
    const identity = displacementIdentity(winner.record, assessed.displaced);
    const recheck = assess.pipe(
      Effect.flatMap((current) =>
        current?.kind === "revive" &&
        current.winner.record[C.HEADER_HASH].toString("hex") === header &&
        current.winner.node.node.utxo.txHash === winner.node.node.utxo.txHash &&
        current.winner.node.node.utxo.outputIndex ===
          winner.node.node.utxo.outputIndex &&
        displacementIdentity(current.winner.record, current.displaced) ===
          identity
          ? Effect.succeed(current)
          : Effect.fail<
              | HistoryRecoverySuperseded
              | DatabaseError
              | SignedIntentReplacementIntegrityError
            >(
              new HistoryRecoverySuperseded({
                message: `The revival evidence for block ${header} changed`,
              }),
            ),
      ),
    );
    const publish = Effect.gen(function* () {
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
      yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, serialized);
    });
    const retained = yield* owned(
      retainedPreparedRecoveryPlan(input.binding.digest),
    );
    const requiresRewind =
      retained?.kind === "displaced_block_revival" ||
      assessed.displaced.some(
        (record) => record[C.BASE_UTXOS_ROOT] !== record[C.EXPECTED_UTXOS_ROOT],
      );
    let displaced: string[];
    if (requiresRewind) {
      const restored = yield* recoverDisplacement({
        bindingDigest: input.binding.digest,
        checkpoint,
        preparation,
        config,
        winner: winner.record,
        displaced: assessed.displaced,
        verify: recheck.pipe(Effect.asVoid),
        repair: reviveOver(winner.record, assessed.displaced).pipe(
          Effect.asVoid,
        ),
        afterSqlCommit: publish,
      });
      if (!restored) return;
      displaced = assessed.displaced.map((record) =>
        record[C.HEADER_HASH].toString("hex"),
      );
    } else {
      displaced = yield* owned(
        recheck.pipe(
          Effect.flatMap((current) =>
            reviveOver(current.winner.record, current.displaced),
          ),
        ),
      );
      yield* publish;
    }
    input.deferral.revival = undefined;
    yield* clearLivenessReasonIf(
      globals,
      HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
      SIGNED_INTENT_UNDECIDED,
    );
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logWarning(
      `Revived replaced block ${header}: ${winner.evidence}, and no journal was active${displaced.length === 0 ? "" : `; it displaced locally finalized block ${displaced.join(", ")}, now abandoned`}; local finalization replays it.`,
    );
  }).pipe(heldOnIntegrityFailure(HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE));
