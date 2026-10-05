import { Effect, Option, Ref } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import {
  journalAbandonment,
  REVIVAL_BLOCKING_SIBLING_STATUSES,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import { Globals } from "./globals.globals.js";
import {
  journalBase,
  sameBaseJournals,
} from "./history-expired-intent-release.base-spend.js";
import {
  displacement,
  observedRemoval,
} from "./history-expired-intent-release.displacement.js";
import {
  chainOrder,
  replacedBlockLanding,
  replacedSiblings,
} from "./history-expired-intent-release.replaced-block-landing.js";
import {
  type Decision,
  type ReleaseEvidence,
  signedCommitNode,
} from "./history-expired-intent-release.signed-commit-node.js";
import {
  C,
  ROOT_TAIL_HEADER_HASH,
} from "./history-expired-intent-release.table.js";
import {
  clearLivenessIncident,
  HISTORY_SIGNED_INTENT_RELEASE_SOURCE as UNDECIDED_SOURCE,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import {
  SIGNED_INTENT_UNDECIDED,
  SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
} from "./signed-intent-undecided.js";
import { loadStateQueueCorrectionObserverState } from "./state-queue-correction-rewind.js";

/** Reads which block holds the active journal's base slot (see the module
 * comment). Checkpoint-bound evidence decides first; the correction observer's
 * view (not bound to the checkpoint, and pending transitions may still be
 * retracted) only explains a correction of E, or why neither E nor its base D
 * is on the checkpoint's queue. Read-only; runs in the recovery transaction. */
export const decide = (record: Pending.Record, evidence: ReleaseEvidence) =>
  decideRelease(record, evidence).pipe(
    Effect.tap((decision) =>
      Effect.flatMap(Globals, (globals) =>
        decision.kind === "wait" &&
        decision.reason.startsWith(SIGNED_INTENT_UNDECIDED)
          ? raiseLivenessIncident(
              globals,
              UNDECIDED_SOURCE,
              SIGNED_INTENT_UNDECIDED,
              `signed intent of block ${record[C.HEADER_HASH].toString("hex")}: ${decision.reason}`,
              { escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS },
            )
          : // Only its own reason: an integrity hold raised under the same
            // source (see `heldOnIntegrityFailure`) clears when its
            // preparation next completes.
            Effect.flatMap(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
              reasons.get(UNDECIDED_SOURCE) === SIGNED_INTENT_UNDECIDED
                ? clearLivenessIncident(globals, UNDECIDED_SOURCE)
                : Effect.void,
            ),
      ),
    ),
  );

/** A replaced block of this node holds the base slot while a sibling on the
 * same base is already locally finalized: until L1 evidence shows that
 * sibling displaced (see `displacement`), no release can be decided without
 * writing over one of them, so none is. The intent stays in place, the
 * history gate stays closed, and the next evaluation decides again. Two
 * landed siblings of one base stay an integrity failure. */
const undecided = (winner: string, detail: string) =>
  ({
    kind: "wait",
    reason: `${SIGNED_INTENT_UNDECIDED}: this node's replaced block ${winner} holds its base's slot, but ${detail}`,
  }) satisfies Decision;

const decideRelease = (record: Pending.Record, evidence: ReleaseEvidence) =>
  Effect.gen(function* () {
    const { queue, contracts } = evidence;
    const header = record[C.HEADER_HASH].toString("hex");
    const base = record[C.BASE_TAIL_HEADER_HASH].toString("hex");
    const find = (hash: string) =>
      queue.nodes.find((entry) => entry.headerHash === hash);
    const own = find(header);
    if (own !== undefined && own !== queue.root)
      return {
        kind: "landed",
        node: own,
        evidence: "its node is on the queue",
      } satisfies Decision;
    if (queue.root.headerHash === header)
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: "the confirmed state is its header",
      } satisfies Decision;
    const observer = yield* loadStateQueueCorrectionObserverState(
      evidence.rewindAuthority,
      true,
    );
    /** The observed removal of `hash` of one kind, an admitted one first. */
    const removing = (hash: string, merge: boolean) =>
      observedRemoval(observer, hash, merge);
    /** Only an admitted correction is resolved by the correction path, so
     * only it makes the deferral sticky. A pending one may be retracted; the
     * next change of the observer's view decides again. */
    const deferToCorrection = (
      removal: NonNullable<ReturnType<typeof removing>>,
      removed: string,
    ) =>
      ({
        kind: "defer",
        sticky: removal.admitted,
        reason: removal.admitted
          ? `admitted state-queue correction ${removal.transition.transactionHash} removed ${removed}, and the correction path reconciles this node's removed block with its unlanded descendants`
          : `pending state-queue correction ${removal.transition.transactionHash} removed ${removed}; it is not admitted yet`,
      }) satisfies Decision;
    // A block that landed and was then removed is the correction path's,
    // never a landed block to finalize.
    const correctionOfBlock = removing(header, false);
    if (correctionOfBlock !== undefined)
      return deferToCorrection(correctionOfBlock, "it after it landed");
    const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
    if (signed !== undefined && evidence.canonicalHistory.has(signed))
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: "its signed commit is in the journaled canonical history",
      } satisfies Decision;

    /** `winner` holds D's slot. A merged slot's winner may have left the
     * queue too, or be the confirmed state itself; its node is then the one
     * its own signed commit created (the root is never a block's node). */
    const successor = (winner: string, merged: boolean) =>
      Effect.gen(function* () {
        if (winner === header)
          return merged
            ? ({
                kind: "landed",
                node: yield* signedCommitNode(record, contracts),
                evidence: `it was its merged base ${base}'s successor`,
              } satisfies Decision)
            : ({
                kind: "wait",
                reason: `its base ${base} links to it but its node is absent`,
              } satisfies Decision);
        const found = yield* Pending.retrieveByHeaderHash(
          Buffer.from(winner, "hex"),
        );
        if (Option.isNone(found))
          return {
            kind: "replace",
            cause: `foreign block ${winner} holds its base's slot`,
          } satisfies Decision;
        const revived = found.value;
        // The same base: the same output, or (a replacement built on a later
        // incarnation of a non-root base node, after its output was spent in
        // place) the same base header and ledger root.
        if (
          revived[C.STATUS] !== Pending.Status.Abandoned ||
          journalAbandonment(revived) !== "replacement" ||
          !revived[C.BASE_TAIL_HEADER_HASH].equals(
            record[C.BASE_TAIL_HEADER_HASH],
          ) ||
          (revived[C.BASE_TAIL_OUT_REF] !== record[C.BASE_TAIL_OUT_REF] &&
            record[C.BASE_TAIL_HEADER_HASH].equals(ROOT_TAIL_HEADER_HASH)) ||
          revived[C.BASE_UTXOS_ROOT] !== record[C.BASE_UTXOS_ROOT]
        )
          return {
            kind: "wait",
            reason: `this node's block ${winner} (status ${revived[C.STATUS]}, abandonment ${revived[C.STATUS] === Pending.Status.Abandoned ? journalAbandonment(revived) : "none"}) holds its base's slot but is not a replaced block on the same base`,
          } satisfies Decision;
        // The active block's own local finalization, if written, is reversed
        // by the repair like any other unlanded block's.
        const siblings = yield* sameBaseJournals(journalBase(record), [
          record[C.HEADER_HASH],
          revived[C.HEADER_HASH],
        ]);
        const blocking = siblings.filter(({ status }) =>
          REVIVAL_BLOCKING_SIBLING_STATUSES.includes(status),
        );
        const finalized = blocking.map(
          ({ header_hash, status }) =>
            `block ${header_hash.toString("hex")} built on the same base is already ${status}`,
        );
        if (blocking.length > 0 && evidence.canonicalDepth === undefined)
          return undecided(winner, finalized.join("; "));
        const onQueue = find(winner);
        const queued = onQueue !== undefined && onQueue !== queue.root;
        const node = queued
          ? onQueue
          : merged
            ? yield* signedCommitNode(revived, contracts)
            : undefined;
        if (node === undefined)
          return blocking.length > 0
            ? undecided(winner, finalized.join("; "))
            : ({
                kind: "wait",
                reason: `the queue names ${winner} as its base's successor but its node is absent`,
              } satisfies Decision);
        if (blocking.length === 0)
          return {
            kind: "revive",
            revived,
            node,
            displaced: [],
          } satisfies Decision;
        const displaced = yield* displacement({
          blocking: blocking.map(({ header_hash }) =>
            header_hash.toString("hex"),
          ),
          node,
          queued,
          winner: revived,
          base,
          baseRoot: record[C.BASE_UTXOS_ROOT],
          queue,
          observer,
          depth: evidence.canonicalDepth!,
          required: evidence.rewindAuthority.requiredFinalityDepth,
        });
        if (typeof displaced === "string")
          return undecided(winner, `${finalized.join("; ")}, and ${displaced}`);
        return { kind: "revive", revived, node, displaced } satisfies Decision;
      });

    const tail = find(base);
    if (tail !== undefined) {
      const next = tail.node.datum.next;
      const at = `${tail.node.utxo.txHash}#${tail.node.utxo.outputIndex.toString()}`;
      if (next === "Empty")
        return {
          kind: "replace",
          cause:
            at === record[C.BASE_TAIL_OUT_REF]
              ? `its base ${base} is still the queue tail`
              : `its base output ${record[C.BASE_TAIL_OUT_REF]} was spent by ${evidence.baseSpend ?? "another transaction"}, and its base ${base} continues as the queue tail at ${at}`,
        } satisfies Decision;
      return yield* successor(next.Key.key, false);
    }
    // The block that took D's slot links to D by its header. Once that block
    // is merged, the confirmed state names it and links to D (a merge sets
    // the confirmed predecessor to the header it folded over), so the
    // checkpoint's queue still says who won when no removal names D: the root
    // E was built on, whose confirmed header no transition removes, or a D
    // the observer never saw leave. A merged D was never corrected, so this
    // needs no observer. A queue node linking to an absent D is not read:
    // only a correction removes a node and keeps its successor, and the
    // correction arm below decides that once it is recorded.
    if (queue.root.prevHeaderHash === base && queue.root.headerHash !== base)
      return yield* successor(queue.root.headerHash, true);
    // Neither E nor D is on the checkpoint's queue. A recorded removal, a
    // replaced sibling's own landing, or the recorded transitions around a
    // root base say how; absence alone never decides. The observer records
    // independently of the history gate, so the gate stays open and the next
    // change of its view decides again.
    if (observer.kind === "blocked")
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue and the state-queue correction observer has no usable view of why yet (${observer.reason})`,
      } satisfies Decision;
    const mergeOfBlock = removing(header, true);
    if (mergeOfBlock !== undefined)
      return {
        kind: "landed",
        node: yield* signedCommitNode(record, contracts),
        evidence: `merge ${mergeOfBlock.transition.transactionHash} folded it into the confirmed state`,
      } satisfies Decision;
    const correctionOfBase = removing(base, false);
    if (correctionOfBase !== undefined) {
      const baseJournal = yield* Pending.retrieveByHeaderHash(
        Buffer.from(base, "hex"),
      );
      if (
        Option.isSome(baseJournal) &&
        baseJournal.value[C.STATUS] !== Pending.Status.Abandoned
      )
        return deferToCorrection(correctionOfBase, `its base ${base}`);
      return {
        kind: "replace",
        cause: `state-queue correction ${correctionOfBase.transition.transactionHash} consumed the node of its base ${base}, which this node does not journal`,
      } satisfies Decision;
    }
    // Every commit built on E's base spends the same output, so whichever of
    // them landed holds the slot and E never can. A replaced sibling of this
    // node on that output shows it by its own landing (read as the reviver
    // reads it), which needs no link from the slot back to the base: once two
    // merges pass a root base, nothing links that confirmed header to the
    // block that took its slot.
    const landedSiblings: string[] = [];
    for (const sibling of yield* replacedSiblings(record))
      if (
        replacedBlockLanding(
          sibling,
          queue,
          observer,
          evidence.canonicalHistory,
        ) !== undefined
      )
        landedSiblings.push(sibling[C.HEADER_HASH].toString("hex"));
    if (landedSiblings.length > 1)
      return yield* Effect.fail(
        new SignedIntentReplacementIntegrityError(
          landedSiblings[0]!,
          `blocks ${landedSiblings.join(", ")} of this node, all built on the base output of block ${header}, landed`,
        ),
      );
    if (landedSiblings.length === 1)
      return yield* successor(landedSiblings[0]!, true);
    // E's base output is a root that a recorded transition left with an empty
    // queue (a merge of the tail, or a correction of the only node): E was
    // built on the root. Only a commit spends a root with an empty queue, so
    // the first block appended after that transition took the slot, and every
    // later transition sees it first on the queue until it leaves: the next
    // recorded transition names it, however many merges followed. The merge
    // of D, made before E was built on the root, says nothing about E's slot,
    // so this decides before it.
    const recorded = [...observer.state.pending, ...observer.state.admitted];
    recorded.sort(chainOrder);
    const emptied = recorded.findIndex(
      ({ nextQueue }) =>
        nextQueue.length === 1 &&
        nextQueue[0]!.headerHash === null &&
        nextQueue[0]!.outRef === record[C.BASE_TAIL_OUT_REF],
    );
    if (emptied >= 0) {
      const after = recorded[emptied + 1];
      const holder = after?.previousQueue[1]?.headerHash ?? null;
      if (holder === null)
        return {
          kind: "defer",
          sticky: false,
          reason: `it was built on the root that state-queue transition ${recorded[emptied]!.transactionHash} left empty, and the state-queue correction observer has recorded no later transition naming the block that took that slot yet`,
        } satisfies Decision;
      const correctionOfHolder = removing(holder, false);
      if (correctionOfHolder !== undefined) {
        const own = yield* Pending.retrieveByHeaderHash(
          Buffer.from(holder, "hex"),
        );
        if (Option.isSome(own) && !correctionOfHolder.admitted)
          return {
            kind: "defer",
            sticky: false,
            reason: `this node's block ${holder} took the slot of its root base, and pending state-queue correction ${correctionOfHolder.transition.transactionHash} removed it; it is not admitted yet`,
          } satisfies Decision;
        return {
          kind: "replace",
          cause: `block ${holder} spent the root output it was built on, and state-queue correction ${correctionOfHolder.transition.transactionHash} then removed that block`,
        } satisfies Decision;
      }
      return yield* successor(holder, true);
    }
    const mergeOfBase = removing(base, true);
    if (mergeOfBase === undefined)
      return {
        kind: "defer",
        sticky: false,
        reason: `its base ${base} is no longer on the queue, the confirmed state does not link to it, no replaced block of this node on the same base shows it landed, and the state-queue correction observer has recorded neither a correction nor a merge of it, nor a transition that left the root it was built on empty`,
      } satisfies Decision;
    const previous = mergeOfBase.transition.previousQueue;
    const index = previous.findIndex((node) => node.headerHash === base);
    const merged = previous[index + 1]?.headerHash ?? null;
    if (merged === null)
      return {
        kind: "replace",
        cause: `its base ${base} was merged into the confirmed state by ${mergeOfBase.transition.transactionHash} while it was still the queue tail`,
      } satisfies Decision;
    return yield* successor(merged, true);
  });
