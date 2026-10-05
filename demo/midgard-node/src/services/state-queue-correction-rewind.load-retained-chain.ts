import { Effect, Option } from "effect";

import { type CorrectionRewindIntent } from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import {
  admittedRemovals,
  C,
  chainIdentity,
  type ChainMember,
  journal,
  type Obligation,
  type StateQueueCorrectionRewindAuthority,
} from "./state-queue-correction-rewind.admitted-removals.js";
import {
  loadObligation,
  proveUnlanded,
  validateChain,
} from "./state-queue-correction-rewind.prove-unlanded.js";

/** Re-derives a retained plan's chain from fresh authority. The members are
 * the plan's identity, so a later admission never widens an interrupted one.
 * A chain that does not prove right now is blocked with its reason, never a
 * failure: its authority may return (an observer that re-scores a removal's
 * finality admits it again) and its members may prove again, so the caller
 * holds the plan behind a closed gate and re-evaluates it. */
export const loadRetainedChain = (
  authority: StateQueueCorrectionRewindAuthority,
  intent: CorrectionRewindIntent,
  lock = false,
) =>
  Effect.gen(function* () {
    const blocked = (reason: string): Obligation => ({
      kind: "blocked",
      reason,
    });
    const admitted = yield* admittedRemovals(authority, lock);
    if (admitted.kind === "blocked")
      return blocked(
        `Retained correction rewind ${intent.headerHash} lost its authority: ${admitted.reason}`,
      );
    const chain: ChainMember[] = [];
    for (const member of intent.members) {
      if (member.kind === "unlanded") {
        const proof = yield* proveUnlanded(
          chain.at(-1)!,
          member.headerHash,
          admitted,
        );
        if (
          proof.kind !== "unlanded" ||
          proof.member.transitionDigest !== member.transitionDigest
        )
          return blocked(
            `Retained correction rewind member ${member.headerHash} is no longer provably unlanded: ${proof.kind === "blocked" ? proof.reason : "its proving correction changed"}`,
          );
        chain.push(proof.member);
        continue;
      }
      const record = yield* journal(member.headerHash);
      if (
        admitted.removals.get(member.headerHash) !== member.transitionDigest ||
        Option.isNone(record) ||
        record.value[C.STATUS] === Pending.Status.Abandoned
      )
        return blocked(
          `Retained correction rewind member ${member.headerHash} lost its admitted correction or journal`,
        );
      chain.push({
        record: record.value,
        transitionDigest: member.transitionDigest,
        kind: "removed",
      });
    }
    const validated = yield* validateChain(chain, authority.manifestId);
    if (validated.kind !== "ready")
      return blocked(
        `Retained correction rewind ${intent.headerHash} is no longer provable: ${validated.kind === "blocked" ? validated.reason : "no chain"}`,
      );
    if (
      chainIdentity(chain) !== intent.journalDigest ||
      chain[0]!.record[C.BASE_UTXOS_ROOT] !== intent.targetRoot
    )
      return blocked(
        `Retained correction rewind ${intent.headerHash} journal identity changed`,
      );
    return validated;
  });

/** Last blocked reason per history binding. A blocked obligation is
 * re-evaluated on every convergence; it is logged at WARN when its reason
 * first appears or changes, and at debug while it persists. */
export const blockedReasons = new Map<string, string>();

export const logBlocked = (bindingDigest: string, reason: string | undefined) =>
  Effect.suspend(() => {
    if (reason === undefined) {
      blockedReasons.delete(bindingDigest);
      return Effect.void;
    }
    const message = `State-queue correction rewind is blocked: ${reason}`;
    if (blockedReasons.get(bindingDigest) === reason)
      return Effect.logDebug(message);
    blockedReasons.set(bindingDigest, reason);
    return Effect.logWarning(message);
  });

/** Read-only view of the rewind obligation, for diagnostics and tests. */
export const inspectStateQueueCorrectionRewindObligation = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  loadObligation(authority).pipe(
    Effect.map((obligation) =>
      obligation.kind === "ready"
        ? {
            kind: "ready" as const,
            members: obligation.chain.map(({ record, kind }) => ({
              headerHash: record[C.HEADER_HASH].toString("hex"),
              kind,
            })),
          }
        : obligation,
    ),
  );
