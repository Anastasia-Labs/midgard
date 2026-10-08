/**
 * The working-ledger rebase onto the processed landed blocks (plan §7.3,
 * N3): after a foreign block is processed, or a rollback removed one the
 * working ledger held, the follower-change driver's recompute
 * (`services/l1-follower.recompute.ts`) moves the native MPF to the
 * target's root and recomputes `mempool_ledger` and the event statuses from
 * the target. It is rewind plus recompute; nothing is inverted.
 *
 * Native MPF: when its durable root is a root of the target chain, the
 * blocks after it are applied forward (fork, apply, promote). Otherwise it
 * is restored to the newest root of the target chain it retains in full,
 * with
 *
 *   owner.restoreCanonicalRoot({
 *     recoveryId: sha256hex(REBASE_RECOVERY_DOMAIN ‖ durableRoot ‖ targetRoot),
 *     expectedRoot: durableRoot,
 *     targetRoot,
 *   })
 *
 * and the rest is applied forward. Each step is idempotent, so a crash
 * between them resumes from the root the owner holds.
 *
 * SQL, in one transaction under the driver's capability: the own journals the
 * target disposes of are abandoned and their members made pending again,
 * and the abandoned ones whose block landed are revived (`own-journals.ts`);
 * every event a removed block, a disposed-of journal or an unknown header
 * held goes back to `awaiting`; every event a processed foreign or revived
 * own block holds is projected to it; the shared working-ledger recompute
 * (`working-ledger-recompute.rebuild.ts`) runs on the target's ledger;
 * removed rows are deleted, processed ones marked applied, the
 * ledger-store root stamp moves to the target, and the retained plans of
 * retired kinds the native move superseded are discarded
 * (`retired-plans.ts`).
 *
 * Once that commits, the commit path's globals follow: a revived block's
 * local finalization is pending (its node read back from its signed
 * commit), and with the unfinished journal disposed of nothing is.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Ref } from "effect";

import { MpfEngineStateDB } from "../database/index.js";
import type * as Pending from "../database/pendingBlockFinalizations.js";
import type { Globals } from "../services/globals.globals.js";
import { MidgardContracts } from "../services/midgard-contracts.js";
import {
  type NativeMpfOwnerService,
  NativeMpfRootNotRetained,
} from "../services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../services/mpf-native-owner/service.js";
import { signedCommitNode } from "../services/own-block-node.js";
import { rebuildWorkingLedger } from "../services/working-ledger-recompute.js";
import { sha256Hex } from "../sha256.js";
import { serializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { LANDED_BLOCK_REBASE_FAILED } from "./holds.js";
import { depositOutputs } from "./ledger.js";
import { disposeJournals, reviveJournals } from "./own-journals.js";
import {
  assignChainEvents,
  resetUnheldEvents,
  settleChainDeposits,
} from "./rebase-events.js";
import { type RebaseTarget, walkTarget } from "./rebase-target.js";
import {
  LandedChainRootNotRetained,
  restoreRefusalHold,
} from "./restore-holds.js";
import { discardRetiredPlans } from "./retired-plans.js";
import { markRows } from "./settlements.js";
import { deleteRows, markApplied } from "./store.js";

export const REBASE_RECOVERY_DOMAIN = "midgard/landed-block-rebase/v1";

export const REBASE_REJECTIONS = {
  direct: {
    code: "E_REBASE_SPENT_INPUT",
    detail: "A landed block spent or removed this transaction's input",
  },
  dependent: {
    code: "E_REBASE_DEPENDENT_INPUT",
    detail: "This transaction spends the output of a rejected transaction",
  },
} as const;

const promise = <A>(work: () => Promise<A>) =>
  Effect.tryPromise({ try: work, catch: (cause) => cause });

export const rebaseRecoveryId = (durableRoot: string, targetRoot: string) =>
  sha256Hex(
    Buffer.concat([
      Buffer.from(REBASE_RECOVERY_DOMAIN, "utf8"),
      Buffer.from(durableRoot, "hex"),
      Buffer.from(targetRoot, "hex"),
    ]),
  );

/** Fails once the recompute that runs the move was superseded. */
export type RebaseGuard = Readonly<{
  assertCurrent: Effect.Effect<void, unknown>;
}>;

/** Moves the native MPF to the target's last root. */
export const moveNativeRoot = (
  owner: NativeMpfOwnerService,
  target: RebaseTarget,
  preparation: RebaseGuard,
) =>
  Effect.gen(function* () {
    const { roots, events } = walkTarget(target);
    const { durableRoot } = yield* promise(() => owner.diagnostics());
    if (durableRoot === roots.at(-1)) return;
    let from = roots.lastIndexOf(durableRoot);
    for (let index = roots.length - 1; from < 0 && index >= 0; index--) {
      yield* preparation.assertCurrent;
      const restored = yield* Effect.either(
        promise(() =>
          owner.restoreCanonicalRoot({
            recoveryId: rebaseRecoveryId(durableRoot, roots[index]!),
            expectedRoot: durableRoot,
            targetRoot: roots[index]!,
          }),
        ),
      );
      if (restored._tag === "Right") from = index;
      else if (!(restored.left instanceof NativeMpfRootNotRetained))
        return yield* Effect.fail(restored.left);
    }
    // The store only ever adds the nodes of a root it promotes, and every
    // block folded into `confirmed_ledger` was applied natively first, so
    // the frontier's root is retained unless the store lost it.
    if (from < 0)
      return yield* Effect.fail(
        new LandedChainRootNotRetained(durableRoot, roots[0]!),
      );
    for (let index = from + 1; index < roots.length; index++) {
      const base = roots[index - 1]!;
      const expected = roots[index]!;
      const stepEvents = events[index - 1]!;
      if (stepEvents.length === 0) {
        if (base !== expected)
          return yield* Effect.fail(
            new Error(
              `A landed block with no delta moves the root to ${expected}`,
            ),
          );
        continue;
      }
      // Fork, apply and promote run to their end or their discard: an
      // interrupt never leaves a fork behind. The preparation is still
      // re-checked between the steps.
      yield* Effect.uninterruptible(
        Effect.gen(function* () {
          yield* preparation.assertCurrent;
          const handle = yield* promise(() => owner.fork(base));
          yield* Effect.gen(function* () {
            const applied = yield* promise(() =>
              owner.applyEvents(
                handle,
                encodeNativeMpfEventLog(base, stepEvents),
              ),
            );
            if (applied.candidateRoot !== expected)
              return yield* Effect.fail(
                new Error(
                  `A landed block's delta reaches native root ${applied.candidateRoot}, not ${expected}`,
                ),
              );
            yield* preparation.assertCurrent;
            yield* promise(() => owner.promote(handle));
          }).pipe(
            Effect.onError(() =>
              promise(() => owner.discard(handle)).pipe(Effect.ignore),
            ),
          );
        }),
      );
    }
  });

/** Recomputes the working ledger and event statuses on the target. */
export const rebaseSql = (target: RebaseTarget) =>
  Effect.gen(function* () {
    const { ledger, roots } = walkTarget(target);
    const { dispose, revive } = target.journals;
    const reviving = new Set(revive);
    const chained = target.steps.flatMap((step) =>
      step.row !== undefined &&
      (step.kind === "foreign" || reviving.has(step.headerHash))
        ? [step.row]
        : [],
    );
    const foreign = chained.filter((row) => row.kind === "foreign");
    const ownTxIds = target.steps.flatMap((step) =>
      step.kind === "own" && step.row !== undefined
        ? step.row.txIds
        : step.kind === "live"
          ? (target.live?.txIds ?? [])
          : [],
    );
    const depositIds = [
      ...target.steps.flatMap((step) => step.row?.depositIds ?? []),
      ...(target.live?.depositIds ?? []),
    ];
    const deposits = yield* depositOutputs(depositIds);
    const removed = target.rows.filter((row) => row.state === "removed");
    yield* disposeJournals(dispose);
    yield* resetUnheldEvents(target, [
      ...removed.map((row) => row.headerHash),
      ...dispose.map((disposal) => disposal.headerHash),
    ]);
    const revived = yield* reviveJournals(revive);
    yield* assignChainEvents(chained);
    // A processed (landed) row marks the transactions it includes in the
    // pending tables; the live own block has not landed.
    const processed = target.steps.flatMap((step) =>
      step.row === undefined ? [] : [step.row],
    );
    yield* markRows(processed);
    const rebuilt = yield* rebuildWorkingLedger({
      base: ledger,
      baseDeposits: new Map(
        [...deposits].map(([outRef, { txId, eventId }]) => [
          outRef,
          { txId, sourceEventId: eventId },
        ]),
      ),
      includedByForeign: new Set(
        foreign.flatMap((row) => row.txIds.map((id) => id.toString("hex"))),
      ),
      includedByOwn: new Set(ownTxIds.map((id) => id.toString("hex"))),
      codes: REBASE_REJECTIONS,
    });
    yield* settleChainDeposits(chained, rebuilt.ledger, deposits);
    yield* deleteRows(removed.map((row) => row.headerHash));
    yield* markApplied(chained.map((row) => row.headerHash));
    yield* MpfEngineStateDB.stampLedgerMigration(roots.at(-1)!);
    yield* discardRetiredPlans(target.retired);
    return {
      ...rebuilt,
      revived,
      disposedActive: dispose.some((disposal) => disposal.active),
    };
  });

/**
 * The commit path's globals once a rebase that revived or disposed of own
 * journals committed: a revived block's local finalization is pending (the
 * hold on the blocks after it keeps it the newest), and with the unfinished
 * journal disposed of nothing is pending or awaiting confirmation.
 */
export const followJournals = (
  globals: Globals,
  outcome: Readonly<{
    revived: readonly Pending.Record[];
    disposedActive: boolean;
  }>,
) =>
  Effect.gen(function* () {
    const winner = outcome.revived.at(-1);
    if (winner === undefined && !outcome.disposedActive) return;
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
    if (winner === undefined) {
      yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
      yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
      return;
    }
    const contracts = yield* MidgardContracts;
    const block = yield* signedCommitNode(winner, contracts).pipe(
      Effect.flatMap(({ node }) => serializeStateQueueUTxO(node)),
      Effect.catchAll((cause) =>
        Effect.logWarning(
          "A revived block's node cannot be read back from its signed commit; the commit preflight recovers its local finalization from the landed tail",
          cause,
        ).pipe(Effect.as("" as const)),
      ),
    );
    yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
    yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, block);
  });

/**
 * A failure and the causes under it: the follower write gate reports a
 * failed step under its own message, with the step's error as its cause.
 */
const causeChain = (failure: unknown) => {
  const chain: unknown[] = [];
  let current = failure;
  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    chain.push(current);
    current =
      typeof current === "object" && current !== null
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return chain;
};

/**
 * The hold a failed rebase shows: its reason, the failure as detail, and
 * how long it stays raised before it escalates. A refused native restore
 * is named by its refusal (`restore-holds.ts`).
 */
export const failureHold = (failure: unknown) => {
  const chain = causeChain(failure);
  const parts: string[] = [];
  for (const part of chain.map((cause) => formatUnknownError(cause)))
    if (parts.at(-1) !== part) parts.push(part);
  const restore = restoreRefusalHold(chain);
  return {
    reason: restore?.reason ?? LANDED_BLOCK_REBASE_FAILED,
    detail: parts.join("; caused by "),
    escalateAfterMs: restore?.escalateAfterMs,
  };
};
