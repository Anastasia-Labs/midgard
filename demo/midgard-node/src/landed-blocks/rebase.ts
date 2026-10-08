/**
 * The working-ledger rebase onto the processed landed blocks (plan §7.3,
 * N3, ruling E-N3-1 option B): after a foreign block is processed, or a
 * rollback removed one the working ledger held, the history owner's
 * recovery preparation moves the native MPF to the target's root and
 * recomputes `mempool_ledger` and the event statuses from the target. It is
 * rewind plus recompute; nothing is inverted.
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
 * SQL, in one transaction under the preparation: the shared working-ledger
 * recompute (`working-ledger-recompute.rebuild.ts`) on the target's ledger;
 * every event a removed block or an unknown header held goes back to
 * `awaiting`; every event a processed foreign block holds is projected to
 * it; removed rows are deleted, processed ones marked applied, and the
 * ledger-store root stamp moves to the target.
 */
import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Cause, Effect, Ref } from "effect";

import { MpfEngineStateDB } from "../database/index.js";
import { NodeConfig } from "../services/config.js";
import { isRecoverableHistorySourceFailure } from "../services/event-history-owner.source-failure.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import {
  HistoryPreparation,
  type HistoryRecoveryPreparation,
} from "../services/event-history-recovery.js";
import { Globals } from "../services/globals.globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  type NativeMpfOwnerService,
  NativeMpfRootNotRetained,
} from "../services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../services/mpf-native-owner/service.js";
import { initializeArchitectureGOwner } from "../services/native-mpf-startup.js";
import { rebuildWorkingLedger } from "../services/working-ledger-recompute.js";
import { sha256Hex } from "../sha256.js";
import {
  LANDED_BLOCK_REBASE_FAILED,
  LANDED_BLOCK_REBASE_SOURCE,
} from "./holds.js";
import { depositOutputs } from "./ledger.js";
import {
  assignChainEvents,
  resetUnheldEvents,
  settleChainDeposits,
} from "./rebase-events.js";
import { rebasePlan, type RebaseTarget, walkTarget } from "./rebase-target.js";
import { markRows, recordSettlements } from "./settlements.js";
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
  batch: {
    code: "E_REBASE_BATCH_MEMBER",
    detail: "This transaction was accepted in one batch with a rejected one",
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

/** Moves the native MPF to the target's last root. */
export const moveNativeRoot = (
  owner: NativeMpfOwnerService,
  target: RebaseTarget,
  preparation: Pick<HistoryRecoveryPreparation, "assertCurrent">,
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
    if (from < 0)
      return yield* Effect.fail(
        new Error(
          `The native MPF retains no root of the processed landed chain (durable root ${durableRoot})`,
        ),
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
    const foreign = target.steps.flatMap((step) =>
      step.kind === "foreign" && step.row !== undefined ? [step.row] : [],
    );
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
    yield* resetUnheldEvents(target, removed);
    yield* assignChainEvents(foreign);
    // A receipt member a processed (landed) row includes is settled by it
    // from this rebuild on, and the row marks it in the pending tables; the
    // live own block has not landed.
    const processed = target.steps.flatMap((step) =>
      step.row === undefined ? [] : [step.row],
    );
    yield* markRows(processed);
    yield* recordSettlements(processed);
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
    yield* settleChainDeposits(foreign, rebuilt.ledger, deposits);
    yield* deleteRows(removed.map((row) => row.headerHash));
    yield* markApplied(foreign.map((row) => row.headerHash));
    yield* MpfEngineStateDB.stampLedgerMigration(roots.at(-1)!);
    return rebuilt;
  });

/**
 * A failure and the causes under it: the history write gate reports a
 * failed step under its own message, with the step's error as its cause.
 */
const failureDetail = (failure: unknown) => {
  const parts: string[] = [];
  let current = failure;
  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    const part = formatUnknownError(current);
    if (parts.at(-1) !== part) parts.push(part);
    current =
      typeof current === "object" && current !== null
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return parts.join("; caused by ");
};

/**
 * The rebase, run from the history owner's pending-reconciliation
 * preparation. A rebase that cannot run yet (no lease, or a target that
 * waits for an own journal's resolution) leaves the reconciliation pending.
 *
 * A failure the owner treats as recoverable (a transport-class SQL error, a
 * deadline, a superseded preparation) and an interrupt propagate as before.
 * Any other failure is caught here: it is recorded
 * (`LANDED_BLOCK_REBASE_FAILURE`), raised as the liveness reason
 * `landed_block_rebase_failed` with its detail, and the preparation returns,
 * so the reconciliation stays pending on the owner's backoff and the owner
 * retries it; the record and the reason clear once a rebase runs.
 */
export const prepareLandedBlockRebase = (
  preparation: HistoryRecoveryPreparation,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    const attempt = Effect.gen(function* () {
      const plan = yield* withHistoryWrite(rebasePlan);
      if (plan.kind !== "ready") return "idle" as const;
      const run = MpfEngineStateDB.tryWithLedgerStoreLease(
        `landed-block-rebase:${randomUUID()}`,
        () =>
          Effect.gen(function* () {
            const current = yield* Ref.get(globals.NATIVE_MPF_OWNER);
            if (current === undefined)
              yield* initializeArchitectureGOwner(
                globals,
                config,
                preparation,
                (owner) => moveNativeRoot(owner, plan.target, preparation),
              );
            else yield* moveNativeRoot(current, plan.target, preparation);
            // Producers are drained for the preparation: the rows the plan
            // read cannot change before the SQL step.
            yield* preparation.assertCurrent;
            yield* withHistoryWrite(rebaseSql(plan.target));
          }),
      );
      const result = yield* run;
      if (result._tag !== "Busy") return "ran" as const;
      yield* Effect.logInfo(
        "Landed-block rebase waits for the ledger store lease",
      );
      return "busy" as const;
    });
    const outcome = yield* attempt.pipe(
      Effect.catchAllCause((cause) => {
        const failure = Cause.squash(cause);
        if (
          Cause.isInterruptedOnly(cause) ||
          isRecoverableHistorySourceFailure(failure)
        )
          return Effect.failCause(cause);
        const detail = failureDetail(failure);
        return Effect.gen(function* () {
          yield* Ref.set(globals.LANDED_BLOCK_REBASE_FAILURE, detail);
          yield* raiseLivenessIncident(
            globals,
            LANDED_BLOCK_REBASE_SOURCE,
            LANDED_BLOCK_REBASE_FAILED,
            detail,
          );
          return "failed" as const;
        });
      }),
    );
    if (outcome === "ran" || outcome === "idle") {
      yield* Ref.set(globals.LANDED_BLOCK_REBASE_FAILURE, undefined);
      yield* clearLivenessIncident(globals, LANDED_BLOCK_REBASE_SOURCE);
    }
  }).pipe(Effect.provideService(HistoryPreparation, preparation));

/** The owner's reconcile: pending while a rebase is due and can run. */
export const landedBlockRebaseDisposition = rebasePlan.pipe(
  Effect.map((plan) =>
    plan.kind === "ready"
      ? {
          status: "pending" as const,
          reason: "The working ledger waits for the landed-block rebase",
        }
      : undefined,
  ),
);
