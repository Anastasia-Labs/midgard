import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { ForeignTipReconciliationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { isHistoryProducerGateClosed } from "../services/event-history-producer.js";
import { ContractDeploymentIdentity, Database } from "../services/index.js";
import {
  activeEvidenceScope,
  awaiting,
  entryVerdict,
  entryWindow,
  firstBlocking,
  refusesUnconditionally,
  storedVerdictRefusesUnconditionally,
  undecodableUnresolved,
  type Unresolved,
} from "./t2-foreign-event-reconciliation.foreign-window-gate.js";
import { reconcileRetainedForeignTipEntry } from "./t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js";
import {
  emptyIds,
  normalizedIds,
  type T2ForeignEventResolution,
} from "./t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";

export const reconcileOverdueAwaitingEventsAgainstForeignTip = ({
  foreignHeaderHash,
  header,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
}): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const suppliedHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to authenticate supplied foreign header",
            cause,
          }),
      ),
    );
    if (suppliedHash !== foreignHeaderHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Supplied foreign header does not match reconciliation key",
          cause: `provided=${foreignHeaderHash},recomputed=${suppliedHash}`,
        }),
      );
    }
    const reconciliation =
      yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
        foreignHeaderHash,
      );
    return Option.isNone(reconciliation)
      ? ({ type: "Ready", absent: emptyIds() } as const)
      : yield* reconcileRetainedForeignTipEntry(reconciliation.value);
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: "t2_foreign_event_reconciliation",
            message: "Failed to reconcile overdue events against foreign tip",
            cause,
          }),
    ),
  );

/**
 * Every actionable retained window of the active deployment, page by page:
 * awaiting rows, resolved rows whose window holds a late awaiting event, and
 * every row that no longer decodes, whatever its status (see
 * `retrieveActionableEvidencePage`).
 */
const forEachActionableEvidencePage = <E, R>(
  visit: (
    page: ForeignTipReconciliationsDB.ForeignTipEvidencePage,
  ) => Effect.Effect<boolean, E, R>,
): Effect.Effect<
  void,
  E | DatabaseError,
  R | Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const scope = yield* activeEvidenceScope;
    let cursor:
      | ForeignTipReconciliationsDB.ForeignTipEvidencePageCursor
      | undefined;
    do {
      const page =
        yield* ForeignTipReconciliationsDB.retrieveActionableEvidencePage(
          scope,
          cursor,
        );
      if (!(yield* visit(page))) return;
      cursor = page.next;
    } while (cursor !== undefined);
  });

/**
 * Replays every actionable retained foreign window of the active deployment,
 * so indexer-late Awaiting rows remain recoverable after restart or after the
 * live state-queue tip advances beyond the foreign header. Each row is
 * replayed on its own: one that fails is logged and treated as unverified
 * rather than failing the commit. An unverified row refuses the commit only
 * while its own window can still collide with the block (see
 * `windowBlockingCauses` in the window-gate module), unless its verdict is one
 * no window lifts; it is never marked resolved without verified DA. Settled
 * rows are pruned by the retention sweeper (`pruneBeyondRetention`).
 */
export const reconcileOverdueAwaitingEventsAgainstRetainedForeignTips = ({
  eventsIngestedThrough,
}: {
  readonly eventsIngestedThrough: Date;
}): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const unresolved: Unresolved[] = [];
    const combinedAbsent = {
      deposits: [] as string[],
      forcedTransactions: [] as string[],
      withdrawals: [] as string[],
    };
    yield* forEachActionableEvidencePage((page) =>
      Effect.gen(function* () {
        unresolved.push(...(yield* undecodableUnresolved(page.undecodable)));
        for (const entry of page.entries) {
          const window = entryWindow(entry);
          const replayed = yield* Effect.either(
            reconcileRetainedForeignTipEntry(entry).pipe(
              Effect.catchAllDefect((defect) =>
                Effect.fail(
                  new DatabaseError({
                    table: ForeignTipReconciliationsDB.tableName,
                    message: "Foreign-tip reconciliation replay died",
                    cause: String(defect),
                  }),
                ),
              ),
            ),
          );
          if (
            replayed._tag === "Left" &&
            isHistoryProducerGateClosed(replayed.left)
          ) {
            // A recovery the history owner is running, not a fault of this row.
            return yield* Effect.fail(replayed.left);
          }
          if (replayed._tag === "Left") {
            yield* Effect.logWarning(
              `🔹 Foreign-tip reconciliation replay failed header_hash=${window.foreignHeaderHash}; its window still gates the commit: ${replayed.left.message}`,
              replayed.left,
            );
            unresolved.push({
              window,
              resolution: awaiting(
                window,
                "replay_failed",
                replayed.left.message,
              ),
              unconditional: storedVerdictRefusesUnconditionally(
                entryVerdict(entry),
              ),
            });
          } else if (replayed.right.type === "AwaitingForeignDa") {
            unresolved.push({
              window,
              resolution: replayed.right,
              unconditional: refusesUnconditionally(replayed.right.reason),
            });
          } else {
            combinedAbsent.deposits.push(...replayed.right.absent.deposits);
            combinedAbsent.forcedTransactions.push(
              ...replayed.right.absent.forcedTransactions,
            );
            combinedAbsent.withdrawals.push(
              ...replayed.right.absent.withdrawals,
            );
          }
        }
        return true;
      }),
    );
    const blocking = yield* firstBlocking(unresolved, eventsIngestedThrough);
    if (blocking !== undefined) return blocking;
    if (unresolved.length > 0) {
      yield* Effect.logInfo(
        `🔹 ${unresolved.length.toString()} foreign-tip reconciliation(s) still lack verified DA, but no event of the next block lies in their windows; committing past them (first header_hash=${unresolved[0]!.window.foreignHeaderHash}).`,
      );
    }
    return {
      type: "Ready",
      absent: {
        deposits: normalizedIds(combinedAbsent.deposits),
        forcedTransactions: normalizedIds(combinedAbsent.forcedTransactions),
        withdrawals: normalizedIds(combinedAbsent.withdrawals),
      },
    } as const;
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: "t2_foreign_event_reconciliation",
            message:
              "Failed to reconcile overdue events against retained foreign tips",
            cause,
          }),
    ),
  );

/**
 * The read-only gate for a speculative build, which must not write. It reads
 * only stored verdicts, and refuses on an unverified row whose stored verdict
 * no window lifts or whose window can still collide with the block, or on a
 * resolved row holding late events only a replay can classify. It cannot see
 * a verdict no replay has derived yet: a row never replayed (still
 * `pending_evidence`) whose payload would fail verification gates only by its
 * window until the ordinary path replays it. A refusal hands the build back to
 * the ordinary path, which replays.
 */
export const assessRetainedForeignTipWindows = ({
  eventsIngestedThrough,
}: {
  readonly eventsIngestedThrough: Date;
}): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const unresolved: Unresolved[] = [];
    let lateEvent: T2ForeignEventResolution | undefined;
    yield* forEachActionableEvidencePage((page) =>
      Effect.gen(function* () {
        unresolved.push(...(yield* undecodableUnresolved(page.undecodable)));
        for (const entry of page.entries) {
          const window = entryWindow(entry);
          if (
            entry[ForeignTipReconciliationsDB.Columns.STATUS] !==
            ForeignTipReconciliationsDB.Status.Awaiting
          ) {
            // An actionable resolved row holds a late event.
            lateEvent = awaiting(
              window,
              "replay_required",
              "late_event_in_resolved_window",
            );
            return false;
          }
          unresolved.push({
            window,
            resolution: awaiting(
              window,
              "replay_required",
              entry[ForeignTipReconciliationsDB.Columns.BLOCKING_REASON] ??
                "awaiting",
            ),
            unconditional: storedVerdictRefusesUnconditionally(
              entryVerdict(entry),
            ),
          });
        }
        return true;
      }),
    );
    return (
      lateEvent ??
      (yield* firstBlocking(unresolved, eventsIngestedThrough)) ??
      ({ type: "Ready", absent: emptyIds() } as const)
    );
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: "t2_foreign_event_reconciliation",
            message: "Failed to assess retained foreign-tip windows",
            cause,
          }),
    ),
  );

/**
 * The commit's foreign-tip gate. Every event table is ingested through
 * `eventsIngestedThrough`. A speculative build must not write, so it takes the
 * read-only check and leaves any replay to the ordinary build.
 */
export const gateCommitOnRetainedForeignTips = ({
  speculative,
  eventsIngestedThrough,
}: {
  readonly speculative: boolean;
  readonly eventsIngestedThrough: Date;
}) =>
  speculative
    ? assessRetainedForeignTipWindows({ eventsIngestedThrough })
    : reconcileOverdueAwaitingEventsAgainstRetainedForeignTips({
        eventsIngestedThrough,
      });
