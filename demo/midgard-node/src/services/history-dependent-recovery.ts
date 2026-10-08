import { Cause, Effect } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  applyHistoryRecoveryPlan,
  type DependentRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import { DatabaseError } from "../database/utils/common.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import {
  clearLivenessReasonIf,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import type {
  NativeMpfFullIndexCapExceeded,
  NativeMpfOwnerService,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "./mpf-native-owner/protocol.js";

/** Why the native owner refused a canonical restore, before it changed its
 * marker: the target root's node closure is not in its store
 * (`not_retained`), it is but its full index is over a full-index cap
 * (`index_cap`), or reading it failed (`read_transient`). */
export type NativeRestoreRefusal =
  | Readonly<{ kind: "not_retained"; error: NativeMpfRootNotRetained }>
  | Readonly<{ kind: "index_cap"; error: NativeMpfFullIndexCapExceeded }>
  | Readonly<{ kind: "read_transient"; error: NativeMpfRestoreReadFailed }>;

const refusalKinds = new Map<string, NativeRestoreRefusal["kind"]>([
  ["NativeMpfRootNotRetained", "not_retained"],
  ["NativeMpfFullIndexCapExceeded", "index_cap"],
  ["NativeMpfRestoreReadFailed", "read_transient"],
]);

/** The native restore refusal on `cause`'s chain, if any, matched by its
 * tag through the `cause` links a failure that wraps it adds. */
export const nativeRestoreRefusal = (
  cause: unknown,
): NativeRestoreRefusal | undefined => {
  let current = cause;
  for (let depth = 0; depth < 8 && current instanceof Object; depth += 1) {
    const tag = (current as { _tag?: unknown })._tag;
    const kind = typeof tag === "string" ? refusalKinds.get(tag) : undefined;
    if (kind !== undefined)
      return { kind, error: current } as NativeRestoreRefusal;
    current = (current as { cause?: unknown }).cause;
  }
  return undefined;
};

/** What a hold on each refusal adds to the refusal's message: what stays
 * unchanged, what retries it, and what the operator does. `subject` names
 * the held work ("recovery", "rewind"). */
export const nativeRestoreHoldText = (
  kind: NativeRestoreRefusal["kind"],
  subject: string,
): string => {
  const unchanged = `The ${subject} holds with its plan retained and native MPF, the SQL root and the journals unchanged, and the history gate stays closed, which holds block production; every evaluation retries the restore.`;
  switch (kind) {
    case "not_retained":
      return `${unchanged} Operator action is needed: stop the node, install at LEDGER_MPF_DB_PATH a native MPF store that retains this root in full (such as a backup of the store taken while it did), and restart it; the next evaluation completes the ${subject}.`;
    case "index_cap":
      return `${unchanged} The root's node closure is in the native MPF store, but the node cannot load an index that large; it would refuse to start on that root for the same reason. Operator action is needed: run a node build whose full-index caps (the TypeScript owner's and its native child's) cover this root, and restart it; the next evaluation completes the ${subject}.`;
    case "read_transient":
      return `${unchanged} The next evaluation whose read of the native MPF store succeeds completes the ${subject}. Operator action is needed only if the read keeps failing: check the disk and the store at LEDGER_MPF_DB_PATH.`;
  }
};

/** The reason a hold on `refusal` raises under its caller's source: the
 * caller's own not-retained reason, or the shared cap and transient ones. */
const holdReason = (
  kind: NativeRestoreRefusal["kind"],
  notRetainedReason: string,
) =>
  kind === "not_retained"
    ? notRetainedReason
    : kind === "index_cap"
      ? NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED
      : NATIVE_MPF_RESTORE_READ_TRANSIENT;

/** How long a hold on `kind` stays raised before it escalates: a store that
 * lacks the root, or a cap it is over, needs the operator at once; a read
 * failure gets `NATIVE_MPF_RESTORE_READ_ESCALATION_MS` to pass. */
export const nativeRestoreHoldEscalation = (
  kind: NativeRestoreRefusal["kind"],
): number =>
  kind === "read_transient" ? NATIVE_MPF_RESTORE_READ_ESCALATION_MS : 0;

/** The raise, under `source`, of the native restore refusal `cause` carries
 * (in any of its failures), or undefined when it carries none. */
export const holdNativeRestoreRefusal = (input: {
  readonly globals: Globals;
  readonly source: string;
  readonly notRetainedReason: string;
  readonly subject: string;
  readonly cause: Cause.Cause<unknown>;
}): Effect.Effect<void> | undefined => {
  const refusal = [...Cause.failures(input.cause)]
    .map(nativeRestoreRefusal)
    .find((value) => value !== undefined);
  if (refusal === undefined) return undefined;
  return raiseLivenessIncident(
    input.globals,
    input.source,
    holdReason(refusal.kind, input.notRetainedReason),
    `${refusal.error.message}. ${nativeRestoreHoldText(refusal.kind, input.subject)}`,
    { escalateAfterMs: nativeRestoreHoldEscalation(refusal.kind) },
  );
};

/** Clears, under `source`, whichever native restore refusal reason it
 * raised. */
export const clearNativeRestoreRefusal = (
  globals: Globals,
  source: string,
  notRetainedReason: string,
) =>
  Effect.forEach(
    [
      notRetainedReason,
      NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
      NATIVE_MPF_RESTORE_READ_TRANSIENT,
    ],
    (reason) => clearLivenessReasonIf(globals, source, reason),
    { discard: true },
  );

/** What a preparation held by `heldOnNativeRestoreRefusal` returns. */
export const NATIVE_RESTORE_HELD = "native_restore_held" as const;

/**
 * A history recovery preparation whose native restore the owner refuses
 * holds instead of failing the history owner, whose supervisor would only
 * restart it into the same store: the refusal is raised under `source` (as
 * `notRetainedReason`, `native_mpf_restore_index_cap_exceeded` or
 * `native_mpf_restore_read_transient`), which readiness reports, and the
 * preparation returns `NATIVE_RESTORE_HELD`. The owner refuses before it
 * changes its marker and before the SQL transaction opens, so the plan stays
 * retained, its disposition keeps the history gate closed, and every
 * evaluation retries the restore on the owner's backoff. A completion clears
 * whichever of the three reasons it raised. Any other failure propagates
 * unchanged.
 */
export const heldOnNativeRestoreRefusal =
  (source: string, notRetainedReason: string, subject: string) =>
  <A, E, R>(work: Effect.Effect<A, E, R>) =>
    Effect.flatMap(Globals, (globals) =>
      work.pipe(
        Effect.tap(() =>
          clearNativeRestoreRefusal(globals, source, notRetainedReason),
        ),
        Effect.catchAllCause(
          (cause) =>
            holdNativeRestoreRefusal({
              globals,
              source,
              notRetainedReason,
              subject,
              cause,
            })?.pipe(Effect.as(NATIVE_RESTORE_HELD)) ?? Effect.failCause(cause),
        ),
      ),
    );

/** Production ordering for a source-authorized dependent rollback. The plan was
 * committed under recovery authority before this call; native mutation holds no
 * SQL transaction. SQL disposition and its applied receipt are one transaction.
 * Cache reload and Ready remain exclusively the source owner's responsibility.
 * Repeating after interruption uses the retained native operation ID, including
 * when its marker changed but its acknowledgement or SQL transaction was lost.
 */
export const executeHistoryDependentRecovery = <E, R>(input: {
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly plan: DependentRecoveryPlan;
  readonly owner: NativeMpfOwnerService;
  /** Must recheck exact journal/preimages as part of this bounded SQL work. */
  readonly repair: Effect.Effect<void, E, R>;
  /** Bounded in-memory publication after SQL COMMIT, before cancellation can
   * return control. No IO or fallible work; process restart rebuilds these refs. */
  readonly afterSqlCommit: Effect.Effect<void>;
}) =>
  Effect.gen(function* () {
    yield* input.preparation.assertCurrent;
    yield* Effect.tryPromise({
      try: () => input.owner.restoreCanonicalRoot(input.plan.native),
      catch: (cause) =>
        new DatabaseError({
          table: "event_history_recovery_plans",
          message: "Native dependent rollback requires resumable recovery",
          cause,
        }),
    });
    yield* input.preparation.assertCurrent;
    yield* Effect.uninterruptible(
      Authority.withRecovery(
        input.preparation.token,
        input.preparation.assertCurrent.pipe(
          Effect.zipRight(
            applyHistoryRecoveryPlan(
              input.checkpoint,
              input.plan,
              input.repair,
            ),
          ),
          Effect.tap(() => input.preparation.assertCurrent),
        ),
      ).pipe(Effect.zipRight(input.afterSqlCommit)),
    );
    yield* input.preparation.assertCurrent;
  });
