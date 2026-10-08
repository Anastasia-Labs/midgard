import { Effect } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  applyHistoryRecoveryPlan,
  type DependentRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import { DatabaseError } from "../database/utils/common.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { NATIVE_MPF_RESTORE_READ_ESCALATION_MS } from "./liveness-halt.js";
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

/** How long a hold on `kind` stays raised before it escalates: a store that
 * lacks the root, or a cap it is over, needs the operator at once; a read
 * failure gets `NATIVE_MPF_RESTORE_READ_ESCALATION_MS` to pass. */
export const nativeRestoreHoldEscalation = (
  kind: NativeRestoreRefusal["kind"],
): number =>
  kind === "read_transient" ? NATIVE_MPF_RESTORE_READ_ESCALATION_MS : 0;

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
