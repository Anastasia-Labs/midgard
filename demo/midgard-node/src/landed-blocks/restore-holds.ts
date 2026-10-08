/**
 * The named holds a refused native restore raises while the landed-block
 * rebase moves native MPF (plan §1.4 "MPF interim", N4): the only
 * `restoreCanonicalRoot` caller. A refusal comes before the owner changes
 * its marker, so nothing changed: the rebase fails under its source with
 * the refusal's reason, the process stays up, the history gate stays
 * closed (holding block production), and the history owner retries the
 * rebase on its backoff. The next rebase evaluation that runs, or finds no
 * rebase due, clears the reason.
 */
import {
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  NATIVE_MPF_RESTORE_ROOT_NOT_RETAINED,
} from "../services/liveness-halt.js";

/**
 * Every `restoreCanonicalRoot` the rebase tried was refused with
 * `NativeMpfRootNotRetained`: the store retains no root of the processed
 * landed chain in full.
 */
export class LandedChainRootNotRetained extends Error {
  readonly _tag = "LandedChainRootNotRetained";
  constructor(
    readonly durableRoot: string,
    readonly frontierRoot: string,
  ) {
    super(
      `The native MPF retains no root of the processed landed chain (durable root ${durableRoot}, confirmed-ledger frontier root ${frontierRoot}). The native MPF store keeps every root it promoted, and the frontier's root was promoted when its block was applied, so the store at LEDGER_MPF_DB_PATH lost it (replaced, restored from an older copy or damaged). The rebase holds with native MPF, the SQL root and the journals unchanged, and the history owner retries it on its backoff while the history gate stays closed, which holds block production. Operator action is needed: stop the node, install at LEDGER_MPF_DB_PATH a native MPF store that retains root ${frontierRoot} in full (such as a copy of this node's store taken at or after that root), and restart it; the next rebase completes.`,
    );
  }
}

const RESTORE_HOLDS = new Map<
  string,
  Readonly<{ reason: string; escalateAfterMs: number }>
>([
  [
    "LandedChainRootNotRetained",
    { reason: NATIVE_MPF_RESTORE_ROOT_NOT_RETAINED, escalateAfterMs: 0 },
  ],
  [
    "NativeMpfFullIndexCapExceeded",
    { reason: NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED, escalateAfterMs: 0 },
  ],
  [
    "NativeMpfRestoreReadFailed",
    {
      reason: NATIVE_MPF_RESTORE_READ_TRANSIENT,
      escalateAfterMs: NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
    },
  ],
]);

/**
 * The named hold for the first refused native restore on a failure's cause
 * chain, matched by its tag (a refusal crosses the native child's boundary
 * by tag, not by class): a store that lacks the root, or a cap it is over,
 * escalates at once; a read failure gets
 * `NATIVE_MPF_RESTORE_READ_ESCALATION_MS` to pass.
 */
export const restoreRefusalHold = (chain: readonly unknown[]) => {
  for (const cause of chain) {
    const tag =
      typeof cause === "object" && cause !== null
        ? (cause as { readonly _tag?: unknown })._tag
        : undefined;
    const hold = typeof tag === "string" ? RESTORE_HOLDS.get(tag) : undefined;
    if (hold !== undefined) return hold;
  }
  return undefined;
};
