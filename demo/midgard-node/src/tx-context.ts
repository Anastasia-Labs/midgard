/**
 * Canonical TxOutRef ordering helpers for the node.
 * This module keeps ledger-context input/reference-input ordering rules in one
 * place so workers do not reimplement them.
 */
import {
  compareOutRefs as compareCoreOutRefs,
  outRefLabel as coreOutRefLabel,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";

/**
 * Lightweight transaction-output reference shape used for ordering.
 */
export type { OutRefLike };

/**
 * Canonical ledger ordering for inputs and reference inputs: lexicographic by
 * TxOutRef (`txHash`, then `outputIndex`).
 */
export const compareOutRefs = compareCoreOutRefs;

/**
 * Formats an outref as `txHash#outputIndex`.
 */
export const outRefLabel = coreOutRefLabel;

/**
 * Removes duplicate outrefs while preserving first-seen order.
 */
export const dedupeByOutRef = <T extends OutRefLike>(
  outRefs: readonly T[],
): readonly T[] => {
  const byOutRef = new Map<string, T>();
  for (const outRef of outRefs) {
    const label = outRefLabel(outRef);
    if (!byOutRef.has(label)) {
      byOutRef.set(label, outRef);
    }
  }
  return [...byOutRef.values()];
};
