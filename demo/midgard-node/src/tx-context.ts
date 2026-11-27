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
