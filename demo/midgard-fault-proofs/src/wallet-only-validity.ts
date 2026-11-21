/**
 * The validity window of a fault-proof transaction whose inputs are only
 * wallet coins.
 *
 * A transaction that spends the proof thread, a certificate, a run or any
 * other specific output of ours cannot land twice: its own input excludes a
 * duplicate. A transaction that spends only wallet coins has no such input,
 * so a copy rebuilt after a restart could land beside the original. Each one
 * therefore carries a short upper validity bound: after a restart the watcher
 * waits out one window, and from then on the chain alone says whether the
 * transaction landed, because the ledger no longer accepts it.
 *
 * A field-preimage carriage publication carries no bound: a full chunk's
 * publication already lands exactly on the publication size target, with no
 * byte to spare for one. A duplicate is harmless: it is a second output at the
 * prover's own proof-materials address holding the same datum, costing only
 * that output's min-ada, which the proof-materials sweep recovers, and the step
 * that reads the carriage is indifferent to which copy it reads.
 */
import { slotAlignedUpperBoundAtOrBefore } from "@al-ft/midgard-sdk/core";
import type { LucidEvolution } from "@lucid-evolution/lucid";

/**
 * How long a wallet-only transaction stays valid after it is built: three
 * minutes.
 *
 * The watcher waits this long after a restart before rebuilding such a
 * transaction, and that wait spends proof time. It must stay well inside the
 * worst-case proof-time bound (half of block maturity, 7.5 minutes on the
 * 15-minute profiles; `MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs` in
 * `@al-ft/midgard-core/retention-window`), and is held to at most half of
 * that bound on the shortest-maturity deployment profile.
 */
export const WALLET_ONLY_VALIDITY_MS = 180_000;

/**
 * The upper validity bound (POSIX ms, for `validTo`) of a wallet-only
 * transaction built now: the current slot's time plus
 * {@link WALLET_ONLY_VALIDITY_MS}, at a slot boundary at or before it.
 */
export const walletOnlyValidTo = (
  lucid: Pick<
    LucidEvolution,
    "currentSlot" | "slotToUnixTime" | "unixTimeToSlot"
  >,
): number => {
  const now = lucid.slotToUnixTime(lucid.currentSlot());
  return Number(
    slotAlignedUpperBoundAtOrBefore(
      lucid,
      BigInt(now) + BigInt(WALLET_ONLY_VALIDITY_MS),
    ),
  );
};
