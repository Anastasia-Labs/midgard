/**
 * The pooled DA bond states in which Apply is refused before it is built.
 *
 * - `pool-under-backed`: the pool backs less than one `da_bond_lovelace` above
 *   its floor.
 * - `pool-withdrawing`: the pool is `Withdrawing` and backs no new attestation
 *   until the withdrawal is cancelled.
 * - `pool-unavailable`: the authentic pool could not be fetched.
 */
export const DA_BOND_POOL_APPLY_BACKOFF_REASONS = [
  "pool-under-backed",
  "pool-unavailable",
  "pool-withdrawing",
] as const;

export type DaBondPoolApplyBackoffReason =
  (typeof DA_BOND_POOL_APPLY_BACKOFF_REASONS)[number];

export const isDaBondPoolApplyBackoffReason = (
  reason: string,
): reason is DaBondPoolApplyBackoffReason =>
  (DA_BOND_POOL_APPLY_BACKOFF_REASONS as readonly string[]).includes(reason);

const REASON_SUMMARY: Readonly<Record<DaBondPoolApplyBackoffReason, string>> = {
  "pool-under-backed":
    "the pooled DA bond backs less than one DA bond above its floor; the committee must top it up",
  "pool-withdrawing":
    "the pooled DA bond is withdrawing and backs no new attestation until the withdrawal is cancelled",
  "pool-unavailable": "the authentic pooled DA bond could not be read from L1",
};

/**
 * Apply was refused because of the pooled DA bond's state, not because the
 * transaction lost a race. Init is refused for the same states (`stage`
 * `init`): an attestation the pool cannot let Apply only lapses, and its
 * min-ADA and fees are spent for nothing. Retrying at once cannot help: the pool changes only
 * when an operator tops it up, cancels a withdrawal, or L1 becomes readable
 * again. The coordinator therefore does not treat it as a recoverable race; it
 * reports the header as not posted, the node keeps running, and the next
 * reconcile tries again.
 *
 * The message names the reason and never embeds the underlying fetch error,
 * which is kept in `detail`: that text can mention a missing or spent UTxO and
 * would otherwise read as a race to anything matching on messages.
 */
export class DaBondPoolApplyBackoffError extends Error {
  override readonly name = "DaBondPoolApplyBackoffError";

  constructor(
    readonly reason: DaBondPoolApplyBackoffReason,
    readonly detail: string,
    readonly stage: "init" | "apply" = "apply",
  ) {
    super(
      `DA attestation ${stage} backed off (${reason}): ${REASON_SUMMARY[reason]}`,
    );
  }
}
