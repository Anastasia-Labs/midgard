/**
 * What the settlement worker reports (`/readyz`, `settlement.detail`) while a
 * journaled settlement transaction is not yet confirmed. The devnet's
 * settlement kill drill recognises both.
 */
export const settlementAttemptDetail = {
  /** S6 is sending it; no block holds it yet. */
  journaled: (txHash: string) =>
    `settlement transaction ${txHash} journaled; S6 sends its exact bytes until it lands`,
  /** A block holds it, short of confirmation depth. */
  landed: (txHash: string, depth: number) =>
    `settlement transaction ${txHash} landed at depth ${depth}; waiting for confirmation depth`,
};
