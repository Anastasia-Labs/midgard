import type * as SDK from "@al-ft/midgard-sdk";

/** The linear L2 fee schedule a header commits. */
export type JourneyFeeSchedule = Pick<SDK.Header, "minFeeA" | "minFeeB">;

/** Fee rounds before the priced transaction stops growing. */
const FEE_ROUNDS = 32;

/**
 * `build(fee)` at a fee covering the minimum its own canonical size prices
 * under `schedule`: `minFeeA` per canonical byte plus `minFeeB`, as replay
 * prices it. A forced order's envelope drops the native validity flag's byte
 * and replay adds one byte back for a forced source, so the native size
 * prices a forced transaction too. Starts at `minFeeB` and raises the fee to
 * each round's price until the transaction pays it. `available`, when given,
 * is the value the fee is paid from.
 */
export const buildAtJourneyMinFee = <T extends { canonicalCbor: Buffer }>(
  schedule: JourneyFeeSchedule,
  build: (fee: bigint) => T,
  available?: bigint,
): T => {
  let fee = schedule.minFeeB;
  let transaction = build(fee);
  for (let round = 0; ; round += 1) {
    const required =
      schedule.minFeeA * BigInt(transaction.canonicalCbor.length) +
      schedule.minFeeB;
    if (fee >= required) return transaction;
    if (round === FEE_ROUNDS)
      throw new Error("Journey transaction fee did not settle");
    if (available !== undefined && required >= available)
      throw new Error("Journey transaction has insufficient value for its fee");
    fee = required;
    transaction = build(fee);
  }
};
