import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Validates that a block's event interval is fully contained within a transaction's validity interval.
 *
 * Formal rule:
 * The block's time range must be a subset of the transaction's validity interval.
 *
 * Specification:
 * - A transaction `t` is only valid if:
 *   time_range(b) ⊆ validity_interval(t)
 *
 * Validity interval:
 * - Lower bound (`validity_interval_start`): Inclusive. The transaction is only valid starting from this slot.
 * - Upper bound (`ttl`, Time To Live): Inclusive. The transaction becomes invalid after this slot.
 * - If either bound is not specified (null), it is treated as unbounded (open-ended).
 *
 * Block's event interval:
 * - Defined by `blockRange.start` and `blockRange.end` (both inclusive).
 * - Both values are expected to be integers representing slots.
 *
 * @param blockRange - The block's event interval `{ start, end }` to validate against
 * @param body - The transaction body containing validity interval parameters
 * @throws Error if the block's event interval falls outside the transaction's validity interval
 * @returns `true` if the block's interval is fully within the transaction's validity interval
 */
export const validateValidityRange = (
  blockRange: { start: number; end: number },
  body: CML.TransactionBody,
) =>
  Effect.gen(function* () {
    const lowerBound = body.validity_interval_start()
      ? parseInt(body.validity_interval_start()!.toString())
      : null;
    const upperBound = body.ttl() ? parseInt(body.ttl()!.toString()) : null;
    // Validate block's start time against lower bound
    if (lowerBound !== null && blockRange.start < lowerBound) {
      throw new Error(
        `Block start (${blockRange.start}) is before transaction's lower bound (${lowerBound}).`,
      );
    }
    // Validate block's end time against upper bound
    if (upperBound !== null && blockRange.end > upperBound) {
      throw new Error(
        `Block end (${blockRange.end}) is after transaction's upper bound (${upperBound}).`,
      );
    }
    return true;
  });
