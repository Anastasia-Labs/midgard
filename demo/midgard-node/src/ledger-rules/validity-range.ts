import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Validates the transaction's validity interval against the current slot.
 *
 * The validity interval is defined by:
 * - Lower bound (validity_interval_start): Inclusive. Transaction is only valid starting from this slot.
 * - Upper bound (ttl - Time To Live): Inclusive. Transaction is invalid after this slot.
 *
 * If either bound is not specified (null), that side of the interval is unbounded.
 *
 * @param currentSlot - The current slot number to validate against
 * @param body - The transaction body containing validity interval parameters
 * @throws Error if the currentSlot is outside the validity interval
 */
export const validateValidityRange = (
  currentSlot: number,
  body: CML.TransactionBody,
) =>
  Effect.gen(function* () {
    const lowerBound = body.validity_interval_start()
      ? parseInt(body.validity_interval_start()!.toString())
      : null;
    const upperBound = body.ttl() ? parseInt(body.ttl()!.toString()) : null;

    if (Number.isInteger(lowerBound) && currentSlot < lowerBound!) {
      throw new Error(
        `Lower bound (${lowerBound}) not in slot range (${currentSlot}).`,
      );
    }

    if (Number.isInteger(upperBound) && currentSlot > upperBound!) {
      throw new Error(
        `Upper bound (${upperBound}) not in slot range (${currentSlot}).`,
      );
    }
    return true;
  });
