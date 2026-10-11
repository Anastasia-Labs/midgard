import { Address, getAddressDetails } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ActiveOperatorUTxO } from "./active-operators.js";
import { AddressData } from "./common.fraud-proofs.js";
import { Bech32DeserializationError, LucidError } from "./errors.js";
import { RetiredOperatorUTxO } from "./retired-operators.js";

/**
 * The address's ledger data. A pointer address fails: event-history admission
 * accepts only orders the node decodes, and the node does not map a pointer
 * stake credential, so an order carrying one would never be admitted.
 */
export const addressDataFromBech32 = (
  address: Address,
): Effect.Effect<AddressData, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const addressDetails = yield* Effect.try({
      try: () => getAddressDetails(address),
      catch: (error) =>
        new Bech32DeserializationError({
          message: `Failed to parse address: ${address}`,
          cause: error,
        }),
    });
    const { paymentCredential, stakeCredential, type } = addressDetails;

    if (type === "Pointer") {
      return yield* Effect.fail(
        new Bech32DeserializationError({
          message: "Pointer addresses are not supported",
          cause: `Invalid address: ${address}`,
        }),
      );
    }

    if (!paymentCredential) {
      return yield* Effect.fail(
        new Bech32DeserializationError({
          message: "Address missing payment credential",
          cause: `Invalid address: ${address}`,
        }),
      );
    }

    return {
      paymentCredential:
        paymentCredential.type === "Key"
          ? { PublicKeyCredential: [paymentCredential.hash] }
          : { ScriptCredential: [paymentCredential.hash] },
      stakeCredential: stakeCredential
        ? {
            Inline: [
              stakeCredential.type === "Key"
                ? { PublicKeyCredential: [stakeCredential.hash] }
                : { ScriptCredential: [stakeCredential.hash] },
            ],
          }
        : null,
    };
  });

/**
 * TODO: Move to the `operatorDirectory` module after refactoring.`
 */
export const findOperatorByPKH = (
  activeOperators: ActiveOperatorUTxO[],
  retiredOperators: RetiredOperatorUTxO[],
  operatorPKH: string,
): Effect.Effect<
  | (ActiveOperatorUTxO & { isActive: true })
  | (RetiredOperatorUTxO & { isActive: false }),
  LucidError
> => {
  const activeOperatorMatch = activeOperators.find((utxo) =>
    utxo.assetName.endsWith(operatorPKH),
  );
  if (activeOperatorMatch !== undefined) {
    return Effect.succeed({ ...activeOperatorMatch, isActive: true });
  }

  const retiredOperatorMatch = retiredOperators.find((utxo) =>
    utxo.assetName.endsWith(operatorPKH),
  );
  if (retiredOperatorMatch !== undefined) {
    return Effect.succeed({ ...retiredOperatorMatch, isActive: false });
  }

  return Effect.fail(
    new LucidError({
      message: `No Operator UTxO with key "${operatorPKH}" found`,
      cause: "Operator not found in active or retired UTxOs",
    }),
  );
};
