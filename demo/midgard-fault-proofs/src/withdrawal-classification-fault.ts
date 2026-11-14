/**
 * Who a withdrawal classification's throw is attributed to.
 *
 * The SDK classifier (`classifyWithdrawalFromLedger`) fails typed on input
 * it cannot decode, and returns an invalid signature for input-determined
 * signature failures; a throw from it is a runtime fault of the code it
 * runs. A few of its synchronous reads of the input (the event info's
 * body field, the ledger output's address and value) run outside its typed
 * failures, so a throw is named local only when the info is well-formed
 * Data and those reads succeed here first: then the throw is not the
 * input's, however deep the info nests. Any other throw keeps its
 * attribution.
 */
import {
  decodeMidgardAddressText,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardCardanoValueCborLength,
} from "@al-ft/midgard-core/codec";
import {
  assertMidgardPlutusDataWellFormed,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { WithdrawalLedgerClassificationInput } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { ReplayLocalFault } from "./block-replay.faults.js";

/** Whether `input`'s event info is well-formed Data and the classifier's
 * untyped reads of `input` succeed. */
export const withdrawalClassificationInputReadable = (
  input: WithdrawalLedgerClassificationInput,
): boolean => {
  try {
    assertMidgardPlutusDataWellFormed(Buffer.from(input.eventInfoCbor, "hex"));
    plutusConstrFieldCbor(input.eventInfoCbor, [0]);
    if (input.ledgerOutput !== null) {
      const output = decodeMidgardTxOutput(input.ledgerOutput);
      decodeMidgardAddressText(encodeMidgardAddressText(output.address));
      midgardCardanoValueCborLength(output.value);
    }
    return true;
  } catch {
    return false;
  }
};

/**
 * `classification` (the classifier run on `input`), with a throw on input
 * {@link withdrawalClassificationInputReadable} admits re-raised as a
 * `ReplayLocalFault`. Any other throw passes through unchanged.
 */
export const attributeWithdrawalClassificationFault = <A, E, R>(
  input: WithdrawalLedgerClassificationInput,
  classification: Effect.Effect<A, E, R>,
): Effect.Effect<A, E, R> =>
  Effect.catchAllDefect(classification, (cause) =>
    Effect.die(
      withdrawalClassificationInputReadable(input)
        ? new ReplayLocalFault({
            message: `withdrawal classification failed locally: ${String(cause)}`,
            cause,
          })
        : cause,
    ),
  );
