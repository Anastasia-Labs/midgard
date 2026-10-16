/**
 * Who a withdrawal classification's throw is attributed to.
 *
 * The SDK classifier (`classifyWithdrawalFromLedger`) fails typed on input
 * it cannot decode, and returns an invalid signature for input-determined
 * signature failures; a throw from it is a runtime fault of the code it
 * runs. A few of its synchronous reads of the input (the event info's
 * body field, the ledger output's address and value) run outside its typed
 * failures, so a throw is named local only when those reads succeed here
 * first, on input within a bound the Data parser handles: then the throw
 * is not the input's. Any other throw keeps its attribution.
 */
import {
  decodeMidgardAddressText,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardCardanoValueCborLength,
} from "@al-ft/midgard-core/codec";
import {
  assertMidgardPlutusDataWellFormed,
  countPlutusDataCborNodes,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { WithdrawalLedgerClassificationInput } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { ReplayLocalFault } from "./block-replay.faults.js";

/**
 * The most Data nodes an event info may carry for a throw to be named
 * local. It bounds the info's nesting well below the depth (about 1,522
 * nodes) at which the wasm Data parser exhausts its stack.
 */
export const WITHDRAWAL_INFO_NODE_BOUND = 1_024n;

/** Whether the classifier's untyped reads of `input` succeed, on an event
 * info within {@link WITHDRAWAL_INFO_NODE_BOUND}. */
export const withdrawalClassificationInputBounded = (
  input: WithdrawalLedgerClassificationInput,
): boolean => {
  try {
    assertMidgardPlutusDataWellFormed(Buffer.from(input.eventInfoCbor, "hex"));
    countPlutusDataCborNodes(input.eventInfoCbor, WITHDRAWAL_INFO_NODE_BOUND);
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
 * {@link withdrawalClassificationInputBounded} admits re-raised as a
 * `ReplayLocalFault`. Any other throw passes through unchanged.
 */
export const attributeWithdrawalClassificationFault = <A, E, R>(
  input: WithdrawalLedgerClassificationInput,
  classification: Effect.Effect<A, E, R>,
): Effect.Effect<A, E, R> =>
  Effect.catchAllDefect(classification, (cause) =>
    Effect.die(
      withdrawalClassificationInputBounded(input)
        ? new ReplayLocalFault({
            message: `withdrawal classification failed locally: ${String(cause)}`,
            cause,
          })
        : cause,
    ),
  );
