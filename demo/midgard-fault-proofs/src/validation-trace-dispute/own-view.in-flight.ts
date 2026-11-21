/** The in-flight port of the validation-trace dispute's own view (own-view.ts
 * `inFlight`): each transaction the process's in-flight set holds for the
 * workflow, decoded from its signed bytes. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type { InFlightTransaction } from "../workflow/in-flight.js";
import type { ValidationTraceInFlight } from "./own-view.js";

const decoded = new WeakMap<InFlightTransaction, ValidationTraceInFlight>();

/** `transaction` as the own view's in-flight transaction, decoded once. */
const inFlightOf = (
  transaction: InFlightTransaction,
): ValidationTraceInFlight => {
  const known = decoded.get(transaction);
  if (known !== undefined) return known;
  const body = CML.Transaction.from_cbor_hex(
    transaction.signedTransactionCborHex,
  ).body();
  const inputs = body.inputs();
  const references = body.reference_inputs();
  const outputs = body.outputs();
  const ttl = body.ttl();
  const outRefOf = (input: CML.TransactionInput) =>
    `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  const found = Object.freeze({
    txHash: transaction.txHash,
    spends: Array.from({ length: inputs.len() }, (_, index) =>
      outRefOf(inputs.get(index)),
    ),
    reads: Array.from({ length: references?.len() ?? 0 }, (_, index) =>
      outRefOf(references!.get(index)),
    ),
    creates: Array.from({ length: outputs.len() }, (_, index) => {
      const output = outputs.get(index);
      const { address } = coreToTxOutput(output);
      return {
        outRef: `${transaction.txHash}#${index.toString()}`,
        address,
        datumCbor: output.datum()?.as_datum()?.to_cbor_hex(),
      };
    }),
    validTo: ttl === undefined ? undefined : Number(ttl),
  });
  decoded.set(transaction, found);
  return found;
};

/** The transactions `inFlight` holds, oldest first. */
export const validationTraceInFlight = (
  inFlight: readonly InFlightTransaction[],
): readonly ValidationTraceInFlight[] => inFlight.map(inFlightOf);
