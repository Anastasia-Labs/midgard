/** The in-flight port of the validation-trace dispute's own view (own-view.ts
 * `inFlight`): each transaction the process's in-flight set holds for the
 * workflow, decoded from its signed bytes. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type { InFlightTransaction } from "../workflow/in-flight.js";
import type { ValidationTraceInFlight } from "./own-view.js";

const decoded = new WeakMap<InFlightTransaction, ValidationTraceInFlight>();

/** The out-refs `inputs` names, in order; frees each input it reads. */
const outRefsOf = (inputs: CML.TransactionInputList): string[] =>
  Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    const id = input.transaction_id();
    try {
      return `${id.to_hex()}#${input.index().toString()}`;
    } finally {
      id.free();
      input.free();
    }
  });

/** One output's address and inline datum; frees what it reads. */
const createdBy = (
  output: CML.TransactionOutput,
): Readonly<{ address: string; datumCbor: string | undefined }> => {
  const option = output.datum();
  const datum = option?.as_datum();
  try {
    return {
      address: coreToTxOutput(output).address,
      datumCbor: datum?.to_cbor_hex(),
    };
  } finally {
    datum?.free();
    option?.free();
    output.free();
  }
};

/** `transaction` as the own view's in-flight transaction, decoded once.
 * Every wasm object read is freed once its plain values are out: a turn
 * that decodes many large transactions must not leave them to finalizers. */
const inFlightOf = (
  transaction: InFlightTransaction,
): ValidationTraceInFlight => {
  const known = decoded.get(transaction);
  if (known !== undefined) return known;
  const tx = CML.Transaction.from_cbor_hex(
    transaction.signedTransactionCborHex,
  );
  const body = tx.body();
  const inputs = body.inputs();
  const references = body.reference_inputs();
  const outputs = body.outputs();
  try {
    const ttl = body.ttl();
    const found = Object.freeze({
      txHash: transaction.txHash,
      spends: outRefsOf(inputs),
      reads: references === undefined ? [] : outRefsOf(references),
      creates: Array.from({ length: outputs.len() }, (_, index) => ({
        outRef: `${transaction.txHash}#${index.toString()}`,
        ...createdBy(outputs.get(index)),
      })),
      validTo: ttl === undefined ? undefined : Number(ttl),
    });
    decoded.set(transaction, found);
    return found;
  } finally {
    outputs.free();
    references?.free();
    inputs.free();
    body.free();
    tx.free();
  }
};

/** The transactions `inFlight` holds, oldest first. */
export const validationTraceInFlight = (
  inFlight: readonly InFlightTransaction[],
): readonly ValidationTraceInFlight[] => inFlight.map(inFlightOf);
