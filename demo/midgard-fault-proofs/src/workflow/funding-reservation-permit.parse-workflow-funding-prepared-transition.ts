import { createHash } from "node:crypto";

import { CML, coreToUtxo } from "@lucid-evolution/lucid";

import {
  ACTION_KIND,
  canonicalOutRefs,
  DIGEST,
  exact,
  reservedInput,
  type WorkflowFundingPreparedTransition,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { journalJsonDigest } from "./journal.js";

export const parseWorkflowFundingPreparedTransition = (
  value: unknown,
): WorkflowFundingPreparedTransition => {
  const record = exact(
    value,
    [
      "actionKind",
      "signedTransactionCborHex",
      "transactionHash",
      "transactionBodySha256",
      "consumedOutRefs",
      "producedInputs",
    ],
    "funding prepared transition",
  );
  if (
    typeof record.actionKind !== "string" ||
    !ACTION_KIND.test(record.actionKind) ||
    typeof record.signedTransactionCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(record.signedTransactionCborHex) ||
    typeof record.transactionHash !== "string" ||
    !DIGEST.test(record.transactionHash) ||
    typeof record.transactionBodySha256 !== "string" ||
    !DIGEST.test(record.transactionBodySha256) ||
    !Array.isArray(record.consumedOutRefs) ||
    record.consumedOutRefs.some((outRef) => typeof outRef !== "string") ||
    !Array.isArray(record.producedInputs)
  )
    throw new Error("funding prepared transition is malformed");
  const transaction = CML.Transaction.from_cbor_hex(
    record.signedTransactionCborHex,
  );
  if (
    CML.hash_transaction(transaction.body()).to_hex() !==
      record.transactionHash ||
    createHash("sha256")
      .update(transaction.body().to_cbor_bytes())
      .digest("hex") !== record.transactionBodySha256
  )
    throw new Error(
      "funding prepared transaction bytes changed their identity",
    );
  const consumedOutRefs = canonicalOutRefs(
    record.consumedOutRefs as string[],
    "funding prepared consumed inputs",
  );
  const inputs = transaction.body().inputs();
  const actual = new Set(
    Array.from({ length: inputs.len() }, (_, index) => {
      const input = inputs.get(index);
      return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    }),
  );
  if (consumedOutRefs.some((outRef) => !actual.has(outRef)))
    throw new Error(
      "funding prepared consumed input is outside the signed transaction",
    );
  const producedInputs = record.producedInputs.map((input, index) =>
    reservedInput(input, `funding produced input ${index.toString()}`),
  );
  canonicalOutRefs(
    producedInputs.map(({ outRef }) => outRef),
    "funding prepared produced inputs",
  );
  const transactionHash = record.transactionHash;
  const outputs = transaction.body().outputs();
  for (const produced of producedInputs) {
    const index = Number(produced.outRef.split("#")[1]);
    if (
      !produced.outRef.startsWith(`${transactionHash}#`) ||
      !Number.isSafeInteger(index) ||
      index >= outputs.len() ||
      produced.role !== "funding"
    )
      throw new Error("funding produced input belongs to another transaction");
    const decoded = coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(transactionHash),
          BigInt(index),
        ),
        outputs.get(index),
      ),
    );
    const assets = Object.entries(decoded.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([unit, quantity]) => ({ unit, quantity: quantity.toString() }));
    if (
      decoded.assets.lovelace?.toString() !== produced.lovelace ||
      journalJsonDigest(assets) !== journalJsonDigest(produced.assets)
    )
      throw new Error("funding produced input changed its signed output value");
  }
  return Object.freeze({
    actionKind: record.actionKind,
    signedTransactionCborHex: record.signedTransactionCborHex,
    transactionHash: record.transactionHash,
    transactionBodySha256: record.transactionBodySha256,
    consumedOutRefs,
    producedInputs: Object.freeze(producedInputs),
  });
};
