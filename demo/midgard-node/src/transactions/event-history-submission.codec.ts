import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import type * as Journal from "../database/eventHistorySubmissions.js";

const encodeAssets = (assets: Assets) =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, amount.toString()]),
  );
const decodeAssets = (assets: Readonly<Record<string, string>>): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, BigInt(amount)]),
  );

export const encodeHistorySubmissionRequest = (
  request: SDK.EventHistorySubmissionRequest,
): Journal.StoredRequest => {
  if (
    request.nonce.datum != null ||
    request.nonce.datumHash != null ||
    request.nonce.scriptRef != null
  )
    throw new Error("History nonce must be a plain wallet output");
  if (request.payload !== undefined && request.payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const payloadCbor =
    request.payloadCbor ?? Data.to(request.payload, SDK.EventHistoryPayload);
  Data.from(payloadCbor, SDK.EventHistoryPayload);
  return {
    payloadCbor,
    reclaimAuthCbor: Data.to(request.reclaimAuth, SDK.CredentialD),
    assets: encodeAssets(request.assets),
    structuralLovelace: request.structuralLovelace.toString(),
    structuralRefundKey: request.structuralRefundKey,
    nonce: {
      txHash: request.nonce.txHash,
      outputIndex: request.nonce.outputIndex,
      address: request.nonce.address,
      assets: encodeAssets(request.nonce.assets),
    },
  };
};

export const decodeHistorySubmissionRequest = (
  request: Journal.StoredRequest,
): Extract<SDK.EventHistorySubmissionRequest, { payloadCbor: string }> => {
  Data.from(request.payloadCbor, SDK.EventHistoryPayload);
  return {
    payloadCbor: request.payloadCbor,
    reclaimAuth: Data.from(request.reclaimAuthCbor, SDK.CredentialD),
    assets: decodeAssets(request.assets),
    structuralLovelace: BigInt(request.structuralLovelace),
    structuralRefundKey: request.structuralRefundKey,
    nonce: { ...request.nonce, assets: decodeAssets(request.nonce.assets) },
  };
};

export const assertHistorySubmissionAttempt = (
  attempt: SDK.EventHistorySubmissionAttempt,
) => {
  const transaction = CML.Transaction.from_cbor_hex(attempt.transactionCbor);
  if (
    CML.hash_transaction(transaction.body()).to_hex() !== attempt.txHash ||
    !Number.isSafeInteger(attempt.outputIndex) ||
    attempt.outputIndex < 0 ||
    attempt.outputIndex >= transaction.body().outputs().len()
  )
    throw new Error("History attempt does not match its completed transaction");
};

export const historyAdmissionMetadata = (
  lucid: LucidEvolution,
  attempt: SDK.EventHistorySubmissionAttempt,
) => {
  assertHistorySubmissionAttempt(attempt);
  if (attempt.phase !== "Admission")
    throw new Error("Expected history admission attempt");
  const body = CML.Transaction.from_cbor_hex(attempt.transactionCbor).body();
  const output = coreToTxOutput(body.outputs().get(attempt.outputIndex));
  if (output.datum == null || body.ttl() === undefined)
    throw new Error("History admission lacks a datum or finite validity bound");
  const node = Data.from(output.datum, SDK.EventHistoryNode);
  if (
    node.position === "Root" ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    throw new Error("History admission output is not an Order");
  return {
    output,
    key: node.position.Key[0],
    facts: node.payload.Order.facts,
    validTo: lucid.slotToUnixTime(Number(body.ttl())),
  };
};
