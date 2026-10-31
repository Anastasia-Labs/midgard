import { assetsEqual } from "@al-ft/midgard-core/assets";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { decodeAcceptanceCanonicalTransaction } from "./acceptance-payout-transaction.js";
import {
  type AcceptanceCanonicalTransaction,
  acceptanceOutRefKey,
  type AcceptancePayoutConfig,
  type AcceptanceTransaction,
  requireAcceptance,
} from "./acceptance-payout-types.js";

/** An admitted Order may be the predecessor recreated by insertion or retirement. */
export const traceAcceptanceOrderSuccessors = (
  original: AcceptanceTransaction,
  originalIndex: number,
  eventKey: string,
  config: AcceptancePayoutConfig,
  evidence: readonly AcceptanceCanonicalTransaction[],
) => {
  const unit = config.withdrawalPolicyId + eventKey;
  const originalOutput = original.outputs[originalIndex]!;
  const facts = plutusConstrFieldCbor(originalOutput.datum!, [3, 0]);
  const remaining = evidence.map((row) =>
    decodeAcceptanceCanonicalTransaction(row, config),
  );
  requireAcceptance(
    new Set([original.txHash, ...remaining.map((tx) => tx.txHash)]).size ===
      remaining.length + 1,
    "duplicate Order update transaction",
  );
  let current = { txHash: original.txHash, outputIndex: originalIndex };
  const lineage: { phase: string; txHash: string }[] = [];
  while (remaining.length > 0) {
    const next = remaining.filter((tx) =>
      tx.inputs.some(
        (ref) => acceptanceOutRefKey(ref) === acceptanceOutRefKey(current),
      ),
    );
    requireAcceptance(
      next.length === 1,
      "Order next update is missing or ambiguous",
    );
    const tx = next[0]!;
    remaining.splice(remaining.indexOf(tx), 1);
    requireAcceptance(
      tx.mint[unit] === undefined,
      "Order update mints or burns its event NFT",
    );
    const indexes = tx.outputs.flatMap((output, index) =>
      output.assets[unit] === undefined ? [] : [index],
    );
    requireAcceptance(
      indexes.length === 1,
      "Order update lacks unique preserved NFT",
    );
    const index = indexes[0]!;
    const output = tx.outputs[index]!;
    requireAcceptance(
      output.assets[unit] === 1n &&
        output.address === config.withdrawalAddress &&
        output.datum !== undefined &&
        output.datumHash === undefined &&
        output.scriptRef === undefined &&
        assetsEqual(output.assets, originalOutput.assets),
      "Order update NFT/address/datum/full value mismatch",
    );
    const node = Data.from(output.datum!, SDK.EventHistoryNode);
    requireAcceptance(
      typeof node.position === "object" &&
        node.position.Key[0] === eventKey &&
        typeof node.payload === "object" &&
        "Order" in node.payload &&
        plutusConstrFieldCbor(output.datum!, [3, 0]) === facts,
      "Order update facts/position changed",
    );
    current = { txHash: tx.txHash, outputIndex: index };
    lineage.push({ phase: "order-update", txHash: tx.txHash });
  }
  return { current, lineage };
};
