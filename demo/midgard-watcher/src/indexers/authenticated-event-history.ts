import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  CardanoDatum,
  EventHistoryData,
  EventHistoryNode,
  EventHistoryObserve,
  EventHistoryPayload,
  EventHistoryRetirementArgs,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { CML, Data, datumToHash } from "@lucid-evolution/lucid";

import type { WatcherNormalizedL1Block } from "../l1/l1-adapter.js";

type Transaction = WatcherNormalizedL1Block["transactions"][number];

/** This decoder grants no chain authority. Callers must supply an admitted,
 * valid transaction and deployment-derived hashes, never archived assertions. */
export const exactHistoryWithdrawal = (
  transaction: Transaction,
  scriptHash: string,
) => {
  const body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
  const withdrawals = body.withdrawals();
  const keys = withdrawals?.keys();
  const matches = [];
  for (let index = 0; index < (keys?.len() ?? 0); index++) {
    const account = keys!.get(index);
    if (account.payment().as_script()?.to_hex() !== scriptHash) continue;
    if (withdrawals!.get(account) !== 0n) return null;
    const redeemers = transaction.redeemers.filter(
      (entry) =>
        entry.purpose === "withdrawal" && entry.index === String(index),
    );
    if (redeemers.length !== 1) return null;
    matches.push({
      redeemer: redeemers[0]!,
      globalIndex: transaction.redeemers.indexOf(redeemers[0]!),
    });
  }
  return matches.length === 1 ? matches[0]! : null;
};

export const historyListObservation = (
  transaction: Transaction,
  policyId: string,
) => {
  const withdrawal = exactHistoryWithdrawal(transaction, policyId);
  if (withdrawal === null) return null;
  try {
    return {
      ...withdrawal,
      observe: Data.from(
        withdrawal.redeemer.bytes.bytesHex,
        EventHistoryObserve,
      ),
    };
  } catch {
    return null;
  }
};

export const historyRetirementObservation = (
  transaction: Transaction,
  scriptHash: string,
) => {
  const withdrawal = exactHistoryWithdrawal(transaction, scriptHash);
  if (withdrawal === null) return null;
  try {
    return {
      ...withdrawal,
      args: Data.from(
        withdrawal.redeemer.bytes.bytesHex,
        EventHistoryRetirementArgs,
      ),
    };
  } catch {
    return null;
  }
};

export const historyNodeFromOutput = (
  output: CML.TransactionOutput,
  policyId: string,
) => {
  const cbor = output.datum()?.as_datum()?.to_cbor_hex();
  if (
    cbor === undefined ||
    output.script_ref() !== undefined ||
    output.address().payment_cred()?.as_script()?.to_hex() !== policyId
  )
    return null;
  try {
    const node = Data.from(cbor, EventHistoryNode);
    const key = node.position === "Root" ? "" : node.position.Key[0];
    const assets = output
      .amount()
      .multi_asset()
      .get_assets(CML.ScriptHash.from_hex(policyId));
    if (
      assets?.len() !== 1 ||
      assets.get(CML.AssetName.from_hex(key)) !== 1n ||
      (key === "") !== (node.payload === "RootContent") ||
      (key !== "" && node.next !== null && key >= node.next)
    )
      return null;
    if (
      node.payload !== "RootContent" &&
      "Order" in node.payload &&
      datumToHash(
        Data.to(node.payload.Order.facts.event_id, OutputReference),
      ) !== key
    )
      return null;
    return { node, key, cbor };
  } catch {
    return null;
  }
};

/** Open external data only from the transaction's authenticated reference roster. */
export const historyPayloadFromNode = (
  authenticated: { readonly node: EventHistoryNode; readonly cbor: string },
  kind: "deposit" | "withdrawal",
  key: string,
  external: CML.TransactionOutput | null,
  retentionScriptHash: string | undefined,
): {
  payload: EventHistoryPayload;
  payloadCbor: string;
  eventCbor: string;
} | null => {
  const { node, cbor: nodeCbor } = authenticated;
  if (node.payload === "RootContent" || !("Order" in node.payload)) return null;
  const facts = node.payload.Order.facts;
  try {
    let payloadCbor: string;
    if ("Inline" in facts.location)
      payloadCbor = historyRawField(nodeCbor, [3, 0, 2, 0]);
    else {
      const cbor = external?.datum()?.as_datum()?.to_cbor_hex();
      if (
        external === null ||
        cbor === undefined ||
        external.script_ref() !== undefined ||
        external?.address().payment_cred()?.as_script()?.to_hex() !==
          retentionScriptHash ||
        CML.EnterpriseAddress.from_address(external.address()) === undefined ||
        datumToHash(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor)) !==
          facts.location.External.storage_datum_hash
      )
        return null;
      const retained = Data.from(cbor, EventHistoryData);
      if (retained.event_key !== key) return null;
      payloadCbor = historyRawField(cbor, [1]);
    }
    const payload = Data.from(payloadCbor, EventHistoryPayload);
    if ((kind === "deposit") !== "DepositPayload" in payload) return null;
    const event =
      "DepositPayload" in payload
        ? payload.DepositPayload.event
        : payload.WithdrawalPayload.event;
    if (
      Data.to(event.id, OutputReference) !==
      Data.to(facts.event_id, OutputReference)
    )
      return null;
    return {
      payload,
      payloadCbor,
      eventCbor: historyRawField(payloadCbor, [0]),
    };
  } catch {
    return null;
  }
};

/** Pointer churn may change only links/protection. Original facts, Value and
 * full address remain fixed; the order's origin is retained by the caller. */
export const historyOrderContinuationMatches = (
  before: CML.TransactionOutput,
  after: CML.TransactionOutput,
  policyId: string,
): boolean => {
  const old = historyNodeFromOutput(before, policyId);
  const next = historyNodeFromOutput(after, policyId);
  if (
    old === null ||
    next === null ||
    old.key !== next.key ||
    old.node.payload === "RootContent" ||
    !("Order" in old.node.payload) ||
    next.node.payload === "RootContent" ||
    !("Order" in next.node.payload)
  )
    return false;
  return (
    before.address().to_hex() === after.address().to_hex() &&
    before.amount().to_canonical_cbor_hex() ===
      after.amount().to_canonical_cbor_hex() &&
    historyRawField(old.cbor, [3, 0]) === historyRawField(next.cbor, [3, 0])
  );
};

/** Extract actual Data before serialiseData normalization; never pass arbitrary
 * maps through a typed JS Map, which discards repeated keys. */
export const historyRawField = (
  cbor: string,
  path: readonly number[],
): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(cbor, path),
  );

/** Preserve the raw CardanoDatum's inline data while checking its typed arm. */
export const historyCardanoDatumMatches = (
  output: CML.TransactionOutput,
  cbor: string,
): boolean => {
  const datum = Data.from(cbor, CardanoDatum);
  if (datum === "NoDatum") return output.datum() === undefined;
  if ("DatumHash" in datum)
    return output.datum()?.as_hash()?.to_hex() === datum.DatumHash.hash;
  const actual = output.datum()?.as_datum()?.to_cbor_hex();
  return (
    actual !== undefined &&
    aikenSerialisedPlutusDataCborPreservingMapOrder(actual) ===
      historyRawField(cbor, [0])
  );
};

/** Payout fields copied from the actual withdrawal body, including raw datum. */
export const historyWithdrawalPayoutDatum = (
  payloadCbor: string,
  refund = false,
): string => {
  const payload = Data.from(payloadCbor, EventHistoryPayload);
  if (!("WithdrawalPayload" in payload))
    throw new Error("Expected withdrawal history payload");
  const fields = refund
    ? [
        "a0",
        historyRawField(payloadCbor, [1]),
        historyRawField(payloadCbor, [2]),
      ]
    : [2, 3, 4].map((index) => historyRawField(payloadCbor, [0, 1, 0, index]));
  return aikenSerialisedPlutusDataCborPreservingMapOrder(
    `d8799f${fields.join("")}ff`,
  );
};
