import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { type Assets, Data, datumToHash } from "@lucid-evolution/lucid";

import { OutputReference, Value } from "../common.js";
import { assetsToValue } from "../reserve-payout/assets.js";
import {
  type EventHistoryCommitment,
  type EventHistoryFacts,
  EventHistoryNode,
  EventHistoryOpening,
  EventHistoryPayload,
} from "./history.js";
import { EventHistoryData, type EventHistoryKind } from "./history-data.js";
import { type EventHistoryWitness } from "./history-query.js";

const hash = (cbor: string) =>
  datumToHash(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));

/** Derive funds from an already authenticated Order. This validates the local
 * representation; only a list witness or admitted L1 transaction grants authority. */
export const eventHistoryOriginalAssets = (
  node: EventHistoryNode,
  lockedAssets: Readonly<Assets>,
  policy: string,
): Assets => {
  if (
    node.position === "Root" ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    throw new Error("Original event funds require a history Order");
  const facts = node.payload.Order.facts;
  const key = hash(Data.to(facts.event_id, OutputReference));
  const unit = policy + key;
  if (
    !/^[0-9a-f]{56}$/u.test(policy) ||
    node.position.Key[0] !== key ||
    lockedAssets[unit] !== 1n ||
    Object.keys(lockedAssets).some(
      (candidate) => candidate.startsWith(policy) && candidate !== unit,
    )
  )
    throw new Error("History Order is missing its exact event authentication");
  const assets = { ...lockedAssets };
  delete assets[unit];
  const lovelace = (assets.lovelace ?? 0n) - facts.structural_lovelace;
  if (facts.structural_lovelace < 0n || lovelace < 0n)
    throw new Error("History structural ADA exceeds actual funds");
  if (lovelace === 0n) delete assets.lovelace;
  else assets.lovelace = lovelace;
  return assets;
};

/** Byte twin of order_facts.commitment; this function does not confer authority. */
export const eventHistoryCommitment = (
  policy: string,
  kind: EventHistoryKind,
  facts: Pick<EventHistoryFacts, "event_id" | "inclusion_time">,
  payload: EventHistoryPayload,
  originalAssets: Value,
): EventHistoryCommitment => ({
  policy,
  kind,
  event_id: facts.event_id,
  inclusion_time: facts.inclusion_time,
  payload_hash: hash(Data.to(payload, EventHistoryPayload)),
  original_assets_hash: hash(Data.to(originalAssets, Value)),
});

/** Preflight a retained opening. L1 authenticity comes from the earlier stage. */
export const opensEventHistoryCommitment = (
  retained: EventHistoryCommitment,
  payload: EventHistoryPayload,
  originalAssets: Value,
): boolean => {
  const kind = "DepositPayload" in payload ? "Deposit" : "Withdrawal";
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  return (
    kind === retained.kind &&
    Data.to(id, OutputReference) ===
      Data.to(retained.event_id, OutputReference) &&
    hash(Data.to(payload, EventHistoryPayload)) === retained.payload_hash &&
    hash(Data.to(originalAssets, Value)) === retained.original_assets_hash
  );
};

/** Verify raw retained Data without losing map order or repeated pairs. This
 * checks an opening only; it does not establish chain authority. */
export const opensEventHistoryCommitmentCbor = (
  retained: EventHistoryCommitment,
  payloadCbor: string,
  originalAssetsCbor: string,
): boolean => {
  const payload = Data.from(payloadCbor, EventHistoryPayload);
  Data.from(originalAssetsCbor, Value);
  const kind = "DepositPayload" in payload ? "Deposit" : "Withdrawal";
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  return (
    kind === retained.kind &&
    Data.to(id, OutputReference) ===
      Data.to(retained.event_id, OutputReference) &&
    hash(payloadCbor) === retained.payload_hash &&
    hash(originalAssetsCbor) === retained.original_assets_hash
  );
};

/** Preserve original deposit assets separately from list NFT and structural ADA.
 * The transaction must reference this exact authenticated witness on chain. */
export const captureEventHistoryWitness = (
  witness: Extract<EventHistoryWitness, { kind: "Present" }>,
  policy: string,
  kind: EventHistoryKind,
) => {
  const { anchor } = witness;
  if (anchor.utxo.datum == null)
    throw new Error("History presence requires its original datum");
  const node = Data.from(anchor.utxo.datum, EventHistoryNode);
  if (
    anchor.key === null ||
    node.position === "Root" ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    throw new Error("History presence requires an authenticated Order");
  const facts = node.payload.Order.facts;
  if (anchor.key !== node.position.Key[0])
    throw new Error("History anchor key differs from its Order");
  const factsCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(anchor.utxo.datum, [3, 0]),
  );
  let rawPayload: string;
  if ("Inline" in facts.location) {
    rawPayload = plutusConstrFieldCbor(factsCbor, [2, 0]);
  } else {
    const retained = witness.retainedDataUtxo;
    if (
      retained?.datum == null ||
      retained.scriptRef != null ||
      hash(retained.datum) !== facts.location.External.storage_datum_hash ||
      Data.from(retained.datum, EventHistoryData).event_key !== anchor.key
    )
      throw new Error("History payload requires its exact retained datum");
    rawPayload = plutusConstrFieldCbor(retained.datum, [1]);
  }
  const payloadCbor =
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload);
  if (
    payloadCbor !==
    aikenSerialisedPlutusDataCborPreservingMapOrder(witness.payloadCbor)
  )
    throw new Error("History payload differs from its authenticated datum");
  const payload = Data.from(payloadCbor, EventHistoryPayload);
  const originalAssets = assetsToValue(
    eventHistoryOriginalAssets(node, anchor.utxo.assets, policy),
  );
  const originalAssetsCbor = Data.to(originalAssets, Value);
  const commitment: EventHistoryCommitment = {
    policy,
    kind,
    event_id: facts.event_id,
    inclusion_time: facts.inclusion_time,
    payload_hash: hash(payloadCbor),
    original_assets_hash: hash(originalAssetsCbor),
  };
  if (
    !opensEventHistoryCommitmentCbor(
      commitment,
      payloadCbor,
      originalAssetsCbor,
    )
  )
    throw new Error("History payload kind or identity differs from its Order");
  const openingCbor = replacePlutusConstrFieldCbor(
    Data.to({ payload, original_assets: originalAssets }, EventHistoryOpening),
    [0],
    payloadCbor,
  );
  return {
    commitment,
    payload,
    originalAssets,
    payloadCbor,
    factsCbor,
    openingCbor,
  };
};
