import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, datumToHash } from "@lucid-evolution/lucid";

import { OutputReference, Value } from "../common.js";
import { assetsToValue } from "../reserve-payout/assets.js";
import {
  type EventHistoryCommitment,
  type EventHistoryFacts,
  EventHistoryPayload,
} from "./history.js";
import { type EventHistoryKind } from "./history-data.js";
import { type EventHistoryWitness } from "./history-query.js";

const hash = (cbor: string) =>
  datumToHash(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));

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

/** Preserve original deposit assets separately from list NFT and structural ADA.
 * The transaction must reference this exact authenticated witness on chain. */
export const captureEventHistoryWitness = (
  witness: Extract<EventHistoryWitness, { kind: "Present" }>,
  policy: string,
  kind: EventHistoryKind,
) => {
  const { anchor, payload } = witness;
  if (
    anchor.key === null ||
    anchor.node.payload === "RootContent" ||
    !("Order" in anchor.node.payload)
  ) {
    throw new Error("History presence requires an authenticated Order");
  }
  const facts = anchor.node.payload.Order.facts;
  const assets = { ...anchor.utxo.assets };
  const unit = policy + anchor.key;
  if (assets[unit] !== 1n)
    throw new Error("History Order is missing its exact NFT");
  delete assets[unit];
  const lovelace = (assets.lovelace ?? 0n) - facts.structural_lovelace;
  if (lovelace < 0n)
    throw new Error("History structural ADA exceeds actual funds");
  if (lovelace === 0n) delete assets.lovelace;
  else assets.lovelace = lovelace;
  const originalAssets = assetsToValue(assets);
  const commitment = eventHistoryCommitment(
    policy,
    kind,
    facts,
    payload,
    originalAssets,
  );
  if (!opensEventHistoryCommitment(commitment, payload, originalAssets)) {
    throw new Error("History payload kind or identity differs from its Order");
  }
  return { commitment, payload, originalAssets };
};
