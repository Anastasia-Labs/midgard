import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash } from "@lucid-evolution/lucid";

/** Retained proof material captured by an authenticated reader, not new L1 authority. */
export type PublishedDepositHistory = Readonly<{
  commitmentCbor: string;
  openingCbor: string;
}>;

type Metadata = Readonly<{
  depositAuthUnit: string;
  depositAssetName: string;
  inclusionTime: number;
}>;
const canonical = aikenSerialisedPlutusDataCborPreservingMapOrder;

/** Reopen exact captured bytes. The current UTxO is navigation only. */
export const readPublishedDepositHistory = (
  history: PublishedDepositHistory,
  metadata: Metadata,
) => {
  if (
    history === null ||
    typeof history !== "object" ||
    Object.keys(history).sort().join(",") !== "commitmentCbor,openingCbor" ||
    typeof history.commitmentCbor !== "string" ||
    typeof history.openingCbor !== "string" ||
    !/^(?:[0-9a-f]{2})+$/.test(history.commitmentCbor) ||
    !/^(?:[0-9a-f]{2})+$/.test(history.openingCbor)
  )
    throw new Error("Authenticated deposit history capture is required");
  const commitment = Data.from(
    history.commitmentCbor,
    SDK.EventHistoryCommitment,
  );
  const opening = Data.from(history.openingCbor, SDK.EventHistoryOpening);
  if (
    canonical(Data.to(commitment, SDK.EventHistoryCommitment)) !==
      history.commitmentCbor ||
    canonical(history.openingCbor) !== history.openingCbor ||
    commitment.kind !== "Deposit" ||
    !("DepositPayload" in opening.payload) ||
    !SDK.opensEventHistoryCommitmentCbor(
      commitment,
      plutusConstrFieldCbor(history.openingCbor, [0]),
      plutusConstrFieldCbor(history.openingCbor, [1]),
    ) ||
    !Number.isSafeInteger(metadata.inclusionTime) ||
    metadata.inclusionTime < 0 ||
    commitment.inclusion_time !== BigInt(metadata.inclusionTime) ||
    metadata.depositAuthUnit !==
      commitment.policy + metadata.depositAssetName ||
    metadata.depositAssetName !==
      datumToHash(canonical(Data.to(commitment.event_id, SDK.OutputReference)))
  )
    throw new Error(
      "Deposit history opening differs from its captured commitment or metadata",
    );
  const originalAssets = SDK.valueToAssets(opening.original_assets);
  if (Object.values(originalAssets).some((quantity) => quantity < 0n))
    throw new Error("Deposit history has negative original funds");
  return {
    commitment,
    event: opening.payload.DepositPayload.event,
    originalAssets,
  };
};

/** Call only with an Order returned by the actual authenticated history reader. */
export const capturePublishedDepositHistory = (
  order: SDK.DepositUTxO,
  policyId: string,
): PublishedDepositHistory => {
  const captured = SDK.captureEventHistoryWitness(
    order.history,
    policyId,
    "Deposit",
  );
  const node = order.history.anchor.node;
  if (
    !("DepositPayload" in captured.payload) ||
    node.payload === "RootContent" ||
    !("Order" in node.payload) ||
    canonical(Data.to(order.event, SDK.DepositEvent)) !==
      canonical(
        Data.to(captured.payload.DepositPayload.event, SDK.DepositEvent),
      ) ||
    canonical(Data.to(order.facts, SDK.EventHistoryFacts)) !==
      canonical(Data.to(node.payload.Order.facts, SDK.EventHistoryFacts)) ||
    order.idCbor.toString("hex") !==
      canonical(plutusConstrFieldCbor(captured.payloadCbor, [0, 0])) ||
    order.infoCbor.toString("hex") !==
      canonical(plutusConstrFieldCbor(captured.payloadCbor, [0, 1])) ||
    order.history.anchor.utxo.datum == null ||
    captured.factsCbor !==
      canonical(
        plutusConstrFieldCbor(order.history.anchor.utxo.datum, [3, 0]),
      ) ||
    canonical(Data.to(SDK.assetsToValue(order.originalAssets), SDK.Value)) !==
      canonical(Data.to(captured.originalAssets, SDK.Value))
  )
    throw new Error(
      "Authenticated deposit reader disagrees with its history witness",
    );
  const history = Object.freeze({
    commitmentCbor: canonical(
      Data.to(captured.commitment, SDK.EventHistoryCommitment),
    ),
    openingCbor: canonical(captured.openingCbor),
  });
  readPublishedDepositHistory(history, {
    depositAuthUnit: policyId + order.assetName,
    depositAssetName: order.assetName,
    inclusionTime: order.inclusionTime.getTime(),
  });
  return history;
};
