import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  EventHistoryNode,
  eventHistoryOriginalAssets,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import type { WatcherIndexedUserEvent } from "../indexers/user-event-indexer.js";

/** Recompute funds from the output bound by the local event capability. The
 * serialized cache fields alone never grant originating-event authority. */
export const watcherOriginalDepositAssets = (
  event: Pick<
    WatcherIndexedUserEvent,
    | "kind"
    | "eventId"
    | "assetNameHex"
    | "policyId"
    | "datumCborHex"
    | "outputCborHex"
  >,
) => {
  if (event.kind !== "deposit")
    throw new Error("Original deposit funds require a deposit event");
  const output = coreToTxOutput(
    CML.TransactionOutput.from_cbor_hex(event.outputCborHex),
  );
  if (
    output.datum == null ||
    aikenSerialisedPlutusDataCborPreservingMapOrder(output.datum) !==
      aikenSerialisedPlutusDataCborPreservingMapOrder(event.datumCborHex)
  )
    throw new Error(
      "History event datum differs from its authenticated output",
    );
  const node = Data.from(output.datum, EventHistoryNode);
  if (
    node.position === "Root" ||
    node.position.Key[0] !== event.assetNameHex ||
    node.payload === "RootContent" ||
    !("Order" in node.payload) ||
    Data.to(node.payload.Order.facts.event_id, OutputReference) !==
      event.eventId
  )
    throw new Error("History output does not describe the selected deposit");
  return eventHistoryOriginalAssets(node, output.assets, event.policyId);
};
