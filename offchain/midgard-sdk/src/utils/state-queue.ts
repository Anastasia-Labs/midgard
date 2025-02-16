import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { Data, UTxO } from "@lucid-evolution/lucid";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Effect } from "effect";
import { Datum } from "@/types/contracts/state-queue.js";
import { getNodeDatumFromUTxO } from "./linked-list.js";

export const getLinkFromBlockUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<NodeKey, Error> => {
  const nodeDatum = getNodeDatumFromUTxO(blockUTxO);
  return Effect.map(nodeDatum, (nd) => nd.next);
};

/**
 * Given a block UTxO, this function confirmes the node is root (i.e. no keys
 * in its datum), and attempts to coerce its underlying data into a
 * `ConfirmedState`.
 */
export const getConfirmedStateFromUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<{ data: ConfirmedState; link: NodeKey }, Error> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
      if (nodeDatum.key === "Empty") {
        const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
        return Effect.succeed({ data: confirmedState, link: nodeDatum.next });
      } else {
        return Effect.fail(new Error("Given UTxO is not root"));
      }
    } catch {
      return Effect.fail(new Error("Could not coerce to a node datum"));
    }
  } else {
    return Effect.fail(new Error("No datum found"));
  }
};
