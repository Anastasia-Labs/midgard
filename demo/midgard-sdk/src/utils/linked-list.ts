import { Effect } from "effect";
import { NodeDatum } from "../tx-builder/linked-list.js";
import { Data, UTxO } from "@lucid-evolution/lucid";

export const getNodeDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<NodeDatum, Error> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, NodeDatum);
      return Effect.succeed(nodeDatum);
    } catch {
      return Effect.fail(new Error("Could not coerce to a node datum"));
    }
  } else {
    return Effect.fail(new Error("No datum found"));
  }
};
