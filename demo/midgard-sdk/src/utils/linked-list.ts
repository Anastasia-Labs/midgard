import { Data as EffectData, Effect } from "effect";
import { NodeDatum } from "../tx-builder/linked-list/linked-list.js";
import { Data, UTxO } from "@lucid-evolution/lucid";
import {
  DataCoercionError,
  GenericErrorFields,
  MissingDatumError,
} from "./common.js";

export const getNodeDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<NodeDatum, DataCoercionError | MissingDatumError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, NodeDatum);
      return Effect.succeed(nodeDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: "Could not coerce provided UTxO's datum to a node datum",
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new MissingDatumError({
        message: "Provided UTxO was expected to carry an inlined `NodeDatum`",
        cause: `No datum found in ${nodeUTxO.txHash}.${nodeUTxO.outputIndex}`,
      }),
    );
  }
};

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}
