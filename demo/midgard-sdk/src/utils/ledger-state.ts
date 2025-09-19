import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Header } from "../tx-builder/ledger-state.js";
import {
  hashHexWithBlake2b224,
  HashingError,
  StateQueueError,
} from "./common.js";
import { Datum as StateQueueDatum } from "@/tx-builder/state-queue/types.js";

export const getHeaderFromStateQueueDatum = (
  nodeDatum: StateQueueDatum,
): Effect.Effect<Header, StateQueueError> =>
  Effect.gen(function* () {
    const header = yield* Effect.try({
      try: () => Data.castFrom(nodeDatum.data, Header),
      catch: (e) =>
        new StateQueueError({
          message: `Failed coercing block's datum data to Header`,
          cause: e,
        }),
    });
    return header;
  });

export const hashHeader = (
  header: Header,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b224(Data.to(header, Header));
