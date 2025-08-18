import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Header } from "../tx-builder/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";
import { Datum as StateQueueDatum } from "@/tx-builder/state-queue/types.js";

export const getHeaderFromStateQueueDatum = (
  nodeDatum: StateQueueDatum,
): Effect.Effect<Header, Error> =>
  Effect.gen(function* () {
    const header = yield* Effect.try({
      try: () => Data.castFrom(nodeDatum.data, Header),
      catch: (e) =>
        new Error(`Failed coercing block's datum data to Header: ${e}`),
    });
    return header;
  });

export const hashHeader = (header: Header): Effect.Effect<string, Error> =>
  hashHexWithBlake2b224(Data.to(header, Header));
