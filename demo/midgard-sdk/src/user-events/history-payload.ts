import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data, datumToHash } from "@lucid-evolution/lucid";

import { type CredentialD, OutputReference } from "../common.js";
import { EventHistoryPayload, type EventHistoryRecipe } from "./history.js";
import {
  encodeEventHistoryData,
  type EventHistoryData,
  eventHistoryDataHash,
} from "./history-data.js";

export type EventHistoryPayloadBounds = Pick<
  EventHistoryRecipe,
  "inlineLimitBytes" | "maxPayloadBytes" | "maxPayloadNodes"
>;

/** Containers and map keys count too; stop without expanding a wide container. */
export const countHistoryDataNodes = (data: Data, maximum: bigint): bigint => {
  const work: Iterator<Data>[] = [[data][Symbol.iterator]()];
  let count = 0n;
  while (work.length > 0) {
    const next = work[work.length - 1]!.next();
    if (next.done) {
      work.pop();
      continue;
    }
    if (++count > maximum) {
      throw new Error("Event history payload exceeds the Data-node bound");
    }
    const item = next.value;
    if (item instanceof Constr) work.push(item.fields[Symbol.iterator]());
    else if (Array.isArray(item)) work.push(item[Symbol.iterator]());
    else if (item instanceof Map) {
      work.push(
        (function* () {
          for (const [key, value] of item) {
            yield key;
            yield value;
          }
        })(),
      );
    }
  }
  return count;
};

/** Select storage before publication. External plans still require a confirmed
 * retention UTxO before admission; their hash is never sufficient evidence. */
export const prepareEventHistoryPayload = (
  payload: EventHistoryPayload,
  reclaimAuth: CredentialD,
  bounds: EventHistoryPayloadBounds,
) => {
  if (
    bounds.inlineLimitBytes <= 0n ||
    bounds.maxPayloadBytes < bounds.inlineLimitBytes ||
    bounds.maxPayloadNodes <= 0n
  ) {
    throw new Error("Invalid event history payload bounds");
  }
  const raw = Data.from(Data.to(payload, EventHistoryPayload));
  const payloadNodes = countHistoryDataNodes(raw, bounds.maxPayloadNodes);
  const payloadCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(raw),
  );
  const payloadBytes = BigInt(payloadCbor.length / 2);
  if (payloadBytes > bounds.maxPayloadBytes) {
    throw new Error("Event history payload exceeds the byte bound");
  }
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  const key = datumToHash(Data.to(id, OutputReference));
  const measurement = { key, payloadCbor, payloadBytes, payloadNodes };
  if (payloadBytes <= bounds.inlineLimitBytes) {
    return {
      ...measurement,
      kind: "Inline" as const,
      location: { Inline: { payload } },
    };
  }
  const datum: EventHistoryData = {
    event_key: key,
    event_payload: raw,
    reclaim_auth: reclaimAuth,
  };
  return {
    ...measurement,
    kind: "External" as const,
    location: { External: { storage_datum_hash: eventHistoryDataHash(datum) } },
    datum,
    datumCbor: encodeEventHistoryData(datum),
  };
};
