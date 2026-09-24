import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  countPlutusDataCborNodes,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data, datumToHash } from "@lucid-evolution/lucid";

import { type CredentialD, OutputReference } from "../common.js";
import { EventHistoryPayload, type EventHistoryRecipe } from "./history.js";
import { encodeEventHistoryData, EventHistoryData } from "./history-data.js";

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

/** Count exact raw pairs, rather than a decoded Map that can collapse keys. */
export const countHistoryDataNodesCbor = countPlutusDataCborNodes;

/** Select storage from the exact semantic Data bytes. Typed fields are views;
 * publication/admission must use the retained payloadCbor/datumCbor. */
export const prepareEventHistoryPayloadCbor = (
  rawPayloadCbor: string,
  reclaimAuth: CredentialD,
  bounds: EventHistoryPayloadBounds,
) => {
  if (
    bounds.inlineLimitBytes <= 0n ||
    bounds.maxPayloadBytes < bounds.inlineLimitBytes ||
    bounds.maxPayloadNodes <= 0n
  )
    throw new Error("Invalid event history payload bounds");
  const payloadNodes = countHistoryDataNodesCbor(
    rawPayloadCbor,
    bounds.maxPayloadNodes,
  );
  const payloadCbor =
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayloadCbor);
  const payloadBytes = BigInt(payloadCbor.length / 2);
  if (payloadBytes > bounds.maxPayloadBytes)
    throw new Error("Event history payload exceeds the byte bound");
  const payload = Data.from(payloadCbor, EventHistoryPayload);
  const id =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  const key = datumToHash(Data.to(id, OutputReference));
  const measurement = { key, payload, payloadCbor, payloadBytes, payloadNodes };
  if (payloadBytes <= bounds.inlineLimitBytes)
    return {
      ...measurement,
      kind: "Inline" as const,
      location: { Inline: { payload } },
    };
  const template: EventHistoryData = {
    event_key: key,
    event_payload: 0n,
    reclaim_auth: reclaimAuth,
  };
  const datumCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    replacePlutusConstrFieldCbor(
      encodeEventHistoryData(template),
      [1],
      payloadCbor,
    ),
  );
  return {
    ...measurement,
    kind: "External" as const,
    location: { External: { storage_datum_hash: datumToHash(datumCbor) } },
    datum: Data.from(datumCbor, EventHistoryData),
    datumCbor,
  };
};

export type EventHistoryPayloadPlan = ReturnType<
  typeof prepareEventHistoryPayloadCbor
>;

/** Typed construction delegates before storage selection; existing raw Data
 * callers use prepareEventHistoryPayloadCbor to preserve ordered pairs. */
export const prepareEventHistoryPayload = (
  payload: EventHistoryPayload,
  reclaimAuth: CredentialD,
  bounds: EventHistoryPayloadBounds,
): EventHistoryPayloadPlan =>
  prepareEventHistoryPayloadCbor(
    Data.to(payload, EventHistoryPayload),
    reclaimAuth,
    bounds,
  );
