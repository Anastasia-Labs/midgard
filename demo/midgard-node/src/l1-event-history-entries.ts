import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  depositDataToEntry,
  withdrawalDataToEntry,
} from "./database/user-event-entries.js";
import type { HistoryIncarnation } from "./l1-event-history-provenance.js";

/** Convert freshly verified journal facts without inventing a live UTxO.
 * Retirement keeps the last real Order location. Orphan eligibility and existing
 * classifications must be reconciled by the SQL caller before these defaults
 * can be persisted; this function only derives immutable entry content.
 */
export const historyIncarnationEntry = (
  incarnation: HistoryIncarnation,
  network: Network,
) =>
  Effect.gen(function* () {
    const base = yield* Effect.try({
      try: () => {
        const event = incarnation.event;
        const payload = Data.from(event.payloadCbor, SDK.EventHistoryPayload);
        if ((incarnation.kind === "deposit") !== "DepositPayload" in payload)
          throw new Error("History kind disagrees with its payload");
        const time = Number(event.inclusionTime);
        const inclusionTime = new Date(time);
        if (
          !Number.isSafeInteger(time) ||
          !Number.isFinite(inclusionTime.getTime())
        )
          throw new Error(
            "History inclusion time cannot be represented by a row",
          );
        const idCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(event.payloadCbor, [0, 0]),
        );
        if (idCbor !== event.idCbor)
          throw new Error("History identity disagrees with its payload");
        const location =
          incarnation.placement?.current?.outRef ??
          incarnation.placement?.retirement?.outRef ??
          event.outRef;
        return {
          idCbor: Buffer.from(idCbor, "hex"),
          infoCbor: Buffer.from(
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
            ),
            "hex",
          ),
          inclusionTime,
          location,
        };
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to read history incarnation entry",
          cause,
        }),
    });
    if (incarnation.kind === "deposit") {
      const originalAssets = yield* Effect.try({
        try: () =>
          SDK.valueToAssets(
            Data.from(incarnation.event.originalAssetsCbor, SDK.Value),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to read original deposit Value",
            cause,
          }),
      });
      return {
        kind: "deposit" as const,
        entry: yield* depositDataToEntry({ ...base, originalAssets }, network),
      };
    }
    return {
      kind: "withdrawal" as const,
      entry: yield* withdrawalDataToEntry({
        ...base,
        assetName: incarnation.event.key,
        payloadCbor: incarnation.event.payloadCbor,
      }),
    };
  });
