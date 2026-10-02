import { CML, Data, datumToHash } from "@lucid-evolution/lucid";

import { CredentialD, Value } from "../common.js";
import { assetsToValue } from "../reserve-payout/assets.js";
import type {
  EventHistoryAdmission,
  EventHistoryBuildContext,
  EventHistoryPayloadInput,
} from "./history-build.js";
import {
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "./history-payload.js";

export type EventHistorySubmissionRequest = Omit<
  EventHistoryAdmission,
  "validFrom" | "validTo" | "externalData" | "payload" | "payloadCbor"
> &
  EventHistoryPayloadInput;

export const eventHistorySubmissionRequestHash = (
  policyId: string,
  request: EventHistorySubmissionRequest,
  recipe: EventHistoryBuildContext["recipe"],
): string => {
  if (request.payload !== undefined && request.payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const plan =
    request.payloadCbor === undefined
      ? prepareEventHistoryPayload(request.payload, request.reclaimAuth, recipe)
      : prepareEventHistoryPayloadCbor(
          request.payloadCbor,
          request.reclaimAuth,
          recipe,
        );
  const fields = [
    Data.to(policyId),
    plan.payloadCbor,
    Data.to(request.reclaimAuth, CredentialD),
    Data.to(assetsToValue(request.assets), Value),
    Data.to(request.structuralLovelace),
    Data.to(request.structuralRefundKey),
  ];
  const list = CML.PlutusData.from_cbor_hex(Data.to([])).as_list()!;
  for (const field of fields) list.add(CML.PlutusData.from_cbor_hex(field));
  return datumToHash(CML.PlutusData.new_list(list).to_cbor_hex());
};
