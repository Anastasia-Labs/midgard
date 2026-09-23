import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  Data,
  datumToHash,
  type Network,
  type SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";

import { CredentialSchema } from "../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
} from "../fraud-proof/contracts/blueprint.js";

/** Provisional slice schema; deployment integration and ABI fit gates remain open. */
export const EventHistoryDataSchema = Data.Object({
  event_key: Data.Bytes({ minLength: 32, maxLength: 32 }),
  event_payload: Data.Any(),
  reclaim_auth: CredentialSchema,
});
export type EventHistoryData = Data.Static<typeof EventHistoryDataSchema>;
export const EventHistoryData = asDataType<EventHistoryData>(
  EventHistoryDataSchema,
);

/** Match serialiseData, including maps nested inside arbitrary user data. */
export const encodeEventHistoryData = (datum: EventHistoryData): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    Data.to(datum, EventHistoryData),
  );

export const eventHistoryDataHash = (datum: EventHistoryData): string =>
  datumToHash(encodeEventHistoryData(datum));

export const EventHistoryKindSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Literal("Withdrawal"),
]);
export type EventHistoryKind = Data.Static<typeof EventHistoryKindSchema>;
export const EventHistoryKind = asDataType<EventHistoryKind>(
  EventHistoryKindSchema,
);

/** Applies both parameters through the shared arity and shape checked path. */
export const applyEventHistoryRetentionValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  hubPolicyId: string,
  kind: EventHistoryKind,
): { readonly validator: SpendingValidator; readonly address: string } => {
  if (!/^[0-9a-f]{56}$/u.test(hubPolicyId)) {
    throw new Error(
      "Event history hub policy must be a 28-byte lowercase hash",
    );
  }
  const validator: SpendingValidator = {
    type: "PlutusV3",
    script: applyBlueprintParams(
      blueprint,
      "user_events/history_data.retention.spend",
      [hubPolicyId, Data.from(Data.to(kind, EventHistoryKind))],
    ),
  };
  return { validator, address: validatorToAddress(network, validator) };
};
