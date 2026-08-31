import * as SDK from "@al-ft/midgard-sdk";

export type DaAttestationMarkerClassification =
  | { readonly kind: "unattested" }
  | {
      readonly kind: "already_attested_expected";
      readonly availabilityKind: Exclude<
        SDK.DaAvailabilityStateQueueStatusKindV1,
        "Unattested"
      >;
    };

export const classifyDaAttestationMarker = (
  status: SDK.DaAvailabilityStateQueueStatusV1,
): DaAttestationMarkerClassification => {
  const kind = SDK.daAvailabilityStateQueueStatusKindV1(status);
  if (kind === "Unattested") {
    return { kind: "unattested" };
  }
  return { kind: "already_attested_expected", availabilityKind: kind };
};
