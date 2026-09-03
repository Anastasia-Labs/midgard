import * as SDK from "@al-ft/midgard-sdk";

export type DaAttestationMarkerClassification =
  | { readonly kind: "unattested" }
  | {
      readonly kind: "already_attested_expected";
      readonly availabilityKind: Exclude<
        SDK.DaAvailabilityStateQueueStatusKind,
        "Unattested"
      >;
    };

export const classifyDaAttestationMarker = (
  status: SDK.DaAvailabilityStateQueueStatus,
): DaAttestationMarkerClassification => {
  const kind = SDK.daAvailabilityStateQueueStatusKind(status);
  if (kind === "Unattested") {
    return { kind: "unattested" };
  }
  return { kind: "already_attested_expected", availabilityKind: kind };
};
