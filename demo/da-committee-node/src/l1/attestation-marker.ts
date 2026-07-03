import * as SDK from "@al-ft/midgard-sdk";

import { normalizeHex } from "../utils/hex.js";

export type DaAttestationMarkerClassification =
  | { readonly kind: "unattested" }
  | {
      readonly kind: "already_attested_expected";
      readonly policyId: string;
    }
  | {
      readonly kind: "already_attested_foreign";
      readonly policyId: string;
      readonly expectedPolicyId: string;
    }
  | {
      readonly kind: "invalid";
      readonly marker: string;
      readonly expectedPolicyId: string;
      readonly reason: string;
    };

export const classifyDaAttestationMarker = (
  marker: string,
  expectedPolicyId: string,
): DaAttestationMarkerClassification => {
  const expected = normalizeHex(expectedPolicyId, {
    fieldName: "expected DA attestation policy id",
    byteLength: 28,
  });
  if (marker === SDK.NO_DA_ATTESTATION || marker.trim() === "") {
    return { kind: "unattested" };
  }
  try {
    const policyId = normalizeHex(marker, {
      fieldName: "state queue DA attestation marker",
      byteLength: 28,
    });
    return policyId === expected
      ? { kind: "already_attested_expected", policyId }
      : {
          kind: "already_attested_foreign",
          policyId,
          expectedPolicyId: expected,
        };
  } catch (error) {
    return {
      kind: "invalid",
      marker,
      expectedPolicyId: expected,
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};

export const formatUnexpectedDaAttestationMarker = (
  headerHash: string,
  marker: Exclude<
    DaAttestationMarkerClassification,
    { readonly kind: "unattested" | "already_attested_expected" }
  >,
): string =>
  marker.kind === "invalid"
    ? `state queue header ${headerHash} has invalid DA attestation marker ${JSON.stringify(marker.marker)}: ${marker.reason}`
    : `state queue header ${headerHash} was attested by unexpected policy ${marker.policyId}; expected ${marker.expectedPolicyId}`;
