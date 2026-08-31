import { Data } from "@lucid-evolution/lucid";

/** Exact on-chain DA lifecycle carried by a state-queue node. */
export const DaAvailabilityStateQueueStatusV1Schema = Data.Enum([
  Data.Literal("Unattested"),
  Data.Object({
    Attested: Data.Object({
      da_bond_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    Challenged: Data.Object({
      da_bond_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    Published: Data.Object({
      terminal_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
]);
export type DaAvailabilityStateQueueStatusV1 = Data.Static<
  typeof DaAvailabilityStateQueueStatusV1Schema
>;
export const DaAvailabilityStateQueueStatusV1 =
  DaAvailabilityStateQueueStatusV1Schema as unknown as DaAvailabilityStateQueueStatusV1;

export const NO_DA_ATTESTATION: DaAvailabilityStateQueueStatusV1 = "Unattested";

export type DaAvailabilityStateQueueStatusKindV1 =
  | "Unattested"
  | "Attested"
  | "Challenged"
  | "Published";

export const daAvailabilityStateQueueStatusKindV1 = (
  status: DaAvailabilityStateQueueStatusV1,
): DaAvailabilityStateQueueStatusKindV1 => {
  if (status === "Unattested") return "Unattested";
  if ("Attested" in status) return "Attested";
  if ("Challenged" in status) return "Challenged";
  return "Published";
};

export const daAvailabilityStateQueueStatusPermitsMergeV1 = (
  status: DaAvailabilityStateQueueStatusV1,
): boolean => {
  const kind = daAvailabilityStateQueueStatusKindV1(status);
  return kind === "Attested" || kind === "Published";
};

/** Canonical diagnostic/idempotency identity for a decoded status datum. */
export const daAvailabilityStateQueueStatusIdentityV1 = (
  status: DaAvailabilityStateQueueStatusV1,
): string => {
  if (status === "Unattested") return status;
  if ("Attested" in status) {
    return `Attested:${status.Attested.da_bond_asset_name}`;
  }
  if ("Challenged" in status) {
    return `Challenged:${status.Challenged.da_bond_asset_name}:${status.Challenged.challenge_asset_name}`;
  }
  return `Published:${status.Published.terminal_commitment}`;
};
