import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

/** Exact on-chain DA lifecycle carried by a state-queue node. */
export const DaAvailabilityStateQueueStatusSchema = Data.Enum([
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
export type DaAvailabilityStateQueueStatus = Data.Static<
  typeof DaAvailabilityStateQueueStatusSchema
>;
export const DaAvailabilityStateQueueStatus =
  asDataType<DaAvailabilityStateQueueStatus>(
    DaAvailabilityStateQueueStatusSchema,
  );

export const NO_DA_ATTESTATION: DaAvailabilityStateQueueStatus = "Unattested";

export type DaAvailabilityStateQueueStatusKind =
  | "Unattested"
  | "Attested"
  | "Challenged"
  | "Published";

export const daAvailabilityStateQueueStatusKind = (
  status: DaAvailabilityStateQueueStatus,
): DaAvailabilityStateQueueStatusKind => {
  if (status === "Unattested") return "Unattested";
  if ("Attested" in status) return "Attested";
  if ("Challenged" in status) return "Challenged";
  return "Published";
};

export const daAvailabilityStateQueueStatusPermitsMerge = (
  status: DaAvailabilityStateQueueStatus,
): boolean => {
  const kind = daAvailabilityStateQueueStatusKind(status);
  return kind === "Attested" || kind === "Published";
};

/** Canonical diagnostic/idempotency identity for a decoded status datum. */
export const daAvailabilityStateQueueStatusIdentity = (
  status: DaAvailabilityStateQueueStatus,
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
