import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

/**
 * Exact on-chain DA lifecycle carried by a state-queue node (Aiken
 * `StateQueueStatusV1`). `commitment_hash` is the untagged blake2b-256 of the
 * attested commitment's Plutus Data (`daAvailabilityCommitmentHash`); a
 * challenged node also carries its challenge record's DACH identity.
 */
export const DaAvailabilityStateQueueStatusSchema = Data.Enum([
  Data.Literal("Unattested"),
  Data.Object({
    Attested: Data.Object({
      commitment_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    Challenged: Data.Object({
      commitment_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
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

/**
 * Mirrors the state-queue validator's merge rule: only an Attested or
 * Published head may merge to the confirmed state.
 */
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
    return `Attested:${status.Attested.commitment_hash}`;
  }
  if ("Challenged" in status) {
    return `Challenged:${status.Challenged.commitment_hash}:${status.Challenged.challenge_asset_name}`;
  }
  return `Published:${status.Published.terminal_commitment}`;
};
