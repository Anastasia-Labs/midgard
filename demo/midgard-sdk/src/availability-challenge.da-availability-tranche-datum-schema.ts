import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES,
  MIDGARD_MAX_DA_PAYLOAD_BYTES,
} from "@al-ft/midgard-core";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { HeaderHashSchema } from "./ledger-state.js";

export const DA_AVAILABILITY_COMMITMENT_VERSION = 1n;

export const DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES = 64 * 1024;

export const DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES =
  MIDGARD_MAX_DA_PAYLOAD_BYTES;

export const DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms;

export const DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms;

/**
 * The selected deployment profile's pooled DA committee bond amounts
 * (`config/deployments/*.yaml` `da_bond`). Availability parameters must carry
 * exactly these; builders spread them into `daAvailabilityParameters`.
 */
export const DA_AVAILABILITY_PROFILE_BOND_AMOUNTS = Object.freeze(
  Object.fromEntries(
    Object.entries(daBondManifestAmounts()).map(([key, value]) => [
      key,
      BigInt(value),
    ]),
  ) as {
    readonly daBondLovelace: bigint;
    readonly daSlashPenaltyLovelace: bigint;
    readonly daBondMinTopUpLovelace: bigint;
    readonly daBondPoolFloorLovelace: bigint;
    readonly challengeRecordLovelace: bigint;
  },
);

/** Deploy-time and independent of the DA bond; the fee reserve binds it alone. */
export const DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE =
  10_000_000_000n;

/**
 * The first response-publication measurement candidate. It is deliberately
 * named as a candidate: Q58 may promote it to the compiled response chunk
 * bound only after a signed, reference-script-backed testnet-profile
 * transaction retains the protocol's 512-byte reliability reserve.
 */
export const DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE =
  Object.freeze({
    // Exact signed reference-script transaction frontier: 15,872 bytes,
    // retaining the required 512-byte maxTxSize reserve. 14,021 serializes to
    // 15,873 and is therefore rejected by the adjacent measurement.
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  });

/** Absolute safety ceilings, not an activated response geometry. */
export const DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES =
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES;

export const DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY =
  MIDGARD_CONSENSUS_LIMITS.maxOutputCount;

export const HASH_28 = /^[0-9a-f]{56}$/u;

export const HASH_32 = /^[0-9a-f]{64}$/u;

export const CANONICAL_CBOR_HEX = /^(?:[0-9a-f]{2})+$/u;

export const ATTESTATION_COMMITMENT_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityAttestationV1",
  "ascii",
);

export const TRANCHE_START_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTrancheStartV1",
  "ascii",
);

export const TRANCHE_STEP_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTrancheStepV1",
  "ascii",
);

export const PUBLISHED_TERMINAL_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityPublishedV1",
  "ascii",
);

export const CHUNK_LEAF_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityChunkLeafV1",
  "ascii",
);

export const TERMINAL_ACCUMULATOR_START_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTerminalStartV1",
  "ascii",
);

export const TERMINAL_ACCUMULATOR_STEP_DOMAIN = Buffer.from(
  "MidgardDaAvailabilityTerminalStepV1",
  "ascii",
);

export const DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX = Buffer.from(
  "DACH",
  "ascii",
).toString("hex");

export const DA_AVAILABILITY_TRANCHE_ASSET_NAME_PREFIX = Buffer.from(
  "DT",
  "ascii",
).toString("hex");

export const DA_AVAILABILITY_TERMINAL_ACCUMULATOR_ASSET_NAME_PREFIX =
  Buffer.from("DACT", "ascii").toString("hex");

export const CHALLENGE_ASSET_NAME = new RegExp(
  `^${DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX}[0-9a-f]{56}$`,
  "u",
);

export const DaAvailabilityTrancheDescriptorSchema = Data.Object({
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
  chunk_count: Data.Integer(),
  chunk_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

export type DaAvailabilityTrancheDescriptor = Data.Static<
  typeof DaAvailabilityTrancheDescriptorSchema
>;

export const DaAvailabilityTrancheDescriptor =
  asDataType<DaAvailabilityTrancheDescriptor>(
    DaAvailabilityTrancheDescriptorSchema,
  );

/**
 * Release-bound response geometry. The applied response-transaction
 * measurement selects these values in authenticated deployment/DA parameters;
 * the wire schema does not turn the first 4 MiB/4 KiB sizing probe into
 * protocol law.
 */
export const DaAvailabilityResponseGeometrySchema = Data.Object({
  chunk_byte_length: Data.Integer(),
  tranche_byte_length: Data.Integer(),
  max_tranche_count: Data.Integer(),
});

export type DaAvailabilityResponseGeometry = Data.Static<
  typeof DaAvailabilityResponseGeometrySchema
>;

export const DaAvailabilityResponseGeometry =
  asDataType<DaAvailabilityResponseGeometry>(
    DaAvailabilityResponseGeometrySchema,
  );

/**
 * Authenticated release/DA parameters selected after applied response-cost
 * measurement. Field order is the on-chain `ParametersV1` constructor layout.
 * The DA bond, slash penalty, minimum top-up, pool floor and challenge-record
 * lovelace are the selected profile's `da_bond` amounts; the challenger bond
 * is independent deployment data and the only bond the fee reserve binds.
 */
export const DaAvailabilityParametersSchema = Data.Object({
  response_geometry: DaAvailabilityResponseGeometrySchema,
  da_bond_lovelace: Data.Integer(),
  challenger_bond_lovelace: Data.Integer(),
  max_open_fee_lovelace: Data.Integer(),
  max_publication_fee_lovelace: Data.Integer(),
  max_settlement_fee_lovelace: Data.Integer(),
  max_close_fee_lovelace: Data.Integer(),
  max_timeout_fee_lovelace: Data.Integer(),
  da_slash_penalty_lovelace: Data.Integer(),
  da_bond_min_top_up_lovelace: Data.Integer(),
  da_bond_pool_floor_lovelace: Data.Integer(),
  challenge_record_lovelace: Data.Integer(),
});

export type DaAvailabilityParameters = Data.Static<
  typeof DaAvailabilityParametersSchema
>;

export const DaAvailabilityParameters = asDataType<DaAvailabilityParameters>(
  DaAvailabilityParametersSchema,
);

export const DaAvailabilityCommitmentSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  payload_byte_length: Data.Integer(),
  response_geometry: DaAvailabilityResponseGeometrySchema,
  tranche_descriptors: Data.Array(DaAvailabilityTrancheDescriptorSchema),
});

export type DaAvailabilityCommitment = Data.Static<
  typeof DaAvailabilityCommitmentSchema
>;

export const DaAvailabilityCommitment = asDataType<DaAvailabilityCommitment>(
  DaAvailabilityCommitmentSchema,
);

export const DaAvailabilityTrancheStartSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  start_offset: Data.Integer(),
  byte_length: Data.Integer(),
});

export const DaAvailabilityTrancheStepSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  tranche_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

export const DaAvailabilityTerminalAccumulatorStartSchema = Data.Object({
  version: Data.Integer(),
  deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
  header_hash: HeaderHashSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

export const DaAvailabilityChunkLeafSchema = Data.Object({
  version: Data.Integer(),
  tranche_index: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk_offset: Data.Integer(),
  chunk_byte_length: Data.Integer(),
  chunk_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});

/**
 * The per-challenge record output (Aiken `ChallengeRecordV1`), minted with the
 * DACH token at `OpenChallenge` and holding exactly the deployment's
 * `challenge_record_lovelace`. The commitment is the full signed commitment
 * whose `commitment_hash` the challenged state-queue node carries.
 */
export const DaAvailabilityChallengeRecordSchema = Data.Object({
  commitment: DaAvailabilityCommitmentSchema,
  challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
  challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
  opened_at: Data.Integer(),
  response_deadline: Data.Integer(),
});

export type DaAvailabilityChallengeRecord = Data.Static<
  typeof DaAvailabilityChallengeRecordSchema
>;

export const DaAvailabilityChallengeRecord =
  asDataType<DaAvailabilityChallengeRecord>(
    DaAvailabilityChallengeRecordSchema,
  );

export const DaAvailabilityTrancheDatumSchema = Data.Enum([
  Data.Object({
    Active: Data.Object({
      deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
      header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      descriptor: DaAvailabilityTrancheDescriptorSchema,
      next_offset: Data.Integer(),
      accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
      latest_carrier_output_index: Data.Nullable(Data.Integer()),
      response_deadline: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
  Data.Object({
    Receipt: Data.Object({
      deployment_identity: Data.Bytes({ minLength: 28, maxLength: 28 }),
      header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      descriptor: DaAvailabilityTrancheDescriptorSchema,
      terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
      terminal_carrier_output_index: Data.Integer(),
      challenger: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
]);

export type DaAvailabilityTrancheDatum = Data.Static<
  typeof DaAvailabilityTrancheDatumSchema
>;

export const DaAvailabilityTrancheDatum =
  asDataType<DaAvailabilityTrancheDatum>(DaAvailabilityTrancheDatumSchema);

export const DaAvailabilityTrancheTerminalStatusSchema = Data.Enum([
  Data.Object({
    PublishedTranche: Data.Object({
      terminal_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Object({
    TimedOutTranche: Data.Object({
      next_offset: Data.Integer(),
      partial_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
]);

export type DaAvailabilityTrancheTerminalStatus = Data.Static<
  typeof DaAvailabilityTrancheTerminalStatusSchema
>;

export const DaAvailabilityTrancheTerminalStatus =
  asDataType<DaAvailabilityTrancheTerminalStatus>(
    DaAvailabilityTrancheTerminalStatusSchema,
  );

export const DaAvailabilityTerminalAccumulatorStepSchema = Data.Object({
  version: Data.Integer(),
  previous_accumulator: Data.Bytes({ minLength: 32, maxLength: 32 }),
  tranche_index: Data.Integer(),
  status: DaAvailabilityTrancheTerminalStatusSchema,
});
